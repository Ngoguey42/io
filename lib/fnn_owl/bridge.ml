open struct
  module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end

open Misc
module OptiMap = OptiMap

type tensor = Algodiff.t

type tensormap = Algodiff.t Fnn.Map.t

type unpacked_network =
  | Node01 of {
      node : Fnn.network;
      forward : tensormap -> tensor;
      update : (float -> unit) option;
      pack : unit -> Fnn.network;
    }
  | Node11 of {
      node : Fnn.network;
      up : unpacked_network;
      forward : Algodiff.t -> Algodiff.t;
      pack : Fnn.network -> Fnn.network;
    }
  | Node21 of {
      node : Fnn.network;
      up0 : unpacked_network;
      up1 : unpacked_network;
      forward : Algodiff.t -> Algodiff.t -> Algodiff.t;
      pack : Fnn.network -> Fnn.network -> Fnn.network;
    }
  | Noden1 of {
      node : Fnn.network;
      ups : unpacked_network list;
      forward : Algodiff.t list -> Algodiff.t;
      pack : Fnn.network list -> Fnn.network;
    }

type accumulator = {
  forward : Algodiff.t Fnn.Map.t -> Algodiff.t;
  optimizations : optimization_map;
  pack : unit -> Fnn.network;
}

let _unpack_layer01 (net : Fnn.node01) =
  match net#classify_layer with
  | `Input _ ->
      let net = (net :> Fnn.network) in
      let forward inputs =
        Printf.eprintf "forward input\n%!";
        let tensor = Fnn.Map.find net inputs in
        validate_output_tensor net tensor
      in
      let pack () = net in
      forward, pack, None
  | `Parameter32 net ->
      let var = ref (net#tensor |> Algodiff.pack_arr |> Fun.flip Algodiff.make_reverse 42) in
      let forward _ =
        Printf.eprintf "forward param\n%!";
        !var |> validate_output_tensor net
      in
      let update, opti_pack = _unpack_optimizer net var in
      let pack () =
        let optimizer = opti_pack () in
        let w = !var |> Algodiff.primal' |> Algodiff.unpack_arr in
        (net#replicate ~id:net#id w optimizer :> Fnn.network)
      in
      forward, pack, Some update

let _unpack_layer11 (net : Fnn.node11) =
  match net#classify_layer with
  | `Relu _ ->
      let forward up =
        Printf.eprintf "forward relu\n%!";
        validate_output_tensor net (Algodiff.Maths.relu up)
      in
      let pack up = (net :> Fnn.network)#copy [ up ] in
      (forward, pack)
  | `Softmax net ->
      let tensor_axis = tensor_axis_of_shape_axis net#upstream#out_shape net#axis in
      let forward up =
        Printf.eprintf "forward softmax a\n%!";
        let max =
          up |> Algodiff.primal' |> Algodiff.unpack_arr |> Algodiff.A.max ~axis:tensor_axis
          |> Algodiff.pack_arr
        in
        let x = Algodiff.Maths.sub up max in
        let x = Algodiff.Maths.exp x in
        let deno = Algodiff.Maths.sum ~axis:tensor_axis x in
        let x = Algodiff.Maths.div x deno in
        validate_output_tensor net x
      in
      let pack up = (net :> Fnn.network)#copy [ up ] in
      (forward, pack)
  | `Astype _ -> failwith "`Astype is unsupported with Owl backend"
  | `Padding net ->
      (* if net#is_mixed then failwith "Mixed paddings not yet implemented"; *)
      let value =
        match net#value with
        | `Constant v -> v
        | `Reflection -> failwith "Reflection padding not implemented"
        | `Replication -> failwith "Replication padding not implemented"
      in
      let f =
        let axes = axes_of_shape net#upstream#out_shape in
        (* TODO: does cropping work? *)
        (* if net#is_padding then *)
        let paddings_per_axis =
          List.map
            (fun ax ->
              let bef, aft = net#paddings_of_axis ax in
              [ bef; aft ])
            axes
        in
        Algodiff.NN.pad ~v:value paddings_per_axis
        (* else fun x -> *)
        (*   let shape = x##.shape |> Js.to_array in *)
        (*   let per_axis = *)
        (*     List.mapi *)
        (*       (fun i ax -> *)
        (*         let bef, aft = net#paddings_of_axis ax in *)
        (*         let size = shape.(i) in *)
        (*         (i, abs bef, size + bef + aft)) *)
        (*       axes *)
        (*   in *)
        (*   Tfjs_api.Ops.slice per_axis x *)
      in
      let forward up =
        Printf.eprintf "forward padding\n%!";
        up |> f |> validate_output_tensor net
      in
      let pack up = (net :> Fnn.network)#copy [ up ] in
      (forward, pack)
  | `Maxpool2d net ->
      let b =
        match net#boundary_mode with
        | `Same -> Owl_types.SAME
        | `Valid -> Owl_types.VALID
        | `Assert_fit -> Owl_types.VALID
        | `Pad_fit -> failwith "not implemented"
      in
      let kernel_size =
        let ky, kx = net#kernel_size in
        [| ky; kx |]
      in
      let s =
        let sy, sx = net#stride in
        [| sy; sx |]
      in
      let forward up =
        Printf.eprintf "forward maxpool\n%!";
        Algodiff.NN.max_pool2d b up kernel_size s |> validate_output_tensor net
      in
      let pack up = (net :> Fnn.network)#copy [ up ] in
      (forward, pack)
  | `Transpose net ->
      let transpose_axes, dims1_of_dims0 = derive_configuration_of_transpose_layer net in
      if transpose_axes <> [ 0; 1; 2; 3 ] && transpose_axes <> [ 0 ] then (
        Printf.eprintf "Transpose error!\n%!";
        Printf.eprintf "%s\n%!" (Pshape.to_string net#upstream#out_shape);
        Printf.eprintf "%s\n%!" (Pshape.to_string net#out_shape);
        transpose_axes |> List.map string_of_int |> String.concat ", " |> print_endline;
        failwith "Unsupported transpose. Awaiting next owl-base version for `transpose ~axis`" );
      let forward up =
        Printf.eprintf "Forward transpose\n%!";
        let reshape_shape = up |> Algodiff.primal' |> Algodiff.Arr.shape |> dims1_of_dims0 in
        (* Algodiff.Maths.transpose ~axis:transpose_axes x *)
        Algodiff.Maths.reshape up reshape_shape |> validate_output_tensor net
      in
      let pack up = (net :> Fnn.network)#copy [ up ] in
      (forward, pack)
  | `Normalisation _ -> failwith "Not yet implemented"

let _unpack_layer21 (net : Fnn.node21) =
  match net#classify_layer with
  | `Conv2d net ->
      if net#is_grouped then failwith "Grouped conv2d not implemented";
      if net#is_dilated then failwith "Dilated conv2d not implemented";
      let b =
        match net#boundary_mode with
        | `Same -> Owl_types.SAME
        | `Valid -> Owl_types.VALID
        | `Assert_fit -> Owl_types.VALID
        | `Pad_fit -> failwith "Conv2d ~b:`Pad_fit not implemented"
      in
      let s =
        let sy, sx = net#stride in
        [| sy; sx |]
      in
      let forward up0 up1 =
        Printf.eprintf "forward conv2d\n%!";
        Algodiff.NN.conv2d ~padding:b up1 up0 s
        |> validate_output_tensor net
      in
      let pack up0 up1 = (net :> Fnn.network)#copy [ up0; up1 ] in
      (forward, pack)
  | `Tensordot net ->
      let mapping, perm = derive_configuration_of_tensordot_layer net in
      if mapping <> [ (1, 0) ] then failwith "Tensordot only supported as dot";
      if perm <> [ 0; 1 ] then
        failwith
          "Unsupported transpose in tensordot. Awaiting next owl-base version for `transpose ~axis`";

      let forward up0 up1 =
        Printf.eprintf "forward tensordot\n%!";
        Algodiff.Maths.dot up0 up1
        (* |> Tfjs_api.Ops.transpose ~perm *)
        |> validate_output_tensor net
      in
      let pack up0 up1 = (net :> Fnn.network)#copy [ up0; up1 ] in
      (forward, pack)

let _unpack_layern1 (net : Fnn.noden1) =
  match net#classify_layer with
  | `Sum net ->
      let forward ups =
        Printf.eprintf "forward sum\n%!";
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl ->
              let x = Algodiff.Maths.add x x' in
              aux (x :: tl)
        in
        ups |> aux |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Prod net ->
      Printf.eprintf "forward prod\n%!";
      let forward ups =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Algodiff.Maths.mul x x' :: tl)
        in
        ups |> aux |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Concatenate net ->
      let axis = tensor_axis_of_shape_axis net#out_shape net#axis in
      let forward ups =
        Printf.eprintf "forward concat\n%!";
        ups
        |> Array.of_list |> Algodiff.Maths.concatenate ~axis |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)

let _unpack_node follow (net : Fnn.network) =
  match (net#classify_node, List.map follow net#upstreams) with
  | `Node01 node, [] ->
      let forward, pack, update = _unpack_layer01 node in
      Node01 { forward; pack; update; node = (node :> Fnn.network) }
  | `Node11 node, [ up ] ->
      let forward, pack = _unpack_layer11 node in
      Node11 { forward; pack; node = (node :> Fnn.network); up }
  | `Node21 node, [ up0; up1 ] ->
      let forward, pack = _unpack_layer21 node in
      Node21 { forward; pack; node = (node :> Fnn.network); up0; up1 }
  | `Noden1 node, ups ->
      let forward, pack = _unpack_layern1 node in
      Noden1 { forward; pack; node = (node :> Fnn.network); ups }
  | _, _ -> failwith "Corrupted network. A node has an unexpected number of upstream parents"

let unpack_for_training :
    Fnn.network -> (Algodiff.t Fnn.Map.t -> Algodiff.t) * optimization_map * (unit -> Fnn.network) =
 (* Transform a `network` to everything that is needed to perform a training of that network
  * using Owl algodiff ndarray:
  * 1. A callable for the forward pass aware of the upcoming backward pass
  * 2. A map of callbacks that each update a weight tensor given its gradient
  * 3. A thunk to be called to pack everything back to a `network` when done with training
  *)
 fun net ->
  let unet = Fnn.memoized_walk_obj _unpack_node net in
  let id_of_unet = function
    | Node01 v -> Oo.id v.node
    | Node11 v -> Oo.id v.node
    | Node21 v -> Oo.id v.node
    | Noden1 v -> Oo.id v.node
  in
  let forward inputs =
    let forward_node follow = function
      | Node01 v -> v.forward inputs
      | Node11 v -> v.forward (follow v.up)
      | Node21 v -> v.forward (follow v.up0) (follow v.up1)
      | Noden1 v -> v.forward (List.map follow v.ups)
    in
    Fnn.memoized_walk id_of_unet forward_node unet
  in
  let pack () =
    let pack_node follow = function
      | Node01 v -> v.pack ()
      | Node11 v -> v.pack (follow v.up)
      | Node21 v -> v.pack (follow v.up0) (follow v.up1)
      | Noden1 v -> v.pack (List.map follow v.ups)
    in
    Fnn.memoized_walk id_of_unet pack_node unet
  in
  let optimizations =
    let optimizations_of_node follow = function
      | Node01 { update = None; _ } -> OptiMap.empty
      | Node01 { update = Some update; node; _ } ->
          OptiMap.singleton (string_of_int (Oo.id node)) update
      | Node11 v -> follow v.up
      | Node21 v -> OptiMap.union_silent (follow v.up0) (follow v.up1)
      | Noden1 v -> List.map follow v.ups |> OptiMap.union_list_silent
    in
    Fnn.memoized_walk id_of_unet optimizations_of_node unet
  in
  (forward, optimizations, pack)
