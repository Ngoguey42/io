open struct
  module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end

open Misc
module OptiMap = OptiMap

type tensor = Algodiff.t

type tensormap = tensor Ocann.Map.t

type unpacked_network =
  | Node01 of {
      node : Ocann.network;
      forward : tensormap -> tensor;
      update : (float -> unit) option;
      pack : unit -> Ocann.network;
    }
  | Node11 of {
      node : Ocann.network;
      up : unpacked_network;
      forward : tensor -> tensor;
      pack : Ocann.network -> Ocann.network;
    }
  | Node21 of {
      node : Ocann.network;
      up0 : unpacked_network;
      up1 : unpacked_network;
      forward : tensor -> tensor -> tensor;
      pack : Ocann.network -> Ocann.network -> Ocann.network;
    }
  | Noden1 of {
      node : Ocann.network;
      ups : unpacked_network list;
      forward : tensor list -> tensor;
      pack : Ocann.network list -> Ocann.network;
    }

let _unpack_layer01 (net : Ocann.node01) =
  match net#classify_layer with
  | `Input _ ->
      let net = (net :> Ocann.network) in
      let forward inputs =
        let tensor = Ocann.Map.find net inputs in
        validate_output_tensor net tensor
      in
      let pack () = net in
      (forward, pack, None)
  | `Parameter32 net ->
      let var = ref (net#tensor |> Algodiff.pack_arr |> Fun.flip Algodiff.make_reverse 42) in
      let forward _ = !var |> validate_output_tensor net in
      let update, opti_pack = _unpack_optimizer net var in
      let pack () =
        let optimizer = opti_pack () in
        let w = !var |> Algodiff.primal' |> Algodiff.unpack_arr in
        (net#replicate ~id:net#id w optimizer :> Ocann.network)
      in
      (forward, pack, Some update)

let _unpack_layer11 (net : Ocann.node11) =
  match net#classify_layer with
  | `Relu _ ->
      let forward up = validate_output_tensor net (Algodiff.Maths.relu up) in
      let pack up = (net :> Ocann.network)#copy [ up ] in
      (forward, pack)
  | `Softmax net ->
      let tensor_axis = tensor_axis_of_shape_axis net#upstream#out_shape net#axis in
      let forward up =
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
      let pack up = (net :> Ocann.network)#copy [ up ] in
      (forward, pack)
  | `Astype _ -> failwith "`Astype is unsupported with Owl backend"
  | `Padding net ->
      if net#is_mixed then failwith "Mixed paddings not yet implemented";
      let value =
        match net#value with
        | `Constant v -> v
        | `Reflection -> failwith "Reflection padding not implemented"
        | `Replication -> failwith "Replication padding not implemented"
      in
      let f =
        let axes = axes_of_shape net#upstream#out_shape in
        if net#is_padding then
          let paddings_per_axis =
            List.map
              (fun ax ->
                let bef, aft = net#paddings_of_axis ax in
                [ bef; aft ])
              axes
          in
          Algodiff.NN.pad ~v:value paddings_per_axis
        else fun x ->
          let shape = x |> Algodiff.primal' |> Algodiff.Arr.shape in
          let per_axis =
            List.mapi
              (fun i ax ->
                let bef, aft = net#paddings_of_axis ax in
                let size = shape.(i) in
                if bef = 0 && aft = 0 then [] else [ abs bef; size + bef + aft ])
              axes
          in
          Algodiff.Maths.get_slice per_axis x
      in
      let forward up = up |> f |> validate_output_tensor net in
      let pack up = (net :> Ocann.network)#copy [ up ] in
      (forward, pack)
  | `Maxpool2d net ->
      let b =
        match net#boundary_mode with
        | `Same -> Owl_types.SAME
        | `Valid -> Owl_types.VALID
        | `Assert_fit -> Owl_types.VALID
        | `Pad_fit -> failwith "`Pad_fit in maxpool2d not implemented"
      in
      let kernel_size =
        let ky, kx = net#kernel_size in
        [| ky; kx |]
      in
      let s =
        let sy, sx = net#stride in
        [| sy; sx |]
      in
      let forward up = Algodiff.NN.max_pool2d b up kernel_size s |> validate_output_tensor net in
      let pack up = (net :> Ocann.network)#copy [ up ] in
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
        let reshape_shape = up |> Algodiff.primal' |> Algodiff.Arr.shape |> dims1_of_dims0 in
        (* Algodiff.Maths.transpose ~axis:transpose_axes x *)
        Algodiff.Maths.reshape up reshape_shape |> validate_output_tensor net
      in
      let pack up = (net :> Ocann.network)#copy [ up ] in
      (forward, pack)
  | `Normalisation _ -> failwith "Not yet implemented"

let _unpack_layer21 (net : Ocann.node21) =
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
      let forward up0 up1 = Algodiff.NN.conv2d ~padding:b up1 up0 s |> validate_output_tensor net in
      let pack up0 up1 = (net :> Ocann.network)#copy [ up0; up1 ] in
      (forward, pack)
  | `Tensordot net ->
      let mapping, perm = derive_configuration_of_tensordot_layer net in
      if mapping <> [ (1, 0) ] then failwith "Tensordot only supported as dot";
      if perm <> [ 0; 1 ] then
        failwith
          "Unsupported transpose in tensordot. Awaiting next owl-base version for `transpose ~axis`";

      let forward up0 up1 =
        Algodiff.Maths.dot up0 up1
        (* |> Tfjs_api.Ops.transpose ~perm *)
        |> validate_output_tensor net
      in
      let pack up0 up1 = (net :> Ocann.network)#copy [ up0; up1 ] in
      (forward, pack)

let _unpack_layern1 (net : Ocann.noden1) =
  match net#classify_layer with
  | `Sum net ->
      let forward ups =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl ->
              let x = Algodiff.Maths.add x x' in
              aux (x :: tl)
        in
        ups |> aux |> validate_output_tensor net
      in
      (forward, (net :> Ocann.network)#copy)
  | `Prod net ->
      let forward ups =
        let rec aux = function
          | [] -> failwith "unreachable (prod layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Algodiff.Maths.mul x x' :: tl)
        in
        ups |> aux |> validate_output_tensor net
      in
      (forward, (net :> Ocann.network)#copy)
  | `Concatenate net ->
      let axis = tensor_axis_of_shape_axis net#out_shape net#axis in
      let forward ups =
        ups |> Array.of_list |> Algodiff.Maths.concatenate ~axis |> validate_output_tensor net
      in
      (forward, (net :> Ocann.network)#copy)

let _unpack_node follow (net : Ocann.network) =
  match (net#classify_node, List.map follow net#upstreams) with
  | `Node01 node, [] ->
      let forward, pack, update = _unpack_layer01 node in
      Node01 { forward; pack; update; node = (node :> Ocann.network) }
  | `Node11 node, [ up ] ->
      let forward, pack = _unpack_layer11 node in
      Node11 { forward; pack; node = (node :> Ocann.network); up }
  | `Node21 node, [ up0; up1 ] ->
      let forward, pack = _unpack_layer21 node in
      Node21 { forward; pack; node = (node :> Ocann.network); up0; up1 }
  | `Noden1 node, ups ->
      let forward, pack = _unpack_layern1 node in
      Noden1 { forward; pack; node = (node :> Ocann.network); ups }
  | _, _ -> failwith "Corrupted network. A node has an unexpected number of upstream parents"

let unpack_for_training :
    Ocann.network -> (tensormap -> tensor) * optimization_map * (unit -> Ocann.network) =
 (* Transform a `network` to everything that is needed to perform a training of that network
  * using Owl algodiff ndarray:
  * 1. A callable for the forward pass aware of the upcoming backward pass
  * 2. A map of callbacks that each update a weight tensor given its gradient
  * 3. A thunk to be called to pack everything back to a `network` when done with training
  *)
 fun net ->
  let unet = Ocann.memoized_walk_obj _unpack_node net in
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
    Ocann.memoized_walk id_of_unet forward_node unet
  in
  let pack () =
    let pack_node follow = function
      | Node01 v -> v.pack ()
      | Node11 v -> v.pack (follow v.up)
      | Node21 v -> v.pack (follow v.up0) (follow v.up1)
      | Noden1 v -> v.pack (List.map follow v.ups)
    in
    Ocann.memoized_walk id_of_unet pack_node unet
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
    Ocann.memoized_walk id_of_unet optimizations_of_node unet
  in
  (forward, optimizations, pack)
