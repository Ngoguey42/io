open struct
  module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end

open Misc
module OptiMap = OptiMap

type accumulator = {
  forward : Algodiff.t Fnn.Map.t -> Algodiff.t;
  optimizations : optimization_map;
  pack : unit -> Fnn.network;
}

let _unpack_node01 (net : Fnn.node01) =
  match net#classify_layer with
  | `Input _ ->
      let net = (net :> Fnn.network) in
      let forward inputs =
        Printf.eprintf "forward input\n%!";
        let tensor = Fnn.Map.find net inputs in
        validate_output_tensor net tensor
      in
      let pack () = net in
      { optimizations = OptiMap.empty; forward; pack }
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
      { optimizations = OptiMap.singleton (string_of_int (Oo.id net)) update; forward; pack }

let _unpack_layer11 (net : Fnn.node11) up_forward =
  match net#classify_layer with
  | `Relu _ ->
      let forward inputs =
        Printf.eprintf "forward relu\n%!";
        validate_output_tensor net (Algodiff.Maths.relu (up_forward inputs))
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Softmax net ->
      let tensor_axis = tensor_axis_of_shape_axis net#upstream#out_shape net#axis in
      let forward inputs =
        Printf.eprintf "forward softmax a\n%!";

        let x = up_forward inputs in
        let max =
          x |> Algodiff.primal' |> Algodiff.unpack_arr |> Algodiff.A.max ~axis:tensor_axis
          |> Algodiff.pack_arr
        in
        let x = Algodiff.Maths.sub x max in
        let x = Algodiff.Maths.exp x in
        let deno = Algodiff.Maths.sum ~axis:tensor_axis x in
        let x = Algodiff.Maths.div x deno in
        validate_output_tensor net x
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
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
      let forward inputs =
        Printf.eprintf "forward padding\n%!";
        up_forward inputs |> f |> validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
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
      let forward inputs =
        Printf.eprintf "forward maxpool\n%!";
        let x = up_forward inputs in
        Algodiff.NN.max_pool2d b x kernel_size s |> validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Transpose net ->
      let transpose_axes, dims1_of_dims0 = derive_configuration_of_transpose_layer net in
      if transpose_axes <> [ 0; 1; 2; 3 ] && transpose_axes <> [ 0 ] then (
        Printf.eprintf "Transpose error!\n%!";
        Printf.eprintf "%s\n%!" (Pshape.to_string net#upstream#out_shape);
        Printf.eprintf "%s\n%!" (Pshape.to_string net#out_shape);
        transpose_axes |> List.map string_of_int |> String.concat ", " |> print_endline;
        failwith "Unsupported transpose. Awaiting next owl-base version for `transpose ~axis`" );
      let forward inputs =
        Printf.eprintf "Forward transpose\n%!";
        let x = up_forward inputs in
        let reshape_shape = x |> Algodiff.primal' |> Algodiff.Arr.shape |> dims1_of_dims0 in
        (* Algodiff.Maths.transpose ~axis:transpose_axes x *)
        Algodiff.Maths.reshape x reshape_shape |> validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Normalisation _ -> failwith "Not yet implemented"

(*     let shape = net#upstream#out_shape in *)
(*     let axes = List.map (tensor_axis_of_shape_axis shape) net#axes in *)
(*     let norm_forward, norm_pack = _unpack_normalisation_algorithm axes net#algorithm in *)
(*     let forward inputs = up_forward inputs |> norm_forward |> validate_output_tensor net in *)
(*     let copy = function *)
(*       | [ upstream ] -> (net#replicate (norm_pack ()) upstream :> Fnn.network) *)
(*       | _ -> failwith "unreachable: Normalisation.pack takes only 1 input" *)
(*     in *)
(*     (forward, copy) *)

let _unpack_layer21 (net : Fnn.node21) up0_forward up1_forward =
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
      (* let d = net#dilation in *)
      let s =
        let sy, sx = net#stride in
        [| sy; sx |]
      in
      let forward inputs =
        Printf.eprintf "forward conv2d\n%!";
        Algodiff.NN.conv2d ~padding:b (up1_forward inputs) (up0_forward inputs) s
        |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Tensordot net ->
      let mapping, perm = derive_configuration_of_tensordot_layer net in
      if mapping <> [ (1, 0) ] then failwith "Tensordot only supported as dot";
      if perm <> [ 0; 1 ] then
        failwith
          "Unsupported transpose in tensordot. Awaiting next owl-base version for `transpose ~axis`";

      let forward inputs =
        Printf.eprintf "forward tensordot\n%!";
        Algodiff.Maths.dot (up0_forward inputs) (up1_forward inputs)
        (* |> Tfjs_api.Ops.transpose ~perm *)
        |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)

let _unpack_layern1 (net : Fnn.noden1) up_forwards =
  match net#classify_layer with
  | `Sum net ->
      let forward inputs =
        Printf.eprintf "forward sum\n%!";
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl ->
              let x = Algodiff.Maths.add x x' in
              aux (x :: tl)
        in
        List.map (fun fn -> fn inputs) up_forwards |> aux |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Prod net ->
      Printf.eprintf "forward prod\n%!";
      let forward inputs =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Algodiff.Maths.mul x x' :: tl)
        in
        List.map (fun fn -> fn inputs) up_forwards |> aux |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Concatenate net ->
      let axis = tensor_axis_of_shape_axis net#out_shape net#axis in
      let forward inputs =
        Printf.eprintf "forward concat\n%!";
        List.map (fun fn -> fn inputs) up_forwards
        |> Array.of_list |> Algodiff.Maths.concatenate ~axis |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)

let _unpack_node follow (net : Fnn.network) =
  match (net#classify_node, List.map follow net#upstreams) with
  | `Node01 net, [] -> _unpack_node01 net
  | `Node11 net, [ up_acc ] ->
      let forward, pack = _unpack_layer11 net up_acc.forward in
      let pack () = (pack [ up_acc.pack () ] :> Fnn.network) in
      { up_acc with forward; pack }
  | `Node21 net, [ up0_acc; up1_acc ] ->
      let forward, pack = _unpack_layer21 net up0_acc.forward up1_acc.forward in
      let pack () = (pack [ up0_acc.pack (); up1_acc.pack () ] :> Fnn.network) in
      let optimizations = OptiMap.union_silent up0_acc.optimizations up1_acc.optimizations in
      { forward; optimizations; pack }
  | `Noden1 net, up_accs ->
      let up_forwards = List.map (fun acc -> acc.forward) up_accs in
      let up_packs = List.map (fun acc -> acc.pack) up_accs in
      let up_optimizations = List.map (fun acc -> acc.optimizations) up_accs in
      let optimizations = OptiMap.union_list_silent up_optimizations in
      let forward, pack = _unpack_layern1 net up_forwards in
      let pack () = List.map (fun pack -> pack ()) up_packs |> pack in
      { forward; optimizations; pack }
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
  let { forward; optimizations; pack } = Fnn.memoized_walk _unpack_node net in
  (forward, optimizations, pack)
