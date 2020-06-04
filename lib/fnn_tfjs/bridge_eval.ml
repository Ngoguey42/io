open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Typed_array = Js_of_ocaml.Typed_array
end

open Misc

type unpacked_network =
  | Node01 of { node : Fnn.network; forward : tftensor Fnn.Map.t -> tftensor }
  | Node11 of { node : Fnn.network; up : unpacked_network; forward : tftensor -> tftensor }
  | Node21 of {
      node : Fnn.network;
      up0 : unpacked_network;
      up1 : unpacked_network;
      forward : tftensor -> tftensor -> tftensor;
    }
  | Noden1 of {
      node : Fnn.network;
      ups : unpacked_network list;
      forward : tftensor list -> tftensor;
    }

let _unpack_normalisation_algorithm axes = function
  | `Local epsilon ->
      let normaliser = Tfjs_api.create_local_normaliser epsilon axes in
      let forward = normaliser#normalise in
      forward
  | `Global32 (epsilon, step, avg, var) ->
      let normaliser = Tfjs_api.create_global_normaliser Tfjs_api.Eval epsilon step avg var axes in
      let forward = normaliser#normalise in
      forward
  | `Exp_moving32 (epsilon, momentum, avg, var) ->
      let normaliser =
        Tfjs_api.create_exp_moving32_normaliser Tfjs_api.Eval epsilon momentum avg var axes
      in
      let forward = normaliser#normalise in
      forward
  | `Global64 _ -> failwith "Unhandled float64 normalisation"
  | `Exp_moving64 _ -> failwith "Unhandled float64 normalisation"

let _unpack_layer01 (net : Fnn.node01) =
  match net#classify_layer with
  | `Input _ ->
      let net = (net :> Fnn.network) in
      let forward inputs =
        let tensor = Fnn.Map.find net inputs in
        validate_output_tensor net tensor
      in
      forward
  | `Parameter32 net ->
      let tensor = Tfjs_api.tensor_of_ba net#tensor in
      let forward _ = validate_output_tensor net tensor in
      forward

let _unpack_layer11 (net : Fnn.node11) =
  match net#classify_layer with
  | `Relu _ ->
      let forward up = validate_output_tensor net (Tfjs_api.Ops.relu up) in
      forward
  | `Softmax net ->
      let tensor_axis = tensor_axis_of_shape_axis net#upstream#out_shape net#axis in
      let forward up = validate_output_tensor net (Tfjs_api.Ops.softmax tensor_axis up) in
      forward
  | `Astype net ->
      let dtype = tfdtype_of_dtype net#dtype in
      let forward up = up |> Tfjs_api.Ops.astype dtype |> validate_output_tensor net in
      forward
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
            List.mapi
              (fun i ax ->
                let bef, aft = net#paddings_of_axis ax in
                (i, [ bef; aft ]))
              axes
          in
          Tfjs_api.Ops.pad ~value paddings_per_axis
        else fun x ->
          let shape = x##.shape |> Js.to_array in
          let per_axis =
            List.mapi
              (fun i ax ->
                let bef, aft = net#paddings_of_axis ax in
                let size = shape.(i) in
                (i, abs bef, size + bef + aft))
              axes
          in
          Tfjs_api.Ops.slice per_axis x
      in
      let forward up = up |> f |> validate_output_tensor net in
      forward
  | `Maxpool2d net ->
      let b =
        match net#boundary_mode with
        | `Same -> `Same
        | `Valid -> `Valid
        | `Assert_fit -> `Valid
        | `Pad_fit -> failwith "not implemented"
      in
      let kernel_size = net#kernel_size in
      let s = net#stride in
      let forward up =
        up |> Tfjs_api.Ops.maxpool2d ~s ~b kernel_size |> validate_output_tensor net
      in
      forward
  | `Transpose net ->
      let tftranspose_axes, dims1_of_dims0 = derive_configuration_of_transpose_layer net in
      let forward up =
        let tfreshape_shape = up##.shape |> Js.to_array |> dims1_of_dims0 in
        Tfjs_api.Ops.transpose ~perm:tftranspose_axes up
        |> Tfjs_api.Ops.reshape tfreshape_shape
        |> validate_output_tensor net
      in
      forward
  | `Normalisation net ->
      let shape = net#upstream#out_shape in
      let axes = List.map (tensor_axis_of_shape_axis shape) net#axes in
      let norm_forward = _unpack_normalisation_algorithm axes net#algorithm in
      let forward up = up |> norm_forward |> validate_output_tensor net in
      forward

let _unpack_layer21 (net : Fnn.node21) =
  match net#classify_layer with
  | `Conv2d net ->
      if net#is_grouped && not net#is_depthwise then
        failwith "Grouped conv2d not implemented by tfjs";
      if net#is_dilated then failwith "Dilated conv2d backward not implemented by tfjs";
      let b =
        match net#boundary_mode with
        | `Same -> `Same
        | `Valid -> `Valid
        | `Assert_fit -> `Valid
        | `Pad_fit -> failwith "Conv2d ~b:`Pad_fit not implemented"
      in
      let d = net#dilation in
      let s = net#stride in
      let f = if net#is_depthwise then Tfjs_api.Ops.depthwise_conv2d else Tfjs_api.Ops.conv2d in
      let forward up0 up1 = f ~b ~d ~s up1 up0 |> validate_output_tensor net in
      forward
  | `Tensordot net ->
      let mapping, perm = derive_configuration_of_tensordot_layer net in
      let forward up0 up1 =
        Tfjs_api.Ops.tensordot mapping up0 up1
        |> Tfjs_api.Ops.transpose ~perm |> validate_output_tensor net
      in
      forward

let _unpack_layern1 (net : Fnn.noden1) =
  match net#classify_layer with
  | `Sum net ->
      (* Could also be implemented with `tf.broadcast` and `tf.addN` *)
      let forward ups =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Tfjs_api.Ops.add x x' :: tl)
        in
        ups |> aux |> validate_output_tensor net
      in
      forward
  | `Prod net ->
      let forward ups =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Tfjs_api.Ops.mul x x' :: tl)
        in
        ups |> aux |> validate_output_tensor net
      in
      forward
  | `Concatenate net ->
      let axis = tensor_axis_of_shape_axis net#out_shape net#axis in
      let forward ups = ups |> Tfjs_api.Ops.concat axis |> validate_output_tensor net in
      forward

let _unpack_node follow (net : Fnn.network) =
  match (net#classify_node, List.map follow net#upstreams) with
  | `Node01 node, [] ->
      let forward = _unpack_layer01 node in
      Node01 { forward; node = (node :> Fnn.network) }
  | `Node11 node, [ up ] ->
      let forward = _unpack_layer11 node in
      Node11 { forward; node = (node :> Fnn.network); up }
  | `Node21 node, [ up0; up1 ] ->
      let forward = _unpack_layer21 node in
      Node21 { forward; node = (node :> Fnn.network); up0; up1 }
  | `Noden1 node, ups ->
      let forward = _unpack_layern1 node in
      Noden1 { forward; node = (node :> Fnn.network); ups }
  | _, _ -> failwith "Corrupted network. A node has an unexpected number of upstream parents"

let unpack_for_evaluation : Fnn.network -> tftensor Fnn.Map.t -> tftensor =
 (* Transform a `network` to everything that is needed to perform an evaluation with that network
  * using tensorflow.js:
  * 1. A callable for the forward pass
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
  forward
