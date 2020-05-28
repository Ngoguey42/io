open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Typed_array = Js_of_ocaml.Typed_array
end

open Misc
module OptiMap = OptiMap

type accumulator = {
  forward : tftensor Fnn.Map.t -> tftensor;
  optimizations : optimization_map;
  pack : unit -> Fnn.network;
}

let _unpack_normalisation_algorithm axes = function
  | `Local epsilon ->
      let normaliser = Tfjs_api.create_local_normaliser epsilon axes in
      let forward = normaliser#normalise in
      let pack () = `Local epsilon in
      (forward, pack)
  | `Global32 (epsilon, step, avg, var) ->
      let normaliser = Tfjs_api.create_global_normaliser Tfjs_api.Train epsilon step avg var axes in
      let forward = normaliser#normalise in
      let pack () =
        `Global32 (epsilon, normaliser#get_step, normaliser#get_avg, normaliser#get_var)
      in
      (forward, pack)
  | `Exp_moving32 (epsilon, momentum, avg, var) ->
      let normaliser =
        Tfjs_api.create_exp_moving32_normaliser Tfjs_api.Train epsilon momentum avg var axes
      in
      let forward = normaliser#normalise in
      let pack () = `Exp_moving32 (epsilon, momentum, normaliser#get_avg, normaliser#get_var) in
      (forward, pack)
  | `Global64 _ -> failwith "Unhandled float64 normalisation"
  | `Exp_moving64 _ -> failwith "Unhandled float64 normalisation"

let _unpack_optimizer optim var =
  match optim with
  | `Sgd ->
      let updater = Tfjs_api.create_sgd_updater var in
      let pack () = `Sgd in
      (updater#update, pack)
  | `Adam (epsilon, beta1, beta2, step, rgrad, rgrad_sq) ->
      let updater = Tfjs_api.create_adam32_updater epsilon beta1 beta2 step rgrad rgrad_sq var in
      let pack () =
        `Adam (epsilon, beta1, beta2, updater#get_step, updater#get_rgrad, updater#get_rgrad_sq)
      in
      (updater#update, pack)

let _unpack_node01 (net : Fnn.node01) =
  match net#classify_layer with
  | `Input _ ->
      let net = (net :> Fnn.network) in
      let forward inputs =
        let tensor = Fnn.Map.find net inputs in
        validate_output_tensor net tensor
      in
      let pack () = net in
      { optimizations = OptiMap.empty; forward; pack }
  | `Parameter32 net ->
      let var =
        Tfjs_api.tensor_of_ba net#tensor
        |> Tfjs_api.variable ~name:(string_of_int (Oo.id net)) ~trainable:true
      in
      let forward _ = validate_output_tensor net var in
      let update, opti_pack = _unpack_optimizer net#optimizer var in
      let pack () =
        let tensor = Tfjs_api.ba_of_tensor Bigarray.Float32 var in
        let optimizer = opti_pack () in
        (net#replicate ~id:net#id tensor optimizer :> Fnn.network)
      in
      { optimizations = OptiMap.singleton (string_of_int (Oo.id net)) update; forward; pack }

let _unpack_layer11 (net : Fnn.node11) up_forward =
  match net#classify_layer with
  | `Relu _ ->
      let forward inputs = validate_output_tensor net (Tfjs_api.Ops.relu (up_forward inputs)) in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Softmax net ->
      let tensor_axis = tensor_axis_of_shape_axis net#upstream#out_shape net#axis in
      let forward inputs =
        validate_output_tensor net (Tfjs_api.Ops.softmax tensor_axis (up_forward inputs))
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Astype net ->
      let dtype = tfdtype_of_dtype net#dtype in
      let forward inputs =
        up_forward inputs |> Tfjs_api.Ops.astype dtype |> validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
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
      let forward inputs = up_forward inputs |> f |> validate_output_tensor net in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
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
      let forward inputs =
        up_forward inputs |> Tfjs_api.Ops.maxpool2d ~s ~b kernel_size |> validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Transpose net ->
      let tftranspose_axes, dims1_of_dims0 = derive_configuration_of_transpose_layer net in
      let forward inputs =
        let x = up_forward inputs in
        let tfreshape_shape = x##.shape |> Js.to_array |> dims1_of_dims0 in
        Tfjs_api.Ops.transpose ~perm:tftranspose_axes x
        |> Tfjs_api.Ops.reshape tfreshape_shape
        |> validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Normalisation net ->
      let shape = net#upstream#out_shape in
      let axes = List.map (tensor_axis_of_shape_axis shape) net#axes in
      let norm_forward, norm_pack = _unpack_normalisation_algorithm axes net#algorithm in
      let forward inputs = up_forward inputs |> norm_forward |> validate_output_tensor net in
      let copy = function
        | [ upstream ] -> (net#replicate (norm_pack ()) upstream :> Fnn.network)
        | _ -> failwith "unreachable: Normalisation.pack takes only 1 input"
      in
      (forward, copy)

let _unpack_layer21 (net : Fnn.node21) up0_forward up1_forward =
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
      let forward inputs =
        f ~b ~d ~s (up1_forward inputs) (up0_forward inputs) |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Tensordot net ->
      let mapping, perm = derive_configuration_of_tensordot_layer net in
      let forward inputs =
        Tfjs_api.Ops.tensordot mapping (up0_forward inputs) (up1_forward inputs)
        |> Tfjs_api.Ops.transpose ~perm |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)

let _unpack_layern1 (net : Fnn.noden1) up_forwards =
  match net#classify_layer with
  | `Sum net ->
      (* Could also be implemented with `tf.broadcast` and `tf.addN` *)
      let forward inputs =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Tfjs_api.Ops.add x x' :: tl)
        in
        List.map (fun fn -> fn inputs) up_forwards |> aux |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Prod net ->
      let forward inputs =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Tfjs_api.Ops.mul x x' :: tl)
        in
        List.map (fun fn -> fn inputs) up_forwards |> aux |> validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Concatenate net ->
      let axis = tensor_axis_of_shape_axis net#out_shape net#axis in
      let forward inputs =
        List.map (fun fn -> fn inputs) up_forwards
        |> Tfjs_api.Ops.concat axis |> validate_output_tensor net
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
    Fnn.network -> (tftensor Fnn.Map.t -> tftensor) * optimization_map * (unit -> Fnn.network) =
 (* Transform a `network` to everything that is needed to perform a training of that network
  * using tensorflow.js:
  * 1. A callable for the forward pass aware of the upcoming backward pass
  * 2. A map of callbacks that each update a weight tensor given its gradient
  * 3. A thunk to be called to pack everything back to a `network` when done with training
  *)
 fun net ->
  let { forward; optimizations; pack } = Fnn.memoized_walk _unpack_node net in
  (forward, optimizations, pack)
