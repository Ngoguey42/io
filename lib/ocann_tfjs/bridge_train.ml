open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Typed_array = Js_of_ocaml.Typed_array
end

module Make (Nn : Ocann.NETWORK) = struct
  open Misc.Make (Nn)

  module OptiMap = OptiMap

  type unpacked_network =
    | Node01 of {
        node : Nn.network;
        forward : tftensor Nn.Map.t -> tftensor;
        pack : unit -> Nn.network;
        update : (float -> tftensor -> unit) option;
      }
    | Node11 of {
        node : Nn.network;
        up : unpacked_network;
        forward : tftensor -> tftensor;
        pack : Nn.network -> Nn.network;
      }
    | Node21 of {
        node : Nn.network;
        up0 : unpacked_network;
        up1 : unpacked_network;
        forward : tftensor -> tftensor -> tftensor;
        pack : Nn.network -> Nn.network -> Nn.network;
      }
    | Noden1 of {
        node : Nn.network;
        ups : unpacked_network list;
        forward : tftensor list -> tftensor;
        pack : Nn.network list -> Nn.network;
      }

  let _unpack_normalisation_algorithm axes = function
    | `Local epsilon ->
        let normaliser = Tfjs_api.create_local_normaliser epsilon axes in
        let forward = normaliser#normalise in
        let pack () = `Local epsilon in
        (forward, pack)
    | `Global32 (epsilon, step, avg, var) ->
        let avg = Nn.Tensor.to_ba avg in
        let var = Nn.Tensor.to_ba var in
        let normaliser =
          Tfjs_api.create_global_normaliser Tfjs_api.Train epsilon step avg var axes
        in
        let forward = normaliser#normalise in
        let pack () =
          let avg = Nn.Tensor.of_ba normaliser#get_avg in
          let var = Nn.Tensor.of_ba normaliser#get_var in
          `Global32 (epsilon, normaliser#get_step, avg, var)
        in
        (forward, pack)
    | `Exp_moving32 (epsilon, momentum, avg, var) ->
        let avg = Nn.Tensor.to_ba avg in
        let var = Nn.Tensor.to_ba var in
        let normaliser =
          Tfjs_api.create_exp_moving32_normaliser Tfjs_api.Train epsilon momentum avg var axes
        in
        let forward = normaliser#normalise in
        let pack () =
          let avg = Nn.Tensor.of_ba normaliser#get_avg in
          let var = Nn.Tensor.of_ba normaliser#get_var in
          `Exp_moving32 (epsilon, momentum, avg, var)
        in
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
        let rgrad = Nn.Tensor.to_ba rgrad in
        let rgrad_sq = Nn.Tensor.to_ba rgrad_sq in

        let updater = Tfjs_api.create_adam32_updater epsilon beta1 beta2 step rgrad rgrad_sq var in
        let pack () =
          let rgrad = Nn.Tensor.of_ba updater#get_rgrad in
          let rgrad_sq = Nn.Tensor.of_ba updater#get_rgrad_sq in
          `Adam (epsilon, beta1, beta2, updater#get_step, rgrad, rgrad_sq)
        in
        (updater#update, pack)

  let _unpack_layer01 (net : Nn.node01) =
    match net#classify_layer with
    | `Input _ ->
        let net = (net :> Nn.network) in
        let forward inputs =
          let tensor = Nn.Map.find net inputs in
          validate_output_tensor net tensor
        in
        let pack () = net in
        (forward, pack, None)
    | `Parameter32 net ->
        let var =
          net#tensor |> Nn.Tensor.to_ba |> Tfjs_api.tensor_of_ba
          |> Tfjs_api.variable ~name:(string_of_int (Oo.id net)) ~trainable:true
        in
        let forward _ = validate_output_tensor net var in
        let update, opti_pack = _unpack_optimizer net#optimizer var in
        let pack () =
          let tensor = Tfjs_api.ba_of_tensor Bigarray.Float32 var |> Nn.Tensor.of_ba in
          let optimizer = opti_pack () in
          (net#replicate ~id:net#id tensor optimizer :> Nn.network)
        in
        (forward, pack, Some update)

  let _unpack_layer11 (net : Nn.node11) =
    match net#classify_layer with
    | `Relu _ ->
        let forward up = validate_output_tensor net (Tfjs_api.Ops.relu up) in
        let pack up = (net :> Nn.network)#copy [ up ] in
        (forward, pack)
    | `Softmax net ->
        let tensor_axis = tensor_axis_of_shape_axis net#upstream#out_shape net#axis in
        let forward up = validate_output_tensor net (Tfjs_api.Ops.softmax tensor_axis up) in
        let pack up = (net :> Nn.network)#copy [ up ] in
        (forward, pack)
    | `Astype net ->
        let dtype = tfdtype_of_dtype net#dtype in
        let forward up = up |> Tfjs_api.Ops.astype dtype |> validate_output_tensor net in
        let pack up = (net :> Nn.network)#copy [ up ] in
        (forward, pack)
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
        let pack up = (net :> Nn.network)#copy [ up ] in
        (forward, pack)
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
        let pack up = (net :> Nn.network)#copy [ up ] in
        (forward, pack)
    | `Transpose net ->
        let tftranspose_axes, dims1_of_dims0 = derive_configuration_of_transpose_layer net in
        let forward up =
          let tfreshape_shape = up##.shape |> Js.to_array |> dims1_of_dims0 in
          Tfjs_api.Ops.transpose ~perm:tftranspose_axes up
          |> Tfjs_api.Ops.reshape tfreshape_shape
          |> validate_output_tensor net
        in
        let pack up = (net :> Nn.network)#copy [ up ] in
        (forward, pack)
    | `Normalisation net ->
        let shape = net#upstream#out_shape in
        let axes = List.map (tensor_axis_of_shape_axis shape) net#axes in
        let norm_forward, norm_pack = _unpack_normalisation_algorithm axes net#algorithm in
        let forward up = up |> norm_forward |> validate_output_tensor net in
        let pack up = (net#replicate (norm_pack ()) up :> Nn.network) in
        (forward, pack)

  let _unpack_layer21 (net : Nn.node21) =
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
        let pack up0 up1 = (net :> Nn.network)#copy [ up0; up1 ] in
        (forward, pack)
    | `Tensordot net ->
        let mapping, perm = derive_configuration_of_tensordot_layer net in
        let forward up0 up1 =
          Tfjs_api.Ops.tensordot mapping up0 up1
          |> Tfjs_api.Ops.transpose ~perm |> validate_output_tensor net
        in
        let pack up0 up1 = (net :> Nn.network)#copy [ up0; up1 ] in
        (forward, pack)

  let _unpack_layern1 (net : Nn.noden1) =
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
        (forward, (net :> Nn.network)#copy)
    | `Prod net ->
        let forward ups =
          let rec aux = function
            | [] -> failwith "unreachable (prod layer without upstream parents)"
            | [ x ] -> x
            | x :: x' :: tl -> aux (Tfjs_api.Ops.mul x x' :: tl)
          in
          ups |> aux |> validate_output_tensor net
        in
        (forward, (net :> Nn.network)#copy)
    | `Concatenate net ->
        let axis = tensor_axis_of_shape_axis net#out_shape net#axis in
        let forward ups = ups |> Tfjs_api.Ops.concat axis |> validate_output_tensor net in
        (forward, (net :> Nn.network)#copy)

  let _unpack_node follow (net : Nn.network) =
    match (net#classify_node, List.map follow net#upstreams) with
    | `Node01 node, [] ->
        let forward, pack, update = _unpack_layer01 node in
        Node01 { forward; pack; update; node = (node :> Nn.network) }
    | `Node11 node, [ up ] ->
        let forward, pack = _unpack_layer11 node in
        Node11 { forward; pack; node = (node :> Nn.network); up }
    | `Node21 node, [ up0; up1 ] ->
        let forward, pack = _unpack_layer21 node in
        Node21 { forward; pack; node = (node :> Nn.network); up0; up1 }
    | `Noden1 node, ups ->
        let forward, pack = _unpack_layern1 node in
        Noden1 { forward; pack; node = (node :> Nn.network); ups }
    | _, _ -> failwith "Corrupted network. A node has an unexpected number of upstream parents"

  let unpack_for_training :
      Nn.network -> (tftensor Nn.Map.t -> tftensor) * optimisation_map * (unit -> Nn.network) =
   (* Transform a `network` to everything that is needed to perform a training of that network
    * using tensorflow.js:
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
    let optimisations =
      let optimisations_of_node follow = function
        | Node01 { update = None; _ } -> OptiMap.empty
        | Node01 { update = Some update; node; _ } ->
            OptiMap.singleton (string_of_int (Oo.id node)) update
        | Node11 v -> follow v.up
        | Node21 v -> OptiMap.union_silent (follow v.up0) (follow v.up1)
        | Noden1 v -> List.map follow v.ups |> OptiMap.union_list_silent
      in
      Ocann.memoized_walk id_of_unet optimisations_of_node unet
    in
    (forward, optimisations, pack)
end
