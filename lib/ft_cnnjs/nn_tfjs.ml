module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array
module L = Tfjs_api.Layers

(* Utility types ******************************************************************************** *)

type tftensor = Tfjs_api.tensor Js.t

type optimization = float -> tftensor -> unit

module OptiMap = struct
  include Map.Make (Stdlib.String)
  module StringSet = Set.Make (Stdlib.String)

  let union_exn : optimization t -> optimization t -> optimization t =
    union (fun name _ _ -> Printf.sprintf "variable name clash: <%s>" name |> failwith)

  let union_silent = union (fun _ a _ -> Some a)

  let union_list_exn l = List.fold_left union_exn empty l

  let union_list_silent l = List.fold_left union_silent empty l

  let key_disjunction m m' =
    let keys = to_seq m |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
    let keys' = to_seq m' |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
    ( StringSet.diff keys keys' |> StringSet.elements,
      StringSet.diff keys' keys |> StringSet.elements )
end

type optimization_map = optimization OptiMap.t

type tflayer = Tfjs_api.layer Js.t

type tfconv2d = Tfjs_api.conv2d Js.t

type tfnode = Tfjs_api.symbolicTensor Js.t

module Tfnode_set = Set.Make (struct
  type t = tfnode

  let compare = compare
end)

type accumulator = {
  forward : tftensor Fnn.Map.t -> tftensor;
  optimizations : optimization_map;
  pack : unit -> Fnn.network;
}

(* Utility functions **************************************************************************** *)

let channel_last_axes = function
  | 5 -> [ `N; `S2; `S1; `S0; `C ]
  | 4 -> [ `N; `S1; `S0; `C ]
  | 3 -> [ `N; `S0; `C ]
  | 2 -> [ `N; `C ]
  | 1 -> [ `N ]
  | 0 -> []
  | _ -> invalid_arg "In channel_last_axes: Invalid ndim"

let tfdtype_of_dtype = function
  | `Float32 -> `Float32
  | `Int32 -> `Int32
  | `Uint8 -> `Int32
  | `Float64 -> failwith "float64 is unsupported in tfjs"
  | `Int64 -> failwith "int64 is unsupported in tfjs"

let _validate_output_tensor net tensor =
  (* TODO: Assert dtype of output tensors *)
  let net = Fnn.downcast net in
  let tensor = (tensor :> Tfjs_api.tensor Js.t) in

  let out_shape = net#out_shape in
  let out_shape =
    if Pshape.is_symbolic out_shape then
      out_shape |> Pshape.to_symbolic
      |> Pshape.desymbolize (channel_last_axes (Pshape.ndim out_shape))
      |> Pshape.to_any
    else out_shape
  in
  let net_dims = out_shape |> Pshape.to_list |> List.map snd in
  let tensor_dims = tensor##.shape |> Js.to_array |> Array.to_list in
  let sizes_invalid = function
    | Pshape.Size.U, _ -> false
    | Pshape.Size.K j, i when i = j -> false
    | _, _ -> true
  in
  if
    List.length net_dims <> List.length tensor_dims
    || List.combine net_dims tensor_dims |> List.exists sizes_invalid
  then
    Printf.sprintf "Output tensor of net %s has shape (%s) but was expected to have shape %s=(%s)"
      net#to_string
      (List.map string_of_int tensor_dims |> String.concat ", ")
      (Pshape.to_string net#out_shape)
      (List.map Pshape.Size.to_string net_dims |> String.concat ", ")
    |> failwith;
  tensor

let _index_in_list l x =
  List.mapi (fun i y -> i, y) l
  |> List.find (fun (_, y) -> x = y)
  |> fst

let _axes_of_shape shape =
  let ndim = Pshape.ndim shape in
  let is_sym = Pshape.is_symbolic shape in
  if is_sym then (channel_last_axes ndim :> Pshape.Axis.t list)
  else (Pshape.Axis.absolute_axes_of_ndim ndim :> Pshape.Axis.t list)

let _tensor_axis_of_shape_axis shape ax =
  List.mapi (fun i ax -> (i, ax)) (_axes_of_shape shape) |> List.find (fun (_, x) -> x = ax) |> fst

let _derive_configuration_of_transpose_layer (net : Fnn.transpose) =
  let mapping = net#mapping in
  let shape0 = net#upstream#out_shape in
  let axes0 = _axes_of_shape shape0 in
  let axes1 = _axes_of_shape net#out_shape in
  let mapping = Pshape.Axis.transpose ~mapping axes0 axes1 in
  let mapping = List.map (List.map (_tensor_axis_of_shape_axis shape0)) mapping in
  let tftranspose_axes = List.concat mapping in
  let dims1_of_dims0 dims =
    List.map
      (fun tensor_axs0 -> tensor_axs0 |> List.map (Array.get dims) |> List.fold_left ( * ) 1)
      mapping
    |> Array.of_list
  in
  (tftranspose_axes, dims1_of_dims0)

let _derive_configuration_of_tensordot_layer net =
  (* Extract shape0 infos *)
  let shape0 = net#upstream0#out_shape in
  let axes0 = _axes_of_shape shape0 in
  let caxes0 = net#contracted_axes0 in
  let kaxes0 = List.filter (fun ax -> not (List.mem ax caxes0)) axes0 in
  let kcount0 = List.length kaxes0 in
  (* Extract shape1 infos *)
  let shape1 = net#upstream1#out_shape in
  let axes1 = _axes_of_shape shape1 in
  let caxes1 = net#contracted_axes1 in
  let kaxes1 = List.filter (fun ax -> not (List.mem ax caxes1)) axes1 in
  (* Compute tensordot argument *)
  let caxes01 =
    List.combine
      (List.map (_index_in_list axes0) caxes0)
      (List.map (_index_in_list axes1) caxes1)
  in
  (* Compute permute argument *)
  let shape2 = net#out_shape in
  let axes2 = _axes_of_shape shape2 in
  let perm =
    List.map (fun ax ->
        match net#input_axis_of_output_axis ax with
        | `Left ax -> 0 + _index_in_list kaxes0 ax
        | `Right ax -> kcount0 + _index_in_list kaxes1 ax
      ) axes2
  in
  caxes01, perm

(* Unpack functions ***************************************************************************** *)
let _unpack_normalisation_algorithm axes = function
  | `Batch epsilon ->
     let normaliser = Tfjs_api.create_batch_normaliser epsilon axes in
     let forward = normaliser#normalise in
     let pack () =
       `Batch epsilon
     in
     forward, pack
  | `Moving32 (epsilon, momentum, step, avg, var) ->
     let forward x =
       ignore x;
       failwith "not implemented"
     in
     let pack () =
       `Moving32 (epsilon, momentum, step, avg, var)
     in
     forward, pack
  | `Moving_exp32 (epsilon, momentum, avg, var) ->
     let sizes = Bigarray.Genarray.dims avg |> Array.to_list in
     let forward x =
       let shape = x##.shape |> Js.to_array in
       if sizes <> List.map (fun i -> shape.(i)) axes then
         failwith "In normalisation@forward, invalid input tensor shape";
       ignore x;
       failwith "not implemented"
     in
     let pack () =
       `Moving_exp32 (epsilon, momentum, avg, var)
     in
     forward, pack

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
        _validate_output_tensor net tensor
      in
      let pack () = net in
      { optimizations = OptiMap.empty; forward; pack }
  | `Parameter32 net ->
      let var =
        Tfjs_api.tensor_of_ba net#tensor
        |> Tfjs_api.variable ~name:(string_of_int (Oo.id net)) ~trainable:true
      in
      let forward _ = _validate_output_tensor net var in
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
      let forward inputs = _validate_output_tensor net (Tfjs_api.Ops.relu (up_forward inputs)) in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Softmax net ->
      let tensor_axis = _tensor_axis_of_shape_axis net#upstream#out_shape net#axis in
      let forward inputs =
        _validate_output_tensor net (Tfjs_api.Ops.softmax tensor_axis (up_forward inputs))
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Astype net ->
      let dtype = tfdtype_of_dtype net#dtype in
      let forward inputs =
        up_forward inputs |> Tfjs_api.Ops.astype dtype |> _validate_output_tensor net
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
        let axes = _axes_of_shape net#upstream#out_shape in
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
      let forward inputs = up_forward inputs |> f |> _validate_output_tensor net in
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
        up_forward inputs |> Tfjs_api.Ops.maxpool2d ~s ~b kernel_size |> _validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Transpose net ->
      let tftranspose_axes, dims1_of_dims0 = _derive_configuration_of_transpose_layer net in
      let forward inputs =
        let x = up_forward inputs in
        let tfreshape_shape = x##.shape |> Js.to_array |> dims1_of_dims0 in
        Tfjs_api.Ops.transpose ~perm:tftranspose_axes x
        |> Tfjs_api.Ops.reshape tfreshape_shape
        |> _validate_output_tensor net
      in
      let copy : Fnn.network list -> Fnn.network = (net :> Fnn.network)#copy in
      (forward, copy)
  | `Normalisation net ->
     let shape = net#upstream#out_shape in
     let axes =
       List.map (_tensor_axis_of_shape_axis shape) net#axes in
     let norm_forward, norm_pack = _unpack_normalisation_algorithm axes net#algorithm in
     let forward inputs =
       up_forward inputs
       |> norm_forward
       |> _validate_output_tensor net
     in
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
        f ~b ~d ~s (up1_forward inputs) (up0_forward inputs) |> _validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Tensordot net ->
     let mapping, perm = _derive_configuration_of_tensordot_layer net in
     let forward inputs =
       Tfjs_api.Ops.tensordot mapping (up0_forward inputs) (up1_forward inputs)
       |> Tfjs_api.Ops.transpose ~perm
       |> _validate_output_tensor net
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
        List.map (fun fn -> fn inputs) up_forwards |> aux |> _validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Prod net ->
      let forward inputs =
        let rec aux = function
          | [] -> failwith "unreachable (sum layer without upstream parents)"
          | [ x ] -> x
          | x :: x' :: tl -> aux (Tfjs_api.Ops.mul x x' :: tl)
        in
        List.map (fun fn -> fn inputs) up_forwards |> aux |> _validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)
  | `Concatenate net ->
      let axis = _tensor_axis_of_shape_axis net#out_shape net#axis in
      let forward inputs =
        List.map (fun fn -> fn inputs) up_forwards
        |> Tfjs_api.Ops.concat axis |> _validate_output_tensor net
      in
      (forward, (net :> Fnn.network)#copy)

let _unpack_node follow (net : Fnn.network) =
  let v =
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
  in
  v

let unpack_for_training :
    Fnn.network -> (tftensor Fnn.Map.t -> tftensor) * optimization_map * (unit -> Fnn.network) =
 (* Transform a `network` to everything that is needed to perform a training of that network
  * using tensorflow.js:
  * 1. A callable for the forward pass
  * 2. A map of callbacks that each update a weight tensor given its gradient
  * 3. A thunk to be called to pack everything back to a `network` when done with training
  *)
 fun net ->
  let { forward; optimizations; pack } = Fnn.memoized_walk _unpack_node net in
  (forward, optimizations, pack)
