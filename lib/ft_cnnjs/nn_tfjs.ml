module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array
module L = Tfjs_api.Layers

type tftensor = Tfjs_api.tensor Js.t

type optimization = float -> tftensor -> unit

module OptiMap = struct
  include Map.Make (Stdlib.String)
  module StringSet = Set.Make (Stdlib.String)

  let union_exn : optimization t ->optimization t ->optimization t =
    union (fun name _ _ -> Printf.sprintf "variable name clash: <%s>" name |> failwith)

  let union_silent =
    union (fun _ a _ -> Some a)

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

let channel_last_axes = function
  | 5 -> [`N; `S2; `S1; `S0; `C]
  | 4 -> [`N; `S1; `S0; `C]
  | 3 -> [`N; `S0; `C]
  | 2 -> [`N; `C]
  | 1 -> [`N]
  | 0 -> []
  | _ -> invalid_arg "In channel_last_axes: Invalid ndim"

let tfdtype_of_dtype = function
  | `Float32 -> `Float32
  | `Int32 -> `Int32
  | `Uint8 -> `Int32
  | `Float64 -> failwith "float64 is unsupported in tfjs"
  | `Int64 -> failwith "int64 is unsupported in tfjs"

let _validate_output_tensor node tensor =
  let node = Fnn.downcast node in
  let tensor = (tensor :> Tfjs_api.tensor Js.t) in

  let out_shape = node#out_shape in
  let out_shape =
    if Pshape.is_symbolic out_shape then
      out_shape
      |> Pshape.to_symbolic
      |> Pshape.desymbolize (channel_last_axes (Pshape.ndim out_shape))
      |> Pshape.to_any
    else out_shape
  in
  let node_dims =
    out_shape
    |> Pshape.to_list
    |> List.map snd
  in
  let tensor_dims =
    tensor##.shape |> Js.to_array |> Array.to_list
  in
  let sizes_invalid = function
    | Pshape.Size.U, _ -> false
    | Pshape.Size.K j, i when i = j -> false
    | _, _ -> true
  in
  if List.combine node_dims tensor_dims |> List.exists sizes_invalid then
    Printf.sprintf "Output tensor of node %s has shape (%s) but was expected to have shape %s=(%s)"
                   (node#to_string)
                   (List.map string_of_int tensor_dims |> String.concat ", ")
                   (Pshape.to_string node#out_shape)
                   (List.map Pshape.Size.to_string node_dims |> String.concat ", ")
    |> failwith;
  tensor

let _unpack_optimizer optim var =
  match optim with
  | `Sgd ->
     let update lr grad = Tfjs_api.sgd_updater#update var lr grad in
     let pack () = `Sgd in
     (update, pack)
  | `Adam (epsilon, beta1, beta2, step, rgrad, rgrad_sq) ->
     let updater =
       Tfjs_api.create_adam_updater epsilon beta1 beta2 step rgrad rgrad_sq
     in
     let update lr grad = updater#update var lr grad in
     let pack () =
       let step = updater#get_step##arraySync_intScalar in
       let rgrad = Tfjs_api.ba_of_tensor_float updater#get_rgrad in
       let rgrad_sq = Tfjs_api.ba_of_tensor_float updater#get_rgrad_sq in
       `Adam (epsilon, beta1, beta2, step, rgrad, rgrad_sq)
     in
     (update, pack)

let _unpack_node01 (net: Fnn.node01) =
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
       let tensor = Tfjs_api.ba_of_tensor_float var in
       let optimizer = opti_pack () in
       (net#replicate ~id:net#id tensor optimizer :> Fnn.network)
     in
     { optimizations = OptiMap.singleton (string_of_int (Oo.id net)) update; forward; pack }

let _unpack_layer11 (net: Fnn.node11) up_forward =
  match net#classify_layer with
  | `Relu _ ->
     let forward inputs =
       _validate_output_tensor net (Tfjs_api.Ops.relu (up_forward inputs))
     in
     forward, net#copy
  | `Transpose node ->
     let mapping = node#mapping in
     let is_sym0 = Pshape.is_symbolic node#upstream#out_shape in
     let is_sym1 = Pshape.is_symbolic node#out_shape in
     let ndim0 = Pshape.ndim node#upstream#out_shape in
     let ndim1 = Pshape.ndim node#out_shape in
     (* Printf.eprintf "> in transpose\n%!"; *)
     (* Printf.eprintf "input shape\n%!"; *)
     (* print_endline (Pshape.to_string node#upstream#out_shape); *)

     (* Printf.eprintf "output shape\n%!"; *)
     (* print_endline (Pshape.to_string node#out_shape); *)

     let shape =
       Array.init ndim0 (fun i -> i + 10)
       |> Pshape.from_int_array
     in
     (* Printf.eprintf "made of fake ints:\n%!"; *)
     (* print_endline (Pshape.to_string shape); *)
     let shape =
       if is_sym0 then Pshape.symbolize (channel_last_axes ndim0) shape |> Pshape.to_any
       else shape |> Pshape.to_any
     in
     (* Printf.eprintf "maybe to sym:\n%!"; *)
     (* print_endline (Pshape.to_string shape); *)
     let shape = Pshape.transpose ~ndim:ndim1 ~mapping shape in
     (* Printf.eprintf "transposed:\n%!"; *)
     (* print_endline (Pshape.to_string shape); *)
     let shape =
       if is_sym1 then
         Pshape.desymbolize (channel_last_axes ndim1) (Pshape.to_symbolic shape)
         |> Pshape.to_absolute
         |> Pshape.to_total
       else shape |> Pshape.to_absolute |> Pshape.to_total
     in
     (* Printf.eprintf "maybe from sym:\n%!"; *)
     (* print_endline (Pshape.to_string shape); *)
     let ints = Pshape.to_int_array shape |> Array.to_list in
     (* ints |> List.map string_of_int |> String.concat "," |> print_endline; *)
     let tftranspose_axes =
       List.filter_map (fun j -> if j >= 10 then Some (j - 10) else None) ints
     in
     (* tftranspose_axes |> List.map string_of_int |> String.concat "," |> print_endline; *)
     let tfexpand_axes =
       List.mapi (fun i j -> (i, j)) ints
       |> List.filter_map (fun (i, j) -> if j >= 10 then None else Some i)
     in
     (* tfexpand_axes |> List.map string_of_int |> String.concat "," |> print_endline; *)

     let forward inputs =
       let x = up_forward inputs in
       let x = Tfjs_api.Ops.transpose ~perm:tftranspose_axes x in
       let rec aux x = function
         | [] -> x
         | axis::tl -> aux (Tfjs_api.Ops.expand_dims axis x) tl
       in
       aux x tfexpand_axes
       |> _validate_output_tensor net
     in
     forward, net#copy
  | _ -> failwith ("soon 11:" ^ net#to_string)

let _unpack_layer21 (net: Fnn.node21) up0_forward up1_forward =
  match net#classify_layer with
  | `Conv2d net ->
     let b = match net#boundary_mode with
       | `Same -> `Same
       | `Valid -> `Valid
       | `Assert_fit -> `Valid
       | `Pad_fit -> failwith "not implemented"
     in
     let d = net#dilation in
     let s = net#stride in
     let forward inputs =
       Tfjs_api.Ops.conv2d ~b ~d ~s (up1_forward inputs) (up0_forward inputs)
     in
     forward, net#copy
  | _ -> failwith ("soon 21:" ^ net#to_string)

let _unpack_layern1 (net: Fnn.noden1) up_forwards =
  match net#classify_layer with
  | `Sum net ->
     let forward inputs =
       let rec aux = function
         | [] -> failwith "unreachable"
         | [x] -> x
         | x::x'::tl -> aux ((Tfjs_api.Ops.add x x')::tl)
       in
       List.map (fun fn -> fn inputs) up_forwards
       |> aux
       |> _validate_output_tensor net
     in
     forward, (net :> Fnn.network)#copy
  | _ -> failwith ("soon n1:" ^ net#to_string)

let _unpack_node follow (net : Fnn.network) =
  match net#classify_node, List.map follow net#upstreams with
  | `Node01 net, [] -> _unpack_node01 net
  | `Node11 net, [ up_acc ] ->
     let forward, pack = _unpack_layer11 net up_acc.forward in
     let pack () = (pack [ up_acc.pack () ] :> Fnn.network) in
     {
       up_acc with
       forward;
       pack;
     }
  | `Node21 net, [ up0_acc; up1_acc ] ->
     let forward, pack = _unpack_layer21 net up0_acc.forward up1_acc.forward in
     let pack () = (pack [ up0_acc.pack (); up1_acc.pack () ] :> Fnn.network) in
     let optimizations = OptiMap.union_silent up0_acc.optimizations up1_acc.optimizations in
     {
       forward;
       optimizations;
       pack;
     }
  | `Noden1 net, up_accs ->
     let up_forwards = List.map (fun acc -> acc.forward) up_accs in
     let up_packs = List.map (fun acc -> acc.pack) up_accs in
     let up_optimizations = List.map (fun acc -> acc.optimizations) up_accs in
     let optimizations = OptiMap.union_list_silent up_optimizations in
     let forward, pack = _unpack_layern1 net up_forwards in
     let pack () =
       List.map (fun pack -> pack ()) up_packs
       |> pack
     in
     {
       forward;
       optimizations;
       pack;
     }
  | _, _ -> failwith "unreachable"

let unpack : Fnn.network -> (tftensor Fnn.Map.t -> tftensor) * optimization_map * (unit -> Fnn.network) =
 (* Transform a `network` to everything that is needed to perform a training of that network
  * using tensorflow.js:
  * 1. A callable for the forward pass
  * 2. A map of callbacks that each update a weight tensor given its gradient
  * 3. A thunk to be called to pack everything back to a `network` when done with training
  *)
  fun net ->
  let { forward; optimizations; pack } = Fnn.memoized_walk _unpack_node net in
  forward, optimizations, pack
