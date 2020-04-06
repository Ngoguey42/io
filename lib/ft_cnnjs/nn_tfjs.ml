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
  (* upstream : tfnode; *)
  forward : tftensor Fnn.Map.t -> tftensor;
  optimizations : optimization_map;
  pack : unit -> Fnn.network;
}

(* let empty_accumulator = *)
  (* { inputs = Tfnode_set.empty; upstream = None; optimizations = OptiMap.empty; pack = (fun () -> None) } *)

let channel_last_axes = [`N; `S1; `S0; `C]

let tfdtype_of_dtype = function
  | `Float32 -> `Float32
  | `Int32 -> `Int32
  | `Uint8 -> `Int32
  | `Float64 -> failwith "float64 is unsupported in tfjs"
  | `Int64 -> failwith "int64 is unsupported in tfjs"

(* let tfaxis_of_axis = function *)
(*   | `N -> 0 *)
(*   | `S1 -> 1 *)
(*   | `S0 -> 2 *)
(*   | `C -> 3 *)
(*   | _ -> failwith "bad axis" *)

(* Optimizer unpacking ************************************************************************ *)
(* let unpack_sgd : *)
(*     string -> (unit -> Tfjs_api.variable Js.t) -> optimization_map * (unit -> Nn.optimizer) = *)
(*  fun name get_weights -> *)
(*   let update lr grad = Tfjs_api.sgd_updater#update (get_weights ()) lr grad in *)
(*   let optimizations = OptiMap.singleton name update in *)
(*   let pack () = `Sgd in *)
(*   (optimizations, pack) *)

(* let unpack_adam : *)
(*     string -> *)
(*     (unit -> Tfjs_api.variable Js.t) -> *)
(*     Nn.adam_content -> *)
(*     optimization_map * (unit -> Nn.optimizer) = *)
(*  fun name get_weights conf -> *)
(*   let updater = *)
(*     Tfjs_api.create_adam_updater conf.epsilon conf.beta1 conf.beta2 conf.step conf.rgrad *)
(*       conf.rgrad_sq *)
(*   in *)
(*   let update lr grad = updater#update (get_weights ()) lr grad in *)
(*   let optimization = OptiMap.singleton name update in *)
(*   let pack () = *)
(*     let step = updater#get_step##arraySync_intScalar in *)
(*     let rgrad = Tfjs_api.ba_of_tensor_float updater#get_rgrad in *)
(*     let rgrad_sq = Tfjs_api.ba_of_tensor_float updater#get_rgrad_sq in *)
(*     `Adam { conf with step; rgrad; rgrad_sq } *)
(*   in *)
(*   (optimization, pack) *)

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

(*     tfconv2d -> *)
(*     Nn.conv_content -> *)
(*     optimization_map * (unit -> Nn.optimizer) * (unit -> Nn.optimizer) = *)
(*  fun tflayer conf -> *)
(*   let kopti, kpack = *)
(*     let name = Printf.sprintf "%s/kernel" (tflayer##.name |> Js.to_string) in *)
(*     let getter () = tflayer##.kernel##.val_ in *)
(*     match conf.kernel_optimizer with *)
(*     | `Sgd -> unpack_sgd name getter *)
(*     | `Adam conf -> unpack_adam name getter conf *)
(*   in *)
(*   let bopti, bpack = *)
(*     let name = Printf.sprintf "%s/bias" (tflayer##.name |> Js.to_string) in *)
(*     let getter () = tflayer##.bias##.val_ in *)
(*     match conf.bias_optimizer with *)
(*     | `Sgd -> unpack_sgd name getter *)
(*     | `Adam conf -> unpack_adam name getter conf *)
(*   in *)
(*   (OptiMap.union_exn kopti bopti, kpack, bpack) *)

(* (\* Layers unpacking *************************************************************************** *\) *)
(* let unpack_conv2d : Nn.conv_content -> tflayer * optimization_map * (unit -> Nn.layer) = *)
(*  fun conf -> *)
(*   let { kernel_size; stride; padding; out_filters; kernel_weights; bias_weights; _ } = conf in *)
(*   let (ky, kx), (sy, sx) = (kernel_size, stride) in *)
(*   let weights = (kernel_weights, bias_weights) in *)
(*   let tflayer = L.conv2d ~weights (`Two (ky, kx)) padding (`Two (sy, sx)) out_filters in *)
(*   let optimizations, pack_kernel_opti, pack_bias_opti = unpack_optimizations tflayer conf in *)
(*   let pack () = *)
(*     let kernel_weights = Tfjs_api.ba_of_tensor_float tflayer##.kernel##.val_ in *)
(*     let bias_weights = Tfjs_api.ba_of_tensor_float tflayer##.bias##.val_ in *)
(*     `Conv2d *)
(*       { *)
(*         conf with *)
(*         kernel_weights; *)
(*         bias_weights; *)
(*         kernel_optimizer = pack_kernel_opti (); *)
(*         bias_optimizer = pack_bias_opti (); *)
(*       } *)
(*   in *)
(*   ((tflayer :> tflayer), optimizations, pack) *)

(* let unpack_maxpool2d : Nn.maxpool2d_content -> tflayer * optimization_map * (unit -> Nn.layer) = *)
(*  fun conf -> *)
(*   let { kernel_size = ky, kx; stride = sy, sx } = conf in *)
(*   let tflayer = L.max_pool2d (`Two (ky, kx)) (`Two (sy, sx)) in *)
(*   (tflayer, OptiMap.empty, fun () -> `Maxpool2d conf) *)

(*   (\* let layer : Nn.layer = layer in *\) *)
(* let unpack_layer : [<Nn.layer] -> tflayer * optimization_map * (unit -> Nn.layer) = fun layer -> *)
(*   match layer with *)
(*   | `Relu -> (L.relu (), OptiMap.empty, fun () -> layer) *)
(*                        (\* |> (fun (a, b, c) -> a, b, (c :> (unit -> Nn.layer))) *\) *)
(*   | `Softmax { axis } -> (L.softmax ~axis (), OptiMap.empty, fun () -> layer) *)
(*                        (\* |> (fun (a, b, c) -> a, b, (c :> (unit -> [`Softmax  of Nn.softmax_content] ))) *\) *)
(*                        (\* |> (fun (a, b, c) -> a, b, (c :> (unit -> Nn.layer))) *\) *)
(*   | `Concatenate { axis } -> (L.concatenate ~axis (), OptiMap.empty, fun () -> layer) *)
(*                        (\* |> (fun (a, b, c) -> a, b, (c :> (unit -> Nn.layer))) *\) *)
(*   | `Add { axis } -> (L.add ~axis (), OptiMap.empty, fun () -> layer) *)
(*                        (\* |> (fun (a, b, c) -> a, b, (c :> (unit -> Nn.layer))) *\) *)
(*   | `Conv2d conf -> unpack_conv2d conf *)
(*                        (\* |> (fun (a, b, c) -> a, b, (c :> (unit -> Nn.layer))) *\) *)
(*   | `Maxpool2d conf -> unpack_maxpool2d conf *)
(*                        (\* |> (fun (a, b, c) -> a, b, (c :> (unit -> Nn.layer))) *\) *)

(* (\* Network unpacking ************************************************************************** *\) *)

let _unpack_node01 (net: Fnn.node01) =
  match net#classify_layer with
  | `Input _ ->
     (* TODO: Assert shape is total *)
     let net = (net :> Fnn.network) in
     let forward inputs =
       let tensor = Fnn.Map.find net inputs in
       (* TODO: Assert shape matches tensor shape *)
       tensor
     in
     let pack () = net in
     { optimizations = OptiMap.empty; forward; pack }
  | `Parameter32 net ->
     let var =
       Tfjs_api.tensor_of_ba net#tensor
       |> Tfjs_api.variable ~name:(string_of_int (Oo.id net)) ~trainable:true
     in
     (* Printf.printf "variable:\n%!"; *)
     (* Firebug.console##log var; *)

     let forward _ = (var :> Tfjs_api.tensor Js.t) in
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
     let forward inputs = Tfjs_api.Ops.relu (up_forward inputs) in
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
  | _ -> failwith "soon 21"

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
  | _ -> failwith "soon node"
  (* | `Node *)


  (* | `Sum net, up_accs -> *)
  (*    let up_tfnodes = *)
  (*      List.map (fun up_acc -> up_acc.upstream) up_accs *)
  (*      |> List.map Option.get *)
  (*      |> Array.of_list *)
  (*      |> Js.array *)
  (*    in *)
  (*    let inputs = *)
  (*      List.map (fun up_acc -> up_acc.inputs) up_accs *)
  (*      |> List.fold_left Tfnode_set.union Tfnode_set.empty *)
  (*    in *)
  (*    let optimizations = *)
  (*      List.map (fun up_acc -> up_acc.optimizations) up_accs *)
  (*      |> OptiMap.union_list_silent *)
  (*    in *)
  (*    let pack () = *)
  (*      let upstreams = *)
  (*        List.map (fun up_acc -> up_acc.pack ()) up_accs *)
  (*        |> List.map Option.get *)
  (*      in *)
  (*      Some (net#copy upstreams :> Fnn.network) *)
  (*    in *)
  (*    { inputs; upstream = Some ((L.add ())##apply_array up_tfnodes); pack; optimizations } *)
  (* | _, _ -> failwith "soon" *)

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

     (* let pack () = match pack () with None -> failwith "unreachable" | Some network -> network in *)
     (* let input = inputs |> Tfnode_set.to_seq |> List.of_seq |> List.hd in *)
     (* (input, network, optimizations, pack) *)

  (*   let aux acc net = *)
(*     match acc, net with *)
(*     | [], Node01 content -> *)
(*     (\* match (acc.input, acc.network, net) with *\) *)
(*     (\* | None, None, Node01 content -> *\) *)
(*         let x = L.input ~dtype:content#dtype [| h; w; content#out_filters |] in *)
(*         let pack () = *)
(*           Some net *)
(*           (\* match acc.pack () with Some _ -> failwith "unreachable" | None -> Some net *\) *)
(*         in *)
(*         { empty_accumulator with input = Some x; network = Some x; pack } *)
(*     | [{ network = Some tfnet; _ } as acc], Node11 content -> *)
(*     (\* | Some _, Some tfnet, Node11 content -> *\) *)
(*         let tflayer, optimizations, pack = unpack_layer (content#layer :> Nn.layer) in *)
(*         let pack () = *)
(*           match acc.pack () with *)
(*           | None -> failwith "unreachable" *)
(*           | Some upstream -> Some (node11 upstream (pack ())) *)
(*         in *)
(*         { *)
(*           acc with *)
(*           network = Some (tflayer##apply tfnet); *)
(*           optimizations = OptiMap.union_exn optimizations acc.optimizations; *)
(*           pack; *)
(*         } *)
(*     | acc_list, Noden1 content -> *)
(*         let tflayer, optimizations, pack = unpack_layer content#layer in *)
(*         let upstream_nodes = *)
(*           List.map *)
(*             (fun a -> match a.network with None -> failwith "unreachable" | Some tfnode -> tfnode) *)
(*             acc_list *)
(*           |> Array.of_list *)
(*           |> Js.array *)
(*         in *)
(*         let optimizations = optimizations::List.map (fun a -> a.optimizations) acc_list in *)
(*         let pack () = *)
(*           let upstreams = *)
(*             List.map *)
(*               (fun acc -> match acc.pack () with *)
(*                           | None -> failwith "unreachable" *)
(*                           | Some upstream -> Some (node11 upstream (pack ()))) *)
(*               acc_list *)
(*           in *)
(*           Some (node21 upstreams (pack ())) *)
(*         in *)
(*         { *)
(*           input = (List.head acc_list).input; (\* TODO: fix that *\) *)
(*           network = Some (tflayer##apply_array upstream_nodes); *)
(*           optimizations = OptiMap.union_list_silent optimizations; *)
(*           pack; *)
(*         } *)


(*     | _, _ -> failwith "unreachable" *)
(*   in *)
