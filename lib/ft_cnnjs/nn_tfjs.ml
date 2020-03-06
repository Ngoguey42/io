module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array
module L = Tfjs_api.Layers
open Nn (* Opening for the records refinitions *)

module OptiMap = struct
  include Map.Make (Stdlib.String)
  module StringSet = Set.Make (Stdlib.String)

  let union_exn =
    union (fun name _ _ -> Printf.sprintf "variable name clash: <%s>" name |> failwith)

  let union_list_exn l =
    List.fold_left union_exn empty l

  let key_disjunction m m' =
    let keys = to_seq m |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
    let keys' = to_seq m' |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
    ( StringSet.diff keys keys' |> StringSet.elements,
      StringSet.diff keys' keys |> StringSet.elements )
end

type optimization = float -> Tfjs_api.tensor Js.t -> unit

type optimization_map = optimization OptiMap.t

type tflayer = Tfjs_api.layer Js.t

type tfconv2d = Tfjs_api.conv2d Js.t

type tfnode = Tfjs_api.symbolicTensor Js.t

type ('a, 'b) accumulator = {
  input : tfnode option;
  network : tfnode option;
  optimizations : optimization_map;
  pack : unit -> Nn.t option;
}

let empty_accumulator =
  { input = None; network = None; optimizations = OptiMap.empty; pack = (fun () -> None) }

(* Optimizer unpacking ************************************************************************ *)
let unpack_sgd :
    string -> (unit -> Tfjs_api.variable Js.t) -> optimization_map * (unit -> Nn.optimizer) =
 fun name get_weights ->
  let update lr grad = Tfjs_api.sgd_updater#update (get_weights ()) lr grad in
  let optimizations = OptiMap.singleton name update in
  let pack () = `Sgd in
  (optimizations, pack)

let unpack_adam :
    string ->
    (unit -> Tfjs_api.variable Js.t) ->
    Nn.adam_content ->
    optimization_map * (unit -> Nn.optimizer) =
 fun name get_weights conf ->
  let updater =
    Tfjs_api.create_adam_updater conf.epsilon conf.beta1 conf.beta2 conf.step conf.rgrad
      conf.rgrad_sq
  in
  let update lr grad = updater#update (get_weights ()) lr grad in
  let optimization = OptiMap.singleton name update in
  let pack () =
    let step = updater#get_step##arraySync_intScalar in
    let rgrad = Tfjs_api.ba_of_tensor_float updater#get_rgrad in
    let rgrad_sq = Tfjs_api.ba_of_tensor_float updater#get_rgrad_sq in
    `Adam { conf with step; rgrad; rgrad_sq }
  in
  (optimization, pack)

let unpack_optimizations :
    tfconv2d ->
    Nn.conv_content ->
    optimization_map * (unit -> Nn.optimizer) * (unit -> Nn.optimizer) =
 fun tflayer conf ->
  let kopti, kpack =
    let name = Printf.sprintf "%s/kernel" (tflayer##.name |> Js.to_string) in
    let getter () = tflayer##.kernel##.val_ in
    match conf.kernel_optimizer with
    | `Sgd -> unpack_sgd name getter
    | `Adam conf -> unpack_adam name getter conf
  in
  let bopti, bpack =
    let name = Printf.sprintf "%s/bias" (tflayer##.name |> Js.to_string) in
    let getter () = tflayer##.bias##.val_ in
    match conf.bias_optimizer with
    | `Sgd -> unpack_sgd name getter
    | `Adam conf -> unpack_adam name getter conf
  in
  (OptiMap.union_exn kopti bopti, kpack, bpack)

(* Layers unpacking *************************************************************************** *)
let unpack_conv2d : Nn.conv_content -> tflayer * optimization_map * (unit -> Nn.layer) =
 fun conf ->
  let { kernel_size; stride; padding; out_filters; kernel_weights; bias_weights; _ } = conf in
  let (ky, kx), (sy, sx) = (kernel_size, stride) in
  let weights = (kernel_weights, bias_weights) in
  let tflayer = L.conv2d ~weights (`Two (ky, kx)) padding (`Two (sy, sx)) out_filters in
  let optimizations, pack_kernel_opti, pack_bias_opti = unpack_optimizations tflayer conf in
  let pack () =
    let kernel_weights = Tfjs_api.ba_of_tensor_float tflayer##.kernel##.val_ in
    let bias_weights = Tfjs_api.ba_of_tensor_float tflayer##.bias##.val_ in
    `Conv2d
      {
        conf with
        kernel_weights;
        bias_weights;
        kernel_optimizer = pack_kernel_opti ();
        bias_optimizer = pack_bias_opti ();
      }
  in
  ((tflayer :> tflayer), optimizations, pack)

let unpack_maxpool2d : Nn.maxpool2d_content -> tflayer * optimization_map * (unit -> Nn.layer) =
 fun conf ->
  let { kernel_size = ky, kx; stride = sy, sx } = conf in
  let tflayer = L.max_pool2d (`Two (ky, kx)) (`Two (sy, sx)) in
  (tflayer, OptiMap.empty, fun () -> `Maxpool2d conf)

let unpack_layer : Nn.layer -> tflayer * optimization_map * (unit -> Nn.layer) = function
  | `Conv2d conf -> unpack_conv2d conf
  | `Maxpool2d conf -> unpack_maxpool2d conf
  | `Relu -> (L.relu (), OptiMap.empty, fun () -> `Relu)
  | `Softmax { axis } -> (L.softmax ~axis (), OptiMap.empty, fun () -> `Softmax { axis })

(* Network unpacking ************************************************************************** *)
let unpack : int -> int -> Nn.t -> tfnode * tfnode * optimization_map * (unit -> Nn.t) =
 (* Transform a `network` to everything that is needed to perform a training of that network
  * using tensorflow.js:
  * 1. An input symbolic tensor
  * 2. An output symbolic tensor
  * 3. A map of callbacks that each update a weight tensor given its gradient
  * 4. A thunk to be called to pack everything back to a `network` when done with training
  *)
 fun h w net ->
  let aux acc net =
    match (acc.input, acc.network, net) with
    | None, None, Input2d { out_filters; dtype } ->
        let x = L.input ~dtype [| h; w; out_filters |] in
        let pack () =
          match acc.pack () with Some _ -> failwith "unreachable" | None -> Some net
        in
        { acc with input = Some x; network = Some x; pack }
    | Some _, Some tfnet, Inner (_, layer) ->
        let tflayer, optimizations, pack = unpack_layer layer in
        let pack () =
          match acc.pack () with
          | None -> failwith "unreachable"
          | Some upstream -> Some (Inner (upstream, pack ()))
        in
        {
          acc with
          network = Some (tflayer##apply tfnet);
          optimizations = OptiMap.union_exn optimizations acc.optimizations;
          pack;
        }
    | _, _, _ -> failwith "unreachable"
  in
  match Nn.fold_top_down aux empty_accumulator net with
  | { input = Some input; network = Some network; optimizations; pack; _ } ->
      let pack () = match pack () with None -> failwith "unreachable" | Some network -> network in
      (input, network, optimizations, pack)
  | _ -> failwith "unreachable"
