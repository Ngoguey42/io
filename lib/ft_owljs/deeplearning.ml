module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Graph = Owl_neural_generic.Make_Embedded (Owl_base_dense_ndarray.S)
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array

type float32_nd = (float, Bigarray.float32_elt) Ndarray.t

type int32_nd = (int, Bigarray.int32_elt) Ndarray.t

(* TODO: Move what can be moved to ft_owlbase and ft_js
   rename to network/neural_network ? `type network`->`type t`? `Network_builder` -> `Builder` ?
*)

type adam_content = {
  beta1 : float;
  beta2 : float;
  epsilon : float;
  (* Number of updates performed *)
  step : int;
  (* Running gradient of kernel *)
  rgrad_kernel : float32_nd;
  (* Running gradient squared of kernel *)
  rgrad_sq_kernel : float32_nd;
  (* Running gradient of bias *)
  rgrad_bias : float32_nd;
  (* Running gradient squared of bias *)
  rgrad_sq_bias : float32_nd;
}

type conv_optimizer = [ `Adam of adam_content | `SGD ]

type conv_content = {
  kernel_size : int * int;
  stride : int * int;
  padding : bool;
  out_filters : int;
  kernel_weights : float32_nd;
  bias_weights : float32_nd;
  optimizer : conv_optimizer;
}

type maxpool2d_content = { kernel_size : int * int; stride : int * int }

type softmax_content = { axis : int }

type layer =
  [ `Conv2d of conv_content | `Maxpool2d of maxpool2d_content | `Relu | `Softmax of softmax_content ]

type network =
  | Input2d of { out_filters : int; dtype : [ `Float32 | `Int32 ] }
  | Inner of (network * layer)

module String = struct
  let of_dtype = function `Float32 -> "float32" | `Uint8 -> "uint8" | `Int32 -> "int32"

  let of_conv_optimizer = function
    | `SGD -> "SGD"
    | `Adam c ->
        let mean_val arr beta =
          if c.step == 0 then 0.
          else
            let correction = 1. -. (beta ** float_of_int c.step) in
            let size = float_of_int (Ndarray.numel arr) in
            Ndarray.sum' arr /. size /. correction
        in
        Printf.sprintf "Adam@%d k:%+.3e/%+.3e b:%+.3e/%+.3e" c.step
          (mean_val c.rgrad_kernel c.beta1)
          (mean_val c.rgrad_sq_kernel c.beta2)
          (mean_val c.rgrad_bias c.beta1)
          (mean_val c.rgrad_sq_bias c.beta2)

  let of_layer : layer -> string = function
    | `Conv2d c ->
        let mean_val arr = Ndarray.sum' arr /. float_of_int (Ndarray.numel arr) in
        let ky, kx = c.kernel_size in
        let sy, sx = c.stride in
        Printf.sprintf "Conv2d k:%dx%d s:%dx%d <%d> k:%+.3e b:%+.3e (%s)" ky kx sy sx c.out_filters
          (mean_val c.kernel_weights) (mean_val c.bias_weights) (of_conv_optimizer c.optimizer)
    | `Maxpool2d _ -> "Maxpool2d"
    | `Relu -> "Relu"
    | `Softmax _ -> "Softmax"

  let rec of_network = function
    | Input2d { out_filters; dtype } ->
        Printf.sprintf "| Input2d <%d> %s" out_filters (of_dtype dtype)
    | Inner (upstream, layer) -> Printf.sprintf "%s\n> %s" (of_network upstream) (of_layer layer)
end

let rec fold_bottom_up f x net =
  match net with Inner (net', _) -> fold_bottom_up f (f x net) net' | Input2d _ -> f x net

let rec fold_top_down f x net =
  match net with Inner (net', _) -> f (fold_top_down f x net') net | Input2d _ -> f x net

module Builder = struct
  type upstream = { filters : int; network : network }

  let input2d ?(dtype = `Float32) f = { filters = f; network = Input2d { out_filters = f; dtype } }

  let conv2d kernel_size padding stride out_filters initialization optimizer up =
    let ((ky, kx) as kernel_size) =
      match kernel_size with `One k -> (k, k) | `Two (ky, kx) -> (ky, kx)
    in
    let kshape = [| ky; kx; up.filters; out_filters |] in
    let bshape = [| out_filters |] in
    let stride = match stride with `One s -> (s, s) | `Two (sy, sx) -> (sy, sx) in
    let kernel_weights = Ft_owlbase.Init.run initialization kshape in
    let bias_weights = Ft_owlbase.Init.run initialization bshape in

    let optimizer =
      match optimizer with
      | `SGD -> `SGD
      | `Adam (beta1, beta2, epsilon) ->
          `Adam
            {
              beta1;
              beta2;
              epsilon;
              step = 0;
              rgrad_kernel = Ndarray.zeros Bigarray.Float32 kshape;
              rgrad_sq_kernel = Ndarray.zeros Bigarray.Float32 kshape;
              rgrad_bias = Ndarray.zeros Bigarray.Float32 bshape;
              rgrad_sq_bias = Ndarray.zeros Bigarray.Float32 bshape;
            }
    in
    let network =
      `Conv2d { kernel_size; stride; padding; out_filters; kernel_weights; bias_weights; optimizer }
    in
    { filters = out_filters; network = Inner (up.network, network) }

  let relu up = { up with network = Inner (up.network, `Relu) }

  let softmax ?(axis = 3) up = { up with network = Inner (up.network, `Softmax { axis }) }

  let maxpool2d kernel_size stride up =
    let kernel_size = match kernel_size with `One k -> (k, k) | `Two (ky, kx) -> (ky, kx) in
    let stride = match stride with `One s -> (s, s) | `Two (sy, sx) -> (sy, sx) in
    { up with network = Inner (up.network, `Maxpool2d { kernel_size; stride }) }

  let finalize { network; _ } = network
end

module Tfjs = struct
  module L = Tfjs_api.Layers

  module OptiMap = struct
    include Map.Make (Stdlib.String)
    module StringSet = Set.Make (Stdlib.String)

    let union_exn =
      union (fun name _ _ -> Printf.sprintf "variable name clash: <%s>" name |> failwith)

    let key_disjunction m m' =
      let keys = to_seq m |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
      let keys' = to_seq m' |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
      ( StringSet.diff keys keys' |> StringSet.elements,
        StringSet.diff keys' keys |> StringSet.elements )
  end

  type optimization_map = (float -> Tfjs_api.tensor Js.t -> unit) OptiMap.t

  type tflayer = Tfjs_api.layer Js.t

  type tfconv2d = Tfjs_api.conv2d Js.t

  type tfnode = Tfjs_api.symbolicTensor Js.t

  type ('a, 'b) accumulator = {
    input : tfnode option;
    network : tfnode option;
    optimizations : optimization_map;
    pack : unit -> network option;
  }

  let empty_accumulator =
    { input = None; network = None; optimizations = OptiMap.empty; pack = (fun () -> None) }

  (* Optimizer unpacking ************************************************************************ *)
  let unpack_sgd : tfconv2d -> optimization_map * (unit -> conv_optimizer) =
   fun tflayer ->
    let kupdate lr grad = Tfjs_api.sgd_updater#update tflayer##.kernel##.val_ lr grad in
    let bupdate lr grad = Tfjs_api.sgd_updater#update tflayer##.bias##.val_ lr grad in
    let kname = Printf.sprintf "%s/kernel" (Js.to_string tflayer##.name) in
    let bname = Printf.sprintf "%s/bias" (Js.to_string tflayer##.name) in
    let optimizations = [ (kname, kupdate); (bname, bupdate) ] |> List.to_seq |> OptiMap.of_seq in
    let pack () = `SGD in
    (optimizations, pack)

  let unpack_adam : tfconv2d -> adam_content -> optimization_map * (unit -> conv_optimizer) =
   fun tflayer conf ->
    let updater = Tfjs_api.create_adam_updater conf.epsilon conf.beta1 conf.beta2 conf.step in
    let kupdater = updater conf.rgrad_kernel conf.rgrad_sq_kernel in
    let bupdater = updater conf.rgrad_bias conf.rgrad_sq_bias in
    let kupdate lr grad = kupdater#update tflayer##.kernel##.val_ lr grad in
    let bupdate lr grad = bupdater#update tflayer##.bias##.val_ lr grad in
    let kname = Printf.sprintf "%s/kernel" (Js.to_string tflayer##.name) in
    let bname = Printf.sprintf "%s/bias" (Js.to_string tflayer##.name) in
    let optimizations = [ (kname, kupdate); (bname, bupdate) ] |> List.to_seq |> OptiMap.of_seq in
    let pack () =
      let step = kupdater#get_step##arraySync_intScalar in
      let rgrad_kernel = Tfjs_api.bigarray_of_tensor_float kupdater#get_rgrad in
      let rgrad_sq_kernel = Tfjs_api.bigarray_of_tensor_float kupdater#get_rgrad_sq in
      let rgrad_bias = Tfjs_api.bigarray_of_tensor_float bupdater#get_rgrad in
      let rgrad_sq_bias = Tfjs_api.bigarray_of_tensor_float bupdater#get_rgrad_sq in
      `Adam { conf with step; rgrad_kernel; rgrad_sq_kernel; rgrad_bias; rgrad_sq_bias }
    in
    (optimizations, pack)

  let unpack_optimizations :
      tfconv2d -> conv_optimizer -> optimization_map * (unit -> conv_optimizer) =
   fun tflayer conf ->
    match conf with `SGD -> unpack_sgd tflayer | `Adam conf -> unpack_adam tflayer conf

  (* Layers unpacking *************************************************************************** *)
  let unpack_conv2d : conv_content -> tflayer * optimization_map * (unit -> layer) =
   fun conf ->
    let { kernel_size; stride; padding; out_filters; kernel_weights; bias_weights; _ } = conf in
    let (ky, kx), (sy, sx) = (kernel_size, stride) in
    let weights = (kernel_weights, bias_weights) in
    let tflayer = L.conv2d ~weights (`Two (ky, kx)) padding (`Two (sy, sx)) out_filters in
    let optimizations, pack_optimizations = unpack_optimizations tflayer conf.optimizer in
    let pack () =
      let kernel_weights = Tfjs_api.bigarray_of_tensor_float tflayer##.kernel##.val_ in
      let bias_weights = Tfjs_api.bigarray_of_tensor_float tflayer##.bias##.val_ in
      `Conv2d { conf with kernel_weights; bias_weights; optimizer = pack_optimizations () }
    in
    ((tflayer :> tflayer), optimizations, pack)

  let unpack_maxpool2d : maxpool2d_content -> tflayer * optimization_map * (unit -> layer) =
   fun conf ->
    let { kernel_size = ky, kx; stride = sy, sx } = conf in
    let tflayer = L.max_pool2d (`Two (ky, kx)) (`Two (sy, sx)) in
    (tflayer, OptiMap.empty, fun () -> `Maxpool2d conf)

  let unpack_layer : layer -> tflayer * optimization_map * (unit -> layer) = function
    | `Conv2d conf -> unpack_conv2d conf
    | `Maxpool2d conf -> unpack_maxpool2d conf
    | `Relu -> (L.relu (), OptiMap.empty, fun () -> `Relu)
    | `Softmax { axis } -> (L.softmax ~axis (), OptiMap.empty, fun () -> `Softmax { axis })

  (* Network unpacking ************************************************************************** *)
  let unpack : int -> int -> network -> tfnode * tfnode * optimization_map * (unit -> network) =
   (* Transform a `network` to everything that is needed to perform a training of that network
    * using tensorflow.js:
    * 1. An input symbolic tensor
    * 2. An output symbolic tensor
    * 3. A map of callbacks that each update a weight tensor given its gradient
    * 4. A thunk to be called to pack everything back to a `network`
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
    match fold_top_down aux empty_accumulator net with
    | { input = Some input; network = Some network; optimizations; pack; _ } ->
        let pack () =
          match pack () with None -> failwith "unreachable" | Some network -> network
        in
        (input, network, optimizations, pack)
    | _ -> failwith "unreachable"
end

module Make_Tfjs_backend (Backend : sig
  val v : Tfjs_api.backend
end) =
struct
  (* TODO: Generic Train/eval functions, what parameters?
   * Move inside MNIST?
   * let train : ~verbose:bool -> ~progress:_ -> ~h:int -> ~w:int -> ~lr:(int -> float)
   *          -> ~datagen:_ (defines batch_size and image count)
   *          -> ~encoders:network list -> ~decoder:network
   *          -> network list * network
   * let predict : ~verbose:bool -> ~progress:_ -> ~h:int -> ~w:int ->
   *          -> ~datagen:_ (defines batch_size and image count)
   *          -> ~encoders:network list -> ~decoder:network
   *          -> Ndarray.t Iter.t (image iterator)
   * let eval : ~verbose:bool -> ~progress:_ -> ~h:int -> ~w:int ->
   *          -> ~datagen:_ (defines batch_size and image count)
   *          -> ~encoders:network list -> ~decoder:network
   *          -> Ndarray.t (confusion_matrix)
   *)

  let train () = Tfjs_api.setup_backend Backend.v
end

let main' train_imgs train_labs test_imgs test_labs =
  let open Builder in
  let open Lwt.Infix in
  Tfjs_api.setup_backend `Webgl >>= fun _ ->

  let optimizer = `Adam (0.9, 0.999, 1e-10) in
  let nn =
    input2d 1
    |> conv2d (`One 4) false (`One 2) 10 `Tanh optimizer
    |> relu
    |> conv2d (`One 3) false (`One 2) 10 `Tanh optimizer
    |> relu
    |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer
    |> relu |> finalize
  in
  let x, encoder, optimizations, pack = Tfjs.unpack 28 28 nn in
  let x', decoder, optimizations', pack' =
    input2d 10
    |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer
    |> maxpool2d (`One 2) (`One 2)
    |> softmax |> finalize |> Tfjs.unpack 28 28
  in

  print_endline (String.of_network nn);

  let optimizations = Tfjs.OptiMap.union_exn optimizations optimizations' in

  let y = Tfjs_api.Layers.chain encoder [ x' ] decoder in
  let m = Tfjs_api.model [ x ] [ y ] in

  (* No need to compile the model *)
  let slice a b arr =
    Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
  in

  let train_on_batch i =
    let time = (new%js Js.date_now)##valueOf /. 1000. in

    let j = Random.int 50000 in
    let batch_size = 10000 in
    let imgs =
      train_imgs
      |> slice (16 + (28 * 28 * j)) (16 + (28 * 28 * (j + batch_size)))
      |> Tfjs_api.tensor_of_ta [| batch_size; 28; 28; 1 |]
    in
    let labs = train_labs |> slice (8 + j) (8 + (j + batch_size)) |> Tfjs_api.one_hot_of_ta 10 in
    let labs = labs##reshape (Js.array [| batch_size; 1; 1; 10 |]) in
    let f () =
      (* The tfjs models use `execute` both inside `predict` and `train` functions,
       * with a `training: bool` parameter.
       * this means that the graph is always allocated and kept in memory, even if
       * no backward will follow. *)
      let pred = m##predict imgs in
      let loss = Tfjs_api.categorical_crossentropy 1e-10 pred labs in
      let loss = Tfjs_api.Ops.sum false loss in
      loss
    in
    let loss, grads = Tfjs_api.variable_grads f in
    ( match Tfjs.OptiMap.key_disjunction optimizations grads with
    | [], [] -> ()
    | name :: _, _ -> Printf.sprintf "Missing at least the <%s> gradient" name |> failwith
    | _, name :: _ -> Printf.sprintf "Missing at least the <%s> optimizer" name |> failwith );

    let lr = 1e-3 in
    Tfjs.OptiMap.iter
      (fun name optimization -> optimization lr (Tfjs_api.Named_tensor_map.find name grads))
      optimizations;

    let time' = (new%js Js.date_now)##valueOf /. 1000. in
    Printf.printf "Step %5d done, loss:%9.6f, took:%.3fsec\n%!" i
      (Bigarray.Genarray.get (Tfjs_api.bigarray_of_tensor_float loss) [| 0 |])
      (time' -. time);
    ignore (time, time', i, loss)
  in
  let rec aux = function
    | 20 -> Lwt.return ()
    | i ->
        Tfjs_api.tidy (fun () -> train_on_batch i);
        Lwt_js.sleep 0.25 >>= fun () -> aux (i + 1)
  in
  aux 0 >>= fun _ ->
  print_endline (String.of_network (pack ()));
  ignore (train_imgs, train_labs, test_imgs, test_labs);
  ignore (pack, pack');
  Lwt.return ()

let main train_imgs train_labs test_imgs test_labs =
  let open Lwt.Infix in
  Tfjs_api.tidy_lwt (fun () -> main' train_imgs train_labs test_imgs test_labs) >>= fun () ->
  Tfjs_api.disposeVariables ();
  Firebug.console##log (Tfjs_api.memory ());

  Lwt.return ()
