module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Graph = Owl_neural_generic.Make_Embedded (Owl_base_dense_ndarray.S)
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module Typed_array = Js_of_ocaml.Typed_array

type float32_nd = (float, Bigarray.float32_elt) Ndarray.t

type int32_nd = (int, Bigarray.int32_elt) Ndarray.t

(* TODO: Move what can be moved to ft_owlbase
   rename to network ? make type network a type t? make Network_builder a Builder ?
*)

type conv_optimizer =
  | Adam of {
      (* beta 1 *)
      beta1 : float;
      (* beta 2 *)
      beta2 : float;
      (* number of updates performed *)
      step : int;
      (* Running gradient of kernel *)
      rgrad_kernel : float32_nd;
      (* Running gradient squared of kernel *)
      rgrad_sq_kernel : float32_nd;
      (* Running gradient of bias *)
      rgrad_bias : float32_nd;
      (* Running gradient squared of bias *)
      rgrad_sq_bias : float32_nd;
      (* TODO: Epsilon *)
    }
  | SGD

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

type layer = [
  | `Conv2d of conv_content
  | `Maxpool2d of maxpool2d_content
  | `Relu
  | `Softmax of softmax_content
]

type network =
  | Input2d of { out_filters : int; dtype : [ `Float32 | `Uint8 ] }
  | Inner of (network * layer)

module String = struct

  let of_dtype = function
    | `Float32 -> "float32"
    | `Uint8 -> "uint8"
    | `Int32 -> "int32"

  let of_conv_optimizer = function
    | SGD -> "SGD"
    | Adam c ->
       let mean_val arr beta =
         if c.step == 0 then
           0.
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
       let mean_val arr = Ndarray.sum' arr /. (float_of_int (Ndarray.numel arr)) in
       let ky, kx = c.kernel_size in
       let sy, sx = c.stride in
       Printf.sprintf "Conv2d k:%dx%d s:%dx%d <%d> k:%+.3e b:%+.3e (%s)"
                      ky kx sy sx c.out_filters
                      (mean_val c.kernel_weights)
                      (mean_val c.bias_weights)
                      (of_conv_optimizer c.optimizer)
    | `Maxpool2d _ -> "Maxpool2d"
    | `Relu -> "Relu"
    | `Softmax _ -> "Softmax"

  let rec of_network = function
    | Input2d { out_filters; dtype } -> Printf.sprintf "| Input2d <%d> %s" out_filters (of_dtype dtype)
    | Inner (upstream, layer) -> Printf.sprintf "%s\n> %s" (of_network upstream) (of_layer layer)
end

let rec fold_bottom_up f x net =
  match net with Inner (net', _) -> fold_bottom_up f (f x net) net' | Input2d _ -> f x net

let rec fold_top_down f x net =
  match net with Inner (net', _) -> f (fold_top_down f x net') net | Input2d _ -> f x net

module Network_builder = struct
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
      | `SGD -> SGD
      | `Adam (beta1, beta2) ->
          Adam
            {
              beta1;
              beta2;
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

let str t =
  (* TODO: Throw away? *)
  let t = t##toString in
  let t = t##replace_string (Js.string "Tensor") (Js.string "") in
  let t = t##trim in
  t |> Js.to_string

module Tfjs = struct
  type optimization_map = (float -> Tfjs_api.tensor Js.t -> unit) StringMap.t

  type ('a, 'b) accu = {
    input : Tfjs_api.symbolicTensor Js.t option;
    network : Tfjs_api.symbolicTensor Js.t option;
    optimizations : optimization_map;
    pack : (unit -> network option);
  }

  type tflayer = Tfjs_api.layer Js.t

  let unpack_optimizations : layer -> tflayer -> optimization_map =
   fun layer tflayer ->
    match (layer, Tfjs_api.Layers.classify tflayer) with
    | `Conv2d { optimizer = SGD; _ }, `Conv2d tflayer ->
        let apply_grads which lr grad =
          let weights =
            match which with `Kernel -> tflayer##.kernel##.val_ | `Bias -> tflayer##.bias##.val_
          in
          Tfjs_api.sgd_updater#update weights lr grad
        in
        let kname = Printf.sprintf "%s/kernel" (Js.to_string tflayer##.name) in
        let bname = Printf.sprintf "%s/bias" (Js.to_string tflayer##.name) in
        StringMap.empty
        |> StringMap.add kname (apply_grads `Kernel)
        |> StringMap.add bname (apply_grads `Bias)
    | `Conv2d { optimizer = Adam conf; _ }, `Conv2d tflayer ->
        let updater = Tfjs_api.create_adam_updater 1e-10 conf.beta1 conf.beta2 conf.step in
        let kernel_updater = updater conf.rgrad_kernel conf.rgrad_sq_kernel in
        let bias_updater = updater conf.rgrad_bias conf.rgrad_sq_bias in
        let apply_grads which lr grad =
          match which with
          | `Kernel -> kernel_updater#update tflayer##.kernel##.val_ lr grad
          | `Bias -> bias_updater#update tflayer##.bias##.val_ lr grad
        in
        let kname = Printf.sprintf "%s/kernel" (Js.to_string tflayer##.name) in
        let bname = Printf.sprintf "%s/bias" (Js.to_string tflayer##.name) in
        StringMap.empty
        |> StringMap.add kname (apply_grads `Kernel)
        |> StringMap.add bname (apply_grads `Bias)
    | _, _ -> StringMap.empty

  let unpack_layer : layer -> tflayer =
   fun layer ->
    let open Tfjs_api.Layers in
    match layer with
    | `Conv2d
        {
          kernel_size = ky, kx;
          stride = sy, sx;
          padding;
          out_filters;
          kernel_weights;
          bias_weights;
          _;
        } ->
        let weights = (kernel_weights, bias_weights) in
        let tflayer = conv2d ~weights (`Two (ky, kx)) padding (`Two (sy, sx)) out_filters in
        (tflayer :> tflayer)
    | `Maxpool2d { kernel_size = ky, kx; stride = sy, sx } ->
        max_pool2d (`Two (ky, kx)) (`Two (sy, sx))
    | `Relu -> relu ()
    | `Softmax { axis } -> softmax ~axis ()

  let unpack : int -> int -> network -> 'a =
   fun h w net ->
    let aux acc net =
      match (acc.input, acc.network, net) with
      | None, None, Input2d { out_filters; dtype } ->
         let x = Tfjs_api.Layers.input ~dtype [| h; w; out_filters |] in
         let pack () =
           let network = match acc.pack () with
             | Some _ -> failwith "no"
             | None -> net
           in
           Some network
         in
         { acc with input = Some x; network = Some x; pack }
      | Some _, Some tfnet, Inner (_, layer) ->
          let tflayer = unpack_layer layer in
          let optimizations = unpack_optimizations layer tflayer in
          let pack () =
            let network = match acc.pack () with
              | None -> failwith "no"
              | Some upstream -> Inner (upstream, layer)
            in
            Some network
          in
          {
            acc with
            network = Some (tflayer##apply tfnet);
            optimizations =
              StringMap.union
                (fun name _ _ -> Printf.sprintf "variable name clash: <%s>" name |> failwith)
                optimizations acc.optimizations;
            pack;
          }
      | _, _, _ -> failwith "unreachable"
    in
    match
      fold_top_down aux { input = None; network = None; optimizations = StringMap.empty; pack = (fun () -> None) } net
    with
    | { input = Some input; network = Some network; optimizations; pack; _ } ->
       let pack () = match pack () with None -> failwith "no3" | Some network -> network in
        (input, network, optimizations, pack)
    | _ -> failwith "unreachable"
end

let main' train_imgs train_labs test_imgs test_labs =
  let open Network_builder in
  let open Lwt.Infix in

  Tfjs_api.setup_backend `Webgl >>= fun _ ->

  (* let optimizer = `SGD in *)
  let optimizer = `Adam (0.9, 0.999) in
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
  print_endline (String.of_network (pack ()));

  let optimizations =
    StringMap.union
      (fun name _ _ -> Printf.sprintf "variable name clash: <%s>" name |> failwith)
      optimizations optimizations'
  in
  let opti_names =
    StringMap.to_seq optimizations |> Seq.map (fun (name, _) -> name) |> StringSet.of_seq
  in

  let y = Tfjs_api.chain encoder [ x' ] decoder in
  let m = Tfjs_api.model [ x ] [ y ] in
  (* No need to compile the model *)

  let slice a b arr =
    Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
  in

  let train_on_batch i =
    let time = (new%js Js.date_now)##valueOf /. 1000. in

    Printf.eprintf
      "********************************************************************************\n%!";
    let j = Random.int 50000 in
    let batch_size = 10000 in
    let imgs =
      train_imgs
      |> slice (16 + (28 * 28 * j)) (16 + (28 * 28 * (j + batch_size)))
      |> Tfjs_api.tensor_of_ta [| batch_size; 28; 28; 1 |]
    in
    let labs =
      train_labs |> slice (8 + j) (8 + (j + batch_size)) |> Tfjs_api.one_hot_of_ta 10
    in
    let labs = labs##reshape (Js.array [| batch_size; 1; 1; 10 |]) in
    let f () =

      (* The tfjs models use `execute` both inside `predict` and `train` functions,
       * with a `training: bool` parameter.
       * this means that the graph is always allocated and kept in memory, even if
       * no backward will follow. *)
      let pred = m##predict imgs in
      let loss = Tfjs_api.categorical_crossentropy 1e-10 pred labs in
      let loss = Tfjs_api.Ops.sum false loss in
      Printf.printf "Loss: %s\n%!" (str loss);
      loss
    in

    let _, grads = Tfjs_api.variable_grads f in
    let grad_names =
      Tfjs_api.Named_tensor_map.to_seq grads
      |> Seq.map (fun (name, _) -> name)
      |> StringSet.of_seq
    in

    let no_grad = StringSet.diff opti_names grad_names in
    let no_opti = StringSet.diff grad_names opti_names in
    ( match (StringSet.choose_opt no_grad, StringSet.choose_opt no_opti) with
      | None, None -> ()
      | Some name, _ -> Printf.sprintf "Missing at least the <%s> gradient" name |> failwith
      | _, Some name -> Printf.sprintf "Missing at least the <%s> optimizer" name |> failwith );

    let lr = 1e-3 in
    StringMap.iter
      (fun name optimization -> optimization lr (Tfjs_api.Named_tensor_map.find name grads))
      optimizations;

    let time' = (new%js Js.date_now)##valueOf /. 1000. in
    Printf.printf "> %d took %fsec\n%!" i (time' -. time);
    (* TODO: Implement repacking *)
  in
  let rec aux = function
    | 1 -> Lwt.return ()
    | i -> Tfjs_api.tidy (fun () -> train_on_batch i);
           Lwt_js.sleep 0.25 >>= fun () -> aux (i + 1)
  in
  aux 0 >>= fun _ ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);
  ignore (pack, pack');
  Lwt.return ()

let main train_imgs train_labs test_imgs test_labs =
  let open Lwt.Infix in

  Tfjs_api.tidy_lwt (fun () -> main' train_imgs train_labs test_imgs test_labs) >>= fun () ->
  Tfjs_api.disposeVariables ();
  Firebug.console##log (Tfjs_api.memory ());

  Lwt.return ()
