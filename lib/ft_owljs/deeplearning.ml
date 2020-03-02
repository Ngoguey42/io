module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Graph = Owl_neural_generic.Make_Embedded (Owl_base_dense_ndarray.S)
module Lwt_js = Js_of_ocaml_lwt.Lwt_js

type float32_nd = (float, Bigarray.float32_elt) Ndarray.t

type int32_nd = (int, Bigarray.int32_elt) Ndarray.t

(* TODO: Move what can be moved to ft_owlbase *)

type conv_optimizer =
  | Adam of {
      (* beta 1 *)
      beta_rgrad : float;
      (* beta 2 *)
      beta_rgrad_sq : float;
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
    }
  | SGD

type layer =
  | Conv2d of {
      kernel_size : int * int;
      stride : int * int;
      padding : bool;
      out_filters : int;
      kernel_weights : float32_nd;
      bias_weights : float32_nd;
      optimizer : conv_optimizer;
    }
  | Maxpool2d of { kernel_size : int * int; stride : int * int }
  | Relu
  | Softmax2d of { axis : int }

type network =
  | Input2d of { out_filters : int; dtype : [ `Float32 | `Uint8 ] }
  | Inner of (network * layer)

module Network_builder = struct
  type upstream = { filters : int; network : network }

  let input2d ?(dtype = `Float32) f = { filters = f; network = Input2d { out_filters = f; dtype } }

  let conv2d kernel_size padding stride out_filters initialization optimizer up =
    (* TODO: Initializer *)
    let ((ky, kx) as kernel_size) =
      match kernel_size with `One k -> (k, k) | `Two (ky, kx) -> (ky, kx)
    in
    let kshape = [| ky; kx; up.filters; out_filters |] in
    let bshape = [| out_filters |] in
    let stride = match stride with `One s -> (s, s) | `Two (sy, sx) -> (sy, sx) in

    (* let kernel_weights = Ndarray.ones Bigarray.Float32 kshape in *)
    (* let bias_weights = Ndarray.ones Bigarray.Float32 bshape in *)

    (* Neuron.Init.run : typ -> int array -> t -> t *)
    let kernel_weights = Ft_owlbase.Init.run initialization kshape in
    let bias_weights = Ft_owlbase.Init.run initialization bshape in

    let optimizer =
      match optimizer with
      | `SGD -> SGD
      | `Adam (beta_rgrad, beta_rgrad_sq) ->
          Adam
            {
              beta_rgrad;
              beta_rgrad_sq;
              step = 0;
              rgrad_kernel = Ndarray.zeros Bigarray.Float32 kshape;
              rgrad_sq_kernel = Ndarray.zeros Bigarray.Float32 kshape;
              rgrad_bias = Ndarray.zeros Bigarray.Float32 bshape;
              rgrad_sq_bias = Ndarray.zeros Bigarray.Float32 bshape;
            }
    in
    let network =
      Conv2d { kernel_size; stride; padding; out_filters; kernel_weights; bias_weights; optimizer }
    in
    { filters = out_filters; network = Inner (up.network, network) }

  let relu up = { up with network = Inner (up.network, Relu) }

  let softmax2d ?(axis = 3) up = { up with network = Inner (up.network, Softmax2d { axis }) }

  let maxpool2d kernel_size stride up =
    let kernel_size = match kernel_size with `One k -> (k, k) | `Two (ky, kx) -> (ky, kx) in
    let stride = match stride with `One s -> (s, s) | `Two (sy, sx) -> (sy, sx) in
    { up with network = Inner (up.network, Maxpool2d { kernel_size; stride }) }

  let finalize { network; _ } = network
end

let rec fold_bottom_up f x net =
  match net with Inner (net', _) -> fold_bottom_up f (f x net) net' | Input2d _ -> f x net

let rec fold_top_down f x net =
  match net with Inner (net', _) -> f (fold_top_down f x net') net | Input2d _ -> f x net

let str t =
  let t = t##toString in
  let t = t##replace_string (Js.string "Tensor") (Js.string "") in
  let t = t##trim in
  t |> Js.to_string

module Tfjs = struct
  type 'a accu = {
    input : Tfjs_api.symbolicTensor Js.t option;
    network : Tfjs_api.symbolicTensor Js.t option;
    optimizations : (Tfjs_api.named_tensor_map Js.t -> unit) list;
  }

  type tflayer = Tfjs_api.layer Js.t

  let tflayer_of_layer : layer -> tflayer =
   fun layer ->
    let open Tfjs_api.Layers in
    match layer with
    | Conv2d
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
    | Maxpool2d { kernel_size = ky, kx; stride = sy, sx } ->
        max_pool2d (`Two (ky, kx)) (`Two (sy, sx))
    | Relu -> relu ()
    | Softmax2d { axis } -> softmax ~axis ()

  let optimizations_of_layer : layer -> tflayer -> 'a list =
   fun layer tflayer ->
    match (layer, Tfjs_api.Layers.classify tflayer) with
    | Conv2d { optimizer = SGD; _ }, `Conv2d tflayer ->
        let o = Tfjs_api.sgd 1e-4 in
        let apply_grads (grads : Tfjs_api.named_tensor_map Js.t) =
          let kname = tflayer##.kernel##.name in
          let bname = tflayer##.bias##.name in
          let kgrad : Tfjs_api.tensor Js.t = Js.Unsafe.get grads kname in
          let bgrad : Tfjs_api.tensor Js.t = Js.Unsafe.get grads bname in

          Printf.printf "----- Updating weights of %s\n%!" (Js.to_string bname);
          Printf.printf "bias grad   : %s\n%!" (str bgrad);
          Printf.printf "bias weights: %s\n%!" (str tflayer##.bias##.val_);

          ignore (o, kgrad, bgrad)
          (* TODO: Fix grad *)
          (* o##applyGradients_array (Js.array [| kgrad; bgrad |]) *)
        in
        [ apply_grads ]
        (* TODO: Implement adam *)
        (* | Conv2d { optimizer = Adam _; _ }, `Conv2d tflayer -> *)
        (* ignore tflayer; *)
        (* [Tfjs_api.sgd ()] *)
    | _, _ -> []

  let unpack : network -> 'a =
    (* TODO: Implement repacking *)
   fun net ->
    let aux acc net =
      match (acc.input, acc.network, net) with
      | None, None, Input2d { out_filters; dtype } ->
          let x = Tfjs_api.Layers.input ~dtype [| 28; 28; out_filters |] in
          { acc with input = Some x; network = Some x }
      | Some _, Some tfnet, Inner (_, layer) ->
          let tflayer = tflayer_of_layer layer in
          let optimizations = optimizations_of_layer layer tflayer in
          {
            acc with
            network = Some (tflayer##apply tfnet);
            optimizations = optimizations @ acc.optimizations;
          }
      | _, _, _ -> failwith "unreachable"
    in
    match fold_top_down aux { input = None; network = None; optimizations = [] } net with
    | { input = Some input; network = Some network; optimizations } ->
        (input, network, optimizations)
    | _ -> failwith "unreachable"
end

let main train_imgs train_labs test_imgs test_labs =
  let open Network_builder in
  let open Lwt.Infix in
  let optimizer = `SGD in
  (* let optimizer = `Adam (0.9, 0.999) in *)
  let x, encoder, optimizations =
    input2d 1
    |> conv2d (`One 4) false (`One 2) 10 `Tanh optimizer
    |> relu
    |> conv2d (`One 3) false (`One 2) 10 `Tanh optimizer
    |> relu
    |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer
    |> relu |> finalize |> Tfjs.unpack
  in
  let x', decoder, optimizations' =
    input2d 10
    |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer
    |> maxpool2d (`One 2) (`One 2)
    |> softmax2d |> finalize |> Tfjs.unpack
  in

  let y = Tfjs_api.chain encoder [ x' ] decoder in
  let m = Tfjs_api.model [ x ] [ y ] in

  let slice a b arr =
    Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
  in

  let rec aux = function
    | 2 -> Lwt.return ()
    | i ->
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

        let f =
          Js.wrap_callback (fun () ->
              (* TODO: predict keeps the graph?!?!?!? why is it working *)
              let pred = m##predict imgs in

              let loss = Tfjs_api.categorical_crossentropy 1e-10 pred labs in
              let loss = Tfjs_api.Ops.sum false loss in
              Printf.printf "Loss: %s\n%!" (str loss);
              loss)
        in
        let _, grads = Tfjs_api.variable_grads f in

        (* List.iter (fun optimization -> optimization grads) optimizations'; *)
        (Tfjs_api.sgd 1e-4)##applyGradients_map grads;
        (* List.iter (fun optimization -> optimization grads) optimizations; *)
        List.iter (fun optimization -> optimization grads) optimizations';

        Lwt_js.sleep 0.25 >>= fun () -> aux (i + 1)
  in
  aux 0 >>= fun _ ->
  ignore (train_imgs, train_labs, test_imgs, test_labs, x, m, optimizations, optimizations');
  Lwt.return ()
