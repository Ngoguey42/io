module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic

type float32_nd = (float, Bigarray.float32_elt) Ndarray.t

type int32_nd = (int, Bigarray.int32_elt) Ndarray.t

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
  | Softmax2d of { axis : int; }

type network =
  | Input2d of { out_filters : int; dtype : [ `Float32 | `Uint8 ] }
  | Inner of (network * layer)

module Network_builder = struct
  type upstream = { filters : int; network : network }

  let input2d ?(dtype = `Float32) f = { filters = f; network = Input2d { out_filters = f; dtype } }

  let conv2d kernel_size padding stride out_filters optimizer up =
    (* TODO: Initializer *)
    let ((ky, kx) as kernel_size) =
      match kernel_size with `One k -> (k, k) | `Two (ky, kx) -> (ky, kx)
    in
    let kshape = [| ky; kx; up.filters; out_filters |] in
    let bshape = [| out_filters |] in
    let stride = match stride with `One s -> (s, s) | `Two (sy, sx) -> (sy, sx) in
    let kernel_weights = Ndarray.empty Bigarray.Float32 kshape in
    let bias_weights = Ndarray.empty Bigarray.Float32 bshape in
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

  let relu up = { up with network = Inner (up.network, Relu ) }

  let softmax2d ?(axis = 3) up =
    { up with network = Inner (up.network, Softmax2d { axis }) }

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

module Tfjs = struct
  type accu = {
    input : Tfjs_api.symbolicTensor Js.t option;
    network : Tfjs_api.symbolicTensor Js.t option;
    optimizers : Tfjs_api.optimizer Js.t list;
  }

  let layer_to_layer : layer -> Tfjs_api.layer Js.t = function
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
        Tfjs_api.conv2d ~weights (`Two (ky, kx)) padding (`Two (sy, sx)) out_filters
    | Maxpool2d { kernel_size = ky, kx; stride = sy, sx } ->
        Tfjs_api.max_pool2d (`Two (ky, kx)) (`Two (sy, sx))
    | Relu -> Tfjs_api.relu ()
    | Softmax2d { axis } -> Tfjs_api.softmax ~axis ()

  let unpack : network -> 'a =
   fun net ->
    let aux acc net =
      match (acc.input, acc.network, net) with
      | None, None, Input2d { out_filters; dtype } ->
          let x = Tfjs_api.input ~dtype [| 28; 28; out_filters |] in
          { acc with input = Some x; network = Some x }
      | Some _, Some tfnet, Inner (_, layer) ->
          { acc with network = Some ((layer_to_layer layer)##apply tfnet) }
      | _, _, _ -> failwith "unreachable"
    in
    match fold_top_down aux { input = None; network = None; optimizers = [] } net with
    | { input = Some input; network = Some network; _ } -> (input, network)
    | _ -> failwith "unreachable"
end

let main train_imgs train_labs test_imgs test_labs =
  let open Network_builder in
  let optimizer = `SGD in
  (* let optimizer = `Adam (0.9, 0.999) in *)
  let x, encoder =
    input2d 1
    |> conv2d (`One 4) false (`One 2) 10 optimizer
    |> relu
    |> conv2d (`One 3) false (`One 2) 10 optimizer
    |> relu
    |> conv2d (`One 3) false (`One 1) 10 optimizer
    |> relu |> finalize |> Tfjs.unpack
  in
  let x', decoder =
    input2d 10
    |> conv2d (`One 3) false (`One 1) 10 optimizer
    |> maxpool2d (`One 2) (`One 2)
    |> softmax2d |> finalize |> Tfjs.unpack
  in

  let y = Tfjs_api.chain encoder [ x' ] decoder in

  let m = Tfjs_api.model [ x ] [ y ] in

  Firebug.console##log m;
  ignore (train_imgs, train_labs, test_imgs, test_labs, x, m);
  Lwt.return ()
