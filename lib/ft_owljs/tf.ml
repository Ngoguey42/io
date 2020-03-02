module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array

type uint8_ta = Typed_array.uint8Array Js.t
type float32_ta = Typed_array.float32Array Js.t
type float32_nd = (float, Bigarray.float32_elt) Ndarray.t

class type symbolicTensor =
  object
  end

class type tensor =
  object
    method shape : int Js.js_array Js.t Js.readonly_prop
    method data : float32_ta Ft_js.promise Js.t Js.meth
    method asType : Js.js_string Js.t Js.meth
    method print : unit Js.meth
    method reshape : int Js.js_array Js.t -> tensor Js.t Js.meth
  end

class type layer =
  object
    method getWeights : tensor Js.js_array Js.t Js.meth
    method apply : symbolicTensor Js.t -> symbolicTensor Js.t Js.meth
  end

class type conv2d =
  object
    inherit layer
  end

class type optimizer =
  object
  end
class type adam =
  object
    inherit optimizer
  end
class type model =
  object
    method layers : layer Js.js_array Js.t Js.readonly_prop
                          (* method compile : < .. > Js.t Js.meth *)
    method trainOnBatch : tensor Js.t -> tensor Js.t -> unit Ft_js.promise Js.t Js.meth
    method predict : tensor Js.t -> tensor Js.t Js.meth
  end

let tensor_of_ta : int array -> float32_ta -> tensor Js.t = fun shape ta ->
  let open Js.Unsafe in
  fun_call global##.tf##.tensor [| inject ta; shape |> Js.array |> inject  |]

let one_hot_of_ta : int -> uint8_ta -> tensor Js.t = fun width arr ->
  let open Js.Unsafe in
  let arr = fun_call global##.tf##.tensor1d [| inject arr; "int32" |> Js.string |> inject |] in
  fun_call global##.tf##.oneHot [| inject arr; inject width |]

let ones : int array -> tensor Js.t = fun shape ->
  let open Js.Unsafe in
  fun_call global##.tf##.ones [| shape |> Js.array |> inject |]

let input : int array -> symbolicTensor Js.t = fun shape ->
  let open Js.Unsafe in
  let params = object%js (self)
      val shape = Js.array shape
    end
  in
  fun_call global##.tf##.input [| params |> inject |]

let conv2d : ?weights:(float32_nd * float32_nd) -> _ -> _ -> _ -> _ -> conv2d Js.t =
  fun ?weights kernel_size padding stride out_filters ->
  let padding = match padding with true -> "same" | false -> "valid" in
  let kx, ky = match kernel_size with `One kx -> (kx, kx) | `Two (kx, ky) -> (kx, ky) in
  let sx, sy = match stride with `One sx -> (sx, sx) | `Two (sx, sy) -> (sx, sy) in
  let weights = match weights with
    | None -> Js.Opt.empty
    | Some (kernel, bias) ->
       let shape = Ndarray.shape kernel in
       let kernel = kernel|> Conv.Reinterpret.Float32.ta_of_nd |> tensor_of_ta shape in
       let shape = Ndarray.shape bias in
       let bias = bias|> Conv.Reinterpret.Float32.ta_of_nd |> tensor_of_ta shape in
       [| kernel, bias |] |> Js.array |> Js.Opt.return
  in
  let params = object%js (self)
      val kernelSize = Js.array [| kx; ky |]
      val filters = out_filters
      val strides = Js.array [| sx; sy |]
      val padding = Js.string padding
      val weights = weights
    end
  in
  let open Js.Unsafe in
  fun_call global##.tf##.layers##.conv2d [| inject params |]

let max_pool2d kernel_size stride : layer Js.t =
  let kx, ky = match kernel_size with `One kx -> (kx, kx) | `Two (kx, ky) -> (kx, ky) in
  let sx, sy = match stride with `One sx -> (sx, sx) | `Two (sx, sy) -> (sx, sy) in
  let params = object%js (self)
      val poolSize = Js.array [| kx; ky |]
      val strides = Js.array [| sx; sy |]
    end
  in
  let open Js.Unsafe in
  fun_call global##.tf##.layers##.maxPooling2d [| inject params |]

let relu () : layer Js.t =
  let open Js.Unsafe in
  fun_call global##.tf##.layers##.reLU [| |]

let softmax axis : layer Js.t =
  let params = object%js (self)
      val axis = axis
    end
  in
  let open Js.Unsafe in
  fun_call global##.tf##.layers##.softmax [| inject params |]

let model : symbolicTensor Js.t list -> symbolicTensor Js.t list -> model Js.t =
  fun inputs outputs ->
  let params = object%js (self)
       val inputs = inputs |> Array.of_list |> Js.array
       val outputs = outputs |> Array.of_list |> Js.array
    end
  in
  let open Js.Unsafe in
  fun_call global##.tf##.model [| inject params |]

let adam : float -> float -> float -> float -> adam Js.t = fun lr beta1 beta2 epsilon ->
  let open Js.Unsafe in
  fun_call global##.tf##.train##.adam [| inject lr; inject beta1; inject beta2; inject epsilon |]

let compile : model Js.t -> optimizer Js.t -> string -> unit = fun m optim loss ->
  let params = object%js (self)
      val optimizer = optim
      val loss = Js.string loss
    end
  in
  let open Js.Unsafe in
  meth_call m "compile" [| inject params |]


let main train_imgs train_labs test_imgs test_labs =

  let (||>) : symbolicTensor Js.t -> layer Js.t -> symbolicTensor Js.t = fun a b -> b##apply a in

  Printf.eprintf "Coucou\n%!";
  let weights = [| ones [| 4; 4; 1; 10 |]; ones [| 10 |] |] in

  Printf.eprintf "a\n%!";
  let x = input [|28; 28; 1|] in
  Printf.eprintf "b\n%!";
  let y =
    x
    ||> conv2d (`One 4) false (`One 2) 10
    ||> relu ()
    ||> conv2d (`One 3) false (`One 2) 10
    ||> relu ()
    ||> conv2d (`One 3) false (`One 1) 10
    ||> relu ()
    ||> conv2d (`One 3) false (`One 1) 10
    ||> max_pool2d (`One 2) (`One 2)
    ||> softmax 3
  in
  Printf.eprintf "c\n%!";
  let m = model [x] [y] in
  Firebug.console##log m;
  let optim = adam 1e-3 0.9 0.999 1e-10 in
  compile m optim "categoricalCrossentropy";

  let slice a b arr =
    Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
  in

  let j = Random.int 55000 in
  let batch_size = 1 in
  let imgs = train_imgs |> slice (16 + (28 * 28 * j)) (16 + (28 * 28 * (j + batch_size))) |> tensor_of_ta [| batch_size; 28; 28; 1 |] in
  let labs = train_labs |> slice (8 + j) (8 + (j + batch_size)) |> one_hot_of_ta 10 in
  let labs = labs##reshape (Js.array [| batch_size; 1; 1; 10 |]) in
  Firebug.console##log imgs##.shape;
  Firebug.console##log labs##.shape;

  (* let open Lwt.Infix in *)
  let preds = m##predict imgs in
  preds##print;
  (* m##trainOnBatch imgs labs |> Ft_js.wrap_promise >>= fun _ -> *)

  ignore (train_imgs, train_labs, test_imgs, test_labs, weights, x, y, m, optim, labs, imgs, preds);
  Lwt.return ()
