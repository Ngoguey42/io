(* Tensorflow.js js_of_ocaml api *)

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

class type symbolicTensor = object end

class type tensor =
  object
    method shape : int Js.js_array Js.t Js.readonly_prop

    method data : float32_ta Ft_js.promise Js.t Js.meth

    method asType : Js.js_string Js.t Js.meth

    method print : unit Js.meth

    method reshape : int Js.js_array Js.t -> tensor Js.t Js.meth

    method toString : Js.js_string Js.t Js.meth
  end

class type variable =
  object
    inherit tensor

    method assign : tensor Js.t -> unit Js.meth
  end

class type layer_variable =
  object
    method name : Js.js_string Js.t Js.readonly_prop

    (* Tfjs documentation states that `.val` only contains the initial value of the variable.
     * The optimizers inside tfjs all use tf.engine().registeredVariables to retrieve their
     * variables, but it seems that using `layer.(bias|weight).val` works too.
     *)
    method val_ : variable Js.t Js.readonly_prop
  end

module Named_tensor_map = struct
  class type ['a] js_t =
    object
      constraint 'a = #tensor Js.t
    end

  include Map.Make (String)

  let of_js : 'a js_t Js.t -> 'a t =
   fun map_js ->
    Js.object_keys map_js |> Js.to_array |> Array.to_list
    |> List.fold_left (fun map k -> add (Js.to_string k) (Js.Unsafe.get map_js k) map) empty
end

class type layer =
  object
    method getWeights : tensor Js.t Js.js_array Js.t Js.meth

    method apply : symbolicTensor Js.t -> symbolicTensor Js.t Js.meth

    method getClassName : Js.js_string Js.t Js.meth
  end

class type conv2d =
  object
    inherit layer

    method kernel : layer_variable Js.t Js.readonly_prop

    method bias : layer_variable Js.t Js.readonly_prop
  end

class type optimizer =
  object
    method applyGradients_array : tensor Js.t Js.js_array Js.t -> unit Js.meth

    method applyGradients_map : tensor Js.t Named_tensor_map.js_t Js.t -> unit Js.meth
  end

class type adam =
  object
    inherit optimizer
  end

class type model =
  object
    inherit layer

    (* About the model##apply method:
     * (On tfjs:1.5.2) the parameter cannot be a `model` instance because it causes an exception
     * about some fictional dtype mismatches.
     * i.e.: m1.apply(m0) // fails (should return a model?)
     *       m1.apply(m0.outputLayers[0]) // works (returns a symbolicTensor)
     * Use the `chain` function below to concatenate two indenpendant trees
     *)
    method layers : layer Js.t Js.js_array Js.t Js.readonly_prop

    method trainOnBatch : tensor Js.t -> tensor Js.t -> unit Ft_js.promise Js.t Js.meth

    method predict : tensor Js.t -> tensor Js.t Js.meth
  end

let tensor_of_ta : int array -> float32_ta -> tensor Js.t =
 fun shape ta ->
  let open Js.Unsafe in
  fun_call global##.tf##.tensor [| inject ta; shape |> Js.array |> inject |]

let scalar : float -> tensor Js.t =
 fun x ->
  let open Js.Unsafe in
  fun_call global##.tf##.scalar [| inject x |]

let one_hot_of_ta : int -> uint8_ta -> tensor Js.t =
 fun width arr ->
  let open Js.Unsafe in
  let arr = fun_call global##.tf##.tensor1d [| inject arr; "int32" |> Js.string |> inject |] in
  fun_call global##.tf##.oneHot [| inject arr; inject width |]

module Ops = struct
  open Js.Unsafe

  let ones : int array -> tensor Js.t =
   fun shape -> fun_call global##.tf##.ones [| shape |> Js.array |> inject |]

  let clip_by_value : float -> float -> #tensor Js.t -> tensor Js.t =
   fun min max x -> fun_call global##.tf##.clipByValue [| inject x; inject min; inject max |]

  let neg : tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.neg [| inject x |]

  let mul : tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.mul [| inject x; inject x' |]

  let add : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.add [| inject x; inject x' |]

  let log : #tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.log [| inject x |]

  let sum : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.sum [| inject x; inject axis; inject keepdims |]

  let mean : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.mean [| inject x; inject axis; inject keepdims |]

  let min : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.min [| inject x; inject axis; inject keepdims |]

  let max : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.max [| inject x; inject axis; inject keepdims |]
end

module Layers = struct
  let input : ?dtype:[ `Float32 | `Uint8 ] -> int array -> symbolicTensor Js.t =
   fun ?(dtype = `Float32) shape ->
    let open Js.Unsafe in
    let params =
      object%js (self)
        val shape = Js.array shape

        val dtype = Js.string (match dtype with `Float32 -> "float32" | `Uint8 -> "uint8")
      end
    in
    fun_call global##.tf##.input [| params |> inject |]

  let conv2d : ?weights:float32_nd * float32_nd -> _ -> bool -> _ -> int -> conv2d Js.t =
   fun ?weights kernel_size padding stride out_filters ->
    let padding = match padding with true -> "same" | false -> "valid" in
    let kx, ky = match kernel_size with `One kx -> (kx, kx) | `Two (kx, ky) -> (kx, ky) in
    let sx, sy = match stride with `One sx -> (sx, sx) | `Two (sx, sy) -> (sx, sy) in
    let weights =
      match weights with
      | None -> Js.Opt.empty
      | Some (kernel, bias) ->
          let shape = Ndarray.shape kernel in
          let kernel = kernel |> Conv.Reinterpret.Float32.ta_of_nd |> tensor_of_ta shape in
          let shape = Ndarray.shape bias in
          let bias = bias |> Conv.Reinterpret.Float32.ta_of_nd |> tensor_of_ta shape in
          [| kernel; bias |] |> Js.array |> Js.Opt.return
    in
    let params =
      object%js (self)
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
    let params =
      object%js (self)
        val poolSize = Js.array [| kx; ky |]

        val strides = Js.array [| sx; sy |]
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.maxPooling2d [| inject params |]

  let relu () : layer Js.t =
    let open Js.Unsafe in
    let params = object%js (self) end in
    fun_call global##.tf##.layers##.reLU [| inject params |]

  let softmax ?(axis = 3) () : layer Js.t =
    let params =
      object%js (self)
        val axis = axis
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.softmax [| inject params |]

  let classify : layer Js.t -> [> ] =
   fun l ->
    match Js.to_string l##getClassName with
    | "ReLU" -> `Relu l
    | "Softmax" -> `Softmax l
    | "MaxPooling2D" -> `Maxpool2d
    | "Conv2D" -> `Conv2d (Js.Unsafe.coerce l :> conv2d Js.t)
    | "Sequential" | "Model" -> `Model (Js.Unsafe.coerce l :> model Js.t)
    | name -> failwith ("unknown class name:" ^ name)
end

let categorical_crossentropy : float -> tensor Js.t -> tensor Js.t -> tensor Js.t =
 (* TODO: Move to a `Custom` (or `Helper`, or...) module? along with the custom optimizers *)
 fun epsilon softmaxed_pred truth ->
  softmaxed_pred
  |> Ops.clip_by_value epsilon (1. -. epsilon)
  |> Ops.log |> Ops.mul truth |> Ops.neg |> Ops.sum ~axis:(-1) true |> Ops.mean ~axis:0 true

let model : symbolicTensor Js.t list -> symbolicTensor Js.t list -> model Js.t =
 fun inputs outputs ->
  let params =
    object%js (self)
      val inputs = inputs |> Array.of_list |> Js.array

      val outputs = outputs |> Array.of_list |> Js.array
    end
  in
  let open Js.Unsafe in
  fun_call global##.tf##.model [| inject params |]

let chain :
    symbolicTensor Js.t -> symbolicTensor Js.t list -> symbolicTensor Js.t -> symbolicTensor Js.t =
 fun a b_inputs b ->
  let b = model b_inputs [ b ] in
  b##apply a

let adam : float -> float -> float -> float -> adam Js.t =
 fun lr beta1 beta2 epsilon ->
  let open Js.Unsafe in
  fun_call global##.tf##.train##.adam [| inject lr; inject beta1; inject beta2; inject epsilon |]

let sgd : float -> optimizer Js.t =
 fun lr ->
  let open Js.Unsafe in
  fun_call global##.tf##.train##.sgd [| inject lr |]

let compile : model Js.t -> optimizer Js.t -> string -> unit =
 fun m optim loss ->
  let params =
    object%js (self)
      val optimizer = optim

      val loss = Js.string loss
    end
  in
  let open Js.Unsafe in
  meth_call m "compile" [| inject params |]

let variable_grads :
    (unit -> tensor Js.t) Js.callback -> tensor Js.t * tensor Js.t Named_tensor_map.t =
 fun f ->
  let open Js.Unsafe in
  let ret = fun_call global##.tf##.variableGrads [| inject f |] in
  (* What if multiple losses? would `value` be a tensor array ? *)
  let value : tensor Js.t = ret##.value in
  let grads : tensor Js.t Named_tensor_map.js_t Js.t = ret##.grads in
  (value, Named_tensor_map.of_js grads)

let engine_registered_variables : unit -> variable Js.t Named_tensor_map.t =
 fun () ->
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [||] in
  let map_js = get e "registeredVariables" in
  Named_tensor_map.of_js map_js
