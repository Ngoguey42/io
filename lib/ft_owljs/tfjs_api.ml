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

(* TODO: Do not use Ndarray, but use Bigarray *)

type uint8_ta = Typed_array.uint8Array Js.t

type float32_ta = Typed_array.float32Array Js.t

type float32_nd = (float, Bigarray.float32_elt) Ndarray.t

class type symbolicTensor = object end

class type tensor =
  object
    method shape : int Js.js_array Js.t Js.readonly_prop

    method size : int Js.readonly_prop

    method data : float32_ta Ft_js.promise Js.t Js.meth

    method asType : Js.js_string Js.t Js.meth

    method print : unit Js.meth

    method reshape : int Js.js_array Js.t -> tensor Js.t Js.meth

    method toString : Js.js_string Js.t Js.meth

    method arraySync_int : int Js.js_array Js.t Js.meth

    method arraySync_float : float Js.js_array Js.t Js.meth

    method arraySync_int_scalar : int Js.meth

    method arraySync_float_scalar : float Js.meth
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

    method name : Js.js_string Js.t Js.readonly_prop
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

class type memory =
  object
    method numTensors : int Js.readonly_prop
  end

let tensor_of_ta : int array -> float32_ta -> tensor Js.t =
 fun shape ta ->
  let open Js.Unsafe in
  fun_call global##.tf##.tensor [| inject ta; shape |> Js.array |> inject |]

(* let scalar : float -> tensor Js.t = *)
(*  fun x -> *)
(*   let open Js.Unsafe in *)
(*   fun_call global##.tf##.scalar [| inject x |] *)

let one_hot_of_ta : int -> uint8_ta -> tensor Js.t =
 fun width arr ->
  let open Js.Unsafe in
  let arr = fun_call global##.tf##.tensor1d [| inject arr; "int32" |> Js.string |> inject |] in
  fun_call global##.tf##.oneHot [| inject arr; inject width |]

let variable :
    ?trainable:bool ->
    ?name:string ->
    ?dtype:[ `Float32 | `Uint8 | `Int32 ] ->
    tensor Js.t ->
    variable Js.t =
 fun ?(trainable = false) ?name ?(dtype = `Float32) arr ->
  let open Js.Unsafe in
  let name =
    match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
  in
  let dtype =
    Js.string (match dtype with `Float32 -> "float32" | `Int32 -> "int32" | `Uint8 -> "uint8")
  in
  fun_call global##.tf##.variable [| inject arr; inject trainable; inject name; inject dtype |]

let float : float -> tensor Js.t =
 fun x ->
  let open Js.Unsafe in
  fun_call global##.tf##.scalar [| inject x; "float32" |> Js.string |> inject |]

let int : int -> tensor Js.t =
 fun x ->
  let open Js.Unsafe in
  fun_call global##.tf##.scalar [| inject x; "int32" |> Js.string |> inject |]

module Ops = struct
  open Js.Unsafe

  let ones : int array -> tensor Js.t =
   fun shape -> fun_call global##.tf##.ones [| shape |> Js.array |> inject |]

  let reshape : int array -> tensor Js.t -> tensor Js.t =
   fun shape x -> fun_call global##.tf##.reshape [| inject x; shape |> Js.array |> inject |]

  let clip_by_value : float -> float -> #tensor Js.t -> tensor Js.t =
   fun min max x -> fun_call global##.tf##.clipByValue [| inject x; inject min; inject max |]

  let neg : #tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.neg [| inject x |]

  let sqrt : #tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.sqrt [| inject x |]

  let mul : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.mul [| inject x; inject x' |]

  let ( * ) = mul

  let div : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.div [| inject x; inject x' |]

  let ( / ) = div

  let add : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.add [| inject x; inject x' |]

  let ( + ) = add

  let sub : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.sub [| inject x; inject x' |]

  let ( - ) = sub

  let pow : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.pow [| inject x; inject x' |]

  let ( ** ) = pow

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
  let input : ?dtype:[ `Float32 | `Uint8 ] -> ?name:string -> int array -> symbolicTensor Js.t =
   fun ?(dtype = `Float32) ?name shape ->
    let open Js.Unsafe in
    let params =
      object%js (self)
        val shape = Js.array shape

        val dtype = Js.string (match dtype with `Float32 -> "float32" | `Uint8 -> "uint8")

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    fun_call global##.tf##.input [| params |> inject |]

  let conv2d :
      ?weights:float32_nd * float32_nd -> ?name:string -> _ -> bool -> _ -> int -> conv2d Js.t =
   fun ?weights ?name kernel_size padding stride out_filters ->
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

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.conv2d [| inject params |]

  let max_pool2d ?name kernel_size stride : layer Js.t =
    let kx, ky = match kernel_size with `One kx -> (kx, kx) | `Two (kx, ky) -> (kx, ky) in
    let sx, sy = match stride with `One sx -> (sx, sx) | `Two (sx, sy) -> (sx, sy) in
    let params =
      object%js (self)
        val poolSize = Js.array [| kx; ky |]

        val strides = Js.array [| sx; sy |]

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.maxPooling2d [| inject params |]

  let relu ?name () : layer Js.t =
    let open Js.Unsafe in
    let params =
      object%js (self)
        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    fun_call global##.tf##.layers##.reLU [| inject params |]

  let softmax ?(axis = 3) ?name () : layer Js.t =
    let params =
      object%js (self)
        val axis = axis

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
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

let sgd_updater =
  object
    method update weights lr grad =
      let open Ops in
      weights##assign (grad |> mul (float lr) |> neg |> add weights)
  end

let create_adam_updater : float -> float -> float -> int -> float32_nd -> float32_nd -> _ =
 fun epsilon beta1 beta2 step rgrad rgrad_sq ->
  let beta1 = beta1 |> float in
  let beta2 = beta2 |> float in
  let beta1' = Ops.sub (float 1.) beta1 in
  let beta2' = Ops.sub (float 1.) beta2 in
  let epsilon = float epsilon in

  let step = step |> int |> variable ~dtype:`Int32 in
  let rgrad =
    rgrad |> Conv.Reinterpret.Float32.ta_of_nd
    |> tensor_of_ta (Ndarray.shape rgrad)
    |> variable ~trainable:false
  in
  let rgrad_sq =
    rgrad_sq |> Conv.Reinterpret.Float32.ta_of_nd
    |> tensor_of_ta (Ndarray.shape rgrad_sq)
    |> variable ~trainable:false
  in

  object
    method update (weights: variable Js.t) lr (grad: tensor Js.t) =
      let open Ops in
      step##assign (step + int 1);
      let correction = float 1. - (beta1 ** step) in
      let correction_sq = float 1. - (beta2 ** step) in
      rgrad##assign ((rgrad * beta1) + (grad * beta1'));
      rgrad_sq##assign ((rgrad_sq * beta2) + (grad * grad * beta2'));
      weights##assign
        (rgrad / correction / (sqrt (rgrad_sq / correction_sq) + epsilon)
        |> mul (float lr)
        |> neg |> add weights )

    method get_step = step##arraySync_int_scalar

    method get_rgrad = rgrad

    method get_rgrad_sq = rgrad_sq
  end

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
    (unit -> tensor Js.t) -> tensor Js.t * tensor Js.t Named_tensor_map.t =
 fun f ->
  let open Js.Unsafe in
  let ret = fun_call global##.tf##.variableGrads [| f |> Js.wrap_callback |> inject |] in
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

let setup_backend b =
  (* WASM backend is not mature yet (some bug and lack of implemented functions)
   * https://cdn.jsdelivr.net/npm/@tensorflow/tfjs-backend-wasm/dist/
   * https://github.com/tensorflow/tfjs/tree/master/tfjs-backend-wasm *)
  let open Js.Unsafe in
  let open Lwt.Infix in
  let b = Js.string (match b with
    | `Webgl -> "webgl"
    | `Cpu -> "cpu"
    | `Wasm -> "wasm")
  in
  fun_call global##.tf##.setBackend [| b |> inject |] |> Ft_js.wrap_promise >>= fun _ ->
  fun_call global##.tf##.ready [| |] |> Ft_js.wrap_promise

let memory : unit -> memory = fun () ->
  let open Js.Unsafe in
  fun_call global##.tf##.memory [|  |]

let disposeVariables : unit -> unit = fun () ->
  (* Dereference the `variables` inside the engine.
   * Always use it. And use in conjunction with a `tensor` dereferencing scheme below. *)
  let open Js.Unsafe in
  fun_call global##.tf##.disposeVariables [|  |]

let engine_reset : unit -> unit = fun () ->
  (* Seems to be the only reliable way to fully get rid of all the `tensor` memory leaks, when no
   * scoping was used. But it should not be called too often because the webgl initialization
   * takes ~3 seconds *)
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [|  |] in
  meth_call e "reset" [| |] |> ignore

let engine_start_scope : unit -> unit = fun () ->
  (* Start a tensor garbage collection scope *)
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [|  |] in
  meth_call e "startScope" [| |] |> ignore

let engine_end_scope : unit -> unit = fun () ->
  (* End a tensor garbage collection scope, dereferencing all `tensors` referenced since the
   * matching `engine_start_scope` *)
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [|  |] in
  meth_call e "endScope" [| |] |> ignore

let tidy : (unit -> unit) -> unit =
  fun f ->
  (* Wrap a synchronous computation in order to dispose the `tensors` referenced inside the engine.
   *)
  let open Js.Unsafe in
  fun_call global##.tf##.tidy [| f |> Js.wrap_callback |> inject |]

let tidy_lwt : (unit -> 'a Lwt.t) -> 'a Lwt.t =
  fun f ->
  (* Wrap an asynchronous computation in order to dispose the `tensors` referenced inside the
     engine.
   *)
  let f () =
    engine_start_scope ();
    f ()
  and f' () =
    engine_end_scope ();
    Lwt.return ()
  in
  Lwt.finalize f f'
