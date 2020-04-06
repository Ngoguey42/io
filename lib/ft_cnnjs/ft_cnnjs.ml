module Js = Js_of_ocaml.Js
module Typed_array = Js_of_ocaml.Typed_array
module Nn = Nn
module Mnist = Mnist
module Tfjs_api = Tfjs_api
module Nn_tfjs = Nn_tfjs
module Mnist_tfjs = Mnist_tfjs

type float32_ta = (float, [ `Float32 ]) Typed_array.typedArray Js.t
type uint8_ta = (int, [ `Uint8 ]) Typed_array.typedArray Js.t

module type TRAINER = sig
  (* TODO: freeze some networks *)
  val train :
    ?verbose:bool ->
    ?progress:(int -> unit) ->
    batch_count:int ->
    get_lr:(int -> float) ->
    get_data:(int -> float32_ta * uint8_ta) ->
    encoders:Fnn.network list ->
    decoder:Fnn.network ->
    (Fnn.network list * Fnn.network) Lwt.t
end

let[@ocamlformat "disable"] get_backend : _ -> (module TRAINER) = function
  (* | _ -> failwith "nope" *)
  | `Tfjs_webgl -> (module Mnist_tfjs.Make_backend (struct let v = `Webgl end))
  | `Tfjs_cpu -> (module Mnist_tfjs.Make_backend (struct let v = `Cpu end))
  (* | `Owl_cpu -> (module Mnist_owl) *)
