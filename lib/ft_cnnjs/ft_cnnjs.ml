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
  val train :
    ?verbose:bool ->
    ?progress:(int -> unit) ->
    batch_count:int ->
    get_lr:(int -> float) ->
    get_data:(int -> float32_ta * uint8_ta) ->
    encoders:Nn.t list ->
    decoder:Nn.t ->
    (Nn.t list * Nn.t) Lwt.t
end

let[@ocamlformat "disable"] get_backend : string -> (module TRAINER) = function
  | "tfjs-webgl" -> (module Mnist_tfjs.Make_backend (struct let v = `Webgl end))
  | "tfjs-cpu" -> (module Mnist_tfjs.Make_backend (struct let v = `Cpu end))
  (* | "owl-cpu" -> (module Mnist_owl) *)
  | _ -> failwith "unknown backend"
