module Typed_array = Js_of_ocaml.Typed_array

type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type float32_ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type backend = [ `Tfjs_webgl | `Tfjs_cpu ]

type training_config = {
    backend : backend;
    lr : [ `Down of float * float ];
    batch_count : int;
    batch_size : int ;
    seed : int;
    verbose : bool;
  }

type stats = {
    loss : float;
    confusion_matrix  : float32_ba;
  }

type training_event =
  [ `Init
  | `Batch_begin of int
  | `Batch_end of int * stats
  | `Abort
  | `Stop of int * Fnn.network * Fnn.network list * stats ]

type training_instruction = [ `Train_to_end  | `Abort | `Early_stop ]

module type TRAINER = sig
  (* TODO: freeze some networks *)
  val train :
    ?verbose:bool ->
    fire_event:(training_event -> unit) ->
    instructions:(training_instruction React.signal) ->
    batch_count:int ->
    get_lr:(int -> float) ->
    get_data:(int -> uint8_ba * uint8_ba) ->
    encoders:Fnn.network list ->
    decoder:Fnn.network ->
    unit Lwt.t
end

let[@ocamlformat "disable"] get_backend : backend -> (module TRAINER) = function
  | `Tfjs_webgl -> (module Mnist_tfjs.Make_backend (struct let v = `Webgl end))
  | `Tfjs_cpu -> (module Mnist_tfjs.Make_backend (struct let v = `Cpu end))
(* | `Owl_cpu -> (module Mnist_owl) *)
