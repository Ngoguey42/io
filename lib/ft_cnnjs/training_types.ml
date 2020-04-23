type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type float32_ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type int32_ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type backend = [ `Tfjs_webgl | `Tfjs_cpu ]

type training_config = {
  backend : backend;
  lr : [ `Down of float * float ];
  batch_count : int;
  batch_size : int;
  seed : int;
  verbose : bool;
}

type stats = { batch_count : int; loss : float; confusion_matrix : int32_ba }

type user_event = [ `Early_stop | `Abort ]

type user_status = [ `Train_to_end | `Early_stop | `Abort ]

type routine_event =
  [ `Init
  | `Batch_begin of int
  | `Batch_end of int * stats
  | `End of Fnn.network list * Fnn.network * stats
  | `Abort
  | `Crash of exn ]

type routine_status = [ `Running | `Ended | `Aborted | `Crashed ]

type outcome = [ `End | `Abort | `Crash of exn ]

module type TRAINER = sig
  (* TODO: freeze some networks *)
  val train :
    ?verbose:bool ->
    fire_event:(routine_event -> unit) ->
    instructions:user_status React.signal ->
    batch_count:int ->
    get_lr:(int -> float) ->
    get_data:(int -> uint8_ba * uint8_ba) ->
    encoders:Fnn.network list ->
    decoder:Fnn.network ->
    unit Lwt.t
end
