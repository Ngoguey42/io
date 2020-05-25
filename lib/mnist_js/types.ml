type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type db = uint8_ba * uint8_ba * uint8_ba * uint8_ba

type backend = [ `Tfjs_webgl | `Tfjs_cpu | `Tfjs_wasm ] [@@deriving enum]

type lr = [ `Down of float * float | `Flat of float ]

type float32_ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type int32_ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type training_config = {
  from_webworker : bool;
  backend : backend;
  lr : lr;
  batch_count : int;
  batch_size : int;
  seed : int;
  verbose : bool;
}

type training_parameters = {
  db : uint8_ba * uint8_ba * uint8_ba * uint8_ba;
  networks : Fnn.network list * Fnn.network;
  config : training_config;
}

type state =
  | Creating_network
  | Selecting_backend of { encoder : Fnn.network; decoder : Fnn.network; seed : int }
  | Creating_training of {
      encoder : Fnn.network;
      decoder : Fnn.network;
      seed : int;
      backend : backend;
      images_seen : int;
    }
  | Training of {
      encoder : Fnn.network;
      decoder : Fnn.network;
      seed : int;
      backend : backend;
      images_seen : int;
      config : training_config;
    }

type event =
  | Network of { encoder : Fnn.network; decoder : Fnn.network; seed : int }
  | Backend of backend
  | Training_conf of { lr : lr; batch_size : int; batch_count : int }
  | End of { encoder : Fnn.network; decoder : Fnn.network; images_seen : int }
  | Crash of exn
  | Abort

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

type outcome = [ `End of Fnn.network * Fnn.network * int | `Abort | `Crash of exn ]

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
