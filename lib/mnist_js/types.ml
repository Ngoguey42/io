(* Misc ***************************************************************************************** *)
type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type db = uint8_ba * uint8_ba * uint8_ba * uint8_ba

type db_train = uint8_ba * uint8_ba

type db_test = uint8_ba * uint8_ba

type backend = [ `Tfjs_webgl | `Tfjs_cpu | `Tfjs_wasm ] [@@deriving enum]

type lr = [ `Down of float * float | `Flat of float ]

type float32_ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type int32_ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t

(* Training ************************************************************************************* *)
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
  db : db_train;
  networks : Fnn.network list * Fnn.network;
  config : training_config;
}

type training_stats = { batch_count : int; loss : float; confusion_matrix : int32_ba }

type training_user_status = [ `Train_to_end | `Early_stop | `Abort ]

type training_outcome =
  [ `End of Fnn.network list * Fnn.network * training_stats | `Abort | `Crash of exn ]

type training_routine_event =
  [ `Init | `Batch_begin of int | `Batch_end of int * training_stats | `Outcome of training_outcome ]

type training_routine_status = [ `Running | `Ended | `Aborted | `Crashed ]

type training_backend_routine =
  ?verbose:bool ->
  fire_event:(training_routine_event -> unit) ->
  instructions:training_user_status React.signal ->
  batch_count:int ->
  get_lr:(int -> float) ->
  get_data:(int -> uint8_ba * uint8_ba) ->
  encoders:Fnn.network list ->
  decoder:Fnn.network ->
  unit Lwt.t

module type TRAINER = sig
  val train : training_backend_routine
end

(* Eval ***************************************************************************************** *)
type eval_parameters = {
  db : db_test;
  encoder : Fnn.network;
  decoder : Fnn.network;
  backend : backend;
}

type eval_stats = { confusion_matrix : int32_ba; marked_digits_probas : float32_ba }

type eval_outcome = [ `End of eval_stats | `Crash of exn ]

type eval_routine_status = [ `Running | `Ended | `Crashed ]

module type EVALUATOR = sig
  val train :
    ?verbose:bool ->
    fire_event:(eval_outcome -> unit) ->
    db:db_test ->
    encoder:Fnn.network ->
    decoder:Fnn.network ->
    unit Lwt.t
end

(* Global *************************************************************************************** *)
type tab_state =
  | Creating_network
  | Selecting_backend of { encoder : Fnn.network; decoder : Fnn.network; seed : int }
  | Evaluating of {
      encoder : Fnn.network;
      decoder : Fnn.network;
      seed : int;
      backend : backend;
      images_seen : int;
    }
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

type tab_event =
  | Network_made of { encoder : Fnn.network; decoder : Fnn.network; seed : int }
  | Backend_selected of backend
  | Evaluated of eval_outcome
  | Training_conf of { lr : lr; batch_size : int; batch_count : int }
  | Training_event of training_routine_event

type state = Loading | Loaded of db * int * tab_state array
