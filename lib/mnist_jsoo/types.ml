(* Misc ***************************************************************************************** *)
type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type db = uint8_ba * uint8_ba * uint8_ba * uint8_ba

type db_train = uint8_ba * uint8_ba

type db_test = uint8_ba * uint8_ba

type backend = [ `Tfjs_webgl | `Tfjs_cpu | `Tfjs_wasm | `Owl_algodiff_cpu ] [@@deriving show]

type lr = [ `Down of float * float | `Flat of float ] [@@deriving show]

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
  images_seen : int;
  verbose : bool;
}
[@@deriving show]

type training_parameters = {
  db : db_train; [@opaque]
  networks : Ocann.network list * Ocann.network; [@opaque]
  config : training_config;
}
[@@deriving show]

type training_stats = {
  mean_iou_top1 : float;
  mean_recall_top1 : float;
  mean_precision_top1 : float;
  batch_count : int;
  (* Number of batches aggregated *)
  image_count : int;
  (* Number of images aggregated *)
  mean_loss_per_image : float;
  mean_learning_rate : float;
}
[@@deriving show]

type training_user_status = [ `Train_to_end | `Early_stop | `Abort ] [@@deriving show]

type training_outcome =
  [ `End of (Ocann.network list[@opaque]) * (Ocann.network[@opaque]) * (training_stats[@opaque])
  | `Abort
  | `Crash of string ]
[@@deriving show]

type training_routine_event =
  [ `Init
  | `Batch_begin of int
  | `Batch_end of int * (training_stats[@opaque])
  | `Outcome of training_outcome ]
[@@deriving show]

type training_routine_status = [ `Allocating | `Running | `Ended | `Aborted | `Crashed ]
[@@deriving show]

type training_backend_routine =
  ?verbose:bool ->
  yield_sleep_length:float ->
  fire_event:(training_routine_event -> unit) ->
  instructions:training_user_status React.signal ->
  batch_count:int ->
  get_lr:(int -> float) ->
  get_data:(int -> uint8_ba * uint8_ba) ->
  encoders:Ocann.network list ->
  decoder:Ocann.network ->
  unit Lwt.t

module type TRAINER = sig
  val train : training_backend_routine
end

(* Eval ***************************************************************************************** *)
type evaluation_config = {
  from_webworker : bool;
  verbose : bool;
  batch_size : int;
  backend : backend;
}
[@@deriving show]

type evaluation_parameters = {
  db : db_test; [@opaque]
  encoder : Ocann.network; [@opaque]
  decoder : Ocann.network; [@opaque]
  config : evaluation_config;
}
[@@deriving show]

type evaluation_stats = {
  mean_iou_top1 : float;
  mean_recall_top1 : float;
  mean_precision_top1 : float;
  test_set_sample_probas : float32_ba; [@opaque]
}
[@@deriving show]

type evaluation_outcome = [ `End of evaluation_stats [@opaque] | `Crash of string ]
[@@deriving show]

type evaluation_routine_event =
  [ `Init | `Batch_begin of int | `Batch_end of int | `Outcome of evaluation_outcome ]
[@@deriving show]

type evaluation_routine_status = [ `Running | `Ended | `Crashed ] [@@deriving show]

type evaluation_backend_routine =
  ?verbose:bool ->
  yield_sleep_length:float ->
  fire_event:(evaluation_routine_event -> unit) ->
  batch_size:int ->
  db:db_test ->
  encoder:Ocann.network ->
  decoder:Ocann.network ->
  unit Lwt.t

module type EVALUATOR = sig
  val eval : evaluation_backend_routine
end

(* Global *************************************************************************************** *)
module type BACKEND = sig
  include TRAINER

  include EVALUATOR
end

type tab_state =
  | Creating_network
  | Selecting_backend of {
      encoder : Ocann.network; [@opaque]
      decoder : Ocann.network; [@opaque]
      seed : int;
    }
  | Evaluating of {
      old : (Ocann.network * Ocann.network * int) option; [@opaque]
      (* `old` contains the states from previous `Creating_training` state (if any).
         Necessary to rollback states when eval crashes. *)
      encoder : Ocann.network; [@opaque]
      decoder : Ocann.network; [@opaque]
      seed : int;
      backend : backend;
      from_webworker : bool;
      images_seen : int;
      config : evaluation_config; [@opaque]
    }
  | Creating_training of {
      encoder : Ocann.network; [@opaque]
      decoder : Ocann.network; [@opaque]
      seed : int;
      backend : backend;
      from_webworker : bool;
      images_seen : int;
    }
  | Training of {
      encoder : Ocann.network; [@opaque]
      decoder : Ocann.network; [@opaque]
      seed : int;
      backend : backend;
      from_webworker : bool;
      images_seen : int;
      config : training_config; [@opaque]
    }
[@@deriving show]

module Tab_state = struct
  let equal a b =
    match (a, b) with
    | Creating_network, Creating_network -> true
    | Creating_network, _ -> false
    | Selecting_backend _, Selecting_backend _ -> true
    | Selecting_backend _, _ -> false
    | Evaluating s, Evaluating s' -> s.images_seen = s'.images_seen
    | Evaluating _, _ -> false
    | Creating_training s, Creating_training s' ->
        s.backend = s'.backend
        && s.from_webworker = s'.from_webworker
        && s.images_seen = s'.images_seen
    | Creating_training _, _ -> false
    | Training s, Training s' -> s.images_seen = s'.images_seen
    | Training _, _ -> false

  module Eq = struct
    type _ t = tab_state

    let equal = equal
  end

  module S = React.S.Make (Eq)
end

type tab_event =
  | Network_made of { encoder : Ocann.network; [@opaque] decoder : Ocann.network; [@opaque] seed : int }
  | Backend_selected of (backend * bool)
  | Evaluation_event of evaluation_routine_event
  | Training_conf of { lr : lr; batch_size : int; batch_count : int }
  | Training_event of training_routine_event
[@@deriving show]

type state = Loading | Loaded of db * int * tab_state array
