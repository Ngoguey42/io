open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

include Training_types

let[@ocamlformat "disable"] create_backend : backend -> (module TRAINER) = function
  (* | `Owl_cpu -> (module Mnist_owl) *)
  | `Tfjs_webgl -> (module Mnist_tfjs.Make_backend (struct let v = `Webgl end))
  | `Tfjs_cpu -> (module Mnist_tfjs.Make_backend (struct let v = `Cpu end))
  | `Tfjs_wasm -> (module Mnist_tfjs.Make_backend (struct let v = `Wasm end))

let routine { db = train_imgs, train_labs, _, _; networks = encoders, decoder; config } fire_event
    instructions =
  let module Backend = (val create_backend config.backend) in
  let verbose = config.verbose in
  let rng = Random.State.make [| config.seed |] in
  let batch_count, batch_size = (config.batch_count, config.batch_size) in
  let get_lr =
    match config.lr with
    | `Down (lr0, lr1) ->
        assert (0. <= lr1);
        assert (lr1 <= lr0);
        let lr2 = lr0 -. lr1 in
        fun i ->
          let ratio = float_of_int i /. float_of_int batch_count in
          lr1 +. (lr2 *. (1. -. ratio))
  in
  let get_data _ =
    let indices = Array.init batch_size (fun _ -> Random.State.int rng 60000) in
    let imgs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_imgs i 1) indices
      |> Ndarray.concatenate ~axis:0
    in
    let labs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_labs i 1) indices
      |> Ndarray.concatenate ~axis:0
    in
    (imgs, labs)
  in
  Backend.train ~fire_event ~instructions ~verbose ~batch_count ~get_lr ~get_data ~encoders ~decoder

module Webworker_routine = struct
  type training_parameters = {
    db : uint8_ba * uint8_ba * uint8_ba * uint8_ba;
    networks : Misc.storable_nn list * Misc.storable_nn;
    config : training_config;
  }

  type _in_msg = [ `Prime of training_parameters | user_status ]

  type routine_event =
    [ `Init
    | `Batch_begin of int
    | `Batch_end of int * stats
    | `End of Misc.storable_nn list * Misc.storable_nn * stats
    | `Abort
    | `Crash of exn ]

  type _out_msg = routine_event

  let preprocess_in_msg : _ -> _in_msg = function
    | `Prime Training_types.{ db; networks = encoders, decoder; config } ->
        let f = Misc.storable_of_fnn in
        let networks = (List.map f encoders, f decoder) in
        `Prime { networks; db; config }
    | #user_status as msg -> msg

  let postprocess_in_msg : _in_msg -> _ = function
    | `Prime { db; networks = encoders, decoder; config } ->
        let f = Misc.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER) in
        let networks = (List.map f encoders, f decoder) in
        `Prime Training_types.{ networks; db; config }
    | #user_status as msg -> msg

  let preprocess_out_msg : Training_types.routine_event -> routine_event = function
    | `End (encoders, decoder, stats) ->
        let f = Misc.storable_of_fnn in
        `End (List.map f encoders, f decoder, stats)
    | `Init as ev -> (ev :> routine_event)
    | `Batch_begin _ as ev -> (ev :> routine_event)
    | `Batch_end _ as ev -> (ev :> routine_event)
    | `Abort as ev -> (ev :> routine_event)
    | `Crash _ as ev -> (ev :> routine_event)

  let postprocess_out_msg : _out_msg -> Training_types.routine_event = function
    | `End (encoders, decoder, stats) ->
        let f = Misc.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER) in
        `End (List.map f encoders, f decoder, stats)
    | `Init as ev -> (ev :> Training_types.routine_event)
    | `Batch_begin _ as ev -> (ev :> Training_types.routine_event)
    | `Batch_end _ as ev -> (ev :> Training_types.routine_event)
    | `Abort as ev -> (ev :> Training_types.routine_event)
    | `Crash _ as ev -> (ev :> Training_types.routine_event)

  module rec Ww : (Webworker.S with type in_msg = _in_msg and type out_msg = _out_msg) =
  Webworker.Make (struct
    type in_msg = _in_msg

    type out_msg = _out_msg

    let create_on_in_message () =
      let routine_events, fire_routine_event = React.E.create () in
      let user_status, set_user_status = React.S.create `Train_to_end in

      routine_events |> React.E.map preprocess_out_msg |> React.E.map Ww.post_out_message |> ignore;

      let on_in_message msg =
        match postprocess_in_msg msg with
        | `Prime params ->
            Ft_js.Scripts.import_sync `Tfjs;
            let routine () = routine params fire_routine_event user_status in
            Js_of_ocaml_lwt.Lwt_js_events.async routine
        | #user_status as msg -> set_user_status msg
      in
      on_in_message
  end)

  include Ww
end

let render_instructions props =
  let user_status, routine_status, fire_user_event = props in
  let open Reactjs.Jsx in
  let button ?event ?(style = []) txt =
    match event with
    | None -> of_tag "button" ~style ~disabled:true [ of_string txt ]
    | Some event ->
        of_tag "button" ~style ~on_click:(fun _ -> fire_user_event event) [ of_string txt ]
  in
  let green =
    [
      ("color", "green");
      ("borderColor", "transparent");
      ("backgroundColor", "transparent");
      ("fontWeight", "bold");
    ]
  in
  let gray = [ ("borderColor", "transparent"); ("backgroundColor", "transparent") ] in
  match (user_status, routine_status) with
  | `Train_to_end, `Running ->
      of_tag "div"
        [
          button ~style:green "Train to end";
          of_string " | ";
          button ~event:`Early_stop "Early stop";
          of_string " | ";
          button ~event:`Abort "Abort";
        ]
  | `Train_to_end, _ ->
      of_tag "div"
        [
          button ~style:green "Train to end";
          of_string " | ";
          button ~style:gray "Early stop";
          of_string " | ";
          button ~style:gray "Abort";
        ]
  | `Early_stop, _ ->
      of_tag "div"
        [
          button ~style:gray "Train to end";
          of_string " | ";
          button ~style:green "Early stop";
          of_string " | ";
          button ~style:gray "Abort";
        ]
  | `Abort, _ ->
      of_tag "div"
        [
          button ~style:gray "Train to end";
          of_string " | ";
          button ~style:gray "Early stop";
          of_string " | ";
          button ~style:green "Abort";
        ]

type props = training_parameters * (outcome -> unit)

let construct (props : props) =
  let params, on_completion = props in

  let routine_events, fire_routine_event = React.E.create () in
  let routine_progress =
    React.S.fold (fun i a -> match a with `Batch_end (i, _) -> i + 1 | _ -> i) 0 routine_events
  in
  let routine_status =
    let reduce s ev =
      match (s, ev) with
      | `Running, `End _ ->
          on_completion `End;
          `Ended
      | `Running, `Abort ->
          on_completion `Abort;
          `Aborted
      | `Running, `Crash exn ->
          on_completion (`Crash exn);
          `Crashed
      | _, _ -> s
    in
    React.S.fold reduce `Running routine_events
  in

  let user_events, fire_user_event = React.E.create () in
  let fire_user_event : user_event -> unit = fire_user_event in
  let user_status =
    let reduce s ev =
      match (s, ev) with
      | `Train_to_end, `Early_stop -> `Early_stop
      | `Train_to_end, `Abort -> `Abort
      | _, `Early_stop | _, `Abort -> s
    in
    React.S.fold reduce `Train_to_end user_events
  in

  let render _ =
    let open Reactjs.Jsx in
    let routine_status = React.S.value routine_status in
    let routine_progress = React.S.value routine_progress in
    let user_status = React.S.value user_status in

    of_tag "div"
      [
        of_render render_instructions (user_status, routine_status, fire_user_event);
        of_tag "div"
          [
            of_string "Routine: ";
            ( match routine_status with
            | `Running -> "Running"
            | `Ended -> "Ended"
            | `Aborted -> "Aborted"
            | `Crashed -> "Crashed" )
            |> of_string;
          ];
        of_tag "div"
          [
            of_string "Progress: ";
            (routine_progress |> float_of_int)
            /. (params.config.batch_count |> float_of_int)
            *. 100.
            |> Printf.sprintf "%.0f%%" |> of_string;
          ];
      ]
  in
  let mount () =
    let fire_routine_event ev = ev |> Webworker_routine.postprocess_out_msg |> fire_routine_event in
    let ww =
      Webworker_routine.create fire_routine_event (fun _err -> Printf.eprintf "Webworker err\n%!")
    in
    let user_status = React.S.map Webworker_routine.preprocess_in_msg user_status in
    React.S.changes user_status |> React.E.map (Webworker_routine.post_in_message ww) |> ignore;
    Webworker_routine.post_in_message ww (React.S.value user_status);
    Webworker_routine.post_in_message ww (Webworker_routine.preprocess_in_msg (`Prime params))
    (* let routine () = routine params fire_routine_event user_status in *)
    (* Js_of_ocaml_lwt.Lwt_js_events.async routine *)
  in

  Reactjs.construct ~mount ~signal:routine_progress ~signal:user_status ~signal:routine_status
    render
