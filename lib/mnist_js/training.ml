open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

open Types

(* include Training_types *)

let[@ocamlformat "disable"] create_backend : backend -> (module TRAINER) = function
  (* | `Owl_cpu -> (module Mnist_owl) *)
  | `Tfjs_webgl -> (module Training_tfjs.Make_backend (struct let v = `Webgl end))
  | `Tfjs_cpu -> (module Training_tfjs.Make_backend (struct let v = `Cpu end))
  | `Tfjs_wasm -> (module Training_tfjs.Make_backend (struct let v = `Wasm end))

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
    | `Flat lr -> fun _ -> lr
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
    networks : Fnn.storable_nn list * Fnn.storable_nn;
    config : training_config;
  }

  type _in_msg = [ `Prime of training_parameters | user_status ]

  type routine_event =
    [ `Init
    | `Batch_begin of int
    | `Batch_end of int * stats
    | `End of Fnn.storable_nn list * Fnn.storable_nn * stats
    | `Abort
    | `Crash of exn ]

  type _out_msg = routine_event

  let preprocess_in_msg : _ -> _in_msg = function
    | `Prime Types.{ db; networks = encoders, decoder; config } ->
        let f = Fnn.storable_of_fnn in
        let networks = (List.map f encoders, f decoder) in
        `Prime { networks; db; config }
    | #user_status as msg -> msg

  let postprocess_in_msg : _in_msg -> _ = function
    | `Prime { db; networks = encoders, decoder; config } ->
        let f = Fnn.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER) in
        let networks = (List.map f encoders, f decoder) in
        `Prime Types.{ networks; db; config }
    | #user_status as msg -> msg

  let preprocess_out_msg : Types.routine_event -> routine_event = function
    | `End (encoders, decoder, stats) ->
        let f = Fnn.storable_of_fnn in
        `End (List.map f encoders, f decoder, stats)
    | `Init as ev -> (ev :> routine_event)
    | `Batch_begin _ as ev -> (ev :> routine_event)
    | `Batch_end _ as ev -> (ev :> routine_event)
    | `Abort as ev -> (ev :> routine_event)
    | `Crash _ as ev -> (ev :> routine_event)

  let postprocess_out_msg : _out_msg -> Types.routine_event = function
    | `End (encoders, decoder, stats) ->
        let f = Fnn.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER) in
        `End (List.map f encoders, f decoder, stats)
    | `Init as ev -> (ev :> Types.routine_event)
    | `Batch_begin _ as ev -> (ev :> Types.routine_event)
    | `Batch_end _ as ev -> (ev :> Types.routine_event)
    | `Abort as ev -> (ev :> Types.routine_event)
    | `Crash _ as ev -> (ev :> Types.routine_event)

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
            Ft_js.Scripts.urls_of_entry ~what:`Js `Tfjs
            |> List.iter Js_of_ocaml.Worker.import_scripts;
            let routine () = routine params fire_routine_event user_status in
            Js_of_ocaml_lwt.Lwt_js_events.async routine
        | #user_status as msg -> set_user_status msg
      in
      on_in_message
  end)

  include Ww
end

let render_instructions props =
  let user_status, routine_status, set_user_status = props in
  let open Reactjs.Jsx in
  let button txt t =
    match (t, routine_status) with
    | `On event, `Running ->
        let on_click ev =
          set_user_status event;
          ev##preventDefault
        in
        of_bootstrap "Button" ~style:[ ("width", "95px") ] ~size:"sm" ~variant:"primary" ~on_click
          [ of_string txt ]
    | `On _, _ ->
        of_bootstrap "Button" ~as_:"div"
          ~style:[ ("width", "95px"); ("pointerEvents", "none") ]
          ~size:"sm" ~variant:"dark" [ of_string txt ]
    | `Target, _ ->
        of_bootstrap "Button" ~as_:"div"
          ~style:[ ("width", "95px"); ("pointerEvents", "none") ]
          ~size:"sm" ~class_:[ "active"; "focus" ] ~variant:"success" [ of_string txt ]
  in

  match user_status with
  | `Train_to_end ->
      [
        button "Train to end" `Target;
        of_string " | ";
        button "Early stop" (`On `Early_stop);
        of_string " | ";
        button "Abort" (`On `Abort);
      ]
      |> of_tag "div"
  | `Early_stop ->
      [
        button "Train to end" (`On `Train_to_end);
        of_string " | ";
        button "Early stop" `Target;
        of_string " | ";
        button "Abort" (`On `Abort);
      ]
      |> of_tag "div"
  | `Abort ->
      [
        button "Train to end" (`On `Train_to_end);
        of_string " | ";
        button "Early stop" (`On `Early_stop);
        of_string " | ";
        button "Abort" `Target;
      ]
      |> of_tag "div"

type props = training_parameters * (outcome -> unit)

let construct (props : props) =
  Printf.printf "> construct component: training control\n%!";
  let params, on_completion = props in

  let routine_events, fire_routine_event = React.E.create () in
  let routine_progress =
    React.S.fold (fun i a -> match a with `Batch_end (i, _) -> i + 1 | _ -> i) 0 routine_events
  in
  let routine_status =
    let reduce s ev =
      match (s, ev) with
      | `Running, `End (encoders, decoder, stats) ->
          on_completion
            (`End (List.hd encoders, decoder, stats.batch_count * params.config.batch_size));
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

  let user_status, set_user_status = React.S.create `Train_to_end in
  let set_user_status : user_status -> unit = set_user_status in

  let render _ =
    let open Reactjs.Jsx in
    let routine_status = React.S.value routine_status in
    let routine_progress = React.S.value routine_progress in
    let user_status = React.S.value user_status in
    let prog =
      (routine_progress |> float_of_int) /. (params.config.batch_count |> float_of_int) *. 100.
    in
    let style = [ ("display", "flex"); ("alignItems", "center") ] in
    let tbody =
      [
        of_render render_instructions (user_status, routine_status, set_user_status)
        >> of_bootstrap "Col" ~sm:6 ~style;
        [
          of_string "Routine |";
          ( match (routine_status, routine_progress) with
          | `Running, 0 ->
              "Allocating" |> of_string
              >> of_bootstrap "Badge" ~variant:"warning" ~style:[ ("marginLeft", "6px") ]
          | `Running, _ ->
              "Running" |> of_string
              >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ]
          | `Ended, _ ->
              "Ended" |> of_string
              >> of_bootstrap "Badge" ~variant:"success" ~style:[ ("marginLeft", "6px") ]
          | `Aborted, _ ->
              "Aborted" |> of_string
              >> of_bootstrap "Badge" ~variant:"danger" ~style:[ ("marginLeft", "6px") ]
          | `Crashed, _ ->
              "Crashed" |> of_string
              >> of_bootstrap "Badge" ~variant:"danger" ~style:[ ("marginLeft", "6px") ] );
        ]
        |> of_bootstrap "Col" ~sm:3 ~style;
        [
          of_string "Progress |";
          Printf.sprintf "%.0f%%" prog |> of_string
          >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ];
        ]
        |> of_bootstrap "Col" ~sm:3 ~style;
      ]
      |> of_bootstrap "Row" >> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr"
      >> of_tag "tbody"
    in
    let thead = of_string "Training Control" >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~class_:[ "mnist-panel" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  let mount () =
    if params.config.from_webworker then (
      let fire_routine_event ev =
        ev |> Webworker_routine.postprocess_out_msg |> fire_routine_event
      in
      let ww =
        Webworker_routine.create fire_routine_event (fun _err -> Printf.eprintf "Webworker err\n%!")
      in
      let user_status = React.S.map Webworker_routine.preprocess_in_msg user_status in
      React.S.changes user_status |> React.E.map (Webworker_routine.post_in_message ww) |> ignore;
      React.S.changes routine_status
      |> React.E.map (function
           | `Running -> ()
           | `Ended | `Aborted | `Crashed ->
               (* Killing Webworker to free GPU memory *)
               Webworker_routine.terminate ww)
      |> ignore;
      Webworker_routine.post_in_message ww (React.S.value user_status);
      Webworker_routine.post_in_message ww (Webworker_routine.preprocess_in_msg (`Prime params)) )
    else
      let routine () = routine params fire_routine_event user_status in
      Js_of_ocaml_lwt.Lwt_js_events.async routine
  in

  Reactjs.construct ~mount ~signal:routine_progress ~signal:user_status ~signal:routine_status
    render
