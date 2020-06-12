open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

module Raw_data = Chart_data.Raw_data
module Smoothing = Chart_data.Smoothing
open Types

let default_max_sampling = 64

let default_smoothing = `S3

(*
# Plotly events
- plotly_afterplot
  - To be used to avoid spamming redraw when no CPU time available
- plotly_legendclick, plotly_legenddoubleclick
  - Save this config globally (watch out for infinite loops)
- plotly_relayout, plotly_autosize
  - emits only the xaxis and yaxis ranges which were directly changed by the triggering event
  - Save this config globally (watch out for infinite loops)
- plotly_webglcontextlost
  - TODO: Try to trigger error with large batch size
  - When crashed, put a button to reload chart

Plotly.newPlot(graphDiv, data, layout, config)

Plotly.relayout(graphDiv, update)

Plotly.makeTemplate(figure)
  - layout = {template:template}

Plotly.extendTraces(graphDiv, {y: [[rand()]]}, [0])

 *)

(* DEBUG *)
let string_of_rid (a, b) = Printf.sprintf "(%d, %d)" a b

let string_of_smoothing = function
  | `S0 -> "`S0"
  | `S1 -> "`S1"
  | `S2 -> "`S2"
  | `S3 -> "`S3"
  | `S4 -> "`S4"
  | `S5 -> "`S5"

let string_of_ev = function
  | `Start_render _ -> "`Start_render"
  | `Plotly_cooled_down -> "`Plotly_cooled_down"
  | `Smoothing v -> Printf.sprintf "`Smoothing %s" (string_of_smoothing v)
  | `Max_sampling v -> Printf.sprintf "`Max_sampling %d" v

let string_of_temperature = function `Plotly_hot -> "`Plotly_hot" | `Plotly_cool -> "`Plotly_cool"

let string_of_state (temperature, rid, max_sampling, smoothing) =
  Printf.sprintf "(%s, %s, %d, %s)"
    (string_of_temperature temperature)
    (string_of_rid rid) max_sampling (string_of_smoothing smoothing)

let listen_afterplot_event elt fire_render_cooled_down =
  let afterplot_cooldown_length = 2. in
  let event, fire_event = React.E.create () in

  React.S.fold
    (fun afterplot_cd_count ev ->
      match (afterplot_cd_count, ev) with
      | 0, `After_afterplot -> failwith "Unreachable. Not enough afterplot events"
      | 1, `After_afterplot ->
          fire_render_cooled_down ();
          0
      | i, `After_afterplot -> i - 1
      | i, `Afterplot -> i + 1)
    0 event
  |> ignore;

  let on_afterplot () =
    fire_event `Afterplot;
    Lwt_js_events.async (fun () ->
        let open Lwt.Infix in
        Lwt_js.sleep afterplot_cooldown_length >>= fun () ->
        fire_event `After_afterplot;
        Lwt.return ())
  in
  let () =
    Js.Unsafe.(
      [| Js.string "plotly_afterplot" |> inject; Js.wrap_callback on_afterplot |> inject |]
      |> meth_call elt "on")
  in
  ()

let routine elt tab_shown_signal _tabsignal tabevents =
  (* Step 1 - Allocate arrays with ~constant append complexity *)
  let raw_data = Raw_data.create () in

  (* Step 2 - Create signals reflecting the status of raw_data *)
  let stats_events : [ `Test of evaluation_stats | `Train_batch of training_stats ] React.event =
    tabevents
    |> React.E.fmap (function
         | Evaluation_event (`Outcome (`End stats)) -> Some (`Test stats)
         | Training_event (`Batch_end (_, stats)) -> Some (`Train_batch stats)
         | _ -> None)
  in
  let images_seen_signal =
    stats_events
    |> React.S.fold
         (fun s ev -> match ev with `Test _ -> s | `Train_batch stats -> s + stats.image_count)
         0
  in
  let train_sample_count_signal =
    let sample f s e = React.S.sample f e s in
    stats_events
    |> React.E.fmap (function `Test _ -> None | `Train_batch stats -> Some stats)
    |> sample
         (fun (new_stats : training_stats) images_seen ->
           Raw_data.push_train raw_data images_seen new_stats;
           raw_data.train_xs##.length)
         images_seen_signal
    |> React.S.hold 0
  in
  let test_sample_count_signal =
    let sample f s e = React.S.sample f e s in
    stats_events
    |> React.E.fmap (function `Test stats -> Some stats | `Train_batch _ -> None)
    |> sample
         (fun new_stats images_seen ->
           Raw_data.push_test raw_data images_seen new_stats;
           raw_data.test_xs##.length)
         images_seen_signal
    |> React.S.hold 0
  in
  (* RID as in Revision ID *)
  let data_rid_signal =
    React.S.l2 (fun a b -> (a, b)) train_sample_count_signal test_sample_count_signal
  in

  (* Step 3 - Create the primitives necessary for smooth rendering *)

  (* Step 3.1 - Create the React primitives that will be triggered from JS *)
  let render_cooled_down_event, fire_render_cooled_down = React.E.create () in

  let max_sampling_ops, accum_max_sampling = React.E.create () in
  let accum_max_sampling : (int -> int) -> unit = accum_max_sampling in
  let max_sampling_signal = React.S.accum max_sampling_ops default_max_sampling in

  let smoothing_ops, accum_smoothing = React.E.create () in
  let accum_smoothing : (Smoothing.t -> Smoothing.t) -> unit = accum_smoothing in
  let smoothing_signal = React.S.accum smoothing_ops default_smoothing in

  (* Step 3.2 - Define the recursive signal *)
  let states0 = (`Plotly_cool, (0, 0), default_max_sampling, default_smoothing) in
  let define recursive_signal =
    (* Unpack recursive signal (RID as in Revision ID) *)
    let plotly_temperature = React.S.map (fun (v, _, _, _) -> v) recursive_signal in
    let rendered_rid = React.S.map (fun (_, v, _, _) -> v) recursive_signal in
    let rendered_max_sampling = React.S.map (fun (_, _, v, _) -> v) recursive_signal in
    let rendered_smoothing = React.S.map (fun (_, _, _, v) -> v) recursive_signal in

    (* Create the React.event that is triggered by input signals.
       This `start_render_event` will be fired as soon as the conditions are met. (see code)
    *)
    let start_render_event =
      let should_start_render =
        let rendered_long_ago = React.S.map (fun s -> s = `Plotly_cool) plotly_temperature in
        let open React.S.Compare in
        let open React.S.Bool in
        let data_dirty = data_rid_signal <> rendered_rid in
        let max_sampling_dirty = max_sampling_signal <> rendered_max_sampling in
        let smoothing_dirty = smoothing_signal <> rendered_smoothing in
        tab_shown_signal
        && ((data_dirty && rendered_long_ago) || max_sampling_dirty || smoothing_dirty)
      in
      React.S.sample
        (fun () (rid, max_sampling, smoothing) -> (rid, max_sampling, smoothing))
        (React.S.Bool.rise should_start_render)
        (React.S.l3 (fun a b c -> (a, b, c)) data_rid_signal max_sampling_signal smoothing_signal)
    in

    (* Fold all events and the previous states into the new states *)
    let recursive_signal' =
      let fold ((temperature, rid, max_sampling, smoothing) as s) ev =
        let s' =
          match (temperature, ev) with
          | `Plotly_hot, `Plotly_cooled_down -> (`Plotly_cool, rid, max_sampling, smoothing)
          | `Plotly_cool, `Plotly_cooled_down -> s
          | (`Plotly_cool | `Plotly_hot), `Start_render (rid, max_sampling, smoothing) ->
              (`Plotly_hot, rid, max_sampling, smoothing)
        in
        (* Printf.eprintf "> Fold | s:%35s | ev:%20s | s':%35s\n%!" (string_of_state s) *)
        (*   (string_of_ev ev) (string_of_state s'); *)
        s'
      in
      React.E.select
        [
          React.E.map (fun payload -> `Start_render payload) start_render_event;
          React.E.map (fun () -> `Plotly_cooled_down) render_cooled_down_event;
        ]
      |> React.S.fold fold states0
    in

    (* Done *)
    (recursive_signal', start_render_event)
  in
  let start_render_event = React.S.fix states0 define in

  (* Step 4 - Prime Plotly with the div element *)
  let conf = Chart_data.create_conf accum_max_sampling accum_smoothing in
  let smoothing = React.S.value smoothing_signal in
  let max_training_data_points = React.S.value max_sampling_signal in
  let subsampled_raw_data = Raw_data.subsample raw_data smoothing max_training_data_points in
  Js.Unsafe.global ##. Plotly##newPlot
    elt
    (Chart_data.plotly_data_of_raw_data raw_data subsampled_raw_data)
    (Chart_data.create_layout true raw_data subsampled_raw_data)
    conf
  |> ignore;

  (* Step 5 - Listen to the afterplot events to avoid to debounce rendering
     https://plotly.com/javascript/plotlyjs-events/#afterplot-event
  *)
  listen_afterplot_event elt fire_render_cooled_down;

  (* Step 6 - Schedule `extendTraces` calls *)
  let on_start_render (_, max_sampling, smoothing) =
    Lwt_js_events.async (fun () ->
        (* Plotly.react has the same runtime as Plotly.extentTraces *)
        let subsampled_raw_data = Raw_data.subsample raw_data smoothing max_sampling in
        let prev_template = Js.Unsafe.global ##. Plotly##makeTemplate elt in
        Js.Unsafe.global ##. Plotly##react
          elt
          (Chart_data.plotly_data_of_raw_data ~prev_template raw_data subsampled_raw_data)
          (Chart_data.create_layout false raw_data subsampled_raw_data)
          conf
        |> ignore;
        Lwt.return ())
  in
  start_render_event |> React.E.map on_start_render |> ignore;
  ()
