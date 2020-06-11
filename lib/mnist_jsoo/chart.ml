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
open Types

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


# TODO
- Rollback traces on crash/abort
- Move buttons above chart
- Move legend below chart
  - Horizontal legend? (does it fit?)
  - improve background color
- shrink plot height
- Clean code / Improve separation of concerns
- Range improvements
  - Disable possibility for user to affect axes ranges
    - Remove the necessary buttons
    - Disable all mouse interactions that move
    - Keep hover on data !!
  - Set range on each render
  - What about axes sharing between tabs ? (OSEF I think)
  - x range
    - None: [0, 60000]
    - Some: [0, 60000 * ceil (max / 60000)]
  - y range (accuracies)
    - Always: [0, 1]
  - y1 range (loss)
    - None: OSEF
    - Only one. [0, val * 2]
    - Many:  [0; mean + std * 2]
  - y2 range (lr)
    - None: OSEF
    - All same: [0, val * 2]
    - Not all same: [0, maxval * 1.1]
- face
  - make it less complex to point the green crosses
  - Responsive graph
- Cross tab
  - Share axes range (discard?)
  - Share traces visibility (wont do?)
- wontfix
  - Don't let user pan out of (y in [0;1])
  - Don't let user pan out of (x in [0;ceil(tabwise_max_images_seen / 60000) * 60000])
  - Show both epoch count and image count in x-axis hover
  - hide y2/y3 lines ?

 *)

(* DEBUG *)
let string_of_rid (a, b) = Printf.sprintf "(%d, %d)" a b

let string_of_smoothing = function
  | `S0 -> "`S0"
  | `S1 -> "`S1"
  | `S2 -> "`S2"
  | `S3 -> "`S3"
  | `S4 -> "`S4"

let string_of_ev = function
  | `Start_render _ -> "`Start_render"
  | `Plotly_cooled_down -> "`Plotly_cooled_down"
  | `Smoothing v -> Printf.sprintf "`Smoothing %s" (string_of_smoothing v)
  | `Max_sampling v -> Printf.sprintf "`Max_sampling %d" v

let string_of_status = function `Plotly_hot -> "`Plotly_hot" | `Plotly_cool -> "`Plotly_cool"

let string_of_state (rid, status, max_sampling, smoothing) =
  Printf.sprintf "(%s, %s, %d, %s)" (string_of_rid rid) (string_of_status status) max_sampling
    (string_of_smoothing smoothing)

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

  (* Step 2 - Create signals reflecting the status of the above arrays *)
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

  (* Create the React primitives that will be triggered from JS *)
  let render_cooled_down_event, fire_render_cooled_down = React.E.create () in
  let max_sampling_signal, set_max_sampling = React.S.create 100 in
  let smoothing_signal, set_smoothing = React.S.create `S2 in
  ignore (set_smoothing, set_max_sampling);

  (* Define the recursive signal *)
  let states0 = ((0, 0), `Plotly_cool, 100, `S2) in
  let define recursive_signal =
    (* Unpack recursive signal (RID as in Revision ID) *)
    let render_rid = React.S.map (fun (v, _, _, _) -> v) recursive_signal in
    let render_status = React.S.map (fun (_, v, _, _) -> v) recursive_signal in

    (* Create the React.event that is triggered by input signals

       This `start_render_event` will be fired as soon as the 3 conditions are met:
       - render revision differs from data revision
       - enough time passed since Plotly was done rendering
       - the tab is the shown one
    *)
    let start_render_event =
      let dirty = React.S.Compare.(data_rid_signal <> render_rid) in
      let rendered_long_ago = React.S.map (fun s -> s = `Plotly_cool) render_status in
      let should_start_render = React.S.Bool.(dirty && tab_shown_signal && rendered_long_ago) in
      React.S.sample
        (fun () ((_, _, max_sampling, smoothing), rid) -> (rid, max_sampling, smoothing))
        (React.S.Bool.rise should_start_render)
        (React.S.Pair.pair recursive_signal data_rid_signal)
    in

    (* Fold all events and the previous states into the new states *)
    let recursive_signal' =
      let fold ((rid, status, max_sampling, smoothing) as s) ev =
        Printf.eprintf "> Fold | old-state:%s | ev:%s\n%!" (string_of_state s) (string_of_ev ev);
        match (status, ev) with
        | `Plotly_cool, `Start_render (new_rid, _, _) ->
            (new_rid, `Plotly_hot, max_sampling, smoothing)
        | `Plotly_hot, `Plotly_cooled_down -> (rid, `Plotly_cool, max_sampling, smoothing)
        | `Plotly_cool, `Plotly_cooled_down -> s
        | `Plotly_hot, `Start_render _ -> failwith "Unreachable. Start render with plotly hot"
        | (`Plotly_hot | `Plotly_cool), `Max_sampling max_sampling ->
            (rid, status, max_sampling, smoothing)
        | (`Plotly_hot | `Plotly_cool), `Smoothing smoothing ->
            (rid, status, max_sampling, smoothing)
      in
      React.E.select
        [
          React.E.map (fun payload -> `Start_render payload) start_render_event;
          React.E.map (fun () -> `Plotly_cooled_down) render_cooled_down_event;
          React.S.changes max_sampling_signal |> React.E.map (fun v -> `Max_sampling v);
          React.S.changes smoothing_signal |> React.E.map (fun v -> `Smoothing v);
        ]
      |> React.S.fold fold states0
    in

    (* Done *)
    (recursive_signal', start_render_event)
  in
  let start_render_event = React.S.fix states0 define in

  (* Step 4 - Prime Plotly with the div element *)
  let smoothing = React.S.value smoothing_signal in
  let max_training_data_points = React.S.value max_sampling_signal in
  Js.Unsafe.global ##. Plotly##newPlot
    elt
    (Chart_data.plotly_data_of_raw_data raw_data smoothing max_training_data_points)
    (Chart_data.create_layout ())
  |> ignore;

  (* Step 5 - Listen to the afterplot events to avoid to debounce rendering
     https://plotly.com/javascript/plotlyjs-events/#afterplot-event
  *)
  listen_afterplot_event elt fire_render_cooled_down;

  (* Step 6 - Schedule `extendTraces` calls *)
  let on_start_render (_, max_sampling, smoothing) =
    Lwt_js_events.async (fun () ->
        let rev = Random.int ((2 lsl 29) - 1) |> string_of_int in
        Js.Unsafe.global ##. Plotly##react
          elt
          (Chart_data.plotly_data_of_raw_data raw_data smoothing max_sampling)
          (Chart_data.create_layout ~rev ())
        |> ignore;
        Lwt.return ())
  in
  start_render_event |> React.E.map on_start_render |> ignore;

  ()
