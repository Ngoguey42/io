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
  fun () -> React.E.stop ~strong:true event

let construct_chart ~s0:tabsignal ~s1:tab_shown_signal ~e0:tabevents () =
  Debug.on_construct "chart";

  (* Create the Reactjs "reference" that will be used to bind the Plotly lib *)
  let plotly_ref = Reactjs.create_ref () in

  (* Step 1 - Allocate arrays with ~constant append complexity *)
  let raw_data = Raw_data.create () in

  (* Step 2 - Create signals reflecting the status of raw_data *)
  let data_rid_signal =
    let fold ((train_count, test_count) as acc) (s, ev) =
      (* Printf.eprintf "> Chart fold\n%!"; *)
      (* print_endline (show_tab_state s); *)
      (* print_endline (show_tab_event ev); *)
      match (s, ev) with
      | ( Training { images_seen; config = { batch_size; _ }; _ },
          Training_event (`Batch_end (batch_idx, stats)) ) ->
          (* New training point *)
          let x = images_seen + (batch_size * batch_idx) + stats.image_count in
          Raw_data.push_train raw_data x stats;
          let train_count = raw_data.train_xs##.length in
          (train_count, test_count)
      | _, Training_event (`Batch_end _) ->
          failwith "Unreachable - Chart - train batch end with unexpected state"
      | Creating_training { images_seen; _ }, Evaluation_event (`Outcome (`End stats)) ->
          (* New eval point *)
          let x = images_seen in
          Raw_data.push_test raw_data x stats;
          let test_count = raw_data.test_xs##.length in
          (train_count, test_count)
      | _, Evaluation_event (`Outcome (`End _)) ->
          failwith "Unreachable - Chart - eval end with unexpected state"
      | Selecting_backend _, Evaluation_event (`Outcome (`Crash _)) ->
          (* Initial eval crash *)
          Raw_data.rollback raw_data ~-1;
          (0, 0)
      | Creating_training { images_seen; _ }, Evaluation_event (`Outcome (`Crash _))
      | Creating_training { images_seen; _ }, Training_event (`Outcome (`Abort | `Crash _)) ->
          (* Eval or train crash or abort *)
          Raw_data.rollback raw_data images_seen;
          let train_count = raw_data.train_xs##.length in
          let test_count = raw_data.test_xs##.length in
          (train_count, test_count)
      | _, Training_event (`Outcome (`Abort | `Crash _)) ->
          failwith "Unreachable - Chart - train crash or abort with unexpected state"
      | _, Evaluation_event (`Outcome (`Crash _)) ->
          failwith "Unreachable - Chart - eval crash with unexpected state"
      | _, Training_event (`Batch_begin _ | `Init | `Outcome (`End _)) -> acc
      | _, (Network_made _ | Backend_selected _ | Training_conf _) -> acc
      | _, Evaluation_event (`Batch_end _ | `Batch_begin _ | `Init) -> acc
    in

    React.S.sample (fun ev s -> (s, ev)) tabevents tabsignal |> React.S.fold fold (0, 0)
  in

  (* Step 3 - Create the primitives necessary for smooth rendering *)

  (* Step 3.1 - Create the React primitives that will be triggered from JS *)
  let render_cooled_down_event, fire_render_cooled_down =
    React.E.create ()
    (* collected when `collect` is called *)
  in
  let max_sampling_ops, accum_max_sampling =
    React.E.create ()
    (* collected when `collect` is called *)
  in
  let accum_max_sampling : (int -> int) -> unit = accum_max_sampling in
  let max_sampling_signal = React.S.accum max_sampling_ops default_max_sampling in

  let smoothing_ops, accum_smoothing = React.E.create () (* collected when `collect` is called *) in
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
    (recursive_signal', (recursive_signal', start_render_event))
  in
  let recursive_signal, start_render_event = React.S.fix states0 define in

  let render () =
    Debug.on_render "chart";
    let _, (train_points, _), max_sampling, smoothing = React.S.value recursive_signal in
    let max_sampling = min train_points max_sampling in
    let open Reactjs.Jsx in
    let s = Smoothing.half_life_image_count smoothing in
    let s' = float_of_int s /. float_of_int Mnist.train_set_size *. 100. in
    let d0 =
      Printf.sprintf "%d training points currently shown (out of %d) " max_sampling train_points
      |> of_string >> of_tag "p"
      >> of_bootstrap "Col" ~md_span:6 ~classes:[ "text-muted"; "chart-info"; "chart-info-left" ]
    in
    let d1 =
      Printf.sprintf "Smoothing half-life at %d images (%.1f%% of an epoch)" s s'
      |> of_string >> of_tag "p"
      >> of_bootstrap "Col" ~md_span:6 ~classes:[ "text-muted"; "chart-info"; "chart-info-right" ]
    in
    let descr = [ d0; d1 ] |> of_bootstrap "Row" >> of_bootstrap "Container" in

    let chart = of_tag "div" ~key:"plotly" ~ref:plotly_ref ~style:[ ("height", "325px") ] [] in
    [ descr; chart ] |> of_react "Fragment"
  in
  let mount () =
    Debug.on_mount "chart";
    let elt =
      match plotly_ref##.current |> Js.Opt.to_option with
      | None -> failwith "unreachable. React.ref failed"
      | Some elt -> elt
    in
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
    let collect_afterplot = listen_afterplot_event elt fire_render_cooled_down in

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
    fun () ->
      Debug.on_unmount "chart";
      collect_afterplot ();
      React.E.stop ~strong:true render_cooled_down_event;
      React.E.stop ~strong:true max_sampling_ops
  in

  Reactjs.construct ~signal:recursive_signal ~mount render
