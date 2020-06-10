open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

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
- Move buttons and legend below chart
  - Horizontal legend??
- shrink plot height
- Clean code / Improve separation of concerns
- Cross tab
  - Share axes range
  - Share traces visibility
- Catch and deal with webgl context lost? (necessary? i'm using scatter and not glscatter)
- don't show all points, rendering gets really long
- don't render when window out of focus ? possible ?
- face
  - improve color of legend
  - make it less complex to point the green crosses
  - X1 range:
    - Refresh x range when a point is being rendered outside (is it aggressive?)
    - Is there an "autoscale" callback? I could make autoscale the default and fix many problems at once
  - X2 range:
    - Clip between [0; mean + std * 2]
  - Responsive graph ?
- wontfix
  - Don't let user pan out of (y in [0;1])
  - Don't let user pan out of (x in [0;ceil(tabwise_max_images_seen / 60000) * 60000])
  - Show both epoch count and image count in x-axis hover
  - hide y2/y3 lines ?

 *)
type raw_data = {
  train_xs : int Js.js_array Js.t;
  train_ious : float Js.js_array Js.t;
  train_recalls : float Js.js_array Js.t;
  train_losses : float Js.js_array Js.t;
  train_lrs : float Js.js_array Js.t;
  test_xs : int Js.js_array Js.t;
  test_ious : float Js.js_array Js.t;
  test_recalls : float Js.js_array Js.t;
}

(* DEBUG, will be removed *)
let string_of_rid (a, b) = Printf.sprintf "(%d, %d)" a b

let string_of_ev = function
  | `Start_render (oldrid, newrid) ->
      Printf.sprintf "`Start_render (%s -> %s)" (string_of_rid oldrid) (string_of_rid newrid)
  | `Plotly_cooled_down -> "`Plotly_cooled_down"

let string_of_status = function `Plotly_hot -> "`Plotly_hot" | `Plotly_cool -> "`Plotly_cool"

let wrap_raw_data raw_data =
  let train_iou =
    object%js
      val x = raw_data.train_xs
      val y = raw_data.train_ious
      val _type = Js.string "scatter"
      val mode = Js.string "markers"
      val name = Js.string "train iou"
      (* val textinfo = Js.string "percent" *)
      (* val line = *)
      (*   object%js *)
      (*     val color = Js.string "red" *)
      (*   end *)
      val marker =
        object%js
          val color = Js.string "red"
          val size = 3
                       (* val symbol = Js.string "x" *)
        end
    end [@ocamlformat "disable"]
  in
  let train_recall =
    object%js
      val x = raw_data.train_xs
      val y = raw_data.train_recalls
      val _type = Js.string "scatter"
      val mode = Js.string "markers"
      val name = Js.string "train recall"
      val visible = Js.string "legendonly"
      (* val textinfo = Js.string "percent" *)
      (* val line = *)
      (*   object%js *)
      (*     val color = Js.string "red" *)
      (*   end *)
      val marker =
        object%js
          val color = Js.string "red"
          val size = 3
                       (* val symbol = Js.string "x" *)
        end
    end [@ocamlformat "disable"]
  in
  let loss =
    object%js
      val x = raw_data.train_xs
      val y = raw_data.train_losses
      val _type = Js.string "scatter"
      val yaxis = Js.string "y2"
      val mode = Js.string "markers"
      val name = Js.string "loss"
      val line =
        object%js
          val color = Js.string "orange"
        end
      val marker =
        object%js
          val color = Js.string "orange"
          val size = 3
                       (* val symbol = Js.string "x" *)
        end
    end [@ocamlformat "disable"]
  in
  let lr =
    object%js
      val x = raw_data.train_xs
      val y = raw_data.train_lrs
      val _type = Js.string "scatter"
      val yaxis = Js.string "y3"
      val mode = Js.string "lines"
      val name = Js.string "learning rate"
      val visible = Js.string "legendonly"
      val line =
        object%js
          val color = Js.string "red"
          val width = 1
        end
    end [@ocamlformat "disable"]
  in
  let test_iou =
    object%js
      val x = raw_data.test_xs
      val y = raw_data.test_ious
      val _type = Js.string "scatter"
      val mode = Js.string "lines+markers"
      val name = Js.string "test iou"
      (* val textinfo = Js.string "percent" *)
      val line =
        object%js
          val color = Js.string "green"
        end
      val marker =
        object%js
          val color = Js.string "green"
          val size = 10
          val symbol = Js.string "x"
        end
    end [@ocamlformat "disable"]
  in
  let test_recall =
    object%js
      val x = raw_data.test_xs
      val y = raw_data.test_recalls
      val _type = Js.string "scatter"
      val mode = Js.string "lines+markers"
      val name = Js.string "test recall"
      val visible = Js.string "legendonly"
      (* val textinfo = Js.string "percent" *)
      val line =
        object%js
          val color = Js.string "green"
        end
      val marker =
        object%js
          val color = Js.string "green"
          val size = 10
          val symbol = Js.string "x"
        end
    end [@ocamlformat "disable"]
  in

  let i = Js.Unsafe.inject in
  [| i train_iou; i train_recall; i loss; i lr; i test_iou; i test_recall |] |> Js.array

let create_layout () =
  object%js
    val width = 686
    val height = 350
    val showlegend = true
    val clickmode = Js.string "none"
    val dragmode = false
  
    (* val grid = *)
    (*   object%js *)
    (*     val domain = *)
    (*       object%js *)
    (*         val x = [| 0.; 1. |] |> Js.array *)
    (*         val y = [| 0.; 1. |] |> Js.array *)
    (*       end *)
    (*   end *)
  
    val legend =
      object%js
        (* val bgcolor = "#f7f7f780" *)
        val x = 0.8
        val y = 0.5
      end
    val xaxis =
      object%js
        val title = "image seen" |> Js.string
        val gridcolor = Js.string "black"
        val rangemode = Js.string "nonnegative"
        val range = Js.array [| 0.; 60000. |]
        val autorange = true
        val zerolinewidth = 1.5
        val tickfont =
          object%js
            val size = 8
          end
      end
    val yaxis =
      object%js
        (* val title = Js.string "accuracies" *)
        val range = Js.array [| 0.; 1. |]
        val zerolinewidth = 1.5
        val fixedrange = true
        val tickformat = Js.string ",.0%"
        val hoverformat = Js.string ",.2%"
        val rangemode = Js.string "nonnegative"
        val gridcolor = Js.string "black"
        (* val ticks = Js.string "inside" *)
        val tickfont =
          object%js
            val size = 8
          end
      end
    (* Sets the domain of this axis (in plot fraction). *)
    val yaxis2 =
      object%js
        val visible = false
        (* val title = Js.string "loss" *)
        val range = Js.array [| 0.; 20. |]
        val showgrid = false
        val gridcolor = Js.string "F7F7F7"
        val zeroline = false
        val zerolinewidth = 0.01
        val linewidth = 0.01
        val showticklabels = false
        (* val ticks = Js.string "" *)
        val rangemode = Js.string "nonnegative"
        val titlefont =
          object%js
            val color = Js.string "orange"
          end
        val side = Js.string "right"
        val tickfont =
          object%js
            val color = Js.string "#FcFcFc"
            (* val color = Js.string "#F7F7F7" *)
            val size = 1
          end
        val overlaying = Js.string "y"
      end
    val yaxis3 =
      object%js
        val visible = false
        (* val title = Js.string "loss" *)
        val range = Js.array [| 0.; 2e-3 |]
        val showgrid = false
        val gridcolor = Js.string "F7F7F7"
        val zeroline = false
        val hoverformat = Js.string ",.2e"
        val zerolinewidth = 0.01
        val linewidth = 0.01
        val showticklabels = false
        (* val ticks = Js.string "" *)
        val rangemode = Js.string "nonnegative"
        val titlefont =
          object%js
            val color = Js.string "orange"
          end
        val side = Js.string "right"
        val tickfont =
          object%js
            val color = Js.string "#FcFcFc"
            (* val color = Js.string "#F7F7F7" *)
            val size = 1
          end
        val overlaying = Js.string "y"
      end
    val margin =
      object%js
        val l = 30
        val r = 10
        val b = 25
        val t = 10
        val pad = 0
      end
  end [@ocamlformat "disable"]

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
  let raw_data =
    {
      train_xs = new%js Js.array_empty;
      train_ious = new%js Js.array_empty;
      train_recalls = new%js Js.array_empty;
      train_losses = new%js Js.array_empty;
      train_lrs = new%js Js.array_empty;
      test_xs = new%js Js.array_empty;
      test_ious = new%js Js.array_empty;
      test_recalls = new%js Js.array_empty;
    }
  in

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
           raw_data.train_xs##push images_seen |> ignore;
           raw_data.train_ious##push new_stats.mean_iou_top1 |> ignore;
           raw_data.train_recalls##push new_stats.mean_recall_top1 |> ignore;
           raw_data.train_losses##push new_stats.mean_loss_per_image |> ignore;
           raw_data.train_lrs##push new_stats.mean_learning_rate |> ignore;
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
           raw_data.test_xs##push images_seen |> ignore;
           raw_data.test_ious##push new_stats.mean_iou_top1 |> ignore;
           raw_data.test_recalls##push new_stats.mean_recall_top1 |> ignore;
           raw_data.test_xs##.length)
         images_seen_signal
    |> React.S.hold 0
  in
  (* RID as in Revision ID *)
  let data_rid_signal =
    React.S.l2 (fun a b -> (a, b)) train_sample_count_signal test_sample_count_signal
  in

  (* Step 3 - Create the primitives necessary for smooth rendering *)

  (* Create the two React.event that will be triggered by JS *)
  let render_cooled_down_event, fire_render_cooled_down = React.E.create () in

  (* Define the recursive signal *)
  let states0 = ((0, 0), `Plotly_cool) in
  let define recursive_signal =
    (* Unpack recursive signal (RID as in Revision ID) *)
    let render_rid = React.S.Pair.fst recursive_signal in
    let render_status = React.S.Pair.snd recursive_signal in

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
        (fun () (old_rid, new_rid) -> (old_rid, new_rid))
        (React.S.Bool.rise should_start_render)
        (React.S.Pair.pair render_rid data_rid_signal)
    in

    (* Fold all events and the previous states into the new states *)
    let recursive_signal' =
      let fold ((rid, status) as s) ev =
        (* Printf.eprintf "> Fold | rid:%s | status:%s | ev:%s\n%!" (string_of_rid rid) *)
        (*   (string_of_status status) (string_of_ev ev); *)
        match (status, ev) with
        | `Plotly_cool, `Start_render (_, new_rid) -> (new_rid, `Plotly_hot)
        | `Plotly_hot, `Plotly_cooled_down -> (rid, `Plotly_cool)
        | `Plotly_cool, `Plotly_cooled_down -> s
        | `Plotly_hot, `Start_render _ -> failwith "Unreachable. Start render with plotly hot"
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
  Js.Unsafe.global ##. Plotly##newPlot elt (wrap_raw_data raw_data) (create_layout ()) |> ignore;

  (* Step 5 - Listen to the afterplot events to avoid to debounce rendering
     https://plotly.com/javascript/plotlyjs-events/#afterplot-event
  *)
  listen_afterplot_event elt fire_render_cooled_down;

  (* Step 6 - Schedule `extendTraces` calls *)
  let on_start_render ((traini, testi), (trainj, testj)) =
    Lwt_js_events.async (fun () ->
        let data =
          object%js
            val x =
              [|
                raw_data.train_xs##slice traini trainj;
                raw_data.train_xs##slice traini trainj;
                raw_data.train_xs##slice traini trainj;
                raw_data.train_xs##slice traini trainj;
                raw_data.test_xs##slice testi testj;
                raw_data.test_xs##slice testi testj;
              |]
              |> Js.array

            val y =
              [|
                raw_data.train_ious##slice traini trainj;
                raw_data.train_recalls##slice traini trainj;
                raw_data.train_losses##slice traini trainj;
                raw_data.train_lrs##slice traini trainj;
                raw_data.test_ious##slice testi testj;
                raw_data.test_recalls##slice testi testj;
              |]
              |> Js.array
          end
        in
        let trace_list = [| 0; 1; 2; 3; 4; 5 |] |> Js.array in
        let () = Js.Unsafe.global ##. Plotly##extendTraces elt data trace_list in
        Lwt.return ())
  in
  start_render_event |> React.E.map on_start_render |> ignore;

  ()
