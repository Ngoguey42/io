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
- Understand when exactly is afterplot called. Seems too often
- Show a marker when len of line == 1
- Refresh x range when a point is being rendered outside (is it aggressive?)
- Include loss, how to twiny?
- Include recall
- Better legend placement
- Fix legend names
- Share axes between tabs
- Share traces visibility between tabs
- Don't let user pan out of (y in [0;1])
- Don't let user pan out of (x in [0;ceil(tabwise_max_images_seen / 60000) * 60000])
- Clean code / Improve separation of concerns

 *)

(* DEBUG, will be removed *)
let string_of_rid (a, b) = Printf.sprintf "(%d, %d)" a b

let string_of_ev = function
  | `Render_done -> "`Render_done"
  | `Start_render (oldrid, newrid) ->
      Printf.sprintf "`Start_render (%s -> %s)" (string_of_rid oldrid) (string_of_rid newrid)
  | `Render_cooldowned -> "`Render_cooldowned"

let string_of_status = function
  | `Rendering -> "`Rendering"
  | `Rendered_long_ago -> "`Rendered_long_ago"
  | `Rendered_recently -> "`Rendered_recently"

let routine elt tab_shown_signal _tabsignal tabevents =
  (* Step 1 - Allocate arrays with ~constant append complexity *)
  let train_xs = new%js Js.array_empty in
  let train_ious = new%js Js.array_empty in
  let test_xs = new%js Js.array_empty in
  let test_ious = new%js Js.array_empty in

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
           train_xs##push images_seen |> ignore;
           train_ious##push new_stats.mean_iou_top1 |> ignore;
           train_xs##.length)
         images_seen_signal
    |> React.S.hold 0
  in
  let test_sample_count_signal =
    let sample f s e = React.S.sample f e s in
    stats_events
    |> React.E.fmap (function `Test stats -> Some stats | `Train_batch _ -> None)
    |> sample
         (fun new_stats images_seen ->
           test_xs##push images_seen |> ignore;
           test_ious##push new_stats.mean_iou_top1 |> ignore;
           Printf.eprintf "> Received test stats |  images_seen:%d | iou:%.03f%% | length:%d \n%!"
             images_seen (new_stats.mean_iou_top1 *. 100.) test_xs##.length;
           test_xs##.length)
         images_seen_signal
    |> React.S.hold 0
  in
  (* RID as in Revision ID *)
  let data_rid_signal =
    React.S.l2
      (fun a b ->
        Printf.eprintf "> data_rid_signal is now %s\n%!" (string_of_rid (a, b));
        (a, b))
      train_sample_count_signal test_sample_count_signal
  in

  (* Step 3 - Create the primitives necessary for smooth rendering *)

  (* Create the two React.event that will be triggered by JS *)
  let render_done_event, fire_render_done_event = React.E.create () in
  let render_cooldowned_event, fire_render_cooldowned_event = React.E.create () in

  (* Define the recursive signal *)
  let states0 = ((0, 0), `Rendered_long_ago) in
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
      let rendered_long_ago = React.S.map (fun s -> s = `Rendered_long_ago) render_status in
      let should_start_render = React.S.Bool.(dirty && tab_shown_signal && rendered_long_ago) in
      React.S.sample
        (fun () (old_rid, new_rid) -> (old_rid, new_rid))
        (React.S.Bool.rise should_start_render)
        (React.S.Pair.pair render_rid data_rid_signal)
    in

    (* Fold all events and the previous states into the new states *)
    let recursive_signal' =
      let fold ((rid, status) as s) ev =
        Printf.eprintf "> Fold | rid:%s | status:%s | ev:%s\n%!" (string_of_rid rid)
          (string_of_status status) (string_of_ev ev);
        match (status, ev) with
        | `Rendered_long_ago, `Start_render (_, new_rid) -> (new_rid, `Rendering)
        | `Rendering, `Render_done -> (rid, `Rendered_recently)
        | `Rendered_recently, `Render_cooldowned -> (rid, `Rendered_long_ago)
        | _, `Render_done ->
            (* TODO: Deal with all those cases properly *)
            Printf.eprintf "<ignoring that event> \n%!";
            s
        | _, `Render_cooldowned ->
            Printf.eprintf "<ignoring that event>\n%!";
            s
        | `Rendering, _ -> failwith "unreachable 1"
        | `Rendered_recently, _ -> failwith "unreachable 2"
      in
      React.E.select
        [
          React.E.map (fun payload -> `Start_render payload) start_render_event;
          React.E.map (fun () -> `Render_done) render_done_event;
          React.E.map (fun () -> `Render_cooldowned) render_cooldowned_event;
        ]
      |> React.S.fold fold states0
    in

    (* Done *)
    (recursive_signal', start_render_event)
  in
  let start_render_event = React.S.fix states0 define in

  (* Step 4 - Prime Plotly with the div element *)
  Printf.eprintf "> New plot\n%!";
  let data =
    [|
      object%js
        val x = new%js Js.array_empty
        val y = new%js Js.array_empty
        val mode = Js.string "lines"
        val name = "train iou"
        val line =
          object%js
            val color = Js.string "red"
          end
      end [@ocamlformat "disable"];
      object%js
        val x = new%js Js.array_empty
        val y = new%js Js.array_empty
        val mode = Js.string "lines"
        val name = "test iou"
        val line =
          object%js
            val color = Js.string "green"
          end
      end [@ocamlformat "disable"];
    |]
    |> Js.array
  in
  let layout =
    object%js
      val width = 686
      val height = 350
      val showlegend = true
      val xaxis =
        object%js
          val title = "Image Seen" |> Js.string
          val range = Js.array [| 0.; 60000. |]
        end
      val yaxis =
        object%js
          val range = Js.array [| 0.; 1. |]
          val fixedrange = true
        end
      val margin =
        object%js
          val l = 35
          val r = 15
          val b = 35
          val t = 10
          val pad = 0
        end
    end [@ocamlformat "disable"]
  in
  Js.Unsafe.global ##. Plotly##newPlot elt data layout |> ignore;

  (* Step 5 - Bind the JS callbacks that fire events *)
  Printf.eprintf "> Binding JS shit\n%!";
  let on_afterplot () =
    Printf.eprintf "> on_afterplot | called\n%!";
    fire_render_done_event ();
    Lwt_js_events.async (fun () ->
        Printf.eprintf "> on_afterplot | async\n%!";
        let open Lwt.Infix in
        Lwt_js.sleep 0.5 >>= fun () ->
        Printf.eprintf "> on_afterplot | slept\n%!";
        fire_render_cooldowned_event ();
        Lwt.return ())
  in
  let () =
    Js.Unsafe.(
      meth_call elt "on"
        [| Js.string "plotly_afterplot" |> inject; Js.wrap_callback on_afterplot |> inject |])
  in

  let on_start_render (((traini, testi), (trainj, testj)) as ev) =
    Printf.eprintf "> on_start_render | %s | called\n%!" (string_of_ev (`Start_render ev));
    Lwt_js_events.async (fun () ->
        Printf.eprintf "> on_start_render | %s | async\n%!" (string_of_ev (`Start_render ev));
        let data =
          object%js
            val x = [|
                train_xs##slice traini trainj;
                test_xs##slice testi testj;
              |] |> Js.array
            val y = [|
                train_ious##slice traini trainj;
                test_ious##slice testi testj
              |] |> Js.array
          end [@ocamlformat "disable"]
        in
        let trace_list = [| 0; 1 |] |> Js.array in
        let () = Js.Unsafe.global ##. Plotly##extendTraces elt data trace_list in
        Lwt.return ())
  in
  start_render_event |> React.E.map on_start_render |> ignore;

  ()
