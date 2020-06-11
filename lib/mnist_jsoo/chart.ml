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

let round v = if mod_float v 1. >= 0.5 then ceil v else floor v

let round_significant_base10 v b10_sig_digit =
  if v = 0. then v
  else begin
      let b10_sig_digit = float_of_int b10_sig_digit in
      let sign = if v > 0. then 1. else -1. in
      let v = v *. sign in
      let round_place = 10. ** (ceil (log10 v) -. b10_sig_digit) in
      round_place *. round (v /. round_place) *. sign
    end

let round_significant_base10_inplace a b10_sig_digit =
  for i = 0 to a##.length - 1 do
    let v = Js.Unsafe.get a i in
    Js.Unsafe.set a i (round_significant_base10 v b10_sig_digit)
  done

(* Exponential decay smoothing based on the number of images seen in training *)
module Smoothing = struct
  type t = [ `S0 | `S1 | `S2 | `S3 | `S4 ]

  let values = [ `S0; `S1; `S2; `S3; `S4 ]

  let half_life_image_count = function
    | `S0 -> 0 (* no smoothing, but beware of divisions *)
    | `S1 -> 480 (* 1/125 *)
    | `S2 -> 2400 (* 1/25 *)
    | `S3 -> 12000 (* 1/5 *)
    | `S4 -> 60000

  (* one epoch is the half-life *)

  let momentum s =
    let image_count = half_life_image_count s |> float_of_int in
    if image_count = 0. then 0. else log 0.5 /. image_count |> exp

  let correct_ys_inplace s xs ys =
    (* Here `ys` are the raw exp-decayed data stored. The real ys are `ys / (1 - momentum ** xs)`.
     *)
    assert (xs##.length = ys##.length);
    let momentum = momentum s in
    for i = 0 to ys##.length - 1 do
      let x = Js.Unsafe.get xs i in
      let y = Js.Unsafe.get ys i in
      Js.Unsafe.set ys i (y /. (1. -. (momentum ** float_of_int x)))
    done
end

(* Store raw stats for chart in a datastructures with:
   - `O(1)` append (stats arrive per batch)
   - `O(1)` random reads (need to subsample training stats at each redraw)
   - rollbackable (the user may abort an ongoing training).

   We need to subsample and smooth the training data because:
   1. 1k points ~= 1sec of render for plotly (linear complexity)
   2. They are noisy

   The test data do not need smoothing because:
   1. They are few
   2. I don't know how to exp-decay them because they don't have a batch_size correlated to their
      `x` coordinate.

   For each, `train y` and for each `smoothing value` we store the curves values to avoid smoothing
   data on redraw but we will still need to apply the correction for the exponential decay.
 *)
module Raw_data = struct
  module M = Map.Make (struct
    type t = Smoothing.t

    let compare = compare
  end)

  module Subsample = struct
    type t = {
      train_xs : int Js.js_array Js.t;
      train_ious : float Js.js_array Js.t;
      train_recalls : float Js.js_array Js.t;
      train_losses : float Js.js_array Js.t;
      train_lrs : float Js.js_array Js.t;
          (* test_xs : int Js.js_array Js.t; *)
          (* test_ious : float Js.js_array Js.t; *)
          (* test_recalls : float Js.js_array Js.t; *)
    }
  end

  type t = {
    train_xs : int Js.js_array Js.t;
    train_ious : float Js.js_array Js.t M.t;
    train_recalls : float Js.js_array Js.t M.t;
    train_losses : float Js.js_array Js.t M.t;
    train_lrs : float Js.js_array Js.t M.t;
    test_xs : int Js.js_array Js.t;
    test_ious : float Js.js_array Js.t;
    test_recalls : float Js.js_array Js.t;
  }

  let create () =
    let create_y () =
      [
        (`S0, new%js Js.array_empty);
        (`S1, new%js Js.array_empty);
        (`S2, new%js Js.array_empty);
        (`S3, new%js Js.array_empty);
        (`S4, new%js Js.array_empty);
      ]
      |> List.to_seq |> M.of_seq
    in
    {
      train_xs = new%js Js.array_empty;
      train_ious = create_y ();
      train_recalls = create_y ();
      train_losses = create_y ();
      train_lrs = create_y ();
      test_xs = new%js Js.array_empty;
      test_ious = new%js Js.array_empty;
      test_recalls = new%js Js.array_empty;
    }

  let rollback : t -> int -> unit =
   fun rd max_x ->
    let rec find_first_above_max xs i =
      if i = xs##.length then i
      else if Js.Unsafe.get xs i > max_x then i
      else find_first_above_max xs (i + 1)
    in
    let first_out = find_first_above_max rd.train_xs 0 in
    let count = rd.train_xs##.length - first_out in
    rd.train_xs##splice first_out count |> ignore;
    List.iter
      (fun smoothing ->
        (M.find smoothing rd.train_ious)##splice first_out count |> ignore;
        (M.find smoothing rd.train_recalls)##splice first_out count |> ignore;
        (M.find smoothing rd.train_losses)##splice first_out count |> ignore;
        (M.find smoothing rd.train_lrs)##splice first_out count |> ignore)
      Smoothing.values;

    let first_out = find_first_above_max rd.test_xs 0 in
    let count = rd.test_xs##.length - first_out in
    rd.test_xs##splice first_out count |> ignore;
    rd.test_ious##splice first_out count |> ignore;
    rd.test_recalls##splice first_out count |> ignore

  let push_test : t -> int -> evaluation_stats -> unit =
   fun rd x stats ->
    rd.test_xs##push x |> ignore;
    rd.test_ious##push stats.mean_iou_top1 |> ignore;
    rd.test_recalls##push stats.mean_recall_top1 |> ignore

  let push_train : t -> int -> training_stats -> unit =
   fun rd x stats ->
    let len = rd.train_xs##.length in
    let batch_size = if len = 0 then x else x - Js.Unsafe.get rd.train_xs (len - 1) in
    assert (x > 0);

    rd.train_xs##push x |> ignore;
    List.iter
      (fun smoothing ->
        (* Apply exp-decay. We also exponentiate the momentum by the batch size. *)
        let momentum = Smoothing.momentum smoothing ** (batch_size |> float_of_int) in

        let a = M.find smoothing rd.train_ious in
        let yprev = if len = 0 then 0. else Js.Unsafe.get a (len - 1) in
        a##push ((yprev *. momentum) +. (stats.mean_iou_top1 *. (1. -. momentum))) |> ignore;

        let a = M.find smoothing rd.train_recalls in
        let yprev = if len = 0 then 0. else Js.Unsafe.get a (len - 1) in
        a##push ((yprev *. momentum) +. (stats.mean_recall_top1 *. (1. -. momentum))) |> ignore;

        let a = M.find smoothing rd.train_lrs in
        let yprev = if len = 0 then 0. else Js.Unsafe.get a (len - 1) in
        a##push ((yprev *. momentum) +. (stats.mean_learning_rate *. (1. -. momentum))) |> ignore;

        let a = M.find smoothing rd.train_losses in
        let yprev = if len = 0 then 0. else Js.Unsafe.get a (len - 1) in
        a##push ((yprev *. momentum) +. (stats.mean_loss_per_image *. (1. -. momentum))) |> ignore)
      Smoothing.values

  let subsample_vector : 'a Js.js_array Js.t -> int -> 'a Js.js_array Js.t =
   fun a max_subsample_count ->
    let len = a##.length in
    if len <= max_subsample_count then
      (* Perform a copy because we will apply exp-decay correction in place on the y data *)
      a##slice 0 len
    else begin
      (* This will include the first point, the last point and (max_subsample_count - 2) others
         in the middle *)
      let b = new%js Js.array_empty in
      let floating_stride = float_of_int len /. float_of_int (max_subsample_count - 1) in
      for i = 0 to max_subsample_count - 1 do
        let i = float_of_int i *. floating_stride |> int_of_float in
        b##push (Js.Unsafe.get a i) |> ignore
      done;
      b
      end

  let subsample : t -> Smoothing.t -> int -> Subsample.t =
   fun rd smoothing max_subsample_count ->
    let ss =
      Subsample.
        {
          train_xs = subsample_vector rd.train_xs max_subsample_count;
          train_ious = subsample_vector (M.find smoothing rd.train_ious) max_subsample_count;
          train_recalls = subsample_vector (M.find smoothing rd.train_recalls) max_subsample_count;
          train_lrs = subsample_vector (M.find smoothing rd.train_lrs) max_subsample_count;
          train_losses =
            subsample_vector (M.find smoothing rd.train_losses) max_subsample_count
            (* test_xs = subsample_vector rd.test_xs max_subsample_count; *)
            (* test_ious = rd.test_ious; *)
            (* test_recalls = rd.test_recalls; *);
        }
    in
    Smoothing.correct_ys_inplace smoothing ss.train_xs ss.train_ious;
    Smoothing.correct_ys_inplace smoothing ss.train_xs ss.train_recalls;
    Smoothing.correct_ys_inplace smoothing ss.train_xs ss.train_lrs;
    Smoothing.correct_ys_inplace smoothing ss.train_xs ss.train_losses;

    (* Kill noise in learning rate because it may show on plot if all values are equal *)
    round_significant_base10_inplace ss.train_lrs 5;
    ss
end

(* DEBUG, will be removed *)
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
  | `Smoothing v ->
     Printf.sprintf "`Smoothing %s" (string_of_smoothing v)
  | `Max_sampling v ->
     Printf.sprintf "`Max_sampling %d" v

let string_of_status = function `Plotly_hot -> "`Plotly_hot" | `Plotly_cool -> "`Plotly_cool"

let string_of_state (rid, status, max_sampling, smoothing) =
  Printf.sprintf "(%s, %s, %d, %s)"
                 (string_of_rid rid)
                 (string_of_status status)
                 max_sampling
                 (string_of_smoothing smoothing)


let plotly_data_of_raw_data raw_data smoothing max_training_data_points =
  let subsampled_raw_data = Raw_data.subsample raw_data smoothing max_training_data_points in
  let open Raw_data.Subsample in
  let train_iou =
    object%js
      val x = subsampled_raw_data.train_xs
      val y = subsampled_raw_data.train_ious
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
      val x = subsampled_raw_data.train_xs
      val y = subsampled_raw_data.train_recalls
      val _type = Js.string "scatter"
      val mode = Js.string "markers"
      val name = Js.string "train recall"
      val visible = true (* Js.string "legendonly" *)
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
      val x = subsampled_raw_data.train_xs
      val y = subsampled_raw_data.train_losses
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
      val x = subsampled_raw_data.train_xs
      val y = subsampled_raw_data.train_lrs
      val _type = Js.string "scatter"
      val yaxis = Js.string "y3"
      val mode = Js.string "lines"
      val name = Js.string "learning rate"
      val visible = true (* Js.string "legendonly" *)
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
      val visible = true (* Js.string "legendonly" *)
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

let create_layout ?rev () =
  let rev = Js.Opt.option rev in

  object%js
    val width = 686
    val height = 350
    val showlegend = true
    val clickmode = Js.string "none"
    val dragmode = false
    val datarevision = rev

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
        | `Plotly_cool, `Start_render (new_rid, _, _) -> (new_rid, `Plotly_hot, max_sampling, smoothing)
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
    (plotly_data_of_raw_data raw_data smoothing max_training_data_points)
    (create_layout ())
  |> ignore;

  (* Step 5 - Listen to the afterplot events to avoid to debounce rendering
     https://plotly.com/javascript/plotlyjs-events/#afterplot-event
  *)
  listen_afterplot_event elt fire_render_cooled_down;

  (* Step 6 - Schedule `extendTraces` calls *)
  let on_start_render (_, max_sampling, smoothing) =
    Lwt_js_events.async (fun () ->
        (* let smoothing = React.S.value smoothing_signal in *)
        (* let max_training_data_points = React.S.value max_sampling_signal in *)
        let rev =
          Random.int ((2 lsl 29) - 1)
          |> string_of_int
        in
        (* let rev = Printf.sprintf "%d-%d-%d-%d" traini testi trainj testj in *)
        Js.Unsafe.global ##. Plotly##react
          elt
          (plotly_data_of_raw_data raw_data smoothing max_sampling)
          (create_layout ~rev ())
        |> ignore;
        Lwt.return ())
  in
  start_render_event |> React.E.map on_start_render |> ignore;

  ()
