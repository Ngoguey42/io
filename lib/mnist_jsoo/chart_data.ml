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

let round_base10_significants v b10_sig_digit =
  if v = 0. then v
  else
    let b10_sig_digit = float_of_int b10_sig_digit in
    let sign = if v > 0. then 1. else -1. in
    let v = v *. sign in
    let round_place = 10. ** (ceil (log10 v) -. b10_sig_digit) in
    round_place *. Float.round (v /. round_place) *. sign

let round_base10_significants_inplace a b10_sig_digit =
  for i = 0 to a##.length - 1 do
    let v = Js.Unsafe.get a i in
    Js.Unsafe.set a i (round_base10_significants v b10_sig_digit)
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
    | `S4 -> (* one epoch is the half-life *) 60000

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

   Always use smoothing 0 for learning rate

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
        (* Apply exp-decay. Need to exponentiate the momentum by the batch size. *)
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

  let subsample_indices_of_vector : 'a Js.js_array Js.t -> int -> int Js.js_array Js.t =
   fun xs max_subsample_count ->
    (* Create an array of indices in `xs` of len `<=max_subsample_count` such that the spacing
       between selected xs is mostly constant.
    *)
    let len = xs##.length in
    assert (len > max_subsample_count);
    let get : int -> int = Js.Unsafe.get xs in

    let find_closest_x_from_idx idx_root targetx =
      (* Search forward in `xs` for the value closest to `targetx` *)
      assert (idx_root < len);
      let rec aux ((_idx0, _x0, dist0) as prev_closest) idx1 =
        if idx1 = len then prev_closest
        else
          let x1 = get idx1 in
          let dist1 = float_of_int x1 -. targetx |> Float.abs in
          if dist0 <= dist1 then prev_closest else aux (idx1, x1, dist1) (idx1 + 1)
      in
      let x_root = get idx_root in
      let dist_root = float_of_int x_root -. targetx |> Float.abs in
      aux (idx_root, x_root, dist_root) (idx_root + 1)
    in

    let minx = get 0 in
    let maxx = get (len - 1) in
    let span = maxx - minx in
    let stride_float = float_of_int span /. (max_subsample_count - 1 |> float_of_int) in

    let idxs = new%js Js.array_empty in
    let push i = idxs##push i |> ignore in
    let rec aux idx ((i0, _x0) as prev_push) =
      (* `max_subsample_count` iterations *)
      if idx = len - 1 then ()
      else
        let targetx = (float_of_int idx *. stride_float) +. float_of_int minx in
        let i1, x1, _ = find_closest_x_from_idx i0 targetx in
        if i0 = i1 then
          (* Skip that index. This index was already included because the distance between
             consecutive `xs` is larger than `stride_float` near those indices.
          *)
          aux (idx + 1) prev_push
        else (
          push i1;
          aux (idx + 1) (i1, x1) )
    in
    push 0;
    aux 1 (0, minx);
    idxs

  let subsample : t -> Smoothing.t -> int -> Subsample.t =
   fun rd smoothing max_subsample_count ->
    let len = rd.train_xs##.length in
    let ss =
      if len <= max_subsample_count then
        (* Cloning arrays because some inplace transformations follow *)
        let clone a = a##slice 0 a##.length in
        let open Subsample in
        {
          train_xs = rd.train_xs;
          train_ious = M.find smoothing rd.train_ious |> clone;
          train_recalls = M.find smoothing rd.train_recalls |> clone;
          train_lrs = M.find `S0 rd.train_lrs |> clone;
          train_losses = M.find smoothing rd.train_losses |> clone;
        }
      else
        let idxs = subsample_indices_of_vector rd.train_xs max_subsample_count in

        let array_get a i = Js.array_get a i |> Js.Optdef.to_option |> Option.get in
        let map : type a. (int -> a) -> a Js.js_array Js.t =
         fun get ->
          let a = new%js Js.array_empty in
          for i = 0 to idxs##.length - 1 do
            a##push (i |> array_get idxs |> get) |> ignore
          done;
          a
        in

        let train_xs = map (array_get rd.train_xs) in
        let train_ious = map (array_get (M.find smoothing rd.train_ious)) in
        let train_recalls = map (array_get (M.find smoothing rd.train_recalls)) in
        let train_losses = map (array_get (M.find smoothing rd.train_losses)) in
        let train_lrs = map (array_get (M.find `S0 rd.train_lrs)) in

        Subsample.{ train_xs; train_ious; train_recalls; train_losses; train_lrs }
    in

    Smoothing.correct_ys_inplace smoothing ss.train_xs ss.train_ious;
    Smoothing.correct_ys_inplace smoothing ss.train_xs ss.train_recalls;
    Smoothing.correct_ys_inplace `S0 ss.train_xs ss.train_lrs;
    Smoothing.correct_ys_inplace smoothing ss.train_xs ss.train_losses;

    (* Kill noise in learning rate because it may show on plot if all values are equal *)
    round_base10_significants_inplace ss.train_lrs 5;
    ss
end

let plotly_data_of_raw_data _first_render raw_data subsampled_raw_data =
  let open Raw_data.Subsample in
  let train_iou =
    object%js
      val x = subsampled_raw_data.train_xs
      val y = subsampled_raw_data.train_ious
      val _type = Js.string "scatter"
      val mode = Js.string "markers"
      val name = Js.string "train iou"
      val visible = true (* Js.string "legendonly" *)
      val marker =
        object%js
          val color = Js.string "red"
          val size = 3
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
      val visible =  Js.string "legendonly"
      val marker =
        object%js
          val color = Js.string "red"
          val size = 3
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
      val visible = true (* Js.string "legendonly" *)
      val line =
        object%js
          val color = Js.string "orange"
        end
      val marker =
        object%js
          val color = Js.string "orange"
          val size = 3
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

  let open Raw_data in
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

let create_conf accum_max_sampling accum_smoothing =
  let less_points _ =
    accum_max_sampling (function
      | 8 ->
          Printf.eprintf "8\n%!";
          8
      | v ->
          Printf.eprintf "%d / 2\n%!" v;

          v / 2)
  in
  let more_points _ = accum_max_sampling (fun v -> if v * 2 > v then v * 2 else v) in
  let less_smoothing _ =
    accum_smoothing (function `S0 | `S1 -> `S0 | `S2 -> `S1 | `S3 -> `S2 | `S4 -> `S3)
  in
  let more_smoothing _ =
    accum_smoothing (function `S0 -> `S1 | `S1 -> `S2 | `S2 -> `S3 | `S3 | `S4 -> `S4)
  in

  let sampling_button0 =
    object%js
      val name = Js.string "Less training points"
      val click = Js.wrap_callback less_points
      val icon =
        object%js
          val width = 875
          val height = 1000
          val path = Js.string "m0 788l0-876 875 0 0 876-875 0z m688-500l-500 0 0 125 500 0 0-125z"
          val transform = Js.string "matrix(1 0 0 -1 0 850)"
        end
    end [@ocamlformat "disable"]
  in
  let sampling_button1 =
    object%js
      val name = Js.string "More training points"
      val click = Js.wrap_callback more_points
      val icon =
        object%js
          val width = 875
          val height = 1000
          val path = Js.string "m1 787l0-875 875 0 0 875-875 0z m687-500l-187 0 0-187-125 0 0 187-188 0 0 125 188 0 0 187 125 0 0-187 187 0 0-125z"
          val transform = Js.string "matrix(1 0 0 -1 0 850)"
        end
    end [@ocamlformat "disable"]
  in
  let smoothing_button0 =
    object%js
      val name = Js.string "Less smoothing"
      val click = Js.wrap_callback less_smoothing
      val icon =
        object%js
          val width = 875
          val height = 1000
          val path = Js.string "m0 788l0-876 875 0 0 876-875 0z m688-500l-500 0 0 125 500 0 0-125z"
          val transform = Js.string "matrix(1 0 0 -1 0 850)"
        end
    end [@ocamlformat "disable"]
  in
  let smoothing_button1 =
    object%js
      val name = Js.string "More smoothing"
      val click = Js.wrap_callback more_smoothing
      val icon =
        object%js
          val width = 875
          val height = 1000
          val path = Js.string "m1 787l0-875 875 0 0 875-875 0z m687-500l-187 0 0-187-125 0 0 187-188 0 0 125 188 0 0 187 125 0 0-187 187 0 0-125z"
          val transform = Js.string "matrix(1 0 0 -1 0 850)"
        end
    end [@ocamlformat "disable"]
  in

  object%js
    val modeBarButtonsToRemove = [|
        "select2d"; "lasso2d"; "zoomIn2d"; "zoomOut2d"; "autoScale2d";
        "hoverClosestCartesian"; "hoverCompareCartesian"; "toggleSpikelines";
      |] |> Array.map Js.string |> Js.array
  
    val displayModeBar = true
  
    val modeBarButtonsToAdd = [| sampling_button0; sampling_button1; smoothing_button0; smoothing_button1 |] |> Js.array
  end [@ocamlformat "disable"]

let create_layout _first_render raw_data subsampled_raw_data =
  ignore raw_data;
  let rev = Random.int ((2 lsl 29) - 1) |> string_of_int in

  let open Raw_data.Subsample in
  let epochs_shown =
    let a = subsampled_raw_data.train_xs in
    let len = a##.length in
    if len = 0 then 1
    else (Js.Unsafe.get a (len - 1) |> float_of_int) /. 60000. |> ceil |> int_of_float
  in
  let reduce_finite : _ Js.t -> (float -> float -> float) -> float -> float =
   fun a f acc ->
    let rec aux acc i =
      if i = a##.length then acc
      else
        let v = Js.Unsafe.get a i in
        if Float.is_finite v then aux (f acc v) (i + 1) else aux acc (i + 1)
    in
    aux acc 0
  in
  let y2_max =
    let a = subsampled_raw_data.train_losses in
    let count = reduce_finite a (fun acc _ -> acc +. 1.) 0. in
    let default = 20. in
    if count = 0. then default
    else
      let mean = reduce_finite a ( +. ) 0. /. count in
      let maxv = reduce_finite a max Float.neg_infinity in
      let std = reduce_finite a (fun acc v -> acc +. ((v -. mean) ** 2.)) 0. /. count |> sqrt in
      if mean = 0. then default
      else if std = 0. then mean *. 2.
      else mean +. (2. *. std) |> min maxv
  in
  let y3_max =
    let a = subsampled_raw_data.train_lrs in
    if a##.length = 0 then 1e-2
    else
      let maxv = reduce_finite a max Float.neg_infinity in
      let minv = reduce_finite a min Float.infinity in
      if maxv = minv then maxv *. 2. else maxv *. 1.05
  in

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
        val range = Js.array [| 0; epochs_shown * 60000 |]
        (* val autorange = true *)
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
        val range = Js.array [| 0.; y2_max |]
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
        val range = Js.array [| 0.; y3_max |]
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
