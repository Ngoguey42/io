open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray.S
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Tfjs = Fnn_tfjs.Tfjs
end

let _train verbose yield_sleep_length fire_event instructions batch_count get_lr get_data encoders
    decoder =
  let open Lwt.Infix in
  (* Step 1 - Unpack the Fnn networks using the dedicated module ******************************** *)
  let node0 =
    let open Pshape.Size in
    Fnn.Builder.input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> Fnn.downcast
  in
  let encoders =
    List.map
      (fun net ->
        let input = Fnn.inputs [ net ] |> List.hd |> Fnn.downcast in
        Fnn.copy ~sub:[ (input, node0) ] [ net ] |> List.hd)
      encoders
  in
  let node0_decoder = Fnn.inputs [ decoder ] |> List.hd |> Fnn.downcast in
  let forward_encoders, o, pack_encoders =
    List.map Fnn_tfjs.unpack_for_training encoders |> Ft.List.split3
  in
  let optimizations = Fnn_tfjs.OptiMap.union_list_exn o in
  let forward_decoder, o, pack_decoder = Fnn_tfjs.unpack_for_training decoder in
  let optimizations = Fnn_tfjs.OptiMap.union_exn optimizations o in

  let train_on_batch batch_idx =
    (* Step 5 - Fetch and transform batch inputs ************************************************ *)
    let time = (new%js Js.date_now)##valueOf /. 1000. in
    let lr = get_lr batch_idx in
    let x, y = get_data batch_idx in
    let batch_size = (Bigarray.Genarray.dims x).(0) in
    let x =
      Tfjs.tensor_of_ba x
      |> Tfjs.Ops.astype `Float32
      |> Tfjs.Ops.reshape [| batch_size; 28; 28; 1 |]
    in
    let y_1hot =
      y |> Owl_snippets._1hot_of_top1 |> Tfjs.tensor_of_ba
      |> Tfjs.Ops.astype `Float32
      |> Tfjs.Ops.reshape [| batch_size; 10 |]
    in

    (* Step 6 - Forward and backward ************************************************************ *)
    (* Need to prolongate the life of `y'_1hot` to compute the stats at the end.
       ref is initialized with garbage *)
    let y'_1hot = ref y_1hot in
    let forward () =
      (* The tfjs models use `execute` both inside `predict` and `train` functions,
       * with a `training: bool` parameter. Using `predict` for training seems ok.
       *)
      let x = Fnn.Map.singleton node0 x in
      let y' = List.map (fun fw -> fw x) forward_encoders |> Tfjs.Ops.concat 3 in
      let y' = Fnn.Map.singleton node0_decoder y' in
      let y' = forward_decoder y' in
      assert (y'##.shape |> Js.to_array = [| batch_size; 10 |]);
      y'_1hot := Tfjs.keep y';
      let loss = Tfjs.categorical_crossentropy 1e-10 y' y_1hot in
      assert (loss##.shape |> Js.to_array = [| 1; 1 |]);
      let loss = Tfjs.Ops.sum false loss in
      assert (loss##.shape |> Js.to_array = [||]);
      loss
    in
    let loss, grads = Tfjs.variable_grads forward in
    let y'_1hot = !y'_1hot in

    (* Step 7 - Use gradients to update networks' weights *************************************** *)
    ( match Fnn_tfjs.OptiMap.key_disjunction optimizations grads with
    | [], [] -> ()
    | name :: _, _ -> Printf.sprintf "Missing at least the <%s> gradient" name |> failwith
    | _, name :: _ -> Printf.sprintf "Missing at least the <%s> optimizer" name |> failwith );
    Fnn_tfjs.OptiMap.iter
      (fun name optimization -> optimization lr (Tfjs.Named_tensor_map.find name grads))
      optimizations;

    (* Step 8 - Compute / print stats *********************************************************** *)
    let loss = Tfjs.to_float loss in
    let y'_top1 =
      Owl_snippets._top1_of_1hot
        (y'_1hot |> Tfjs.Ops.astype `Float32 |> Tfjs.ba_of_tensor Bigarray.Float32)
    in
    Tfjs.dispose_tensor y'_1hot;
    let confusion_matrix = Owl_snippets.confusion_matrix (y |> Owl_snippets.to_float32) y'_top1 in
    ( if verbose then
      let iou, recall, _ = Owl_snippets.stats_of_cm confusion_matrix in
      let time' = (new%js Js.date_now)##valueOf /. 1000. in
      Printf.printf "%5d, lr:%6.1e, l:%9.6f, iou:%5.1f%%, r:%5.1f%%, %.3fsec\n%!" batch_idx lr loss
        (iou *. 100.) (recall *. 100.) (time' -. time) );
    (batch_size, loss, confusion_matrix)
  in

  (* Step 2 - Prime the accumulated infos that needs to be returned at the end of training ****** *)
  let loss_sum = ref 0. in
  let confusion_matrix_sum = ref (Ndarray.zeros [| 10; 10 |]) in
  let image_count = ref 0 in

  (* Step 3 - Loop one batch at a time until end or user changes his mind *********************** *)
  let fire_stop_event batch_count =
    (* Step 11 - Transform accumulated stats and fire the final event *************************** *)
    let encoders, decoder = (List.map (fun f -> f ()) pack_encoders, pack_decoder ()) in
    let mean_iou_top1, mean_recall_top1, mean_precision_top1 =
      Owl_snippets.stats_of_cm !confusion_matrix_sum
    in
    let stats =
      Types.
        {
          image_count = !image_count;
          batch_count;
          mean_loss_per_image = !loss_sum /. float_of_int !image_count;
          mean_iou_top1;
          mean_recall_top1;
          mean_precision_top1;
        }
    in
    fire_event (`Outcome (`End (encoders, decoder, stats)))
  in
  let rec aux i =
    match React.S.value instructions with
    | `Abort ->
        fire_event (`Outcome `Abort);
        Lwt.return ()
    | `Train_to_end when i = batch_count ->
        fire_stop_event i;
        Lwt.return ()
    | `Early_stop ->
        fire_stop_event i;
        Lwt.return ()
    | `Train_to_end ->
        (* Step 4 - Start a new batch ************************************************************ *)
        fire_event (`Batch_begin i);
        Tfjs.tidy (fun () ->
            let batch_size, loss, confusion_matrix = train_on_batch i in
            (* Step 9 - Accumulate stats and fire intermediate event **************************** *)
            loss_sum := !loss_sum +. (loss *. float_of_int batch_size);
            confusion_matrix_sum := Ndarray.add !confusion_matrix_sum confusion_matrix;
            image_count := !image_count + batch_size;
            let mean_iou_top1, mean_recall_top1, mean_precision_top1 =
              Owl_snippets.stats_of_cm confusion_matrix
            in
            let stats =
              Types.
                {
                  image_count = batch_size;
                  batch_count = 1;
                  mean_loss_per_image = loss;
                  mean_iou_top1;
                  mean_recall_top1;
                  mean_precision_top1;
                }
            in
            fire_event (`Batch_end (i, stats)));
        (* Step 10 - Yield to give a chance to user events to reach us.
           Sleep to give a change to the DOM to refresh (if not web worker)
        *)
        if yield_sleep_length = 0. then Lwt_js.yield () >>= fun () -> aux (i + 1)
        else Lwt_js.sleep yield_sleep_length >>= fun () -> aux (i + 1)
  in
  let time0 = (new%js Js.date_now)##valueOf /. 1000. in
  aux 0 >>= fun _ ->
  let time1 = (new%js Js.date_now)##valueOf /. 1000. in
  Printf.printf "> Took %fsec\n%!" (time1 -. time0);
  Lwt.return ()

module Make_backend (Backend : sig
  val v : Tfjs.backend
end) =
struct
  let train : Types.training_backend_routine =
   fun ?(verbose = true) ~yield_sleep_length ~fire_event ~instructions ~batch_count ~get_lr
       ~get_data ~encoders ~decoder ->
    let open Lwt.Infix in
    Tfjs.setup_backend Backend.v >>= fun _ ->
    Tfjs.tidy_lwt (fun () ->
        _train verbose yield_sleep_length fire_event instructions batch_count get_lr get_data
          encoders decoder)
    >>= fun () ->
    Tfjs.disposeVariables ();
    if verbose then Firebug.console##log (Tfjs.memory ());
    Lwt.return ()
end
