open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray.S
  module Ndarray_g = Owl_base_dense_ndarray_generic
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end

let eval : Types.evaluation_backend_routine =
   fun ?(verbose = false) ~yield_sleep_length ~fire_event ~batch_size ~db:((eval_imgs, eval_labs)) ~encoder ~decoder ->
  let open Lwt.Infix in
  (* Step 1 - Unpack the Fnn networks using the dedicated module ******************************** *)
  let node0 = Fnn.inputs [ encoder ] |> List.hd |> Fnn.downcast in
  let node0_decoder = Fnn.inputs [ decoder ] |> List.hd |> Fnn.downcast in
  let forward_encoder = Fnn_owl.unpack_for_evaluation encoder in
  let forward_decoder = Fnn_owl.unpack_for_evaluation decoder in
  let batch_count = (Mnist.test_set_size + batch_size - 1) / batch_size in

  (* Step 2 - Prime the accumulated infos that needs to be returned at the end of training ****** *)
  (* This is the confusion matrix, will be used for iou/recall/precision *)
  let confusion_matrix_sum = ref (Ndarray.zeros [| 10; 10 |]) in
  (* Those are the 10 samples displayed in `results` *)
  let test_set_sample_probas =
    Bigarray.Genarray.create Bigarray.Float32 Bigarray.c_layout [| 10; 10 |]
  in

  let eval_on_batch batch_idx =
    (* Step 5 - Fetch and transform batch inputs ************************************************ *)
    let time = (new%js Js.date_now)##valueOf /. 1000. in
    let i0 = batch_idx * batch_size in
    let i1 = min Mnist.test_set_size (i0 + batch_size) in
    let x = Ndarray_g.get_slice [ [ i0; i1 - 1 ] ] eval_imgs in
    let y = Ndarray_g.get_slice [ [ i0; i1 - 1 ] ] eval_labs in
    let batch_size =
      let v = (Bigarray.Genarray.dims x).(0) in
      if batch_idx < batch_count - 1 then assert (v = batch_size)
      else assert (v = Mnist.test_set_size - (batch_size * (batch_count - 1)));
      v
    in
    let x = Owl_snippets.to_float32 x
            |> Algodiff.pack_arr
            |> Fun.flip Algodiff.Arr.reshape [|batch_size; 28; 28; 1|]
    in

    (* Step 6 - Forward ************************************************************************* *)
    let x = Fnn.Map.singleton node0 x in
    let y' = forward_encoder x in
    let y' = Fnn.Map.singleton node0_decoder y' in
    let y'_1hot = forward_decoder y' in
    assert (y'_1hot |> Algodiff.primal' |> Algodiff.Arr.shape = [| batch_size; 10 |]);

    (* Step 7 - Compute / print stats *********************************************************** *)
    let y'_top1 = Owl_snippets._top1_of_1hot (y'_1hot |> Algodiff.primal' |> Algodiff.unpack_arr) in
    let confusion_matrix = Owl_snippets.confusion_matrix (y |> Owl_snippets.to_float32) y'_top1 in
    ( if verbose then
      let iou, recall, _ = Owl_snippets.stats_of_cm confusion_matrix in
      let time' = (new%js Js.date_now)##valueOf /. 1000. in
      Printf.printf "%5d, iou:%5.1f%%, r:%5.1f%%, %.3fsec\n%!" i0 (iou *. 100.) (recall *. 100.)
        (time' -. time) );

    (* Step 8 - Accumulate stats and fire intermediate event ************************************ *)
    confusion_matrix_sum := Ndarray.add !confusion_matrix_sum confusion_matrix;
    List.iter
      (fun (digit, idx_in_db) ->
        let idx_in_batch = idx_in_db - i0 in
        if idx_in_batch >= 0 && idx_in_batch < batch_size then (
          let pred = Ndarray.get_slice [[idx_in_batch]; []] (Algodiff.unpack_arr y'_1hot) in
          assert (Ndarray_g.shape pred = [| 1; 10 |]);
          let pred = Ndarray_g.squeeze ~axis:[| 0 |] pred in
          assert (Ndarray_g.shape pred = [| 10 |]);
          Ndarray_g.set_slice [ [ digit ]; [] ] test_set_sample_probas pred ))
      Mnist.test_set_sample
  in

  (* Step 3 - Loop one batch at a time ********************************************************** *)
  let rec aux i =
    if i = batch_count then Lwt.return ()
    else (
      (* Step 4 - Start a new batch ************************************************************* *)
      fire_event (`Batch_begin i);
      eval_on_batch i;
      fire_event (`Batch_end i);
      if yield_sleep_length = 0. then Lwt_js.yield () >>= fun () -> aux (i + 1)
      else Lwt_js.sleep yield_sleep_length >>= fun () -> aux (i + 1) )
  in
  let time0 = (new%js Js.date_now)##valueOf /. 1000. in
  aux 0 >>= fun () ->
  let time1 = (new%js Js.date_now)##valueOf /. 1000. in

  (* Step 11 - Transform accumulated stats and fire the final event ***************************** *)
  assert (Ndarray_g.sum' test_set_sample_probas > 9.5);
  assert (Ndarray_g.sum' test_set_sample_probas < 10.5);
  let mean_iou_top1, mean_recall_top1, mean_precision_top1 =
    Owl_snippets.stats_of_cm !confusion_matrix_sum
  in
  Printf.printf "> Took %fsec - iou:%7.3f%%, r:%7.3f%%, p:%7.3f%%\n%!" (time1 -. time0)
    (mean_iou_top1 *. 100.) (mean_recall_top1 *. 100.) (mean_precision_top1 *. 100.);
  let stats =
    Types.{ test_set_sample_probas; mean_iou_top1; mean_recall_top1; mean_precision_top1 }
  in
  fire_event (`Outcome (`End stats));
  Lwt.return ()
