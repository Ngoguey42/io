open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray_generic
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Typed_array = Js_of_ocaml.Typed_array
  module Tfjs = Fnn_tfjs.Tfjs
end

let _stats_of_cm confusion_matrix =
  let stats = Tfjs.iou_recall_precision_of_cm confusion_matrix in
  let ious = Tfjs.Ops.slice [ (1, 0, 1) ] stats in
  let recalls = Tfjs.Ops.slice [ (1, 1, 1) ] stats in
  let precisions = Tfjs.Ops.slice [ (1, 2, 1) ] stats in

  let mean_iou = Tfjs.Ops.mean false ious |> Tfjs.to_float in
  let mean_recall = Tfjs.Ops.mean false recalls |> Tfjs.to_float in
  let mean_precision = Tfjs.Ops.mean false precisions |> Tfjs.to_float in

  (mean_iou, mean_recall, mean_precision)

let _eval verbose fire_event batch_size (eval_imgs, eval_labs) encoder decoder =
  let node0 = Fnn.inputs [ encoder ] |> List.hd |> Fnn.downcast in
  let node0_decoder = Fnn.inputs [ decoder ] |> List.hd |> Fnn.downcast in

  let forward_encoder = Fnn_tfjs.unpack_for_evaluation encoder in
  let forward_decoder = Fnn_tfjs.unpack_for_evaluation decoder in

  let batch_count = (10000 + batch_size - 1) / batch_size in
  let confusion_matrix_sum =
    Tfjs.Ops.zeros [| 10; 10 |] |> Tfjs.variable ~trainable:false ~dtype:`Float32
  in
  let test_set_sample_probas =
    Bigarray.Genarray.create Bigarray.Float32 Bigarray.c_layout [| 10; 10 |]
  in

  let train_on_batch batch_idx =
    let time = (new%js Js.date_now)##valueOf /. 1000. in

    let i0 = batch_idx * batch_size in
    let i1 = min 10000 (i0 + batch_size) in
    let x = Ndarray.get_slice [ [ i0; i1 - 1 ] ] eval_imgs in
    let y = Ndarray.get_slice [ [ i0; i1 - 1 ] ] eval_labs in
    let batch_size =
      let v = (Bigarray.Genarray.dims x).(0) in
      if batch_idx < batch_count - 1 then assert (v = batch_size)
      else assert (v = 10000 - (batch_size * (batch_count - 1)));
      v
    in

    let x =
      Tfjs.tensor_of_ba x
      |> Tfjs.Ops.astype `Float32
      |> Tfjs.Ops.reshape [| batch_size; 28; 28; 1 |]
    in
    let y_top1 =
      Tfjs.tensor_of_ba y |> Tfjs.Ops.astype `Int32 |> Tfjs.Ops.reshape [| batch_size |]
    in

    let x = Fnn.Map.singleton node0 x in
    let y' = forward_encoder x in
    let y' = Fnn.Map.singleton node0_decoder y' in
    let y' = forward_decoder y' in
    assert (y'##.shape |> Js.to_array = [| batch_size; 10 |]);
    let y'_1hot = y' in

    let y'_top1 = Tfjs.Ops.topk 1 y'_1hot |> snd |> Tfjs.Ops.reshape [| batch_size |] in
    let confusion_matrix = Tfjs.Ops.confusion_matrix 10 y_top1 y'_top1 in

    (* Side effects ********************************************************* *)
    ( if verbose then
      let iou, recall, _ = _stats_of_cm confusion_matrix in
      let time' = (new%js Js.date_now)##valueOf /. 1000. in
      Printf.printf "%5d, iou:%5.1f%%, r:%5.1f%%, %.3fsec\n%!" i0 (iou *. 100.) (recall *. 100.)
        (time' -. time) );
    confusion_matrix_sum##assign (Tfjs.Ops.add confusion_matrix_sum confusion_matrix);
    List.iter
      (fun (digit, idx) ->
        let idx = idx - i0 in
        if idx >= 0 && idx < batch_size then (
          let pred = Tfjs.Ops.slice [ (0, idx, 1) ] y'_1hot in
          let pred = pred |> Tfjs.ba_of_tensor Bigarray.Float32 in
          assert (Ndarray.shape pred = [| 1; 10 |]);
          let pred = Ndarray.squeeze ~axis:[| 0 |] pred in
          assert (Ndarray.shape pred = [| 10 |]);
          Ndarray.set_slice [ [ digit ]; [] ] test_set_sample_probas pred ))
      Constants.test_set_sample
  in

  let rec aux i =
    if i < batch_count then (
      fire_event (`Batch_begin i);
      Tfjs.tidy (fun () ->
          train_on_batch i;
          fire_event (`Batch_end i));
      aux (i + 1) )
  in

  let time0 = (new%js Js.date_now)##valueOf /. 1000. in
  aux 0;
  let time1 = (new%js Js.date_now)##valueOf /. 1000. in
  Printf.printf "> Took %fsec\n%!" (time1 -. time0);

  assert (Ndarray.sum' test_set_sample_probas > 9.5);
  assert (Ndarray.sum' test_set_sample_probas < 10.5);
  let mean_iou_top1, mean_recall_top1, mean_precision_top1 = _stats_of_cm confusion_matrix_sum in
  let stats =
    Types.{ test_set_sample_probas; mean_iou_top1; mean_recall_top1; mean_precision_top1 }
  in
  fire_event (`Outcome (`End stats));
  Lwt.return ()

module Make_backend (Backend : sig
  val v : Tfjs.backend
end) =
struct
  let eval : Types.evaluation_backend_routine =
   fun ?(verbose = false) ~fire_event ~batch_size ~db ~encoder ~decoder ->
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        Tfjs.setup_backend Backend.v >>= fun _ ->
        Tfjs.tidy_lwt (fun () -> _eval verbose fire_event batch_size db encoder decoder)
        >>= fun res ->
        Tfjs.disposeVariables ();
        if verbose then Firebug.console##log (Tfjs.memory ());
        Lwt.return res)
      (fun exn ->
        let msg = Printexc.to_string exn in
        Printf.eprintf "Train fire error %s\n%!" msg;
        fire_event (`Outcome (`Crash exn));
        Lwt.return ())
end
