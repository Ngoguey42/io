open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray.S
  module Ndarray_g = Owl_base_dense_ndarray_generic
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Typed_array = Js_of_ocaml.Typed_array
  module Tfjs = Fnn_tfjs.Tfjs
end

let to_float32 a =
  let dims = Bigarray.Genarray.dims a in
  let numel = Array.fold_left ( * ) 1 dims in
  let a = Bigarray.reshape a [| numel |] |> Bigarray.array1_of_genarray in
  let b = Ndarray.zeros [| numel |] |> Bigarray.array1_of_genarray in
  for i = 0 to numel - 1 do
    b.{i} <- a.{i} |> float_of_int
  done;
  Bigarray.genarray_of_array1 b |> Fun.flip Bigarray.reshape dims

let _stats_of_cm cm =
  let stats_of_cat catidx =
    let true_pos = Ndarray.get cm [| catidx; catidx |] in
    let false_neg = Ndarray.get_slice [ [ catidx ]; [] ] cm |> Ndarray.sum in
    let false_pos = Ndarray.get_slice [ []; [ catidx ] ] cm |> Ndarray.sum in

    let false_neg = Ndarray.get false_neg [| 0 |] -. true_pos in
    let false_pos = Ndarray.get false_pos [| 0 |] -. true_pos in

    let iou = true_pos /. (true_pos +. false_neg +. false_pos) in
    let recall = true_pos /. (true_pos +. false_neg) in
    let precision = true_pos /. (true_pos +. false_pos) in
    (iou, recall, precision)
  in
  let l = List.init 10 stats_of_cat in
  let ious = List.map (fun (v, _, _) -> v) l in
  let recalls = List.map (fun (_, v, _) -> v) l in
  let precisions = List.map (fun (_, _, v) -> v) l in
  let mean l = List.fold_left ( +. ) 0. l /. 10. in
  (mean ious, mean recalls, mean precisions)

let _top1_of_1hot a =
  let numel =
    match Bigarray.Genarray.dims a with [| v; 10 |] -> v | _ -> failwith "_top1_of_1hot bad input"
  in
  let b = Ndarray.zeros [| numel |] in
  for i = 0 to numel - 1 do
    let row =
      List.combine (List.init 10 (fun j -> Ndarray.get a [| i; j |])) (List.init 10 Fun.id)
      |> List.sort (Fun.flip compare)
    in
    let _, nth = List.hd row in
    Bigarray.Genarray.set b [| i |] (float_of_int nth)
  done;
  b

let confusion_matrix truth_top1 pred_top1 =
  let numel =
    match Bigarray.Genarray.dims truth_top1 with
    | [| v |] -> v
    | _ -> failwith "confusion_matrix bad input"
  in
  let cm = Ndarray.zeros [| 10; 10 |] in
  for i = 0 to numel - 1 do
    let y = Ndarray.get truth_top1 [| i |] in
    let y' = Ndarray.get pred_top1 [| i |] in
    let y = int_of_float y in
    let y' = int_of_float y' in
    Bigarray.Genarray.set cm [| y; y' |] (Bigarray.Genarray.get cm [| y; y' |] +. 1.0)
  done;
  cm

let _eval verbose yield_sleep_length fire_event batch_size (eval_imgs, eval_labs) encoder decoder =
  let open Lwt.Infix in
  let node0 = Fnn.inputs [ encoder ] |> List.hd |> Fnn.downcast in
  let node0_decoder = Fnn.inputs [ decoder ] |> List.hd |> Fnn.downcast in

  let forward_encoder = Fnn_tfjs.unpack_for_evaluation encoder in
  let forward_decoder = Fnn_tfjs.unpack_for_evaluation decoder in

  let batch_count = (Mnist.test_set_size + batch_size - 1) / batch_size in
  let confusion_matrix_sum = ref (Ndarray.zeros [| 10; 10 |]) in
  let test_set_sample_probas =
    Bigarray.Genarray.create Bigarray.Float32 Bigarray.c_layout [| 10; 10 |]
  in

  let train_on_batch batch_idx =
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

    let x =
      Tfjs.tensor_of_ba x
      |> Tfjs.Ops.astype `Float32
      |> Tfjs.Ops.reshape [| batch_size; 28; 28; 1 |]
    in

    let x = Fnn.Map.singleton node0 x in
    let y' = forward_encoder x in
    let y' = Fnn.Map.singleton node0_decoder y' in
    let y' = forward_decoder y' in
    assert (y'##.shape |> Js.to_array = [| batch_size; 10 |]);
    let y'_1hot = y' in

    let y'_top1 =
      _top1_of_1hot (y'_1hot |> Tfjs.Ops.astype `Float32 |> Tfjs.ba_of_tensor Bigarray.Float32)
    in
    let confusion_matrix = confusion_matrix (y |> to_float32) y'_top1 in

    (* Side effects ********************************************************* *)
    ( if verbose then
      let iou, recall, _ = _stats_of_cm confusion_matrix in
      let time' = (new%js Js.date_now)##valueOf /. 1000. in
      Printf.printf "%5d, iou:%5.1f%%, r:%5.1f%%, %.3fsec\n%!" i0 (iou *. 100.) (recall *. 100.)
        (time' -. time) );
    confusion_matrix_sum := Ndarray.add !confusion_matrix_sum confusion_matrix;
    List.iter
      (fun (digit, idx) ->
        let idx = idx - i0 in
        if idx >= 0 && idx < batch_size then (
          let pred = Tfjs.Ops.slice [ (0, idx, 1) ] y'_1hot in
          let pred = pred |> Tfjs.ba_of_tensor Bigarray.Float32 in
          assert (Ndarray_g.shape pred = [| 1; 10 |]);
          let pred = Ndarray_g.squeeze ~axis:[| 0 |] pred in
          assert (Ndarray_g.shape pred = [| 10 |]);
          Ndarray_g.set_slice [ [ digit ]; [] ] test_set_sample_probas pred ))
      Mnist.test_set_sample
  in

  let rec aux i =
    if i = batch_count then Lwt.return ()
    else (
      fire_event (`Batch_begin i);
      Tfjs.tidy (fun () ->
          train_on_batch i;
          fire_event (`Batch_end i));

      if yield_sleep_length = 0. then Lwt_js.yield () >>= fun () -> aux (i + 1)
      else Lwt_js.sleep yield_sleep_length >>= fun () -> aux (i + 1) )
  in

  let time0 = (new%js Js.date_now)##valueOf /. 1000. in
  aux 0 >>= fun () ->
  let time1 = (new%js Js.date_now)##valueOf /. 1000. in

  assert (Ndarray_g.sum' test_set_sample_probas > 9.5);
  assert (Ndarray_g.sum' test_set_sample_probas < 10.5);
  let mean_iou_top1, mean_recall_top1, mean_precision_top1 = _stats_of_cm !confusion_matrix_sum in
  Printf.printf "> Took %fsec - iou:%7.3f%%, r:%7.3f%%, p:%7.3f%%\n%!" (time1 -. time0)
    (mean_iou_top1 *. 100.) (mean_recall_top1 *. 100.) (mean_precision_top1 *. 100.);
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
   fun ?(verbose = false) ~yield_sleep_length ~fire_event ~batch_size ~db ~encoder ~decoder ->
    let open Lwt.Infix in
    Tfjs.setup_backend Backend.v >>= fun _ ->
    Tfjs.tidy_lwt (fun () ->
        _eval verbose yield_sleep_length fire_event batch_size db encoder decoder)
    >>= fun () ->
    Tfjs.disposeVariables ();
    if verbose then Firebug.console##log (Tfjs.memory ());
    Lwt.return ()
end
