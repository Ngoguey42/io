open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray.S
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end

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

let _1hot_of_top1 a =
  let numel =
    match Bigarray.Genarray.dims a with [| v |] -> v | _ -> failwith "_1hot_of_top1 bad input"
  in
  let b = Ndarray.zeros [| numel; 10 |] in
  for i = 0 to numel - 1 do
    Bigarray.Genarray.set b [| i; Bigarray.Genarray.get a [| i |] |] 1.0
  done;
  b

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

let rec tofloat v =
  match Algodiff.primal' v with
  | Algodiff.F _ -> Algodiff.unpack_flt v
  | v -> v |> Algodiff.Maths.mean |> tofloat

let toshp v =
  match Algodiff.primal' v with
  | Algodiff.F _ -> "()"
  | v ->
      v |> Algodiff.Arr.shape |> Array.to_list |> List.map string_of_int |> String.concat ", "
      |> Printf.sprintf "(%s)"

let categorical_crossentropy epsilon softmaxed_pred truth =
  (* a truth element must be 0. or 1. *)
  softmaxed_pred
  |> Algodiff.Maths.max2 (epsilon |> Algodiff.pack_flt)
  |> Algodiff.Maths.log |> Algodiff.Maths.mul truth |> Algodiff.Maths.neg
  |> Algodiff.Maths.sum ~axis:(-1) |> Algodiff.Maths.mean

let to_float32 a =
  let dims = Bigarray.Genarray.dims a in
  let numel = Array.fold_left ( * ) 1 dims in
  let a = Bigarray.reshape a [| numel |] |> Bigarray.array1_of_genarray in
  let b = Ndarray.zeros [| numel |] |> Bigarray.array1_of_genarray in
  for i = 0 to numel - 1 do
    b.{i} <- a.{i} |> float_of_int
  done;
  Bigarray.genarray_of_array1 b |> Fun.flip Bigarray.reshape dims

let train : Types.training_backend_routine =
 fun ?(verbose = true) ~yield_sleep_length ~fire_event ~instructions ~batch_count ~get_lr ~get_data
     ~encoders ~decoder ->
  let open Lwt.Infix in
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
    List.map Fnn_owl.unpack_for_training encoders |> Ft.List.split3
  in
  let optimizations = Fnn_owl.OptiMap.union_list_exn o in

  let forward_decoder, o, pack_decoder = Fnn_owl.unpack_for_training decoder in
  let optimizations = Fnn_owl.OptiMap.union_exn optimizations o in

  let train_on_batch batch_idx =
    let time = (new%js Js.date_now)##valueOf /. 1000. in
    let lr = get_lr batch_idx in
    let x, y = get_data batch_idx in
    let batch_size = (Bigarray.Genarray.dims x).(0) in

    let x =
      to_float32 x |> Algodiff.pack_arr |> Fun.flip Algodiff.Arr.reshape [| batch_size; 28; 28; 1 |]
    in
    let y_top1_uint8 = y |> Fun.flip Bigarray.reshape [| batch_size |] in
    let y_1hot = _1hot_of_top1 y_top1_uint8 |> Algodiff.pack_arr in

    let x = Fnn.Map.singleton node0 x in
    let y' =
      List.map (fun fw -> fw x) forward_encoders
      |> Array.of_list
      |> Algodiff.Maths.concatenate ~axis:3
    in
    let y' = Fnn.Map.singleton node0_decoder y' in
    let y'_1hot = forward_decoder y' in
    assert (y'_1hot |> Algodiff.primal' |> Algodiff.Arr.shape = [| batch_size; 10 |]);
    let loss = categorical_crossentropy 1e-10 y'_1hot y_1hot in
    assert (Algodiff.is_float loss);

    Algodiff.reverse_prop (Algodiff.pack_flt 1.) loss;
    Fnn_tfjs.OptiMap.iter (fun _name optimization -> optimization lr) optimizations;

    let y'_top1 = _top1_of_1hot (y'_1hot |> Algodiff.primal' |> Algodiff.unpack_arr) in
    let confusion_matrix = confusion_matrix (y_top1_uint8 |> to_float32) y'_top1 in

    let loss = loss |> Algodiff.primal' |> Algodiff.unpack_flt in
    ( if verbose then
      let iou, recall, _ = _stats_of_cm confusion_matrix in
      let time' = (new%js Js.date_now)##valueOf /. 1000. in
      Printf.printf "%5d, lr:%6.1e, l:%9.6f, iou:%5.1f%%, r:%5.1f%%, %.3fsec\n%!" batch_idx lr loss
        (iou *. 100.) (recall *. 100.) (time' -. time) );
    (batch_size, loss, confusion_matrix)
  in

  let loss_sum = ref 0. in
  let confusion_matrix_sum = ref (Ndarray.zeros [| 10; 10 |]) in
  let image_count = ref 0 in
  let fire_stop_event batch_count =
    let encoders, decoder = (List.map (fun f -> f ()) pack_encoders, pack_decoder ()) in
    let image_count = !image_count in
    let mean_iou_top1, mean_recall_top1, mean_precision_top1 = _stats_of_cm !confusion_matrix_sum in
    let stats =
      Types.
        {
          image_count;
          batch_count;
          mean_loss = !loss_sum;
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
        fire_event (`Batch_begin i);
        let batch_size, loss, confusion_matrix = train_on_batch i in
        loss_sum := !loss_sum +. loss;
        confusion_matrix_sum := Ndarray.add !confusion_matrix_sum confusion_matrix;
        image_count := !image_count + batch_size;
        let mean_iou_top1, mean_recall_top1, mean_precision_top1 = _stats_of_cm confusion_matrix in
        let batch_count = 1 in
        let stats =
          Types.
            {
              image_count = batch_size;
              batch_count;
              mean_loss = loss;
              mean_iou_top1;
              mean_recall_top1;
              mean_precision_top1;
            }
        in
        fire_event (`Batch_end (i, stats));
        if yield_sleep_length = 0. then Lwt_js.yield () >>= fun () -> aux (i + 1)
        else Lwt_js.sleep yield_sleep_length >>= fun () -> aux (i + 1)
  in

  let time0 = (new%js Js.date_now)##valueOf /. 1000. in
  aux 0 >>= fun _ ->
  let time1 = (new%js Js.date_now)##valueOf /. 1000. in
  Printf.printf "> Took %fsec\n%!" (time1 -. time0);

  Lwt.return ()
