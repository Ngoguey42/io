open struct
  module Ndarray = Owl_base_dense_ndarray.S
  module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end

let shuffle rng x =
  (* Copied from owl-base, added rng *)
  let y = Array.copy x in
  let n = Array.length x in
  for i = n - 1 downto 1 do
    let j = i + 1 |> float_of_int |> ( *. ) (Random.State.float rng 1.) |> int_of_float in
    Owl_utils_array.swap y i j
  done;
  y

let to_float32 a =
  let dims = Bigarray.Genarray.dims a in
  let numel = Array.fold_left ( * ) 1 dims in
  let a = Bigarray.reshape a [| numel |] |> Bigarray.array1_of_genarray in
  let b = Ndarray.zeros [| numel |] |> Bigarray.array1_of_genarray in
  for i = 0 to numel - 1 do
    b.{i} <- a.{i} |> float_of_int
  done;
  Bigarray.genarray_of_array1 b |> Fun.flip Bigarray.reshape dims

let stats_of_cm cm =
  let stats_of_cat catidx =
    let true_pos = Ndarray.get cm [| catidx; catidx |] in
    let false_neg = Ndarray.get_slice [ [ catidx ]; [] ] cm |> Ndarray.sum in
    let false_pos = Ndarray.get_slice [ []; [ catidx ] ] cm |> Ndarray.sum in

    let false_neg = Ndarray.get false_neg [| 0 |] -. true_pos in
    let false_pos = Ndarray.get false_pos [| 0 |] -. true_pos in

    (* All those could be nan under some defined circumstances,
       but `iou` and `recall` cannot be nan for all `catidx`.
    *)
    let iou = true_pos /. (true_pos +. false_neg +. false_pos) in
    let recall = true_pos /. (true_pos +. false_neg) in
    let precision = true_pos /. (true_pos +. false_pos) in
    (iou, recall, precision)
  in
  let l = List.init 10 stats_of_cat in
  let ious = List.map (fun (v, _, _) -> v) l in
  let recalls = List.map (fun (_, v, _) -> v) l in
  let precisions = List.map (fun (_, _, v) -> v) l in

  let mean l =
    let sum, count =
      List.fold_left
        (fun (sum, count) v -> if Float.is_finite v then (sum +. v, count + 1) else (sum, count))
        (0., 0) l
    in
    sum /. float_of_int count
  in

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

let categorical_crossentropy epsilon softmaxed_pred truth =
  (* a truth element must be 0. or 1. *)
  softmaxed_pred
  |> Algodiff.Maths.max2 (epsilon |> Algodiff.pack_flt)
  |> Algodiff.Maths.log |> Algodiff.Maths.mul truth |> Algodiff.Maths.neg
  |> Algodiff.Maths.sum ~axis:(-1) |> Algodiff.Maths.mean
