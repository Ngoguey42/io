open struct
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray_generic
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Typed_array = Js_of_ocaml.Typed_array
  module Tfjs = Fnn_tfjs.Tfjs
end

type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

module Make_backend (Backend : sig
  val v : Tfjs.backend
end) =
struct
  (* TODO: Generic eval functions
   * let predict : ~verbose:bool -> ~progress:_ -> ~h:int -> ~w:int ->
   *          -> ~datagen:_ (defines batch_size and image count)
   *          -> ~encoders:network list -> ~decoder:network
   *          -> Ndarray.t Iter.t (image iterator)
   * let eval : ~verbose:bool -> ~progress:_ -> ~h:int -> ~w:int ->
   *          -> ~datagen:_ (defines batch_size and image count)
   *          -> ~encoders:network list -> ~decoder:network
   *          -> Ndarray.t (confusion_matrix)
   *)

  type uint8_ta = (int, [ `Uint8 ]) Typed_array.typedArray

  type float32_ta = (float, [ `Float32 ]) Typed_array.typedArray

  let _train verbose fire_event instructions batch_count get_lr get_data encoders decoder =
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
      List.map Fnn_tfjs.unpack_for_training encoders |> Ft.List.split3
    in
    let optimizations = Fnn_tfjs.OptiMap.union_list_exn o in

    let forward_decoder, o, pack_decoder = Fnn_tfjs.unpack_for_training decoder in
    let optimizations = Fnn_tfjs.OptiMap.union_exn optimizations o in

    let train_on_batch i =
      let time = (new%js Js.date_now)##valueOf /. 1000. in

      let lr = get_lr i in
      let x, y = get_data i in
      let batch_size = (Bigarray.Genarray.dims x).(0) in

      let x =
        Tfjs.tensor_of_ba x
        |> Tfjs.Ops.astype `Float32
        |> Tfjs.Ops.reshape [| batch_size; 28; 28; 1 |]
      in
      let y_top1 =
        Tfjs.tensor_of_ba y |> Tfjs.Ops.astype `Int32 |> Tfjs.Ops.reshape [| batch_size |]
      in
      let y_1hot =
        Tfjs.Ops.one_hot 10 y_top1
        |> Tfjs.Ops.astype `Float32
        |> Tfjs.Ops.reshape [| batch_size; 10 |]
      in

      ignore (verbose, x, y_1hot, lr, forward_decoder, forward_encoders, optimizations);
      let y'_1hot = ref y_1hot in

      let f () =
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
        (* let loss = Tfjs.hinge 1. y' (let open Tfjs.Ops in y_1hot * (Tfjs.float 2.) - (Tfjs.float 1.)) in *)
        assert (loss##.shape |> Js.to_array = [| 1; 1 |]);
        let loss = Tfjs.Ops.sum false loss in
        assert (loss##.shape |> Js.to_array = [||]);
        loss
      in
      let loss, grads = Tfjs.variable_grads f in
      let y'_1hot = !y'_1hot in
      ( match Fnn_tfjs.OptiMap.key_disjunction optimizations grads with
      | [], [] -> ()
      | name :: _, _ -> Printf.sprintf "Missing at least the <%s> gradient" name |> failwith
      | _, name :: _ -> Printf.sprintf "Missing at least the <%s> optimizer" name |> failwith );

      Fnn_tfjs.OptiMap.iter
        (fun name optimization -> optimization lr (Tfjs.Named_tensor_map.find name grads))
        optimizations;

      let y'_top1 = Tfjs.Ops.topk 1 y'_1hot |> snd |> Tfjs.Ops.reshape [| batch_size |] in
      let confusion_matrix = Tfjs.Ops.confusion_matrix 10 y_top1 y'_top1 in
      ignore instructions;
      if verbose then (
        let stats = Tfjs.iou_recall_precision_of_cm confusion_matrix in
        let ious = Tfjs.Ops.slice [ (1, 0, 1) ] stats in
        let recalls = Tfjs.Ops.slice [ (1, 1, 1) ] stats in
        let precisions = Tfjs.Ops.slice [ (1, 2, 1) ] stats in
        ignore (ious, recalls, precisions);

        let mean_recall = Tfjs.Ops.mean false recalls |> Tfjs.to_float in
        let mean_precision = Tfjs.Ops.mean false precisions |> Tfjs.to_float in
        let mean_iou = Tfjs.Ops.mean false ious |> Tfjs.to_float in
        ignore (mean_iou, mean_recall, mean_precision);

        let layer =
          match (Fnn.find_id (Some "classif") [ decoder ])#classify_layer with
          | `Conv2d node -> node#upstream0
          | `Tensordot node -> node#upstream1
          | _ -> failwith "unsupported `classif` layer"
        in
        let classif_grad =
          (* Classif's weights's gradient's l2norm sum *)
          let open Tfjs.Ops in
          let g = Tfjs.Named_tensor_map.find (Oo.id layer |> string_of_int) grads in
          g ** Tfjs.float 2. |> sum false |> sqrt
        in
        assert (classif_grad##.size = 1);
        let classif_grad = Tfjs.to_float classif_grad in

        let time' = (new%js Js.date_now)##valueOf /. 1000. in
        Printf.printf "%5d, lr:%6.1e, l:%9.6f, grad:%9.6f, iou:%5.1f%%, r:%5.1f%%, %.3fsec\n%!" i lr
          (Tfjs.to_float loss) classif_grad (mean_iou *. 100.) (mean_recall *. 100.) (time' -. time);

        Tfjs.dispose_tensor y'_1hot;
        () );
      (loss, confusion_matrix)
    in

    let loss_sum = Tfjs.Ops.zeros [||] |> Tfjs.variable ~trainable:false ~dtype:`Float32 in
    let confusion_matrix_sum =
      Tfjs.Ops.zeros [| 10; 10 |] |> Tfjs.variable ~trainable:false ~dtype:`Float32
    in
    let fire_stop_event batch_count =
      let encoders, decoder = (List.map (fun f -> f ()) pack_encoders, pack_decoder ()) in
      let loss = Tfjs.to_float loss_sum in
      let confusion_matrix = Tfjs.ba_of_tensor Bigarray.Int32 confusion_matrix_sum in
      Types.(
        fire_event (`End (encoders, decoder, { batch_count; loss; confusion_matrix })))
    in
    let rec aux i =
      match React.S.value instructions with
      | `Abort ->
          fire_event `Abort;
          Lwt.return ()
      | `Train_to_end when i = batch_count ->
          fire_stop_event i;
          Lwt.return ()
      | `Early_stop ->
          fire_stop_event i;
          Lwt.return ()
      | `Train_to_end ->
          fire_event (`Batch_begin i);
          Tfjs.tidy (fun () ->
              let loss, confusion_matrix = train_on_batch i in
              loss_sum##assign (Tfjs.Ops.add loss_sum loss);
              confusion_matrix_sum##assign (Tfjs.Ops.add confusion_matrix_sum confusion_matrix);
              Types.(
                let loss = Tfjs.to_float loss in
                let confusion_matrix = Tfjs.ba_of_tensor Bigarray.Int32 confusion_matrix in
                let batch_count = 1 in
                fire_event (`Batch_end (i, { batch_count; loss; confusion_matrix }))));
          Lwt_js.yield () >>= fun () -> aux (i + 1)
    in

    aux 0 >>= fun _ -> Lwt.return ()

  let train :
      ?verbose:bool ->
      fire_event:(Types.routine_event -> unit) ->
      instructions:Types.user_status React.signal ->
      batch_count:int ->
      get_lr:(int -> float) ->
      get_data:(int -> uint8_ba * uint8_ba) ->
      encoders:Fnn.network list ->
      decoder:Fnn.network ->
      unit Lwt.t =
   fun ?(verbose = true) ~fire_event ~instructions ~batch_count ~get_lr ~get_data ~encoders ~decoder ->
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        Tfjs.setup_backend Backend.v >>= fun _ ->
        Tfjs.tidy_lwt (fun () ->
            _train verbose fire_event instructions batch_count get_lr get_data encoders decoder)
        >>= fun res ->
        Tfjs.disposeVariables ();
        if verbose then Firebug.console##log (Tfjs.memory ());
        Lwt.return res)
      (fun exn ->
        let msg = Printexc.to_string exn in
        Printf.eprintf "Train fire error %s\n%!" msg;
        fire_event (`Crash exn);
        Lwt.return ())
end
