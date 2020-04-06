module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array

module Make_backend (Backend : sig
  val v : Tfjs_api.backend
end) = struct
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

  let _train verbose progress batch_count get_lr get_data encoders decoder =
    let open Lwt.Infix in

    let node0 =
      let open Pshape.Size in
      Fnn.Builder.input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
      |> Fnn.downcast
    in
    let encoders =
      List.map
        (fun net ->
          let input = Fnn.inputs [net] |> List.hd |> Fnn.downcast in
          Fnn.copy ~sub:[input, node0] [net] |> List.hd)
        encoders
    in
    let node0_decoder = Fnn.inputs [decoder] |> List.hd |> Fnn.downcast in

    let forward_encoders, o, pack_encoders = List.map Nn_tfjs.unpack encoders |> Ft.List.split3 in
    let optimizations = Nn_tfjs.OptiMap.union_list_exn o in

    let forward_decoder, o, pack_decoder = Nn_tfjs.unpack decoder in
    let optimizations = Nn_tfjs.OptiMap.union_exn optimizations o in

    let train_on_batch i =
      let time = (new%js Js.date_now)##valueOf /. 1000. in

      let lr = get_lr i in
      let x, y = get_data i in
      let batch_size = y##.length in

      let x = Tfjs_api.tensor_of_ta [| batch_size; 28; 28; 1 |] x in
      let x = x##asType (Js.string "float32") in
      let y = Tfjs_api.one_hot_of_ta 10 y in
      let y = y##reshape (Js.array [| batch_size; 1; 1; 10 |]) in
      let y = y##asType (Js.string "float32") in

      ignore (verbose, x, y, lr, forward_decoder, forward_encoders, optimizations);

      let f () =
        (* The tfjs models use `execute` both inside `predict` and `train` functions,
         * with a `training: bool` parameter. Using `predict` for training seems ok.
         *)
        let x = Fnn.Map.singleton node0 x in
        let y' =
          List.map (fun fw -> fw x) forward_encoders
          |> Tfjs_api.Ops.concat 3
        in
        let y' = Fnn.Map.singleton node0_decoder y' in
        let y' = forward_decoder y' in
        let loss = Tfjs_api.categorical_crossentropy 1e-10 y' y in
        let loss = Tfjs_api.Ops.sum false loss in
        loss
      in
      let loss, grads = Tfjs_api.variable_grads f in
      ( match Nn_tfjs.OptiMap.key_disjunction optimizations grads with
      | [], [] -> ()
      | name :: _, _ -> Printf.sprintf "Missing at least the <%s> gradient" name |> failwith
      | _, name :: _ -> Printf.sprintf "Missing at least the <%s> optimizer" name |> failwith );

      Nn_tfjs.OptiMap.iter
        (fun name optimization -> optimization lr (Tfjs_api.Named_tensor_map.find name grads))
        optimizations;

      progress i;
      if verbose then
        let time' = (new%js Js.date_now)##valueOf /. 1000. in
        Printf.printf "Step %5d done, lr:%6.1e, loss:%9.6f, took:%.3fsec\n%!" i lr
          (Bigarray.Genarray.get (Tfjs_api.ba_of_tensor_float loss) [||])
          (time' -. time);
      ()
    in

    let rec aux i =
      if i >= batch_count then Lwt.return ()
      else (
        Tfjs_api.tidy (fun () -> train_on_batch i);
        Lwt_js.sleep 0.25 >>= fun () -> aux (i + 1) )
    in

    aux 0 >>= fun _ -> Lwt.return (List.map (fun f -> f ()) pack_encoders, pack_decoder ())

  let train :
      ?verbose:bool ->
      ?progress:(int -> unit) ->
      batch_count:int ->
      get_lr:(int -> float) ->
      get_data:(int -> float32_ta Js.t * uint8_ta Js.t) ->
      encoders:Fnn.network list ->
      decoder:Fnn.network ->
      (Fnn.network list * Fnn.network) Lwt.t =
   fun ?(verbose = true) ?(progress = fun _ -> ()) ~batch_count ~get_lr ~get_data ~encoders ~decoder ->
    let open Lwt.Infix in
    Tfjs_api.setup_backend Backend.v >>= fun _ ->
    let f () = _train verbose progress batch_count get_lr get_data encoders decoder in
    Tfjs_api.tidy_lwt f >>= fun res ->
    Tfjs_api.disposeVariables ();
    if verbose then Firebug.console##log (Tfjs_api.memory ());
    Lwt.return res
end
