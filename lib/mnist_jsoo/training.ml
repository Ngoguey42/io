open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

open Types

let repair_string : string -> string = fun s -> String.sub s 0 (String.length s)

let repair_bigarray : 'a -> 'a =
 fun a ->
  let f : _ -> _ -> _ -> _ -> 'a = Ft_js.caml_ba_create_unsafe in
  f
    (Js.Unsafe.get a (Js.string "kind"))
    (Js.Unsafe.get a (Js.string "layout"))
    (Js.Unsafe.get a (Js.string "dims"))
    (Js.Unsafe.get a (Js.string "data"))

let repair_storable_nn : 'a -> 'a =
 fun snn ->
  let layer_ids, graph, layers = snn in
  List.iter
    (fun i ->
      match Hashtbl.find layers i with
      | `Parameter32 (id, dimensions, init, o, tensor_opt, optim_opt) ->
          let tensor_opt =
            match tensor_opt with None -> None | Some v -> Some (repair_bigarray v)
          in
          let optim_opt =
            match optim_opt with
            | Some (`Adam (a, b, c, d, t0, t1)) ->
                Some (`Adam (a, b, c, d, repair_bigarray t0, repair_bigarray t1))
            | v -> v
          in
          Hashtbl.add layers i (`Parameter32 (id, dimensions, init, o, tensor_opt, optim_opt))
      | _ -> ())
    layer_ids;

  (layer_ids, graph, layers)

let _routine { db = train_imgs, train_labs; networks = encoders, decoder; config } fire_event
    instructions =
  let module Backend = (val Backend.create config.backend) in
  let verbose = config.verbose in

  let batch_count, batch_size = (config.batch_count, config.batch_size) in
  let get_lr =
    match config.lr with
    | `Down (lr0, lr1) ->
        (* If lr1=0, will decrease the lr on the range [lr0; lr1[
           otherwise, will decrease the lr on the range [lr0; lr1]
        *)
        assert (lr0 >= lr1);
        assert (lr1 >= 0.);
        let big_vec = lr1 -. lr0 in
        let small_vec =
          if batch_count <= 1 then big_vec
          else if lr1 = 0. then big_vec /. float_of_int batch_count
          else big_vec /. float_of_int (batch_count - 1)
        in
        fun batch_idx -> lr0 +. (small_vec *. float_of_int batch_idx)
    | `Flat lr -> fun _ -> lr
  in

  let shuffled_indices = Hashtbl.create 2 in
  let get_shuffled_sample_indices_of_epoch epoch_idx =
    match Hashtbl.find_opt shuffled_indices epoch_idx with
    | Some idxs -> idxs
    | None ->
        let rng = Random.State.make [| config.seed + epoch_idx |] in
        let idxs = Array.init Mnist.train_set_size Fun.id |> Owl_snippets.shuffle rng in
        Hashtbl.add shuffled_indices epoch_idx idxs;
        idxs
  in
  let get_data batch_idx =
    (* Images and labels of a batch are determined by config.seed, config.images_seen,
       config.batch_size and batch_idx.
       That way, given a model seed, the images are always seen in the same order no matter
       the batch size/batch count/number of trainings/early stops.
    *)
    let imgs = Ndarray.empty Bigarray.Int8_unsigned [| batch_size; 28; 28 |] in
    let labs = Ndarray.empty Bigarray.Int8_unsigned [| batch_size |] in
    for batch_sample_idx = 0 to batch_size - 1 do
      let global_sample_idx = config.images_seen + (batch_idx * batch_size) + batch_sample_idx in
      let epoch_idx = global_sample_idx / Mnist.train_set_size in
      let sample_idx_in_epoch = global_sample_idx mod Mnist.train_set_size in
      let train_db_idx = (get_shuffled_sample_indices_of_epoch epoch_idx).(sample_idx_in_epoch) in
      let img = Bigarray.Genarray.sub_left train_imgs train_db_idx 1 in
      let lab = Bigarray.Genarray.sub_left train_labs train_db_idx 1 in
      Ndarray.set_slice [ [ batch_sample_idx ]; []; [] ] imgs img;
      Ndarray.set_slice [ [ batch_sample_idx ] ] labs lab
    done;
    (imgs, labs)
  in
  let yield_sleep_length = if Ft_js.Webworker.is_web_worker then 0. else 0.01 in
  Backend.train ~fire_event ~instructions ~verbose ~yield_sleep_length ~batch_count ~get_lr
    ~get_data ~encoders ~decoder

let routine params fire_event instructions =
  let on_error exn =
    Printf.eprintf "Error inside routine %s!\n%!" (Printexc.to_string exn);
    fire_event (`Outcome (`Crash (Printexc.to_string exn)));
    Lwt.return ()
  in
  Lwt.catch (fun () -> _routine params fire_event instructions) on_error

module Webworker_routine = struct
  type parameters = {
    db : db_train;
    networks : Fnn.storable_nn list * Fnn.storable_nn;
    config : training_config;
  }

  type _in_msg = [ `Prime of parameters | training_user_status ]

  type outcome =
    [ `End of Fnn.storable_nn list * Fnn.storable_nn * training_stats | `Abort | `Crash of string ]

  type routine_event =
    [ `Init | `Batch_begin of int | `Batch_end of int * training_stats | `Outcome of outcome ]

  type _out_msg = routine_event

  let preprocess_in_msg : _ -> _in_msg = function
    | `Prime Types.{ db; networks = encoders, decoder; config } ->
        let f = Fnn.storable_of_fnn in
        let networks = (List.map f encoders, f decoder) in
        `Prime { networks; db; config }
    | #training_user_status as msg -> msg

  let postprocess_in_msg : _in_msg -> _ = function
    | `Prime { db = imgs, labs; networks = encoders, decoder; config } ->
        let f nn =
          nn |> repair_storable_nn |> Fnn.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER)
        in
        let networks = (List.map f encoders, f decoder) in
        let imgs = repair_bigarray imgs in
        let labs = repair_bigarray labs in
        `Prime Types.{ networks; db = (imgs, labs); config }
    | #training_user_status as msg -> msg

  let preprocess_out_msg : Types.training_routine_event -> routine_event = function
    | `Init as ev -> (ev :> routine_event)
    | `Batch_begin _ as ev -> (ev :> routine_event)
    | `Batch_end _ as ev -> (ev :> routine_event)
    | (`Outcome (`Crash _) as ev) | (`Outcome `Abort as ev) -> (ev :> routine_event)
    | `Outcome (`End (encoders, decoder, stats)) ->
        let f = Fnn.storable_of_fnn in
        `Outcome (`End (List.map f encoders, f decoder, stats))

  let postprocess_out_msg : _out_msg -> Types.training_routine_event = function
    | `Init as ev -> (ev :> Types.training_routine_event)
    | `Batch_begin _ as ev -> (ev :> Types.training_routine_event)
    | `Batch_end _ as ev -> (ev :> Types.training_routine_event)
    | `Outcome (`Crash msg) -> `Outcome (`Crash (repair_string msg))
    | `Outcome `Abort as ev -> (ev :> Types.training_routine_event)
    | `Outcome (`End (encoders, decoder, stats)) ->
        let f nn =
          nn |> repair_storable_nn |> Fnn.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER)
        in
        `Outcome (`End (List.map f encoders, f decoder, stats))

  module rec Ww : (Webworker.S with type in_msg = _in_msg and type out_msg = _out_msg) =
  Webworker.Make (struct
    type in_msg = _in_msg

    type out_msg = _out_msg

    let create_on_in_message () =
      let routine_events, fire_routine_event =
        React.E.create ()
        (* collected when WW is stopped *)
      in
      let user_status, set_user_status =
        React.S.create `Train_to_end
        (* collected when WW is stopped *)
      in

      routine_events |> React.E.map preprocess_out_msg |> React.E.map Ww.post_out_message |> ignore;

      let on_in_message msg =
        match postprocess_in_msg msg with
        | `Prime params ->
            Ft_js.Scripts.urls_of_entry ~what:`Js `Tfjs
            |> List.iter Js_of_ocaml.Worker.import_scripts;
            let routine () = routine params fire_routine_event user_status in
            Js_of_ocaml_lwt.Lwt_js_events.async routine
        | #training_user_status as msg -> set_user_status msg
      in
      on_in_message
  end)

  include Ww
end

let routine (params : training_parameters) fire_upstream_event user_status =
  if not params.config.from_webworker then routine params fire_upstream_event user_status
  else
    (* 1. Create worker
       2. Setup events and signals processing
       3. Setup user_state in worker
       4. Prime worker
    *)
    let user_status = React.S.map Fun.id user_status in
    let terminate ww =
      Webworker_routine.terminate ww;
      React.S.stop ~strong:true user_status
    in

    let on_out_msg ww ev =
      Printf.eprintf "> Training-ww (from main) - on_out_msg\n%!";
      match React.S.value user_status with
      | `Abort ->
          Firebug.console##warn (Js.string "Filtering out a message comming from training routine")
      | _ ->
          let ev = Webworker_routine.postprocess_out_msg ev in
          (match ev with `Init | `Batch_begin _ | `Batch_end _ -> () | `Outcome _ -> terminate ww);
          fire_upstream_event ev
    in
    let on_out_error_msg ev =
      Printf.eprintf "> Training-ww (from main) - on_out_error_msg\n%!";
      let msg =
        match
          ( (Js.Unsafe.coerce ev)##.msg |> Js.Optdef.to_option,
            (Js.Unsafe.coerce ev)##.message |> Js.Optdef.to_option )
        with
        | None, None -> "unknown"
        | Some s, _ -> Js.to_string s
        | _, Some s -> Js.to_string s
      in
      match React.S.value user_status with
      | `Abort ->
          let msg =
            Printf.sprintf "Filtering out an error comming from training routine (%s)" msg
          in
          Firebug.console##warn (Js.string msg)
      | _ -> `Outcome (`Crash msg) |> fire_upstream_event
    in
    let on_new_user_status ww s =
      Printf.eprintf "> Training-ww (from main) - on_new_user_status\n%!";
      match s with
      | `Abort ->
          Printf.eprintf "|||| Firing `Abort upstream\n%!";
          fire_upstream_event (`Outcome `Abort);
          Printf.eprintf "|||| Terminating ww\n%!";
          terminate ww
      | s -> Webworker_routine.post_in_message ww s
    in

    let ww = Webworker_routine.create on_out_msg on_out_error_msg in

    user_status |> React.S.changes
    |> React.E.map Webworker_routine.preprocess_in_msg
    |> React.E.map (on_new_user_status ww)
    |> ignore;

    user_status |> React.S.value |> Webworker_routine.preprocess_in_msg
    |> Webworker_routine.post_in_message ww;

    `Prime params |> Webworker_routine.preprocess_in_msg |> Webworker_routine.post_in_message ww;

    Lwt.return ()
