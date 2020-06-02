open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

open Types

let shuffle rng x =
  (* Copied from owl-base, added rng *)
  let y = Array.copy x in
  let n = Array.length x in
  for i = n - 1 downto 1 do
    let j = i + 1 |> float_of_int |> ( *. ) (Random.State.float rng 1.) |> int_of_float in
    Owl_utils_array.swap y i j
  done;
  y

let repair_bigarray : 'a -> 'a =
 fun a ->
  let f : _ -> _ -> _ -> _ -> 'a = Ft_js.caml_ba_create_unsafe in
  f
    (Js.Unsafe.get a (Js.string "kind"))
    (Js.Unsafe.get a (Js.string "layout"))
    (Js.Unsafe.get a (Js.string "dims"))
    (Js.Unsafe.get a (Js.string "data"))

let repair_string : string -> string = fun s -> String.sub s 0 (String.length s)

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
        let idxs = Array.init Mnist.train_set_size Fun.id |> shuffle rng in
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
    | `Prime { db; networks = encoders, decoder; config } ->
        let f = Fnn.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER) in
        let networks = (List.map f encoders, f decoder) in
        `Prime Types.{ networks; db; config }
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
        let f = Fnn.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER) in
        `Outcome (`End (List.map f encoders, f decoder, stats))

  module rec Ww : (Webworker.S with type in_msg = _in_msg and type out_msg = _out_msg) =
  Webworker.Make (struct
    type in_msg = _in_msg

    type out_msg = _out_msg

    let create_on_in_message () =
      let routine_events, fire_routine_event = React.E.create () in
      let user_status, set_user_status = React.S.create `Train_to_end in

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

let construct_instructions (_, _, set_user_status) =
  Printf.printf "> Component - training control instructions | construct\n%!";
  let render (user_status, routine_status, _) =
    Printf.printf "> Component - training control instructions | render\n%!";
    let open Reactjs.Jsx in
    let button txt t =
      match (t, routine_status) with
      | `On event, `Allocating | `On event, `Running ->
          let on_click ev =
            set_user_status event;
            ev##preventDefault
          in
          of_bootstrap "Button" ~style:[ ("width", "95px") ] ~size:"sm" ~variant:"primary" ~on_click
            [ of_string txt ]
      | `On _, _ ->
          of_bootstrap "Button" ~as_:"div"
            ~style:[ ("width", "95px"); ("pointerEvents", "none") ]
            ~size:"sm" ~variant:"dark" [ of_string txt ]
      | `Target, _ ->
          of_bootstrap "Button" ~as_:"div"
            ~style:[ ("width", "95px"); ("pointerEvents", "none") ]
            ~size:"sm" ~classes:[ "active"; "focus" ] ~variant:"success" [ of_string txt ]
    in
    match user_status with
    | `Train_to_end ->
        [
          button "Train to end" `Target;
          of_string " | ";
          button "Early stop" (`On `Early_stop);
          of_string " | ";
          button "Abort" (`On `Abort);
        ]
        |> of_tag "div"
    | `Early_stop ->
        [
          button "Train to end" (`On `Train_to_end);
          of_string " | ";
          button "Early stop" `Target;
          of_string " | ";
          button "Abort" (`On `Abort);
        ]
        |> of_tag "div"
    | `Abort ->
        [
          button "Train to end" (`On `Train_to_end);
          of_string " | ";
          button "Early stop" (`On `Early_stop);
          of_string " | ";
          button "Abort" `Target;
        ]
        |> of_tag "div"
  in
  Reactjs.construct render

type props = training_parameters * (training_routine_event -> unit)

let construct_training (props : props) =
  Printf.printf "> Component - training control | construct\n%!";
  let params, fire_upstream_event = props in

  let routine_events, fire_routine_event = React.E.create () in
  let string_of_batch_idx i =
    (i + 1 |> float_of_int) /. (params.config.batch_count |> float_of_int) *. 100.
    |> Printf.sprintf "%.0f%%"
  in
  let routine_progress =
    React.S.fold
      (fun s a -> match a with `Batch_end (i, _) -> string_of_batch_idx i | _ -> s)
      "0%" routine_events
  in
  let routine_status =
    let reduce s ev =
      match (s, ev) with
      | `Allocating, `Batch_end _ -> `Running
      | `Running, `Outcome (`End _) -> `Ended
      | `Running, `Outcome `Abort -> `Aborted
      | `Running, `Outcome (`Crash _) -> `Crashed
      | _, `Outcome _ ->
          Firebug.console##warn (Js.string "Two outcomes for 1 training");
          s
      | _, _ -> s
    in
    React.S.fold reduce `Allocating routine_events
  in
  let user_status, set_user_status = React.S.create `Train_to_end in
  let set_user_status : training_user_status -> unit = set_user_status in

  let render _ =
    Printf.printf "> Component - training control | render\n%!";
    let open Reactjs.Jsx in
    let routine_status = React.S.value routine_status in
    let prog = React.S.value routine_progress in
    let user_status = React.S.value user_status in
    let badge0 =
      match routine_status with
      | `Allocating ->
          "Allocating" |> of_string
          >> of_bootstrap "Badge" ~variant:"warning" ~style:[ ("marginLeft", "6px") ]
      | `Running ->
          "Running" |> of_string
          >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ]
      | `Ended ->
          "Ended" |> of_string
          >> of_bootstrap "Badge" ~variant:"success" ~style:[ ("marginLeft", "6px") ]
      | `Aborted ->
          "Aborted" |> of_string
          >> of_bootstrap "Badge" ~variant:"danger" ~style:[ ("marginLeft", "6px") ]
      | `Crashed ->
          "Crashed" |> of_string
          >> of_bootstrap "Badge" ~variant:"danger" ~style:[ ("marginLeft", "6px") ]
    in
    let badge1 =
      prog |> of_string >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ]
    in
    let style =
      [
        ("display", "flex");
        ("alignItems", "center");
        ("justifyContent", "center");
        ("marginTop", "4px");
        ("marginBottom", "4px");
      ]
    in
    let tbody =
      [
        of_constructor construct_instructions ~key:"buttons"
          (user_status, routine_status, set_user_status)
        >> of_bootstrap "Col" ~md_span:6 ~style;
        [ of_string "Routine |"; badge0 ] |> of_bootstrap "Col" ~md_span:3 ~style;
        [ of_string "Progress |"; badge1 ] |> of_bootstrap "Col" ~md_span:3 ~style;
      ]
      |> of_bootstrap "Row" >> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr"
      >> of_tag "tbody"
    in
    let thead = of_string "Training Control" >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  let mount () =
    if params.config.from_webworker then (
      (* 1. Create worker
         2. Setup events and signals processing
         3. Setup user_state in worker
         4. Prime worker
      *)
      let on_out_msg _ ev =
        match React.S.value user_status with
        | `Abort ->
            Firebug.console##warn
              (Js.string "Filtering out a message comming from training routine")
        | _ -> ev |> Webworker_routine.postprocess_out_msg |> fire_routine_event
      in
      let on_out_error_msg ev =
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
      let on_new_user_status ww = function
        | `Abort ->
            fire_routine_event (`Outcome `Abort);
            Webworker_routine.terminate ww
        | s -> Webworker_routine.post_in_message ww s
      in
      let on_new_routine_status ww = function
        | `Allocating | `Running -> ()
        | `Ended | `Aborted | `Crashed ->
            (* Killing Webworker to free GPU memory *)
            Webworker_routine.terminate ww
      in

      let ww = Webworker_routine.create on_out_msg on_out_error_msg in

      routine_events |> React.E.map (fun ev -> fire_upstream_event ev) |> ignore;

      user_status
      |> React.S.map Webworker_routine.preprocess_in_msg
      |> React.S.changes
      |> React.E.map (on_new_user_status ww)
      |> ignore;

      routine_status |> React.S.changes |> React.E.map (on_new_routine_status ww) |> ignore;

      user_status |> React.S.value |> Webworker_routine.preprocess_in_msg
      |> Webworker_routine.post_in_message ww;

      `Prime params |> Webworker_routine.preprocess_in_msg |> Webworker_routine.post_in_message ww )
    else (
      routine_events |> React.E.map (fun ev -> fire_upstream_event ev) |> ignore;
      let routine () = routine params fire_routine_event user_status in
      Js_of_ocaml_lwt.Lwt_js_events.async routine )
  in

  Reactjs.construct ~mount ~signal:routine_progress ~signal:user_status ~signal:routine_status
    render
