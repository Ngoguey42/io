module Reactjs = Ft_js.Reactjs
module Firebug = Js_of_ocaml.Firebug
module Js = Js_of_ocaml.Js
module Ndarray = Owl_base_dense_ndarray_generic
open Training

let train (train_imgs, train_labs, test_imgs, test_labs) config (encoders, decoder) fire_event
    instructions =
  ignore (train_imgs, train_labs, test_imgs, test_labs);
  let module Backend = (val Training.create_backend config.backend) in
  let verbose = config.verbose in
  let rng = Random.State.make [| config.seed |] in
  let batch_count, batch_size = (config.batch_count, config.batch_size) in
  let get_lr =
    match config.lr with
    | `Down (lr0, lr1) ->
        assert (0. <= lr1);
        assert (lr1 <= lr0);
        let lr2 = lr0 -. lr1 in
        fun i ->
          let ratio = float_of_int i /. float_of_int batch_count in
          lr1 +. (lr2 *. (1. -. ratio))
  in
  let get_data _ =
    let indices = Array.init batch_size (fun _ -> Random.State.int rng 60000) in
    let imgs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_imgs i 1) indices
      |> Ndarray.concatenate ~axis:0
    in
    let labs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_labs i 1) indices
      |> Ndarray.concatenate ~axis:0
    in
    (imgs, labs)
  in
  Backend.train ~fire_event ~instructions ~verbose ~batch_count ~get_lr ~get_data ~encoders ~decoder

type props' = Training.user_status * (Training.user_event -> unit)

let make_instructions : props' -> Reactjs.Jsx.t Js.t =
  (fun (props : props') ->
    let user_status, fire_user_event = props in
    let render () =
      let open Reactjs.Jsx in
      match user_status with
      | `Train_to_end ->
          of_tag "div"
            [
              of_string ">Train to end<";
              of_string " ";
              of_tag "button"
                ~on_click:(fun _ -> fire_user_event `Early_stop)
                [ of_string "Early stop" ];
              of_string " ";
              of_tag "button" ~on_click:(fun _ -> fire_user_event `Abort) [ of_string "Abort" ];
            ]
      | `Early_stop ->
          of_tag "div"
            [
              of_string "Train to end";
              of_string " ";
              of_string ">Early stop<";
              of_string " ";
              of_string "Abort";
            ]
      | `Abort ->
          of_tag "div"
            [
              of_string "Train to end";
              of_string " ";
              of_string "Early stop";
              of_string " ";
              of_string ">Abort<";
            ]
    in
    Reactjs.Bind.return render)
  |> Reactjs.Bind.constructor

type props =
  (uint8_ba * uint8_ba * uint8_ba * uint8_ba)
  * Training.training_config
  * (Fnn.network list * Fnn.network)
  * (Training.outcome -> unit)

let make : props -> Reactjs.Jsx.t Js.t =
  (fun (props : props) ->
    let db, config, networks, on_completion = props in

    let routine_events, fire_routine_event = React.E.create () in
    let routine_progress =
      React.S.fold (fun i a -> match a with `Batch_end (i, _) -> i + 1 | _ -> i) 0 routine_events
    in
    let routine_status =
      let reduce s ev =
        match (s, ev) with
        | `Running, `End _ ->
            on_completion `End;
            `Ended
        | `Running, `Abort ->
            on_completion `Abort;
            `Aborted
        | `Running, `Crash exn ->
            on_completion (`Crash exn);
            `Crashed
        | _, _ -> s
      in
      React.S.fold reduce `Running routine_events
    in

    let user_events, fire_user_event = React.E.create () in
    let fire_user_event : user_event -> unit = fire_user_event in
    let user_status =
      let reduce s ev =
        match (s, ev) with
        | `Train_to_end, `Early_stop -> `Early_stop
        | `Train_to_end, `Abort -> `Abort
        | _, `Early_stop | _, `Abort -> s
      in
      React.S.fold reduce `Train_to_end user_events
    in

    let render () =
      let open Reactjs.Jsx in
      of_tag "div"
        [
          of_make make_instructions (React.S.value user_status, fire_user_event);
          of_tag "br" [];
          of_string "Routine: ";
          ( match React.S.value routine_status with
          | `Running -> "Running"
          | `Ended -> "Ended"
          | `Aborted -> "Aborted"
          | `Crashed -> "Crashed" )
          |> of_string;
          of_tag "br" [];
          (React.S.value routine_progress |> float_of_int)
          /. (config.batch_count |> float_of_int)
          *. 100.
          |> Printf.sprintf "%.0f%%" |> of_string;
          of_tag "br" [];
        ]
    in
    let unmount () = () in
    let mount () =
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          train db config networks fire_routine_event user_status);
      unmount
    in
    Reactjs.Bind.return ~mount ~signal:routine_progress ~signal:user_status ~signal:routine_status
      render)
  |> Reactjs.Bind.constructor
