module Reactjs = Ft_js.Reactjs
module Firebug = Js_of_ocaml.Firebug
module Js = Js_of_ocaml.Js
module Ndarray = Owl_base_dense_ndarray_generic

open Training

let train (train_imgs, train_labs, test_imgs, test_labs) (config : Training.training_config) (encoders, decoder) fire_event instructions =
  ignore (train_imgs, train_labs, test_imgs, test_labs);
  (* let encoders, decoder = Ft_cnnjs.Fnn_archi.create_nn (Random.State.make [| 42 |]) in *)

  let module Backend = (val Training.get_backend config.backend) in

  let verbose = config.verbose in
  let rng = Random.State.make [| config.seed |] in
  let batch_count, batch_size = config.batch_count, config.batch_size in
  let get_lr = match config.lr with
    | `Down (lr0, lr1) ->
       assert (0. <= lr1);
       assert (lr1 <= lr0);
       let lr2 = lr0 -. lr1 in
       (fun i ->
         let ratio = (float_of_int i) /. (float_of_int batch_count) in
         lr1 +. lr2 *. (1. -. ratio)
       )
  in
  let get_data _ =
    let indices = Array.init batch_size (fun _ -> Random.State.int rng 60000) in
    let imgs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_imgs i (1)) indices
      |> Ndarray.concatenate ~axis:0
    in
    let labs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_labs i (1)) indices
      |> Ndarray.concatenate ~axis:0
    in
    (imgs, labs)
  in

  Backend.train ~fire_event ~instructions ~verbose ~batch_count ~get_lr ~get_data ~encoders ~decoder
  (* >>= fun (encoders, decoder) -> *)
  (* ignore (encoders, decoder); *)
  (* Lwt.return () *)

type db = uint8_ba * uint8_ba * uint8_ba * uint8_ba
type conf = Training.training_config
type nns = Fnn.network list * Fnn.network
type on_completion = [ `Abort | `Stop ] -> unit

let make : (db * conf * nns * on_completion) -> Reactjs.Jsx.t Js.t =
  (fun (db, config, networks, on_completion) ->
    let training_events, fire_training_event = React.E.create () in
    let progress =
      React.S.fold (fun s a -> match a with `Batch_begin s -> s | _ -> s) 0 training_events
    in
    let user_events, fire_user_event = React.E.create () in
    let instructions =
      let reduce s a =
        match a, s with
        | `Stop, `Train_to_end -> `Early_stop
        | `Abort, `Train_to_end -> `Abort
        | `Stop, _ | `Abort, _ -> s
      in
      React.S.fold reduce `Train_to_end user_events
    in

    React.E.map
      (function `Abort -> on_completion `Abort | `Stop _data -> on_completion `Stop | _ -> ())
      training_events
    |> ignore;

    let render () =
      let open Reactjs.Jsx in
      of_tag "div"
        [
          of_tag "button" ~on_click:(fun _ -> fire_user_event `Stop) [ of_string "stop" ];
          of_tag "button" ~on_click:(fun _ -> fire_user_event `Abort) [ of_string "abort" ];
          of_tag "br" [];

          (match React.S.value instructions with `Train_to_end -> "training"
                                               | `Abort -> "aborting"
                                               | `Early_stop -> "stopping"
          )
          |> of_string;

          of_tag "br" [];

          (React.S.value progress |> float_of_int)
          /. (config.batch_count |> float_of_int)
          *. 100.
          |> Printf.sprintf "%.0f%%"
          |> of_string;

          of_tag "br" [];

        ]
    in
    let unmount () = () in
    let mount () =
      Js_of_ocaml_lwt.Lwt_js_events.async
        (fun () -> train db config networks fire_training_event instructions);
      unmount
    in
    Reactjs.Bind.return ~mount ~signal:progress ~signal:instructions render)
  |> Reactjs.Bind.constructor
