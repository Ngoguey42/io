open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

open Types

(* Derive tab's signal and events to a render-friendly signal *)
let create_routines_signal tabsignal tabevents =
  let training_signal =
    React.S.map
      (function
        | Training s -> `On (s.config.batch_count, s.config.backend, s.encoder, s.decoder, s.config)
        | _ -> `Off)
      tabsignal
  in
  let evaluating_signal =
    React.S.map
      (function
        | Evaluating s ->
            let batch_count =
              (Mnist.test_set_size + s.config.batch_size - 1) / s.config.batch_size
            in
            `On (batch_count, s.config.backend, s.encoder, s.decoder, s.config)
        | _ -> `Off)
      tabsignal
  in
  let batch_begin_signal =
    tabevents
    |> React.E.fmap (function
         | Training_event (`Batch_begin i) -> Some (Some i)
         | Training_event (`Outcome _) -> Some None
         | Evaluation_event (`Batch_begin i) -> Some (Some i)
         | Evaluation_event (`Outcome _) -> Some None
         | _ -> None)
    |> React.S.hold None
  in
  let batch_end_signal =
    tabevents
    |> React.E.fmap (function
         | Training_event (`Batch_end (i, _)) -> Some (Some i)
         | Training_event (`Outcome _) -> Some None
         | Evaluation_event (`Batch_end i) -> Some (Some i)
         | Evaluation_event (`Outcome _) -> Some None
         | _ -> None)
    |> React.S.hold None
  in
  React.S.l4
    (fun tra ev bbeg bend ->
      match (tra, ev, bbeg, bend) with
      | `On _, `On _, _, _ ->
          failwith "Dashboard.create_routines_signal@Unreachable. Can't be training and evaluating"
      | _, _, None, Some _ ->
          failwith
            "Dashboard.create_routines_signal@Unreachable. Can't have batch_end without batch_begin"
      | `Off, `Off, _, _ -> `Off
      | `On (_, _, enco, deco, conf), `Off, None, None
      | `On (_, `Tfjs_webgl, enco, deco, conf), `Off, Some 0, None ->
          `Allocating_train (enco, deco, conf)
      | `Off, `On (_, _, enco, deco, conf), None, None
      | `Off, `On (_, `Tfjs_webgl, enco, deco, conf), Some 0, None ->
          `Allocating_test (enco, deco, conf)
      | `On (_, _, _, _, _), `Off, Some 0, None -> `On_train "0%"
      | `Off, `On (_, _, _, _, _), Some 0, None -> `On_test "0%"
      | `On (batch_count, _, _, _, _), `Off, Some _, Some i ->
          let prog =
            (float_of_int i +. 1.) /. float_of_int batch_count *. 100. |> Printf.sprintf "%.0f%%"
          in
          `On_train prog
      | `Off, `On (batch_count, _, _, _, _), Some _, Some i ->
          let prog =
            (float_of_int i +. 1.) /. float_of_int batch_count *. 100. |> Printf.sprintf "%.0f%%"
          in
          `On_test prog
      | `Off, `On _, Some _, None | `On _, `Off, Some _, None ->
          failwith
            "Dashboard.create_routines_signal@unreachable. Can't have batch_begin>=1 with no \
             batch_end")
    training_signal evaluating_signal batch_begin_signal batch_end_signal

let jsx_of_training_instructions user_status routine_status set_user_status =
  let open Reactjs.Jsx in
  let button txt t =
    match (t, routine_status) with
    | `On event, `Allocating_train _ | `On event, `On_train _ ->
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

let jsx_of_routines set_training_user_status training_user_status routine_status =
  let open Reactjs.Jsx in
  let badge variant = of_bootstrap "Badge" ~variant ~style:[ ("marginLeft", "6px") ] in
  let what0 =
    match routine_status with
    | `Off -> [ of_string "Routine |"; "Idle" |> of_string >> badge "dark" ]
    | `On_train _ | `Allocating_train _ ->
        [ of_string "Routine |"; "Training" |> of_string >> badge "info" ]
    | `On_test _ | `Allocating_test _ ->
        [ of_string "Routine |"; "Testing" |> of_string >> badge "info" ]
  in
  let what1 =
    match routine_status with
    | `Off -> []
    | `Allocating_train _ | `Allocating_test _ ->
        [ of_string "Progress |"; "Allocating" |> of_string >> badge "warning" ]
    | `On_train prog | `On_test prog ->
        [ of_string "Progress |"; prog |> of_string >> badge "info" ]
  in
  let col md_span = of_bootstrap "Col" ~md_span ~classes:[ "dashboard-status-col" ] in
  ( match routine_status with
  | `Off -> [ what0 |> col 3 ]
  | `Allocating_train _ | `On_train _ ->
      [
        what0 |> col 3;
        what1 |> col 3;
        jsx_of_training_instructions training_user_status routine_status set_training_user_status
        >> col 6;
      ]
  | `Allocating_test _ | `On_test _ -> [ what0 |> col 3; what1 |> col 3 ] )
  |> of_bootstrap "Row" ~classes:[ "dashboard-status-row" ]

let jsx_of_test_set_sample test_set_sample_urls probas =
  let open Reactjs.Jsx in
  let aux digit url =
    let probas = List.init 10 (fun i -> Ndarray.get probas [| digit; i |]) in
    let img = of_tag "img" ~src:url [] in
    let probas =
      probas
      |> List.mapi (fun i p ->
             let bg = Ft.Color.(Firegrass2.get p |> to_hex_string) in
             let content = i |> string_of_int |> of_string in
             let classes = if i = digit then [ "good-one" ] else [] in
             content >> of_tag "div" ~classes ~style:[ ("background", bg) ])
    in
    let md_order = (digit / 5) + (digit mod 5 * 2) in
    [ img ] @ probas |> of_bootstrap "Col" ~classes:[ "mnist-pred" ] ~md_span:6 ~md_order
  in

  [
    of_string "Test-set sample"
    >> of_bootstrap "Col" ~classes:[ "mnist-pred" ] ~md_span:12 ~as_:"h5"
    >> of_bootstrap "Row";
    List.mapi aux test_set_sample_urls |> of_bootstrap "Row" ~no_gutters:true;
  ]
  |> of_bootstrap "Container"

let construct_dashboard ~s0:tabsignal ~s1:tabshownsignal ~e0:tabevents
    ((traini, trainl, testi, testl), fire_training_event, fire_evaluation_event) =
  Printf.printf "$  dashboard | construct\n%!";

  (* Create the Reactjs "reference" that will be used to bind the Plotly lib *)
  let plotly_ref = Reactjs.create_ref () in

  (* Combute the base64 img urls of the 10 sampled digits *)
  let test_set_sample_urls =
    List.map (fun (_, idx) -> Ndarray.get_slice [ [ idx ]; []; [] ] testi) Mnist.test_set_sample
    |> List.map Mnist.b64_url_of_digit
  in

  (* Create various signals *)
  let routines_signal = create_routines_signal tabsignal tabevents in
  let training_user_status, set_training_user_status =
    React.S.create `Train_to_end
    (* collected on unmount *)
  in
  let set_training_user_status : training_user_status -> unit = set_training_user_status in
  let test_set_sample_signal =
    tabevents
    |> React.E.fmap (function
         | Evaluation_event (`Outcome (`End stats)) -> Some (Some stats.test_set_sample_probas)
         | _ -> None)
    |> React.S.hold ~eq:( == ) None
  in

  (* Quick and dirty launching of the train/test subroutines *)
  routines_signal
  |> React.S.map (function
       | `Allocating_train (encoder, decoder, config) ->
           set_training_user_status `Train_to_end;
           let params = { db = (traini, trainl); networks = ([ encoder ], decoder); config } in
           Lwt_js_events.async (fun () ->
               Training.routine params fire_training_event training_user_status)
       | `Allocating_test (encoder, decoder, config) ->
           let params = { db = (testi, testl); encoder; decoder; config } in
           Lwt_js_events.async (fun () -> Evaluation.routine params fire_evaluation_event)
       | _ -> ())
  |> ignore;

  let render _ =
    Printf.printf "$$ dashboard | render\n%!";
    let open Reactjs.Jsx in
    let training_user_status = React.S.value training_user_status in
    let routine_status = React.S.value routines_signal in
    let routines = jsx_of_routines set_training_user_status training_user_status routine_status in
    let chart =
      let title = of_string "Statistics" >> of_tag "h5" in
      let chart = of_tag "div" ~ref:plotly_ref ~style:[ ("height", "325px") ] [] in
      of_tag "div" ~style:[ ("textAlign", "center") ] [ title; chart ]
    in
    let digits =
      match React.S.value test_set_sample_signal with
      | None ->
          jsx_of_test_set_sample test_set_sample_urls (Ndarray.zeros Bigarray.Float32 [| 10; 10 |])
      | Some probas -> jsx_of_test_set_sample test_set_sample_urls probas
    in
    let tbody = [ routines; chart; digits ] |> of_tag "th" >> of_tag "tr" >> of_tag "tbody" in
    let thead = [ of_string "Dashboard" ] |> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in
  let mount () =
    Printf.printf "$$ dashboard | mount\n%!";
    match plotly_ref##.current |> Js.Opt.to_option with
    | None -> failwith "unreachable. React.ref failed"
    | Some elt -> Chart.routine elt tabshownsignal tabsignal tabevents
  in
  let unmount () =
    Printf.printf "$$ dashboard | unmount\n%!";
    React.S.stop ~strong:true training_user_status
  in
  Reactjs.construct ~signal:routines_signal ~signal:test_set_sample_signal ~mount ~unmount render
