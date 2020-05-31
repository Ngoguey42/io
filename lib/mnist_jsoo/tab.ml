open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
  module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
  module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray_generic
  module Typed_array = Js_of_ocaml.Typed_array
  module Reactjs = Ft_js.Reactjs
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

open Types

let react_main db signal set_signal fire_toast =
  let _traini, _trainl, testi, testl = db in
  let (events : tab_event React.event), fire_event = React.E.create () in
  let fire_evaluation_event ev = Evaluation_event ev |> fire_event in

  let reduce : tab_event -> tab_state -> tab_state =
   fun ev s ->
    match (s, ev) with
    | Creating_network, Network_made ev ->
        (* Just created network. Very first event *)
        Selecting_backend { encoder = ev.encoder; decoder = ev.decoder; seed = ev.seed }
    | Selecting_backend s, Backend_selected (backend, from_webworker) ->
        (* Selected backend for the first time, can select it again later *)
        let config = { backend; from_webworker; verbose = false; batch_size = 500 } in
        let params = { db = (testi, testl); encoder = s.encoder; decoder = s.decoder; config } in
        Lwt_js_events.async (fun () -> Evaluation.routine params fire_evaluation_event);
        Evaluating
          {
            old_encoder = None;
            old_decoder = None;
            old_images_seen = None;
            encoder = s.encoder;
            decoder = s.decoder;
            seed = s.seed;
            backend;
            from_webworker;
            images_seen = 0;
            config;
          }
    | Evaluating _, Evaluation_event `Init
    | Evaluating _, Evaluation_event (`Batch_begin _)
    | Evaluating _, Evaluation_event (`Batch_end _) ->
        (* Ignoring eval events *)
        s
    | ( Evaluating
          {
            old_encoder = Some encoder;
            old_decoder = Some decoder;
            old_images_seen = Some images_seen;
            seed;
            backend;
            from_webworker;
            _;
          },
        Evaluation_event (`Outcome (`Crash msg)) ) ->
        (* Recovering from eval crash following training *)
        fire_toast ("Evaluation crashed", msg);
        Creating_training { encoder; decoder; images_seen; seed; backend; from_webworker }
    | Evaluating s, Evaluation_event (`Outcome (`Crash msg)) ->
        (* Recovering from initial eval crash *)
        fire_toast ("Evaluation crashed", msg);
        Selecting_backend { encoder = s.encoder; decoder = s.decoder; seed = s.seed }
    | Evaluating s, Evaluation_event (`Outcome (`End _)) ->
        (* Eval done *)
        Creating_training
          {
            encoder = s.encoder;
            decoder = s.decoder;
            seed = s.seed;
            backend = s.backend;
            from_webworker = s.from_webworker;
            images_seen = s.images_seen;
          }
    | Creating_training s, Training_conf ev ->
        (* Launching training *)
        Training
          {
            encoder = s.encoder;
            decoder = s.decoder;
            seed = s.seed;
            images_seen = s.images_seen;
            config =
              {
                backend = s.backend;
                from_webworker = s.from_webworker;
                lr = ev.lr;
                batch_size = ev.batch_size;
                batch_count = ev.batch_count;
                seed = s.seed;
                images_seen = s.images_seen;
                verbose = true;
              };
            backend = s.backend;
            from_webworker = s.from_webworker;
          }
    | ( Creating_training { encoder; decoder; seed; images_seen; _ },
        Backend_selected (backend, from_webworker) ) ->
        (* Selecting new backend *)
        Creating_training { encoder; decoder; seed; images_seen; backend; from_webworker }
    | Training _, Training_event `Init
    | Training _, Training_event (`Batch_begin _)
    | Training _, Training_event (`Batch_end _) ->
        (* Ignoring Training events *)
        s
    | Training s, Training_event (`Outcome (`End (encoders, decoder, stats))) ->
        (* Training done, launching eval *)
        let encoder = List.hd encoders in
        let config =
          {
            backend = s.backend;
            from_webworker = s.from_webworker;
            verbose = false;
            batch_size = 500;
          }
        in
        let params = { db = (testi, testl); encoder; decoder; config } in
        Lwt_js_events.async (fun () -> Evaluation.routine params fire_evaluation_event);
        Evaluating
          {
            old_encoder = Some s.encoder;
            old_decoder = Some s.decoder;
            old_images_seen = Some s.images_seen;
            encoder;
            decoder;
            seed = s.seed;
            backend = s.backend;
            from_webworker = s.from_webworker;
            images_seen = s.images_seen + (s.config.batch_size * stats.batch_count);
            config;
          }
    | ( Training { encoder; decoder; seed; images_seen; backend; from_webworker; _ },
        Training_event (`Outcome `Abort) ) ->
        (* Aborting training *)
        Creating_training { encoder; decoder; seed; images_seen; backend; from_webworker }
    | ( Training { encoder; decoder; seed; images_seen; backend; from_webworker; _ },
        Training_event (`Outcome (`Crash msg)) ) ->
        (* Training crashed *)
        fire_toast ("Training crashed", msg);
        Creating_training { encoder; decoder; seed; images_seen; backend; from_webworker }
    | Creating_network, _ -> failwith "react_main@reduce@Creating_network : unexpected event"
    | Selecting_backend _, _ -> failwith "react_main@reduce@Selecting_backend : unexpected event"
    | Evaluating _, _ -> failwith "react_main@reduce@Evaluating : unexpected event"
    | Creating_training _, _ -> failwith "react_main@reduce@Creating_training : unexpected event"
    | Training _, _ -> failwith "react_main@reduce@Training : unexpected event"
  in
  React.E.map (fun ev -> set_signal (reduce ev (React.S.value signal))) events |> ignore;
  (events, fire_event)

let options = [ (`Tfjs_webgl, true); (`Tfjs_cpu, true); (`Tfjs_webgl, false); (`Tfjs_cpu, false) ]

let option_of_idx = function
  | 0 -> (`Tfjs_webgl, true)
  | 1 -> (`Tfjs_wasm, true)
  | 2 -> (`Tfjs_cpu, true)
  | 3 -> (`Tfjs_webgl, false)
  | 4 -> (`Tfjs_wasm, false)
  | 5 -> (`Tfjs_cpu, false)
  | _ -> failwith "Unknown computation option"

let idx_of_option = function
  | `Tfjs_webgl, true -> 0
  | `Tfjs_wasm, true -> 1
  | `Tfjs_cpu, true -> 2
  | `Tfjs_webgl, false -> 3
  | `Tfjs_wasm, false -> 4
  | `Tfjs_cpu, false -> 5

let name_of_option = function
  | `Tfjs_webgl, true -> "TensorFlow.js WebGL from Web Worker"
  | `Tfjs_wasm, true -> "TensorFlow.js WASM from Web Worker"
  | `Tfjs_cpu, true -> "TensorFlow.js cpu from Web Worker"
  | `Tfjs_webgl, false -> "TensorFlow.js WebGL"
  | `Tfjs_wasm, false -> "TensorFlow.js WASM"
  | `Tfjs_cpu, false -> "TensorFlow.js cpu"

let construct_backend_selection : _ Reactjs.constructor =
 fun (fire_upstream_event, tabidx, _) ->
  Printf.printf "> Component - backend_selection | construct\n%!";

  let on_change ev =
    ev##.target##.value |> Js.to_string |> int_of_string |> option_of_idx |> fire_upstream_event
  in
  let render (_, _, enabled) =
    Printf.printf "> Component - backend_selection | render\n%!";
    let open Reactjs.Jsx in
    let tbody =
      options
      |> List.map (fun option ->
             let n = name_of_option option in
             of_bootstrap "Form.Check" ~label:n ~type_:"radio" (* ~id:("radio-" ^ n) *)
               ~id:(Printf.sprintf "selecting-backend-tab%d-radio%s" tabidx n)
               ~name:(Printf.sprintf "selecting-backend-tab%d" tabidx)
               ~value:(idx_of_option option |> string_of_int)
               ~on_change ~inline:true ~disabled:(not enabled) []
             >> of_bootstrap "Col" ~md_span:6)
      |> of_bootstrap "Row" ~no_gutters:true
      >> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr" >> of_tag "tbody"
    in
    let thead = of_string "Backend Selection" >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~class_:[ "mnist-panel" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct render

let construct_tab (db, tabidx, signal, set_signal, fire_toast) =
  Printf.printf "> Component - tab%d | construct\n%!" tabidx;
  let traini, trainl, testi, testl = db in
  let events, fire_event = react_main db signal set_signal fire_toast in
  let fire_network_made (encoder, decoder, seed) =
    Network_made { encoder; decoder; seed } |> fire_event
  in
  let fire_training_event ev = Training_event ev |> fire_event in
  let fire_backend_selected backend = Backend_selected backend |> fire_event in
  let fire_training_conf (lr, batch_size, batch_count) =
    Training_conf { lr; batch_size; batch_count } |> fire_event
  in

  let render _ =
    Printf.printf "> Component - tab%d | render\n%!" tabidx;
    let open Reactjs.Jsx in
    let net enabled =
      of_constructor Network_construction.construct_training_config (fire_network_made, enabled)
    in
    let backend enabled =
      of_constructor construct_backend_selection (fire_backend_selected, tabidx, enabled)
    in
    let results () = of_constructor Results.construct_results ((testi, testl), signal, events) in
    let train enabled =
      of_constructor Training_configuration.construct_training_config (fire_training_conf, enabled)
    in
    let training params =
      of_constructor Training.construct_training (params, fire_training_event)
    in
    match React.S.value signal with
    | Creating_network -> net true >> of_react "Fragment"
    | Selecting_backend _ -> [ net false; backend true ] |> of_react "Fragment"
    | Evaluating s when s.images_seen = 0 ->
        [ net false; backend false; results () ] |> of_react "Fragment"
    | Creating_training _ ->
        [ net false; backend true; results (); train true ] |> of_react "Fragment"
    | Training s ->
        let networks = ([ s.encoder ], s.decoder) in
        let params = Types.{ db = (traini, trainl); networks; config = s.config } in
        [ net false; backend false; results (); train false; training params ]
        |> of_react "Fragment"
    | Evaluating _ -> [ net false; backend false; results (); train false ] |> of_react "Fragment"
  in
  Reactjs.construct ~signal render
