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

let react_main db signal set_signal =
  let _traini, _trainl, testi, testl = db in
  let (events : tab_event React.event), fire_event = React.E.create () in
  let fire_evaluation_event ev = Evaluation_event ev |> fire_event in

  let reduce : tab_event -> tab_state -> tab_state =
   fun ev s ->
    match (s, ev) with
    | Creating_network, Network_made ev ->
        Selecting_backend { encoder = ev.encoder; decoder = ev.decoder; seed = ev.seed }
    | Creating_network, _ -> failwith "react_main@reduce@Creating_network : unexpected event"
    | Selecting_backend s, Backend_selected backend ->
        let params =
          {
            db = (testi, testl);
            encoder = s.encoder;
            decoder = s.decoder;
            config = { backend; from_webworker = true; verbose = true; batch_size = 500 };
          }
        in
        Lwt_js_events.async (fun () -> Evaluation.routine params fire_evaluation_event);
        Evaluating
          { encoder = s.encoder; decoder = s.decoder; seed = s.seed; backend; images_seen = 0 }
    | Selecting_backend _, _ -> failwith "react_main@reduce@Selecting_backend : unexpected event"
    | Evaluating _, Evaluation_event `Init -> s
    | Evaluating _, Evaluation_event (`Batch_begin _) -> s
    | Evaluating _, Evaluation_event (`Batch_end _) -> s
    | Evaluating _, Evaluation_event (`Outcome (`Crash _)) -> Creating_network
    | Evaluating s, Evaluation_event (`Outcome (`End _)) ->
        Creating_training
          {
            encoder = s.encoder;
            decoder = s.decoder;
            seed = s.seed;
            backend = s.backend;
            images_seen = s.images_seen;
          }
    | Evaluating _, _ -> failwith "react_main@reduce@Evaluating : unexpected event"
    | Creating_training s, Training_conf ev ->
        Training
          {
            encoder = s.encoder;
            decoder = s.decoder;
            seed = s.seed;
            images_seen = s.images_seen;
            config =
              {
                from_webworker = true;
                backend = s.backend;
                lr = ev.lr;
                batch_size = ev.batch_size;
                batch_count = ev.batch_count;
                seed = s.seed;
                (* TODO: Images seen *)
                verbose = true;
              };
            backend = s.backend;
          }
    | Creating_training { encoder; decoder; seed; images_seen; _ }, Backend_selected backend ->
        Creating_training { encoder; decoder; seed; images_seen; backend }
    | Creating_training _, _ -> failwith "react_main@reduce@Creating_training : unexpected event"
    | Training _, Training_event `Init -> s
    | Training _, Training_event (`Batch_begin _) -> s
    | Training _, Training_event (`Batch_end _) -> s
    | Training s, Training_event (`Outcome (`End (encoders, decoder, stats))) ->
        let encoder = List.hd encoders in
        let params =
          {
            db = (testi, testl);
            encoder;
            decoder;
            config =
              { from_webworker = true; backend = s.backend; verbose = true; batch_size = 500 };
          }
        in
        Lwt_js_events.async (fun () -> Evaluation.routine params fire_evaluation_event);
        Evaluating
          {
            encoder;
            decoder;
            seed = s.seed;
            backend = s.backend;
            images_seen = s.images_seen + (s.config.batch_size * stats.batch_count);
          }
    | Training { encoder; decoder; seed; images_seen; backend; _ }, Training_event (`Outcome `Abort)
      ->
        Creating_training { encoder; decoder; seed; images_seen; backend }
    | ( Training { encoder; decoder; seed; images_seen; backend; _ },
        Training_event (`Outcome (`Crash _)) ) ->
        Creating_training { encoder; decoder; seed; images_seen; backend }
    | Training _, _ -> failwith "react_main@reduce@Training : unexpected event"
  in
  React.E.map (fun ev -> set_signal (reduce ev (React.S.value signal))) events |> ignore;
  fire_event

let construct_backend_selection : _ Reactjs.constructor =
 fun (fire_upstream_event, _) ->
  Printf.eprintf "> construct component: backend selection\n%!";
  let name_of_backend = function
    | `Tfjs_webgl -> "TensorFlow.js WebGL"
    | `Tfjs_wasm -> "TensorFlow.js WASM"
    | `Tfjs_cpu -> "TensorFlow.js cpu"
  in
  let disable_backend = function `Tfjs_webgl -> false | `Tfjs_wasm -> true | `Tfjs_cpu -> false in

  let on_change ev =
    Printf.eprintf "> construct_backend_selection.on_change `%s`\n%!"
      (ev##.target##.value |> Js.to_string);
    ev##.target##.value
    |> Js.to_string |> int_of_string |> backend_of_enum |> Option.get |> fire_upstream_event
  in
  let render (_, enabled) =
    Printf.eprintf "> Tab.construct_backend_selection.render | render\n%!";
    let open Reactjs.Jsx in
    let tbody =
      List.init (max_backend + 1) backend_of_enum
      |> List.map Option.get
      |> List.map (fun v ->
             let n = name_of_backend v in
             of_bootstrap "Form.Check" ~label:n ~type_:"radio" ~id:("radio-" ^ n)
               ~name:"selecting-backend"
               ~value:(backend_to_enum v |> string_of_int)
               ~on_change ~inline:true
               ~disabled:((not enabled) || disable_backend v)
               [])
      |> of_tag "div" ~style:[ ("display", "flex"); ("justifyContent", "center") ]
      >> of_tag "th" >> of_tag "tr" >> of_tag "tbody"
    in
    let thead = of_string "Backend Selection" >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~class_:[ "mnist-panel" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct render

let construct_tab (db, _tabidx, signal, set_signal) =
  Printf.printf "> construct component: tab%d\n%!" _tabidx;
  let traini, trainl, _testi, _testl = db in
  let fire_event = react_main db signal set_signal in
  let fire_network_made (encoder, decoder, seed) =
    Network_made { encoder; decoder; seed } |> fire_event
  in
  let fire_training_event ev = Training_event ev |> fire_event in
  let fire_backend_selected backend = Backend_selected backend |> fire_event in
  let fire_training_conf (lr, batch_size, batch_count) =
    Training_conf { lr; batch_size; batch_count } |> fire_event
  in

  let render _ =
    Printf.printf "> Tab.construct_tab.render | render\n%!";
    let open Reactjs.Jsx in
    let network_creation enabled =
      of_constructor Network_construction.construct_react_component (fire_network_made, enabled)
    in
    let backend_selection enabled =
      of_constructor construct_backend_selection (fire_backend_selected, enabled)
    in
    let results () = of_constructor Results.construct_results (42, 42) in
    let training_configuration enabled =
      of_constructor Training_configuration.construct_react_component (fire_training_conf, enabled)
    in
    let training params = of_constructor Training.construct (params, fire_training_event) in
    match React.S.value signal with
    | Creating_network -> network_creation true >> of_react "Fragment"
    | Selecting_backend _ ->
        [ network_creation false; backend_selection true ] |> of_react "Fragment"
    | Evaluating _ ->
        [ network_creation false; backend_selection false; results () ] |> of_react "Fragment"
    | Creating_training _ ->
        [ network_creation false; backend_selection true; results (); training_configuration true ]
        |> of_react "Fragment"
    | Training s ->
        let networks = ([ s.encoder ], s.decoder) in
        let params = Types.{ db = (traini, trainl); networks; config = s.config } in
        [
          network_creation false;
          backend_selection false;
          results ();
          training_configuration false;
          training params;
        ]
        |> of_react "Fragment"
  in
  Reactjs.construct ~signal render
