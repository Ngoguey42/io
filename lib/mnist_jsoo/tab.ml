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
                verbose = false;
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

let construct_tab (db, tabshownsignal, tabidx, signal, set_signal, fire_toast) =
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
      of_constructor ~key:"net" Network_construction.construct_training_config
        (fire_network_made, enabled)
    in
    let backend enabled =
      of_constructor ~key:"back" Backend_selection.construct_backend_selection
        (fire_backend_selected, tabidx, enabled)
    in
    let results () =
      of_constructor ~key:"res" Results.construct_results
        ((testi, testl), tabshownsignal, signal, events)
    in
    let train enabled =
      of_constructor ~key:"train" Training_configuration.construct_training_config
        (fire_training_conf, enabled)
    in
    let training params =
      of_constructor ~key:"training" Training.construct_training (params, fire_training_event)
    in

    match React.S.value signal with
    | Creating_network -> [ net true; backend false ] |> of_react "Fragment"
    | Selecting_backend _ -> [ net false; backend true ] |> of_react "Fragment"
    | Evaluating _ -> [ results (); train false; backend false; net false ] |> of_react "Fragment"
    | Creating_training _ ->
        [ results (); train true; backend true; net false ] |> of_react "Fragment"
    | Training s ->
        let networks = ([ s.encoder ], s.decoder) in
        let params = Types.{ db = (traini, trainl); networks; config = s.config } in
        [ results (); training params; train false; backend false; net false ]
        |> of_react "Fragment"
  in
  Reactjs.construct ~signal render
