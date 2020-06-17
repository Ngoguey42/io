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

let tab_states_reducer signal set_signal fire_toast =
  let (events : tab_event React.event), fire_event =
    React.E.create ()
    (* collected on `tab` unmount *)
  in

  let reduce : tab_event -> tab_state -> tab_state =
   fun ev s ->
    (* Printf.eprintf "> Tab reduce\n%!"; *)
    (* print_endline (show_tab_state s); *)
    (* print_endline (show_tab_event ev); *)
    match (s, ev) with
    | Creating_network, Network_made ev ->
        (* Just created network. Very first event *)
        Selecting_backend { encoder = ev.encoder; decoder = ev.decoder; seed = ev.seed }
    | Selecting_backend s, Backend_selected (backend, from_webworker) ->
        (* Selected backend for the first time, can select it again later *)
        let config = { backend; from_webworker; verbose = false; batch_size = 500 } in
        Evaluating
          {
            old = None;
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
    | ( Evaluating { old = Some (encoder, decoder, images_seen); seed; backend; from_webworker; _ },
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
        Evaluating
          {
            old = Some (s.encoder, s.decoder, s.images_seen);
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
    | Creating_network, _ ->
        failwith "tab_states_reducer@reduce@Creating_network : unexpected event"
    | Selecting_backend _, _ ->
        failwith "tab_states_reducer@reduce@Selecting_backend : unexpected event"
    | Evaluating _, _ -> failwith "tab_states_reducer@reduce@Evaluating : unexpected event"
    | Creating_training _, _ ->
        failwith "tab_states_reducer@reduce@Creating_training : unexpected event"
    | Training _, _ -> failwith "tab_states_reducer@reduce@Training : unexpected event"
  in
  React.E.map (fun ev -> set_signal (reduce ev (React.S.value signal))) events |> ignore;
  (events, fire_event)

let construct_tab ~s0:signal ~s1:tabshownsignal (tabidx, db, set_signal, fire_toast) =
  Printf.printf "$  tab%d | construct\n%!" tabidx;
  let events, fire_event = tab_states_reducer signal set_signal fire_toast in
  let fire_network_made (encoder, decoder, seed) =
    Network_made { encoder; decoder; seed } |> fire_event
  in
  let fire_training_event ev = Training_event ev |> fire_event in
  let fire_evaluation_event ev = Evaluation_event ev |> fire_event in
  let fire_backend_selected backend = Backend_selected backend |> fire_event in
  let fire_training_conf (lr, batch_size, batch_count) =
    Training_conf { lr; batch_size; batch_count } |> fire_event
  in

  let render _ =
    Printf.printf "$$ tab%d | render\n%!" tabidx;

    let open Reactjs.Jsx in
    let net enabled =
      of_constructor ~key:"net" Network_construction.construct_training_config
        (fire_network_made, enabled)
    in
    let backend enabled =
      of_constructor ~key:"back" Backend_selection.construct_backend_selection
        (fire_backend_selected, tabidx, enabled)
    in
    let dashboard () =
      of_constructor_sse ~key:"res" Dashboard.construct_dashboard ~s0:signal ~s1:tabshownsignal
        ~e0:events
        (db, fire_training_event, fire_evaluation_event)
    in
    let train enabled =
      of_constructor ~key:"train" Training_configuration.construct_training_config
        (fire_training_conf, enabled)
    in

    match React.S.value signal with
    | Creating_network -> [ net true; backend false ] |> of_react "Fragment"
    | Selecting_backend _ -> [ net false; backend true ] |> of_react "Fragment"
    | Evaluating _ -> [ dashboard (); train false; backend false; net false ] |> of_react "Fragment"
    | Creating_training _ ->
        [ dashboard (); train true; backend true; net false ] |> of_react "Fragment"
    | Training _ -> [ dashboard (); train false; backend false; net false ] |> of_react "Fragment"
  in
  let unmount () = React.E.stop ~strong:true events in
  Reactjs.construct ~signal ~unmount render
