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
end

open Types

let react_main _db signal set_signal =
  let (events : event React.event), fire_event = React.E.create () in

  let reduce : event -> state -> state =
   fun ev s ->
    match (s, ev) with
    | Creating_network, Network ev ->
        Printf.eprintf "> react_main on Network\n%!";
        Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
            Printf.eprintf "> react_main fire dummy training-conf\n%!";
            let conf =
              Training_types.
                {
                  from_webworker = true;
                  backend = `Tfjs_webgl;
                  lr = `Down (1e-3, 0.);
                  batch_size = 10;
                  batch_count = 10;
                  seed = ev.seed;
                  verbose = true;
                }
            in
            fire_event (Training_conf conf);
            Lwt.return ());
        Creating_training
          { encoder = ev.encoder; decoder = ev.decoder; seed = ev.seed; images_seen = 0 }
    | Creating_training s, Training_conf ev ->
        Printf.eprintf "> react_main on Training_conf\n%!";
        Training
          {
            encoder = s.encoder;
            decoder = s.decoder;
            seed = s.seed;
            images_seen = s.images_seen;
            config = ev;
          }
    | Training _s, End _ | Training _s, Abort | Training _s, Crash _ ->
        Printf.eprintf "> react_main on End/Abort/Crash\n%!";
        Creating_network
    | _, _ -> failwith "react_main@reduce : unreachable"
  in
  React.E.map (fun ev -> set_signal (reduce ev (React.S.value signal))) events |> ignore;
  fire_event

let construct_tab (db, _tabidx, signal, set_signal) =
  let fire_event = react_main db signal set_signal in

  (* let tabname = string_of_int tabidx in *)
  let fire_network (encoder, decoder, seed) = fire_event (Network { encoder; decoder; seed }) in
  let fire_trained = function
    | `Crash exn -> fire_event (Crash exn)
    | `Abort -> fire_event Abort
    | `End (encoder, decoder, images_seen) -> fire_event (End { encoder; decoder; images_seen })
  in

  let render _ =
    let open Reactjs.Jsx in
    match React.S.value signal with
    | Creating_network ->
        of_constructor Network_construction.construct_react_component (fire_network, true)
        >> of_react "Fragment"
    | Creating_training _ ->
        of_constructor Network_construction.construct_react_component (fire_network, false)
        >> of_react "Fragment"
    | Training s ->
        let params =
          Training_types.{ db; networks = ([ s.encoder ], s.decoder); config = s.config }
        in
        [
          of_constructor Network_construction.construct_react_component (fire_network, false);
          of_constructor Training.construct (params, fire_trained);
        ]
        |> of_react "Fragment"
  in
  Reactjs.construct ~signal render
