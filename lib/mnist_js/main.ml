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

type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type db = uint8_ba * uint8_ba * uint8_ba * uint8_ba

type backend = [ `Tfjs_webgl | `Tfjs_cpu ]

type lr = [ `Down of float * float ]

type state =
  | Loading_resources
  | Creating_network of { db : db }
  | Creating_training of {
      db : db;
      encoder : Fnn.network;
      decoder : Fnn.network;
      seed : int;
      images_seen : int;
    }
  | Training of {
      db : db;
      encoder : Fnn.network;
      decoder : Fnn.network;
      seed : int;
      images_seen : int;
      config : Training_types.training_config;
    }

type event =
  | Resources of { db : db }
  | Network of { encoder : Fnn.network; decoder : Fnn.network; seed : int }
  | Training_conf of Training_types.training_config
  | End of { encoder : Fnn.network; decoder : Fnn.network; images_seen : int }
  | Crash of exn
  | Abort

let react_main () =
  let (events : event React.event), fire_event = React.E.create () in

  let reduce : state -> event -> state =
   fun s ev ->
    match (s, ev) with
    | Loading_resources, Resources ev ->
        Printf.eprintf "> react_main on Resources\n%!";
        Creating_network { db = ev.db }
    | Creating_network s, Network ev ->
        Printf.eprintf "> react_main on Network\n%!";
        Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
            let conf =
              Training_types.
                {
                  from_webworker = true;
                  backend = `Tfjs_webgl;
                  batch_size = 500;
                  lr = `Down (1e-3, 0.);
                  batch_count = 50;
                  seed = ev.seed;
                  verbose = true;
                }
            in
            fire_event (Training_conf conf);
            Lwt.return ());
        Creating_training
          { db = s.db; encoder = ev.encoder; decoder = ev.decoder; seed = ev.seed; images_seen = 0 }
    | Creating_training s, Training_conf ev ->
        Printf.eprintf "> react_main on Training_conf\n%!";
        Training
          {
            db = s.db;
            encoder = s.encoder;
            decoder = s.decoder;
            seed = s.seed;
            images_seen = s.images_seen;
            config = ev;
          }
    | Training s, End _ | Training s, Abort | Training s, Crash _ ->
        Printf.eprintf "> react_main on End/Abort/Crash\n%!";
        Creating_network { db = s.db }
    | _, _ -> failwith "react_main@reduce : unreachable"
  in
  let signal = React.S.fold reduce Loading_resources events in
  (signal, fire_event)

let construct_mnist_js _ =
  let signal, fire_event = react_main () in

  let fire_resources db = fire_event (Resources { db }) in
  let fire_network (encoder, decoder, seed) = fire_event (Network { encoder; decoder; seed }) in
  let fire_trained = function
    | `Crash exn -> fire_event (Crash exn)
    | `Abort -> fire_event Abort
    | `End (encoder, decoder, images_seen) -> fire_event (End { encoder; decoder; images_seen })
  in
  let render _ =
    let open Reactjs.Jsx in
    match React.S.value signal with
    | Loading_resources ->
        of_tag "div" ~class_:[ "textdiv" ]
          [ of_constructor ~key:"res" Resources.construct_react_table fire_resources ]
    | Creating_network _ ->
        of_tag "div" ~class_:[ "textdiv" ]
          [
            of_constructor ~key:"res" Resources.construct_react_table fire_resources;
            of_constructor Network_construction.construct_react_component (fire_network, true);
          ]
    | Creating_training _ ->
        of_tag "div" ~class_:[ "textdiv" ]
          [
            of_constructor ~key:"res" Resources.construct_react_table fire_resources;
            of_constructor Network_construction.construct_react_component (fire_network, false);
          ]
    | Training s ->
        let params =
          Training_types.{ db = s.db; networks = ([ s.encoder ], s.decoder); config = s.config }
        in
        of_tag "div" ~class_:[ "textdiv" ]
          [
            of_constructor ~key:"res" Resources.construct_react_table fire_resources;
            of_constructor Network_construction.construct_react_component (fire_network, false);
            of_constructor Training.construct (params, fire_trained);
          ]
  in
  Reactjs.construct ~signal render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in
  let div = [%html "<div></div>"] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Ft_js.import_js "https://cdn.plot.ly/plotly-latest.min.js" >>= fun () ->
  Reactjs.render (Reactjs.Jsx.of_constructor construct_mnist_js ()) div;
  Lwt.return ()
