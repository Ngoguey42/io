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
       Selecting_backend { encoder = ev.encoder; decoder = ev.decoder; seed = ev.seed }
    | Creating_network, _ -> failwith "react_main@reduce : unexpected state/event combination"
    | Selecting_backend s, Backend backend ->
        Creating_training
          { encoder = s.encoder; decoder = s.decoder; seed = s.seed; backend; images_seen = 0 }
    | Selecting_backend _, _ -> failwith "react_main@reduce : unexpected state/event combination"
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
    | Creating_training {encoder; decoder; seed; images_seen; _ }, Backend backend ->
       Creating_training {encoder; decoder; seed; images_seen; backend}
    | Training s, End ev ->
        Creating_training
          { encoder = ev.encoder
          ; decoder = ev.decoder
          ; seed = s.seed
          ; backend = s.backend
          ; images_seen = s.images_seen + ev.images_seen }
    | Training {encoder; decoder; seed; images_seen; backend; _ }, Abort ->
       Creating_training {encoder; decoder; seed; images_seen; backend}
    | Training {encoder; decoder; seed; images_seen; backend; _ }, Crash _ ->
       Creating_training {encoder; decoder; seed; images_seen; backend}
    | _, _ -> failwith "react_main@reduce : unreachable"
  in
  React.E.map (fun ev -> set_signal (reduce ev (React.S.value signal))) events |> ignore;
  fire_event

let construct_backend_selection : _ Reactjs.constructor =
 fun (fire_upstream_event, _) ->
  Printf.printf "> construct component: backend selection\n%!";
  let name_of_backend = function
    | `Tfjs_webgl -> "TensorFlow.js WebGL"
    | `Tfjs_wasm -> "TensorFlow.js WASM"
    | `Tfjs_cpu -> "TensorFlow.js cpu"
  in
  let disable_backend = function `Tfjs_webgl -> false | `Tfjs_wasm -> true | `Tfjs_cpu -> false in

  let on_change ev =
    ev##.target##.value
    |> Js.to_string |> int_of_string |> backend_of_enum |> Option.get |> fire_upstream_event
  in
  let render (_, enabled) =
    let open Reactjs.Jsx in
    let tbody =
      List.init (max_backend + 1) backend_of_enum
      |> List.map Option.get
      |> List.map (fun v ->
             of_bootstrap "Form.Check" ~label:(name_of_backend v) ~type_:"radio"
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
  let fire_event = react_main db signal set_signal in

  let fire_network (encoder, decoder, seed) = fire_event (Network { encoder; decoder; seed }) in
  let fire_trained = function
    | `Crash exn -> fire_event (Crash exn)
    | `Abort -> fire_event Abort
    | `End (encoder, decoder, images_seen) -> fire_event (End { encoder; decoder; images_seen })
  in
  let fire_backend backend = fire_event (Backend backend) in
  let fire_training_conf (lr, batch_size, batch_count) =
    fire_event (Training_conf {lr; batch_size; batch_count })
  in

  let render _ =
    let open Reactjs.Jsx in
    match React.S.value signal with
    | Creating_network ->
        of_constructor Network_construction.construct_react_component (fire_network, true)
        >> of_react "Fragment"
    | Selecting_backend _ ->
        [
          of_constructor Network_construction.construct_react_component (fire_network, false);
          of_constructor construct_backend_selection (fire_backend, true);
        ]
        |> of_react "Fragment"
    | Creating_training _ ->
        [
          of_constructor Network_construction.construct_react_component (fire_network, false);
          of_constructor construct_backend_selection (fire_backend, true);
          of_constructor Training_configuration.construct_react_component (fire_training_conf, true);
        ]
        |> of_react "Fragment"
    | Training s ->
        let params =
          Types.{ db; networks = ([ s.encoder ], s.decoder); config = s.config }
        in
        [
          of_constructor Network_construction.construct_react_component (fire_network, false);
          of_constructor construct_backend_selection (fire_backend, false);
          of_constructor Training_configuration.construct_react_component (fire_training_conf, false);
          of_constructor Training.construct (params, fire_trained);
        ]
        |> of_react "Fragment"
  in
  Reactjs.construct ~signal render
