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

type tabdata = int * Types.state React.signal * (Types.state -> unit)

let jsx_of_tabdata db (i, signal, set_signal) =
  let open Reactjs.Jsx in
  let k = string_of_int i in
  let spinner =
    let open Types in
    match React.S.value signal with
    | Creating_network -> []
    | Creating_training _ -> []
    | Training _ ->
        [
          of_string "\u{a0}";
          of_bootstrap "Spinner" ~animation:"border" ~variant:"primary" ~size:"sm" [];
        ]
  in
  let title_jsx = of_react "Fragment" (of_string k :: spinner) in
  of_constructor Tab.construct_tab (db, i, signal, set_signal)
  >> of_bootstrap "Tab" ~title_jsx ~event_key:k

let construct_mnist_js _ =
  let tab_events, fire_tab_event = React.E.create () in
  let append_new_tab tabs : tabdata list =
    let signal, set_signal = React.S.create Types.Creating_network in
    let set_signal : Types.state -> unit = set_signal in
    React.S.changes signal |> React.E.map (fun _ -> fire_tab_event ()) |> ignore;
    tabs @ [ (List.length tabs, signal, set_signal) ]
  in

  let signal, fire_event = React.S.create `Loading_resources in

  let fire_resources db = fire_event (`Loaded (db, append_new_tab (append_new_tab []))) in

  let render _ =
    let open Reactjs.Jsx in
    match React.S.value signal with
    | `Loading_resources ->
        of_constructor ~key:"res" Resources.construct_react_table fire_resources
        >> of_tag "div" ~class_:[ "textdiv" ]
    | `Loaded (db, tabsdata) ->
        let tabs = List.map (jsx_of_tabdata db) tabsdata in
        [
          of_constructor ~key:"res" Resources.construct_react_table fire_resources;
          of_bootstrap "Tabs" ~default_active_key:"0" ~id:"network-tabs" tabs;
        ]
        |> of_tag "div" ~class_:[ "textdiv" ]
  in
  Reactjs.construct ~signal ~events:tab_events render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in
  let div = [%html "<div></div>"] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  (* Ft_js.import_js "https://cdn.plot.ly/plotly-latest.min.js" >>= fun () -> *)
  Reactjs.render (Reactjs.Jsx.of_constructor construct_mnist_js ()) div;
  Lwt.return ()
