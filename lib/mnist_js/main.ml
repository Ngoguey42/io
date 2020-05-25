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

let jsx_of_tab db gsignal set_gsignal i state =
  let open Reactjs.Jsx in
  let k = string_of_int i in
  let spinner =
    let open Types in
    match state with
    | Creating_network -> []
    | Creating_training _ -> []
    | Selecting_backend _ -> []
    | Training _ ->
        [
          of_string "\u{a0}";
          of_bootstrap "Spinner" ~animation:"border" ~variant:"primary" ~size:"sm" [];
        ]
  in

  let signal, set_signal = React.S.create state in
  React.S.map
    (fun s ->
      match s with
      | `Loading -> failwith "unreachable"
      | `Loaded (_, _, tabstates) -> set_signal tabstates.(i))
    gsignal
  |> ignore;
  let set_signal s =
    match React.S.value gsignal with
    | `Loading -> failwith "unreachable"
    | `Loaded (db, focusidx, tabstates) ->
        let tabstates = Array.copy tabstates in
        tabstates.(i) <- s;
        set_gsignal (`Loaded (db, focusidx, tabstates))
  in

  let title_jsx = of_react "Fragment" (of_string k :: spinner) in
  of_constructor Tab.construct_tab (db, i, signal, set_signal)
  >> of_bootstrap "Tab" ~title_jsx ~event_key:k ~key:k

let construct_mnist_js _ =
  Printf.printf "> construct component: mnist_js\n%!";
  let signal, set_signal = React.S.create `Loading in
  let fire_resources db = set_signal (`Loaded (db, 0, [| Types.Creating_network |])) in
  let on_select k _ =
    match React.S.value signal with
    | `Loading -> ()
    | `Loaded (db, _, tabstates) ->
        let k = Js.to_string k in
        if k = "+" then
          let i = Array.length tabstates in
          let tabstates = Array.concat [ tabstates; [| Types.Creating_network |] ] in
          set_signal (`Loaded (db, i, tabstates))
        else
          let i = int_of_string k in
          set_signal (`Loaded (db, i, tabstates))
  in
  let render _ =
    let open Reactjs.Jsx in
    match React.S.value signal with
    | `Loading ->
        of_constructor ~key:"res" Resources.construct_react_table fire_resources
        >> of_tag "div" ~class_:[ "textdiv" ]
    | `Loaded (db, focusidx, tabstates) ->
        let focusidx = string_of_int focusidx in
        let tabs = Array.mapi (jsx_of_tab db signal set_signal) tabstates |> Array.to_list in
        let plus = of_bootstrap "Tab" ~title_jsx:(of_string "+" >> of_tag "b") ~event_key:"+" [] in
        [
          of_constructor ~key:"res" Resources.construct_react_table fire_resources;
          of_bootstrap "Tabs" ~transition:false ~active_key:focusidx ~on_select ~id:"network-tabs"
            (tabs @ [ plus ]);
        ]
        |> of_tag "div" ~class_:[ "textdiv" ]
  in
  Reactjs.construct ~signal render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in

  (* let canvas = [%html {|<div id="chart"></div>|}] |> Tyxml_js.To_dom.of_element in *)
  (* Dom.appendChild body canvas; *)
  (* Ft_js.import_js "https://cdn.plot.ly/plotly-latest.min.js" >>= fun () -> *)
  let div = [%html "<div></div>"] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Ft_js.import_js "https://cdn.plot.ly/plotly-latest.min.js" >>= fun () ->
  Reactjs.render (Reactjs.Jsx.of_constructor construct_mnist_js ()) div;
  Lwt.return ()
