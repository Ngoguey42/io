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

let favicon_routine signal =
  let is_firefox =
    Dom_html.window##.navigator##.userAgent##toLowerCase##indexOf (Js.string "firefox") > -1
  in
  let link =
    [%html {|<link rel="icon" type="image/png" href="images/ocaml.png" />|}]
    |> Tyxml_js.To_dom.of_element |> Dom_html.CoerceTo.link |> Js.Opt.to_option |> Option.get
  in
  Dom.appendChild Dom_html.window##.document##.head link;
  let is_computing = function
    | Loading -> false
    | Loaded (_, _, tabstates) ->
        Array.fold_left
          (fun acc s -> match s with Types.Training _ -> true | _ -> acc)
          false tabstates
  in
  signal |> React.S.map is_computing |> React.S.changes
  |> React.E.map (function
       | false -> link##.href := Js.string "images/ocaml.png"
       | true ->
           if is_firefox then link##.href := Js.string "images/spinner-blue.gif"
           else link##.href := Js.string "images/ocaml-blue.png")
  |> ignore

let jsx_of_tab db gsignal set_gsignal i state =
  let open Reactjs.Jsx in
  let k = string_of_int i in
  let spinner =
    let open Types in
    match state with
    | Creating_network -> []
    | Evaluating _ -> []
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
      | Loading -> failwith "unreachable"
      | Loaded (_, _, tabstates) -> set_signal tabstates.(i))
    gsignal
  |> ignore;
  let set_signal s =
    match React.S.value gsignal with
    | Loading -> failwith "unreachable"
    | Loaded (db, focusidx, tabstates) ->
        let tabstates = Array.copy tabstates in
        tabstates.(i) <- s;
        set_gsignal (Loaded (db, focusidx, tabstates))
  in
  let title_jsx = of_react "Fragment" (of_string k :: spinner) in
  of_constructor Tab.construct_tab (db, i, signal, set_signal)
  >> of_bootstrap "Tab" ~title_jsx ~event_key:k ~key:k

let tab_states_equal a b =
  let open Types in
  match (a, b) with
  | Creating_network, Creating_network -> true
  | Creating_network, _ -> false
  | Selecting_backend _, Selecting_backend _ -> true
  | Selecting_backend _, _ -> false
  | Evaluating _, Evaluating _ -> true
  | Evaluating _, _ -> false
  | Creating_training _, Creating_training _ -> true
  | Creating_training _, _ -> false
  | Training _, Training _ -> true
  | Training _, _ -> false

let states_equal a b =
  match (a, b) with
  | Loading, Loading -> true
  | _, Loading -> false
  | Loading, _ -> false
  | Loaded (_, focusidx, _), Loaded (_, focusidx', _) when focusidx <> focusidx' -> false
  | Loaded (_, _, tabstates), Loaded (_, _, tabstates') ->
      if Array.length tabstates <> Array.length tabstates' then false
      else List.for_all2 tab_states_equal (Array.to_list tabstates) (Array.to_list tabstates')

let construct_mnist_js _ =
  Printf.printf "> construct component: mnist_js\n%!";
  let signal, set_signal = React.S.create ~eq:states_equal Loading in
  let fire_resources db = set_signal (Loaded (db, 0, [| Types.Creating_network |])) in
  let on_select k _ =
    match React.S.value signal with
    | Loading -> ()
    | Loaded (db, _, tabstates) ->
        let k = Js.to_string k in
        if k = "+" then
          let i = Array.length tabstates in
          let tabstates = Array.concat [ tabstates; [| Types.Creating_network |] ] in
          set_signal (Loaded (db, i, tabstates))
        else
          let i = int_of_string k in
          set_signal (Loaded (db, i, tabstates))
  in
  let render _ =
    Printf.printf "> Main.construct.render | rendering \n%!";
    let open Reactjs.Jsx in
    match React.S.value signal with
    | Loading ->
        of_constructor ~key:"res" Resources.construct_react_table fire_resources
        >> of_tag "div" ~class_:[ "mnistdiv" ]
    | Loaded (db, focusidx, tabstates) ->
        let focusidx = string_of_int focusidx in
        let tabs = Array.mapi (jsx_of_tab db signal set_signal) tabstates |> Array.to_list in
        let plus = of_bootstrap "Tab" ~title_jsx:(of_string "+" >> of_tag "b") ~event_key:"+" [] in
        [
          of_constructor ~key:"res" Resources.construct_react_table fire_resources;
          of_bootstrap "Tabs" ~transition:false ~active_key:focusidx ~on_select ~id:"network-tabs"
            (tabs @ [ plus ]);
        ]
        |> of_tag "div" ~class_:[ "mnistdiv" ]
  in

  let mount () = favicon_routine signal in
  Reactjs.construct ~signal ~mount render

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
