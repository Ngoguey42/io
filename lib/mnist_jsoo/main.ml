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

let t0 =
  {|
<h1><cite>MNIST</cite> training with <cite>Js_of_ocaml</cite></h1>
<p>
   As soon as the resources downloaded themselves you may start creating, training and comparing
   your
   <a href="https://en.wikipedia.org/wiki/Artificial_neural_network"><cite>
   Artificial Neural Networks</cite></a> on the task of handwritten digits
   <a href="https://en.wikipedia.org/wiki/Statistical_classification">classification</a> using the
   famous <a href="https://en.wikipedia.org/wiki/MNIST_database"><cite>MNIST</cite></a> dataset. <cite>Firefox</cite> or <cite>Chrome</cite> navigator recommended. It should work on your phone too.
</p>

|}

let favicon_routine signal =
  let link =
    Ft_js.select Dom_html.window##.document##.head "link[rel*='icon']" Dom_html.CoerceTo.link
  in
  let is_computing = function
    | Loading -> false
    | Loaded (_, _, tabstates) ->
        Array.fold_left
          (fun acc s ->
            match s with Types.Training _ -> true | Types.Evaluating _ -> true | _ -> acc)
          false tabstates
  in
  signal |> React.S.map is_computing |> React.S.changes
  |> React.E.map (function
       | false -> link##.href := Js.string "images/ocaml_logo2_release.svg"
       | true -> link##.href := Js.string "images/ocaml_logo2_release_blue.svg")
  |> ignore

let tab_states_equal a b =
  let open Types in
  match (a, b) with
  | Creating_network, Creating_network -> true
  | Creating_network, _ -> false
  | Selecting_backend _, Selecting_backend _ -> true
  | Selecting_backend _, _ -> false
  | Evaluating _, Evaluating _ -> true
  | Evaluating _, _ -> false
  | Creating_training s, Creating_training s' ->
      s.backend = s'.backend && s.from_webworker = s'.from_webworker
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

let construct_tab (db, gsignal, set_gsignal, fire_toast, i) =
  Printf.printf "$  main/tab%d | construct\n%!" i;
  let tabshownsignal =
    gsignal
    |> React.S.fmap (function Loaded (_, j, _) -> Some j | _ -> None) 0
    |> React.S.map (fun j -> j = i)
  in
  let signal =
    gsignal
    |> React.S.map (function
         | Loading -> failwith "Unreachable. A tab constructed before loading"
         | Loaded (_, _, tabstates) -> tabstates.(i))
    |> React.S.changes |> React.S.hold Creating_network
  in
  let set_signal s =
    match React.S.value gsignal with
    | Loading -> failwith "unreachable"
    | Loaded (db, focusidx, tabstates) ->
        let tabstates = Array.copy tabstates in
        tabstates.(i) <- s;
        set_gsignal (Loaded (db, focusidx, tabstates))
  in
  let render _ =
    let open Reactjs.Jsx in
    Printf.printf "$$ main/tab%d | render\n%!" i;
    of_constructor Tab.construct_tab (db, tabshownsignal, i, signal, set_signal, fire_toast)
  in
  Reactjs.construct render

let jsx_of_tab ((_, gsignal, _, _, i) as props) =
  let open Reactjs.Jsx in
  let k = string_of_int i in
  let is_spinning =
    match React.S.value gsignal with
    | Loading -> false
    | Loaded (_, _, tabstates) -> (
        match tabstates.(i) with Evaluating _ | Training _ -> true | _ -> false )
  in
  let spinner =
    if is_spinning then
      [
        of_string "\u{a0}";
        of_bootstrap "Spinner" ~animation_string:"border" ~variant:"primary" ~size:"sm" [];
      ]
    else []
  in
  let title_jsx = of_react "Fragment" (of_string k :: spinner) in
  of_constructor construct_tab props >> of_bootstrap "Tab" ~title_jsx ~event_key:k ~key:k

let construct_mnist_jsoo _ =
  Printf.printf "$  mnist_jsoo | construct\n%!";
  let signal, set_signal = React.S.create ~eq:states_equal Loading
                         (* collected on unmount *)
  in
  let set_signal : state -> unit = set_signal in
  let toast_signal, fire_toast, water_toast, unmount_toast = Toasts.create_frp_primitives () in
  let fire_toast : string * string -> unit = fire_toast in

  let fire_resources db = set_signal (Loaded (db, 0, [| Types.Creating_network |])) in
  let on_select k _ =
    match React.S.value signal with
    | Loading -> ()
    | Loaded (db, _, tabstates) ->
        let k = Js.to_string k in
        if k = "head" then ()
        else if k = "+" then
          let i = Array.length tabstates in
          let tabstates = Array.concat [ tabstates; [| Types.Creating_network |] ] in
          set_signal (Loaded (db, i, tabstates))
        else
          let i = int_of_string k in
          set_signal (Loaded (db, i, tabstates))
  in
  let render _ =
    Printf.printf "$$ mnist_jsoo | render\n%!";
    let open Reactjs.Jsx in
    let toasts = of_constructor ~key:"toasts" Toasts.construct_toasts (toast_signal, water_toast) in
    let res = of_constructor ~key:"res" Resources.construct_resources fire_resources in
    let tabs =
      (* The `Tab` elements must be direct children of `Tabs`. It won't work if anything is
         in-between (like a constructor). Hence the `jsx_of_tab` before the `construct_tab`.
      *)
      match React.S.value signal with
      | Loading -> of_string ""
      | Loaded (db, focusidx, tabstates) ->
          let focusidx = string_of_int focusidx in
          let head =
            of_bootstrap "Tab" ~title_jsx:(of_string "Networks") ~event_key:"head" ~disabled:true []
          in
          let tabs =
            List.init (Array.length tabstates) (fun i ->
                jsx_of_tab (db, signal, set_signal, fire_toast, i))
          in
          let plus =
            of_bootstrap "Tab" ~title_jsx:(of_string "+" >> of_tag "b") ~event_key:"+" []
          in
          of_bootstrap "Tabs" ~transition:false ~active_key:focusidx ~on_select ~id:"network-tabs"
            ([ head ] @ tabs @ [ plus ])
    in
    [
      toasts;
      [ of_tag "div" ~inner_html:t0 []; res; tabs ]
      |> of_bootstrap "Col" >> of_bootstrap "Row"
      >> of_bootstrap "Container" ~fluid:true ~classes:[ "bigbox1" ];
    ]
    |> of_react "Fragment"
  in

  let mount () = favicon_routine signal in
  let unmount () =
    React.S.stop ~strong:true signal;
    unmount_toast () in
  Reactjs.construct ~signal ~mount ~unmount render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in
  let div =
    [%html "<div><div style='text-align: center;'>loading</div></div>"]
    |> Tyxml_js.To_dom.of_element
  in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Ft_js.import_css "mnist-jsoo.css" >>= fun () ->
  Reactjs.render (Reactjs.Jsx.of_constructor construct_mnist_jsoo ()) div;
  Lwt.return ()
