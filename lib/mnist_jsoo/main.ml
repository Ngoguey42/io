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

let render_toast (id, (title, body), water_toast) =
  let open Reactjs.Jsx in
  Printf.printf "> Component - toast%s | render\n%!" id;
  let on_close () = water_toast id in
  let header =
    of_string title >> of_tag "strong" ~classes:[ "mr-auto" ] >> of_bootstrap "Toast.Header"
  in
  let body = of_string body >> of_bootstrap "Toast.Body" in
  of_bootstrap "Toast" ~on_close ~animation_bool:false [ header; body ]

let favicon_routine signal =
  let link =
    [%html {|<link rel="icon" type="image/png" href="images/ocaml.png" />|}]
    |> Tyxml_js.To_dom.of_element |> Dom_html.CoerceTo.link |> Js.Opt.to_option |> Option.get
  in
  Dom.appendChild Dom_html.window##.document##.head link;
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
       | false -> link##.href := Js.string "images/ocaml.png"
       | true -> link##.href := Js.string "images/ocaml-blue.png")
  |> ignore

let jsx_of_tab db gsignal set_gsignal fire_toast i state =
  (* TODO: can this be turned to a constructor to avoid rebinding on each render? *)
  let open Reactjs.Jsx in
  let k = string_of_int i in
  let spinner =
    let open Types in
    match state with
    | Creating_network -> []
    | Creating_training _ -> []
    | Selecting_backend _ -> []
    | Evaluating _ | Training _ ->
        [
          of_string "\u{a0}";
          of_bootstrap "Spinner" ~animation_string:"border" ~variant:"primary" ~size:"sm" [];
        ]
  in

  let tabshownsignal =
    React.S.fmap (function Loaded (_, j, _) -> Some j | _ -> None) 0 gsignal
    |> React.S.map (fun j -> j = i)
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
  of_constructor Tab.construct_tab (db, tabshownsignal, i, signal, set_signal, fire_toast)
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

let create_toast_frp_primitives () =
  (* TODO: move toasts out *)
  let add_toast_events, fire_toast = React.E.create () in
  let rm_toast_events, water_toast = React.E.create () in
  let water_toast : string -> unit = water_toast in
  let toast_count = ref 0 in

  let add_toast_events =
    React.E.map
      (fun data ->
        let id = string_of_int !toast_count in
        incr toast_count;
        `Add (id, data))
      add_toast_events
  in
  let rm_toast_events = React.E.map (fun id -> `Rm id) rm_toast_events in

  let toast_signal =
    React.E.select [ add_toast_events; rm_toast_events ]
    |> React.S.fold
         (fun s ev ->
           match ev with `Add (id, data) -> (id, data) :: s | `Rm id -> List.remove_assoc id s)
         []
  in
  (toast_signal, fire_toast, water_toast)

let construct_mnist_jsoo _ =
  Printf.printf "> Component - mnist_jsoo | construct\n%!";
  let signal, set_signal = React.S.create ~eq:states_equal Loading in
  let toast_signal, fire_toast, water_toast = create_toast_frp_primitives () in
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
    Printf.printf "> Component - mnist_jsoo | render\n%!";
    let open Reactjs.Jsx in
    let toasts =
      toast_signal |> React.S.value
      |> List.map (fun (id, data) -> of_render ~key:id render_toast (id, data, water_toast))
      |> of_tag "div" ~classes:[ "toast-holder" ]
    in
    let res = of_constructor ~key:"res" Resources.construct_resources fire_resources in
    let tabs =
      (* The `Tab` elements must be direct children of `Tabs`. It won't work if anything is
         in-between (like a constructor)
      *)
      match React.S.value signal with
      | Loading -> of_string ""
      | Loaded (db, focusidx, tabstates) ->
          let focusidx = string_of_int focusidx in
          let head =
            of_bootstrap "Tab" ~title_jsx:(of_string "Networks") ~event_key:"head" ~disabled:true []
          in
          let tabs =
            Array.mapi (jsx_of_tab db signal set_signal fire_toast) tabstates |> Array.to_list
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
  Reactjs.construct ~signal ~signal:toast_signal ~mount render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in
  let div = [%html "<div></div>"] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Ft_js.import_css "mnist-jsoo.css" >>= fun () ->
  Reactjs.render (Reactjs.Jsx.of_constructor construct_mnist_jsoo ()) div;
  Lwt.return ()
