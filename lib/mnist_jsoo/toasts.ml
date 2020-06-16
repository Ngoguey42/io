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

let create_frp_primitives () =
  let add_toast_events, fire_toast = React.E.create () (* collected when `unmount` is called *) in
  let rm_toast_events, water_toast = React.E.create () (* collected when `unmount` is called *) in
  let water_toast : string -> unit = water_toast in
  let toast_count = ref 0 in

  let unmount () =
    React.E.stop ~strong:true add_toast_events;
    React.E.stop ~strong:true rm_toast_events
  in

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
  (toast_signal, fire_toast, water_toast, unmount)

let render_toast (id, (title, body), water_toast) =
  let open Reactjs.Jsx in
  let on_close () = water_toast id in
  let header =
    of_string title >> of_tag "strong" ~classes:[ "mr-auto" ] >> of_bootstrap "Toast.Header"
  in
  let body = of_string body >> of_bootstrap "Toast.Body" in
  of_bootstrap "Toast" ~on_close ~animation_bool:false [ header; body ]

let construct_toasts (toast_signal, water_toast) =
  Printf.printf "$  toasts | construct\n%!";
  let render _ =
    Printf.printf "$$ toasts | render\n%!";
    let open Reactjs.Jsx in
    toast_signal |> React.S.value
    |> List.map (fun (id, data) -> of_render ~key:id render_toast (id, data, water_toast))
    |> of_tag "div" ~classes:[ "toast-holder" ]
  in
  Reactjs.construct ~signal:toast_signal render
