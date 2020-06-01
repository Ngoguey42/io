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

let construct_reactjs_snippet () =
  Printf.printf "> Component - reactjs_snippet | construct\n%!";
  let render () =
    let open Reactjs.Jsx in
    Printf.printf "> Component - reactjs_snippet | render\n%!";
    let head = of_string "Reactjs wrapper" in
    let head = head >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in

    let body = of_constructor Reactjs_snip.construct_component () in
    (* let body = of_string "COOODE!" in *)
    let body = body >> of_tag "th" >> of_tag "tr" >> of_tag "tbody" in

    [ head; body ] |> of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm"
  in
  Reactjs.construct render

let construct_snippets () =
  Printf.printf "> Component - snippets | construct\n%!";
  let render () =
    let open Reactjs.Jsx in
    Printf.printf "> Component - snippets | render\n%!";
    [ Reactjs.Jsx.of_constructor construct_reactjs_snippet () ]
    |> of_bootstrap "Col" >> of_bootstrap "Row"
    >> of_bootstrap "Container" ~fluid:true ~classes:[ "bigbox1" ]
  in
  Reactjs.construct render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in

  let div =
    [%html {|<div class="bigbox-title"><h1>Snippets</h1></div>|}] |> Tyxml_js.To_dom.of_element
  in
  Dom.appendChild body div;

  let div = [%html "<div></div>"] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Reactjs.render (Reactjs.Jsx.of_constructor construct_snippets ()) div;
  Lwt.return ()
