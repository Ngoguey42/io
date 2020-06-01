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

let t0 =
  {|
<h1>Reactjs wrapper</h1>
<p>
  <a href="https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib/ft_js/reactjs.ml">
  This small wrapper</a>
  enhances the <cite>Reactjs</cite> experience in OCaml by hiding most of the
  boilerplate.
</p>
|}

let construct_reactjs_snippet () =
  Printf.printf "> Component - reactjs_snippet | construct\n%!";
  let render () =
    let open Reactjs.Jsx in
    Printf.printf "> Component - reactjs_snippet | render\n%!";
    let to_table title content =
      let head = of_string title >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
      let body = content >> of_tag "th" >> of_tag "tr" >> of_tag "tbody" in
      [ head; body ] |> of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm"
    in

    let snip_jsx = of_constructor Reactjs_ex0.construct_component () in
    let snip_code = of_constructor Misc.construct_snippet_code "reactjs_ex0.ml" in

    (* let title = *)
    (*   of_string "Reactjs wrapper" >> of_tag "h1" >> of_tag "div" ~classes:[ "bigbox-title" ] *)
    (* in *)
    let box =
      [
        of_tag "div" ~inner_html:t0 [];
        to_table "Example: Code" snip_code;
        to_table "Example: Result" snip_jsx;
      ]
      |> of_bootstrap "Col" >> of_bootstrap "Row"
      >> of_bootstrap "Container" ~fluid:true ~classes:[ "bigbox1" ]
    in
    of_react "Fragment" [ box ]
  in

  Reactjs.construct render
