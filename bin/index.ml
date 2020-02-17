
module M0 = struct
  (* Using Tyxml, seems mostly incompatible with js_of_ocaml *)
  module Html = Tyxml.Html

  let a0 =
    let open Html in
    a ~a:[a_href "ocaml.org"] [txt "monlien"]

  let a1 =
    (* `let%html` uses the `Html` module visible *)
    let%html c = "<a href='ocaml.org'>monlien</a>" in
    c

  let _ =
    Format.asprintf "%a" (Html.pp_elt ()) a0
    |> Printf.printf "a0: %s\n%!";
    Format.asprintf "%a" (Html.pp_elt ()) a1
    |> Printf.printf "a1: %s\n%!";

end
module M1 = struct
  (* Using js_of_ocaml *)

  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom

  module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
  module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug

  let document = Dom_html.window##.document
  let body = Dom_html.window##.document##.body

  let a0 =
    let open Html in
    a ~a:[a_href "ocaml.org"] [txt "monlien"]
  let a0_js_a = Tyxml_js.To_dom.of_a a0
  let a0_js_elt = Tyxml_js.To_dom.of_element a0

  let a1 =
    (* `let%html` uses the `Html` module visible *)
    let%html c = "<a href='ocaml.org'>monlien</a>" in
    c
  let a1_js_a = Tyxml_js.To_dom.of_a a1
  let a1_js_elt = Tyxml_js.To_dom.of_element a1

  let a2 =
    let a = Dom_html.createA document in
    a##.textContent := Js.Opt.return (Js.string "monlien");
    a##.href := Js.string "ocaml.org";
    a

  let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x)

  let _ =
    (* There is no `Html.pp_elt` available here, let's use console.log *)
    Printf.printf "a0\n%!";
    Firebug.console##log a0_js_a;
    Firebug.console##log a0_js_elt;

    Printf.printf "a1\n%!";
    Firebug.console##log a1_js_a;
    Firebug.console##log a1_js_elt;

    Printf.printf "a2\n%!";
    Firebug.console##log a2

  let onload _ =
    Printf.eprintf "onload\n%!";
    display a0;
    display [%html "<br/>"];
    display a1;
    display [%html "<br/>"];
    Dom.appendChild body a2;
    Js_of_ocaml.Js._false

  let _ =
    Printf.eprintf "placing onload hook\n%!";
    Dom_html.window##.onload := Dom_html.handler onload

end
