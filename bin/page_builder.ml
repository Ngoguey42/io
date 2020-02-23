module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

let document = Dom_html.window##.document

let body = Dom_html.window##.document##.body

let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x)
let header =
  [%html
      "<div style='text-align:center;margin-bottom: 25px;'>"
      "<a href='index.html'>&#127968; Homepage</a> | "
      "<a href='lrcraft-game.html'>&#x1f3ae; Learning Rate Craft</a> | "
      "<a href='about.html'>&#128196; Making-of</a>" "</div>"]

let _ =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= fun _ ->

  display header;

  let pagename =
    Dom_html.window##.location##.pathname
    |> Js.to_string |> Filename.basename |> Filename.remove_extension
  in
  match pagename with
  | "index" | "/" -> display @@ Index.create_content (); Lwt.return ()
  | "lrcraft-game" -> Lrcraft.main ()
  | "about" -> display @@ About.create_content (); Lwt.return ()
  | _ -> Printf.sprintf "Unknown page: %s!" pagename |> Html.txt |> display; Lwt.return ()
