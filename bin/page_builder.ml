(* Website and webworkers entry point *)
open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
  module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
  module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Webworker = Ft_js.Webworker
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

let display x =
  let body = Dom_html.window##.document##.body in
  Dom.appendChild body (Tyxml_js.To_dom.of_element x)

let main () =
  let open Lwt.Infix in
  (* Printexc.record_backtrace true; *)
  let header =
    [%html
      "<div id='header'>" "<a href='index.html'>&#127968; Homepage</a> | "
        "<a href='cinquante.html'>&#x1f3ae; Cinquante</a> | "
        "<a href='mnist-jsoo.html'>&#x1f4bb; MNIST Js_of_ocaml</a> | "
        "<a href='about.html'>&#128196; Making-of</a>" "</div>"]
  in
  let error exn =
    Printf.eprintf "> Exception:\n%!";
    Firebug.console##log exn;

    let msg = Printexc.to_string exn in
    let stack = Printexc.get_backtrace () in
    Printf.sprintf "Exception raised: `%s`. Stacktrace: %s" msg stack |> Html.txt |> display;

    Lwt.return ()
  in
  let main () =
    Printf.printf "Hello\n%!";
    Js_of_ocaml_lwt.Lwt_js_events.onload () >>= fun _ ->
    Printf.printf "Hello (onload)\n%!";
    display header;

    let pagename =
      Dom_html.window##.location##.pathname
      |> Js.to_string |> Filename.basename |> Filename.remove_extension
    in
    match pagename with
    | "index" | "/" ->
        display @@ Index.create_content ();
        Lwt.return ()
    | "mnist-jsoo" -> Mnist_jsoo.main ()
    | "about" ->
        display @@ About.create_content ();
        Lwt.return ()
    | _ ->
        Printf.sprintf "Unknown page: %s!" pagename |> Html.txt |> display;
        Lwt.return ()
  in
  Lwt.catch main error |> ignore

let () = if not Ft_js.Webworker.is_web_worker then main () else Webworker.prime ()
