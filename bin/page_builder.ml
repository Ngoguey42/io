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

let put_header from =
  let blog =
    let icon =
      [%html {|<img class="homepage-icon" alt="" src="images/ocaml_logo2_release.svg"/>|}]
    in
    if from = `Blog then [%html "<span>" [ icon ] "Blog</span> | "]
    else [%html "<a href='index.html'>" [ icon ] "Blog</a> | "]
  in
  let about =
    if from = `About then
      [%html "<span><span style='margin-right: 4px;'>&#128196;</span>Making-of</span>"]
    else
      [%html
        "<a href='about.html'><div style='display: inline-block; margin-right: \
         4px;'>&#128196;</div>Making-of</a>"]
  in
  display [%html "<div id='header'>" (blog @ [ about ]) "</div>"]

let main () =
  let i = 42 in
  let open Lwt.Infix in
  (* Printexc.record_backtrace true; *)
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

    let pagename =
      Dom_html.window##.location##.pathname
      |> Js.to_string |> Filename.basename |> Filename.remove_extension
    in

    match pagename with
    | "index" | "/" ->
        put_header `Blog;
        Index.main ()
    | "mnist-jsoo" ->
        put_header `Mnist;
        Mnist_jsoo.main ()
    | "reactjs-wrapper" ->
        put_header `Other;
        Articles.A0_reactjs_wrapper.main ()
    | "ocann0" ->
        put_header `Other;
        Articles.A1_ocann0.main ()
    | "about" ->
        put_header `About;
        display @@ About.create_content ();
        Lwt.return ()
    | _ ->
        put_header `Other;
        Printf.sprintf "Unknown page: %s!" pagename |> Html.txt |> display;
        Lwt.return ()
  in
  Lwt.catch main error |> ignore

let () = if not Ft_js.Webworker.is_web_worker then main () else Webworker.prime ()
