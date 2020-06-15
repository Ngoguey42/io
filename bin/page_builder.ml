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

let zero_url =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABwAAAAcCAYAAAByDd+UAAAC10lEQVRIS+2Wz0sqURTHv0abAjcJ/bDcWYQiFIER4qa1IK4qCFooZIqYUKt+ULRSkBZBoOhCULJFi8DEIJSgbUqgoP0BFe6iJErLx7kw8waZGSeIx1t0QNS5597PPb9HBaCNfyiqX+BPe/v/dGlvby9cLhfGx8eZwa+vr4jFYqjX63h/f/+WExRZuLe3h+3tbf5glUqFdruNQqGAq6sr9rm9vVUE7gpcWlpCMplkAE44IPe/2WyiWCzi9PQU19fXuLu7k4R3BVYqFUxOTsoChRcgd5+cnGBtbU0UKgs8OjqCx+NBT08Pvr6++APo98PDA9LpNLLZLLNKq9ViYWEBgUAAY2NjbN3hcKBUKqHVav31jlThq9Vq3NzcwGg0gix4eXlBIpHAzMwMLi8vcXBwIGrB4uIiUqkU7xGv14tIJNIduLKygng8zhQJuL6+DrJYiRCQrCW5uLiA3W6XB5J7arUa+vr6mOLT0xMribe3NyU8hMNh+P1+pkvJZDab5YHLy8vMfZwQ3GAwKIKRUjQahdPpZPqHh4fY2NiQBwpvSJqbm5tsoxKx2Ww4Pz/nY0j7aD9fUmJJ0wnU6XR4fHxUwsPx8TFWV1e/lzTUQaxWq2gM5Ki7u7vY2toCtUJqFPf395ibm8Pz87O8hVRnXGfpjIEU0GQyIZfLYXh4mGU19ViLxcKSRiiihf/5+SkZAzEgwTKZDEZHR9kytToqI2H9ycawWq1Cr9fzWSYMeieQ3EgxGxoa4pfcbjebJmIiaqEwaTqzjDuEspEmyPT0NB8zWqNWSGUhJaLA/f19fhzRTckCkv7+fmg0Guzs7PB1Rs+5mPl8PknLZF06ODiIcrmMgYEBpnd2dsa+qSnPzs4ygHBc0TwMBoNsPnYTyWkxMTHBRgxNerJMbB7m83k2fEOhUDeOfFkId4+MjGB+fh5TU1P840ajwb9ifHx8KIYx9/++l37LXwqU/wCEfmAQBQllMAAAAABJRU5ErkJggg=="

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
  let mnist =
    let icon = [%html {|<img class="mnist-icon" alt="0" src="|} zero_url {|"/>|}] in
    if from = `Mnist then [%html "<span>" [ icon ] "mnist-jsoo</span> | "]
    else [%html "<a href='mnist-jsoo.html'>" [ icon ] "mnist-jsoo</a> | "]
  in
  let about =
    if from = `About then
      [%html "<span><span style='margin-right: 4px;'>&#128196;</span>Making-of</span>"]
    else
      [%html
        "<a href='about.html'><div style='display: inline-block; margin-right: \
         4px;'>&#128196;</div>Making-of</a>"]
  in
  display [%html "<div id='header'>" (blog @ mnist @ [ about ]) "</div>"]

let main () =
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
