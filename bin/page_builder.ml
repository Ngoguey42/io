module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Webworker = Ft_js.Webworker
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

let display x =
  let body = Dom_html.window##.document##.body in
  Dom.appendChild body (Tyxml_js.To_dom.of_element x)

module rec Stringifier : (Webworker.S with type in_msg := [ `A | `B ] and type out_msg := string) =
Webworker.Make (struct
  type in_msg = [ `A | `B ]

  type out_msg = string

  let create_on_in_message () =
    let open Lwt.Infix in
    Printf.eprintf "> Worker-routine : init \n%!";
    let events, fire_event = React.E.create () in
    let fire_event : 'a -> 'b = fire_event in
    let next_event = Lwt_react.E.next events in

    let routine () =
      Ft_js.Scripts.import `Tfjs >>= fun () ->
      Printf.eprintf "> Worker-routine : imported tfjs \n%!";
      Ft_js.Scripts.import `Cryptojs >>= fun () ->
      Printf.eprintf "> Worker-routine : imported cryp\n%!";
      Ft_js.Scripts.import `Pako >>= fun () ->
      Printf.eprintf "> Worker-routine : imported pako\n%!";
      Ft_js.Scripts.import `Reactjs >>= fun () ->
      Printf.eprintf "> Worker-routine : imported react\n%!";

      next_event >>= fun _ev ->
      Printf.eprintf "> Worker-routine : on message async\n%!";
      Stringifier.post_out_message "coucou";

      Lwt.return ()
    in
    Lwt_js_events.async routine;

    (* let on_in_msg msg = *)
    (*   Printf.eprintf "> Worker-routine : on message \n%!"; *)
    (*   fire_event msg *)
    (* in *)
    (* on_in_msg *)
    fire_event
end)

let main () =
  let open Lwt.Infix in
  let header =
    [%html
      "<div id='header'>" "<a href='index.html'>&#127968; Homepage</a> | "
        "<a href='lrcraft-game.html'>&#x1f3ae; Learning Rate Craft (wip)</a> | "
        "<a href='about.html'>&#128196; Making-of (wip)</a>" "</div>"]
  in

  (* let ww = *)
  (*   Stringifier.create *)
  (*     (fun msg -> Printf.eprintf "> Main : got out message <%s>\n%!" msg) *)
  (*     (fun err -> *)
  (*       let msg = (Js.Unsafe.coerce err)##.message |> Js.to_string in *)
  (*       Printf.eprintf "> Main : got error msg:%s\n%!" msg) *)
  (* in *)
  (* Stringifier.post_in_message ww `A; *)
  (* Stringifier.post_in_message ww `B; *)
  let error exn =
    let msg = Printexc.to_string exn in
    let stack = Printexc.get_backtrace () in
    let err = Printf.sprintf "there was an error: %s %s" msg stack in
    print_endline err;
    err |> Html.txt |> display;
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
    | "lrcraft-game" -> Lrcraft.main ()
    | "about" ->
        display @@ About.create_content ();
        Lwt.return ()
    | _ ->
        Printf.sprintf "Unknown page: %s!" pagename |> Html.txt |> display;
        Lwt.return ()
  in
  Lwt.catch main error |> ignore

let () = if not Ft_js.Webworker.is_web_worker then main () else Webworker.prime ()
