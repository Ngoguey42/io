module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Worker = Js_of_ocaml.Worker

module type SPEC = sig
  type in_msg

  type out_msg

  val create_on_in_message : unit -> in_msg -> unit
end

module type S = sig
  type t

  type in_msg

  type out_msg

  val create : (out_msg -> unit) -> (Worker.errorEvent Js.t -> unit) -> t

  val post_in_message : t -> in_msg -> unit

  val post_out_message : out_msg -> unit

  val terminate : t -> unit
end

let store : (int, unit -> unit) Hashtbl.t = Hashtbl.create 10

module Make (Spec : SPEC) : S with type in_msg = Spec.in_msg and type out_msg = Spec.out_msg =
struct
  type in_msg = Spec.in_msg

  type out_msg = Spec.out_msg

  type t = (in_msg, out_msg) Worker.worker Js.t

  let idx =
    let idx = Hashtbl.length store in
    Printf.eprintf "> Both : Allocated idx:%d for new worker type\n%!" idx;
    Hashtbl.add store idx (fun () ->
        Printf.eprintf "> Worker : Setting up new on_in_message\n%!";
        Worker.set_onmessage (Spec.create_on_in_message ());
        ());
    idx

  let create on_out_message on_error =
    Printf.eprintf "> Main : creating web worker\n%!";
    let on_out_message =
      Dom.handler (fun ev ->
          on_out_message ev##.data;
          Js._true)
    in
    let on_error =
      Dom.handler (fun ev ->
          on_error ev;
          Js._true)
    in

    let url = Js.Unsafe.global##.document##.currentScript##.src |> Js.to_string in
    Printf.eprintf "    url: %s\n%!" url;
    let w = Worker.create url in
    w##.onmessage := on_out_message;
    w##.onerror := on_error;
    Js.Unsafe.meth_call w "postMessage" [| Js.Unsafe.inject idx |] |> ignore;
    ignore (on_out_message, on_error);
    w

  let post_in_message w msg =
    Printf.eprintf "> Main : post in message\n%!";
    w##postMessage msg;
    ()

  let post_out_message msg =
    Printf.eprintf "> Worker : post out message\n%!";
    Worker.post_message msg;
    ()

  let terminate w =
    w##terminate;
    ()
end

let is_web_worker = not (Js.Unsafe.global##.window == Js.Unsafe.global##.self)

let _on_initial_in_message idx =
  Printf.eprintf "> Worker : On initial message idx:%d\n%!" idx;
  match Hashtbl.find_opt store idx with None -> failwith "Unknown worker type" | Some f -> f ()

let prime () =
  Printf.eprintf "> Worker : Setting up initial on_in_message\n%!";
  Worker.set_onmessage _on_initial_in_message;
  ()
