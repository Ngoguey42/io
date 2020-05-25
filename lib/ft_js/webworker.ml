open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
  module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Worker = Js_of_ocaml.Worker
end

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

let is_web_worker = not (Js.Unsafe.global##.window == Js.Unsafe.global##.self)

let current_script =
  Js.Unsafe.global##.document |> Js.Optdef.to_option
  |> Fun.flip Option.bind (fun o -> o##.currentScript |> Js.Opt.to_option)

module Make (Spec : SPEC) : S with type in_msg = Spec.in_msg and type out_msg = Spec.out_msg =
struct
  type in_msg = Spec.in_msg

  type out_msg = Spec.out_msg

  type t = (in_msg, out_msg) Worker.worker Js.t

  let idx =
    let idx = Hashtbl.length store in
    Hashtbl.add store idx (fun () ->
        Worker.set_onmessage (Spec.create_on_in_message ());
        ());
    idx

  let create on_out_message on_error =
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

    let url =
      match current_script with
      | None -> failwith "In Webworker.Make(...).create: No target url"
      | Some tag -> Js.to_string tag##.src
    in
    let w = Worker.create url in
    w##.onmessage := on_out_message;
    w##.onerror := on_error;
    Js.Unsafe.meth_call w "postMessage" [| Js.Unsafe.inject idx |] |> ignore;

    w

  let post_in_message w msg = w##postMessage msg

  let post_out_message msg = Worker.post_message msg

  let terminate w = w##terminate
end

let _on_initial_in_message idx =
  match Hashtbl.find_opt store idx with None -> failwith "Unknown worker type" | Some f -> f ()

let prime () =
  Worker.set_onmessage _on_initial_in_message;
  ()
