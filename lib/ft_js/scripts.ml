open struct
  module Js = Js_of_ocaml.Js
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
  module Worker = Js_of_ocaml.Worker
  module Dom_html = Js_of_ocaml.Dom_html
  module Firebug = Js_of_ocaml.Firebug
end

type entry = [ `Pagebuilder | `Tfjs | `Cryptojs | `Pako | `Reactjs ]

external import_js : Js.js_string Js.t -> unit Misc.promise Js.t = "ft_js_import"

let import_js name =
  if Webworker.is_web_worker then (
    Worker.import_scripts [ name ];
    Lwt.return () )
  else name |> Js.string |> import_js |> Misc.wrap_promise

let urls_of_entry : entry -> string list = function
  | `Tfjs ->
      [
        "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs@1.7.3/dist/tf.min.js";
        "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs-backend-wasm@1.7.3/dist/tf-backend-wasm.min.js";
      ]
  | `Cryptojs ->
      [
        "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/core.min.js";
        "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/sha1.min.js";
        "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/sha256.min.js";
      ]
  | `Pako -> [ "https://cdnjs.cloudflare.com/ajax/libs/pako/1.0.10/pako_inflate.min.js" ]
  | `Reactjs ->
      [
        "https://unpkg.com/react@16/umd/react.development.js";
        "https://unpkg.com/react-dom@16/umd/react-dom.development.js";
      ]
  | `Pagebuilder ->
      [
        Filename.concat
          (Misc.origin_of_url (Dom_html.window##.location##.href |> Js.to_string))
          "build/default/bin/page_builder.bc.js";
      ]

let import entry =
  let open Lwt.Infix in
  let rec aux = function [] -> Lwt.return () | hd :: tl -> import_js hd >>= fun () -> aux tl in
  aux (urls_of_entry entry)

let import_sync entry =
  if not Webworker.is_web_worker then failwith "From import_sync: Only works from webworker";
  Worker.import_scripts (urls_of_entry entry)
