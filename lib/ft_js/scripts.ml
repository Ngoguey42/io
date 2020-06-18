open struct
  module Js = Js_of_ocaml.Js
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
  module Worker = Js_of_ocaml.Worker
  module Dom_html = Js_of_ocaml.Dom_html
  module Firebug = Js_of_ocaml.Firebug
end

type entry =
  [ `Pagebuilder
  | `Tfjs
  | `Cryptojs
  | `Pako
  | `Reactjs
  | `Reactjsbootstrap
  | `Plotly
  | `Highlightjs ]

let type_of_url url =
  match Filename.extension url with
  | ".js" -> `Js
  | ".css" -> `Css
  | _ -> Printf.sprintf "Unknown type_of_url of %s" url |> failwith

let urls_of_entry : ?what:[ `Js | `Css | `Both ] -> entry -> string list list =
 fun ?(what = `Both) entry ->
  let is_url_accepted url =
    match (what, type_of_url url) with
    | `Js, `Css -> false
    | `Css, `Js -> false
    | `Js, `Js | `Css, `Css -> true
    | `Both, `Css | `Both, `Js -> true
  in
  let root = Dom_html.window##.location##.href |> Js.to_string |> Misc.origin_of_url in
  ( match entry with
  | `Tfjs -> [ [ "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs@2.0.0/dist/tf.min.js" ] ]
  | `Cryptojs ->
      [
        [ "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/core.min.js" ];
        [
          "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/sha1.min.js";
          "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/sha256.min.js";
        ];
      ]
  | `Pako -> [ [ "https://cdnjs.cloudflare.com/ajax/libs/pako/1.0.10/pako_inflate.min.js" ] ]
  | `Reactjs ->
      [
        [ "https://unpkg.com/react@16/umd/react.development.js" ];
        [ "https://unpkg.com/react-dom@16/umd/react-dom.development.js" ];
      ]
  | `Reactjsbootstrap ->
      [
        [
          "https://cdnjs.cloudflare.com/ajax/libs/react-bootstrap/1.0.1/react-bootstrap.min.js";
          "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css";
        ];
      ]
  | `Pagebuilder -> [ [ Filename.concat root "build/default/bin/page_builder.bc.js" ] ]
  | `Highlightjs ->
      [
        [
          "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.0.3/styles/default.min.css";
          "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.0.3/highlight.min.js";
        ];
        [ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.0.3/languages/ocaml.min.js" ];
      ]
  | `Plotly -> [ [ "https://cdn.plot.ly/plotly-1.54.3.min.js" ] ] )
  |> List.map (List.filter is_url_accepted)
  |> List.filter (fun l -> List.length l > 0)

let import ?(what = `Both) entry =
  let open Lwt.Infix in
  let rec aux = function
    | [] -> Lwt.return ()
    | urls :: tl ->
        List.map
          (fun url ->
            match type_of_url url with `Js -> Misc.import_js url | `Css -> Misc.import_css url)
          urls
        |> Lwt.join
        >>= fun () -> aux tl
  in
  aux (urls_of_entry ~what entry)
