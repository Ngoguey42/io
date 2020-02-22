module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

(* let document = Dom_html.window##.fetch *)
let hello =
  let open Lwt.Infix in
  let url = "https://cors-anywhere.herokuapp.com/http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz" in
  (* let v = Js.Unsafe.global##.fetch @@ Js.string "https://cors-anywhere.herokuapp.com/http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.g" in *)

  let progress i j =
    Printf.eprintf "On progress: %d/%d\n%!" i j
  in

  Ft_js.blob_of_url ~progress url >>= fun blob ->
  Ft_js.decompress_blob "gzip" blob >>= fun blob ->
  Ft_js.binary_string_of_blob blob >|= fun s ->
  Printf.eprintf "%d\n%!" @@ String.length s;
  s
