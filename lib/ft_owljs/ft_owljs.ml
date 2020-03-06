module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array

(* module MyHash = Ft_js.CryptoJs.MakeHash(struct let algo = `SHA1 end) *)
(* module I = *)
(*   Irmin.Make (Irmin_indexeddb.Content_store) (Irmin_indexeddb.Branch_store) (Irmin.Metadata.None) *)
(*     (Irmin.Contents.String) *)
(*     (Irmin.Path.String_list) *)
(*     (Irmin.Branch.String) *)
(*     (MyHash) *)

(* module I = Ft.Irmin_mem.Make *)
(*              (Irmin.Metadata.None) *)
(*              (Irmin.Contents.String) *)
(*              (Irmin.Path.String_list) *)
(*              (Irmin.Branch.String) *)
(*              (MyHash) *)
(* let _info msg () = *)
(*   let date = Int64.of_float (Unix.gettimeofday ()) in *)
(*   let author = "No one" in *)
(*   Irmin.Info.v ~date ~author msg *)
