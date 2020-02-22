module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

let _outline = "text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;"

let wrap_promise p =
  let res, w = Lwt.wait () in
  let callback res =
    Lwt.wakeup w res;
    Js._true
  in
  let callback = Dom.handler callback in
  ignore @@ Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject callback |];
  res

let decompress_blob (way: string) b =
  let ds = Js.Unsafe.global##.DecompressionStream in
  let ds = Js.Unsafe.new_obj ds [| Js.string way |> Js.Unsafe.inject |] in

  let rs = Js.Unsafe.meth_call b "stream" [| |] in
  let rs = Js.Unsafe.meth_call rs "pipeThrough" [| ds |] in

  let resp = Js.Unsafe.new_obj Js.Unsafe.global##.Response [| rs |] in
  let resp = Js.Unsafe.meth_call resp "blob" [| |] in

  wrap_promise resp

let blob_of_url: ?progress:(int -> int -> unit) -> string -> 'a = fun ?progress url ->
  let open Lwt.Infix in
  let f = Js_of_ocaml_lwt.XmlHttpRequest.perform_raw ~response_type:Js_of_ocaml.XmlHttpRequest.Blob in
  let future = match progress with
    | Some progress -> f ~progress url
    | None -> f url
  in
  future >|= fun resp ->

  if resp.code <> 200 then
    failwith @@ Printf.sprintf "Download failed with code %d" resp.code;
  match Js.Opt.to_option resp.content with
  | None -> failwith "What?"
  | Some blob -> blob

let binary_string_of_blob b =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.File.readAsBinaryString b >|= fun s ->
  Js.to_bytestring s

let create_softmax_div probas =
  let open Html in
  let rec aux l i =
    match l with
    | x :: tail ->
        let c = Ft.Color.Jet.get x in
        let c = Ft.Color.to_hex_string c in
        ignore c;
        let t = Printf.sprintf "%.0f%%" (x *. 100.) in
        let t = [ i |> string_of_int |> Html.txt; br (); Html.txt t ] in
        let style =
          "width: 34px; height: 34px; text-align: center; color: white;" ^ _outline
          ^ "font-size: small;"
        in
        let style = Printf.sprintf "%s; background: %s" style c in
        let elt = td ~a:[ a_style style ] t in
        elt :: aux tail (i + 1)
    | [] -> []
  in
  table @@ [ tr @@ aux probas 0 ]
