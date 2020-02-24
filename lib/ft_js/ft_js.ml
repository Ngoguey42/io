module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

let _outline = "text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;"

module Idb = Irmin_indexeddb_raw_noutf

module CryptoJs = struct

  class type js_hash = object
    method toString : Js.js_string Js.t Js.meth
  end

  class type hasher = object
    method update : Js.js_string Js.t -> unit Js.meth
    method finalize : js_hash Js.t Js.meth
  end

  module type HashConf = sig
    val algo: [`SHA256 | `SHA1]
  end

  module MakeHash (Conf: HashConf) : Irmin.Hash.S = struct
    type t = string

    let hash_size =
      match Conf.algo with
      | `SHA256 -> 32
      | `SHA1 -> 20

    let _create_hasher: unit -> hasher Js.t = fun () ->
      match Conf.algo with
      | `SHA256 ->
         let ctor = Js.Unsafe.global##.CryptoJS##.algo##.SHA256 in
         Js.Unsafe.meth_call ctor "create" [| |]
      | `SHA1 ->
         let ctor = Js.Unsafe.global##.CryptoJS##.algo##.SHA1 in
         Js.Unsafe.meth_call ctor "create" [| |]

    let short_hash : t -> int = fun h ->
      let l = Ft.List.chunk 8 h in
      let l = List.map (fun x -> Scanf.sscanf x "%x" (fun x -> x)) l in
      let i = List.fold_left (lxor) 0 l in
      i

    let hash f =
      let h = _create_hasher () in
      let aux s =
        h##update (Js.string s)
      in
      f aux;
      h##finalize##toString |> Js.to_string

    let t = Irmin.Type.string
  end

end


let wrap_promise p =
  let lwt, lwt' = Lwt.wait () in
  let callback res =
    Lwt.wakeup lwt' res;
    Js._true
  in
  Js.Unsafe.meth_call p "then" [| callback |> Dom.handler |> Js.Unsafe.inject |] |> ignore;
  lwt

let decompress_array arr =
  (* Has a JavaScript dependency *)
  (* let open Lwt.Infix in *)
  let arr : 'a Js.js_array Js.t =
    Js.Unsafe.fun_call Js.Unsafe.global##.pako##.ungzip [| arr |> Js.Unsafe.inject |]
  in
  arr

let decompress_blob (way : string) b =
  (* Only implemented on chrome as of feb2020 *)
  let ds = Js.Unsafe.global ##. DecompressionStream in
  let ds = Js.Unsafe.new_obj ds [| Js.string way |> Js.Unsafe.inject |] in

  let rs = Js.Unsafe.meth_call b "stream" [||] in
  let rs = Js.Unsafe.meth_call rs "pipeThrough" [| ds |] in

  let resp = Js.Unsafe.new_obj Js.Unsafe.global ##. Response [| rs |] in
  let resp = Js.Unsafe.meth_call resp "blob" [||] in

  wrap_promise resp

let blob_of_url ?progress url =
  let open Lwt.Infix in
  let f =
    Js_of_ocaml_lwt.XmlHttpRequest.perform_raw ~response_type:Js_of_ocaml.XmlHttpRequest.Blob
  in
  let future = match progress with Some progress -> f ~progress url | None -> f url in
  future >|= fun resp ->
  if resp.code <> 200 then failwith @@ Printf.sprintf "Download failed with code %d" resp.code;
  match Js.Opt.to_option resp.content with None -> failwith "What?" | Some blob -> blob

let array_of_url ?progress url =
  let open Lwt.Infix in
  (match progress with None -> blob_of_url url | Some progress -> blob_of_url ~progress url)
  >>= fun blob ->
  let resp = Js.Unsafe.new_obj Js.Unsafe.global ##. Response [| blob |> Js.Unsafe.inject |] in
  Js.Unsafe.meth_call resp "arrayBuffer" [||] |> wrap_promise

let binary_string_of_blob b =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.File.readAsBinaryString b >|= fun s -> Js.to_bytestring s

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
