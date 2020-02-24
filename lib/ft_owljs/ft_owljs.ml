module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

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

module Mnist = struct
  let entries = [ `Train_imgs; `Train_labs; `Test_imgs; `Test_labs ]

  let filename_of_entry = function
    | `Train_imgs -> "train-images-idx3-ubyte"
    | `Train_labs -> "train-labels-idx1-ubyte"
    | `Test_imgs -> "t10k-images-idx3-ubyte"
    | `Test_labs -> "t10k-labels-idx1-ubyte"

  let url_of_entry entry =
    let prefix = "https://cors-anywhere.herokuapp.com/" in
    let prefix' = "http://yann.lecun.com/exdb/mnist/" in
    let suffix = ".gz" in
    Printf.sprintf "%s%s%s%s" prefix prefix' (filename_of_entry entry) suffix

  let _create_status_div () =
    let open Html in
    let rec aux = function
      | [] -> []
      | entry :: tl ->
          let n = filename_of_entry entry in
          let a0 = [ a_style "height: 25px;" ] in
          let a1 = [ a_style "text-align: right;" ] in
          let a2 = [ a_id n; a_style "text-align: center; width: 175px;" ] in
          let elt = tr ~a:a0 [ th ~a:a1 [ txt n ]; th ~a:a2 [ txt "unknown" ] ] in
          elt :: aux tl
    in
    let thead =
      [%html
        "<thead style='background: #EBEBEB'><tr><th colspan='2'>" "MNIST dataset status"
          "</th></tr></thead>"]
    in

    let attrs = [ a_style "border: 1px solid black; border-radius: 3px;" ] in
    table ~a:attrs ~thead @@ aux entries

  let _status_div = lazy (Tyxml_js.To_dom.of_element @@ _create_status_div ())

  let status_div () = Lazy.force _status_div

  let _entry_status_div entry =
    let status_div = status_div () in
    let n = filename_of_entry entry in
    let elt = status_div##querySelector (Js.string ("#" ^ n)) in
    Js.coerce_opt elt Dom_html.CoerceTo.th (fun _ -> assert false)

  let _update_entry_status entry new_msg =
    Printf.eprintf "> _update_entry_status %s -> %s\n%!" (filename_of_entry entry) new_msg;
    let elt = _entry_status_div entry in
    elt##.innerHTML := new_msg |> Js.string

  (* let _info msg () = *)
  (*   let date = Int64.of_float (Unix.gettimeofday ()) in *)
  (*   let author = "No one" in *)
  (*   Irmin.Info.v ~date ~author msg *)

  let _entry_data_of_idb entry store =
    let open Lwt.Infix in
    let n = filename_of_entry entry in

    _update_entry_status entry "Checking...";
    Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun () ->
    let data =
      Ft_js.Idb.get store n >>= function
      | Some data -> data |> Lwt.return
      | None ->
          let progress i j =
            let f = float_of_int i /. float_of_int j *. 100. in
            Printf.sprintf "Downloading... (%.0f%%)" f |> _update_entry_status entry
          in

          _update_entry_status entry "Downloading... (0%)";
          Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun () ->
          Ft_js.array_of_url ~progress (url_of_entry entry) >>= fun arr ->
          _update_entry_status entry "Unzipping...";
          Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun () ->
          let arr = Ft_js.decompress_array arr in
          let data = arr##toString |> Js.to_string in

          _update_entry_status entry "Storing...";
          Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun () ->
          Ft_js.Idb.set store n data >>= fun _ -> Lwt.return data
    in
    data >>= fun _ ->
    _update_entry_status entry "&#10003;";
    Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun () -> data

  let get () =
    let open Lwt.Infix in
    let store_name = Js.string "mnist-store" in
    let init ~old_version upgrader =
      ignore old_version;
      Ft_js.Idb.create_store upgrader store_name
    in
    Ft_js.Idb.make "mnist-db" ~version:1 ~init >>= fun idb ->
    let store = Ft_js.Idb.store idb store_name in

    let promises =
      [
        _entry_data_of_idb `Test_labs store;
        _entry_data_of_idb `Test_imgs store;
        _entry_data_of_idb `Train_labs store;
        _entry_data_of_idb `Train_imgs store;
      ]
    in
    (* Lwt.all promises >|= function [ a; b ] -> (a, b) | _ -> assert false *)
    (* Lwt.all promises >|= function [ a; b; c ] -> (a, b, c) | _ -> assert false *)
    Lwt.all promises >|= function [ a; b; c; d ] -> (a, b, c, d) | _ -> assert false
end
