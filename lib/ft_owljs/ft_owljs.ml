module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

module I =
  Irmin.Make (Irmin_indexeddb.Content_store) (Irmin_indexeddb.Branch_store) (Irmin.Metadata.None)
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA256)
(* module I = Ft.Irmin_mem.KV (Irmin.Contents.String) *)

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
          let elt = tr [ th [ txt n ]; th ~a:[a_id n] [ txt "unknown" ] ] in
          elt :: aux tl
    in
    table @@ aux entries

  let _status_div = lazy (Tyxml_js.To_dom.of_element @@ _create_status_div ())

  let status_div () = Lazy.force _status_div

  let _entry_status_div entry =
    let status_div = status_div () in
    let n = filename_of_entry entry in
    let elt = status_div##querySelector (Js.string ("#" ^ n)) in
    Js.coerce_opt elt Dom_html.CoerceTo.th (fun _ -> assert false)

  let _update_entry_status entry msg =
    (_entry_status_div entry)##.innerHTML := Js.string msg

  let _info msg () =
    let date = Int64.of_float (Unix.gettimeofday ()) in
    let author = "No one" in
    Irmin.Info.v ~date ~author msg

  let _entry_data_of_repo entry repo =
    let open Lwt.Infix in
    let n = filename_of_entry entry in
    _update_entry_status entry "Checking...";
    I.master repo >>= fun t ->
    I.find t [n] >>= fun res ->
    let s = match res with
      | Some s ->
         Lwt.return s
      | None ->
         let progress i j =
           let f = (float_of_int i) /. (float_of_int j) *. 100. in
           Printf.sprintf "Downloading... (%.0f%%)" f |> _update_entry_status entry
         in
         _update_entry_status entry "Downloading... (0%)";
         Ft_js.blob_of_url ~progress (url_of_entry entry) >>= fun blob ->

         _update_entry_status entry "Unzipping...";
         Ft_js.decompress_blob "gzip" blob >>= fun blob ->
         Ft_js.binary_string_of_blob blob >>= fun s ->

         _update_entry_status entry "Committing...";
         I.set_exn t [ n ] s ~info:(_info "Add downloaded file") >>= fun () ->

         Lwt.return s
    in
    s >>= fun _ ->
    _update_entry_status entry "Available";
    s

  let get () =
    let open Lwt.Infix in
    let config = Irmin_indexeddb.config "test-db" in
    (* let config = Ft.Irmin_mem.config () in *)

    I.Repo.v config >>= fun repo ->

    let promises = [
        (* _entry_data_of_repo `Test_labs repo; *)
        _entry_data_of_repo `Test_imgs repo;
        (* _entry_data_of_repo `Train_labs repo; *)
        (* _entry_data_of_repo `Train_imgs repo; *)
      ] in
    Lwt.all promises >|= function
    | [a; b; c] -> (a, b, c)
    | _ -> assert false

end
