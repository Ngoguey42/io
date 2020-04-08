module Dom_html = Js_of_ocaml.Dom_html
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array

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
        [%html "<tr><th>" [ txt n ] "</th><th class=" [ n ] ">unknown</th></tr>"] :: aux tl
  in
  table
    ~a:[ a_class [ "mnist-status" ] ]
    ~thead:[%html "<thead><tr><th colspan='2'>MNIST dataset status</th></tr></thead>"] (aux entries)

let _status_div = lazy (Tyxml_js.To_dom.of_element @@ _create_status_div ())

let status_div () = Lazy.force _status_div

let _entry_status_div entry =
  let status_div = status_div () in
  let n = filename_of_entry entry in
  Ft_js.select status_div ("." ^ n) Dom_html.CoerceTo.th

let _update_entry_status entry new_msg =
  (* Printf.eprintf "> _update_entry_status %s -> %s\n%!" (filename_of_entry entry) new_msg; *)
  let elt = _entry_status_div entry in
  elt##.innerHTML := new_msg |> Js.string

let _entry_data_of_idb entry store =
  let open Lwt.Infix in
  let n = filename_of_entry entry in

  _update_entry_status entry "Checking...";
  Lwt_js.sleep 0.1 >>= fun () ->
  let arr =
    Ft_js.Idb.get store n >>= function
    | Some arr -> arr |> Lwt.return
    | None ->
        let progress i j =
          let f = float_of_int i /. float_of_int j *. 100. in
          Printf.sprintf "Downloading... (%.0f%%)" f |> _update_entry_status entry
        in

        _update_entry_status entry "Downloading... (0%)";
        Lwt_js.sleep 0.1 >>= fun () ->
        Ft_js.array_of_url ~progress (url_of_entry entry) >>= fun arr ->
        _update_entry_status entry "Unzipping...";
        Lwt_js.sleep 0.1 >>= fun () ->
        let arr = Ft_js.decompress_array arr in

        _update_entry_status entry "Storing...";
        Lwt_js.sleep 0.1 >>= fun () ->
        Ft_js.Idb.set store n arr >>= fun _ -> Lwt.return arr
  in
  arr >>= fun _ ->
  _update_entry_status entry "&#10003;";
  Lwt_js.sleep 0.1 >>= fun () -> arr

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
      _entry_data_of_idb `Train_imgs store;
      _entry_data_of_idb `Train_labs store;
      _entry_data_of_idb `Test_imgs store;
      _entry_data_of_idb `Test_labs store;
    ]
  in
  Lwt.all promises >|= fun l ->
  Ft_js.Idb.close store.db;

  match l with [ a; b; c; d ] -> (a, b, c, d) | _ -> assert false

let put_digit_to_canvas img (canvas : Dom_html.canvasElement Js.t) =
  let img =
    img |> Ft_js.Conv.Uint8.ba_of_ta
    |> (fun x -> Ndarray.reshape x [| 28; 28; 1 |])
    |> (fun x -> Ndarray.repeat x [| 1; 1; 3 |])
    |> Ndarray.pad ~v:(char_of_int 255) [ [ 0; 0 ]; [ 0; 0 ]; [ 0; 1 ] ]
    |> Ft_js.Conv.Uint8.ta_of_ba
  in
  canvas##.width := 28;
  canvas##.height := 28;
  canvas##.style##.height := Js.string "100%";
  canvas##.style##.width := Js.string "100%";
  let ctx = canvas##getContext Dom_html._2d_ in
  let idata = ctx##getImageData 0. 0. 28. 28. in
  Js.Unsafe.meth_call idata##.data "set" [| Js.Unsafe.inject img |] |> ignore;
  ctx##putImageData idata 0. 0.

let html_pred_overview img lab pred =
  let txt = Format.kasprintf Html.txt in
  let txt' = Format.kasprintf Html.txt in
  let _, maxi, _ =
    List.fold_left
      (fun (i, i', v') v -> if v > v' then (i + 1, i, v) else (i + 1, i', v'))
      (0, 0, 0.) pred
  in
  let aux i x =
    let bg = "background: " ^ Ft.Color.(Firegrass.get x |> to_hex_string) in
    let cls = [] in
    let cls = if i == lab then "good-one" :: cls else cls in
    let cls = if i == maxi then "highest-one" :: cls else "not-highest-one" :: cls in
    let content = [ txt "%d" i; Html.br (); txt' "%.0f%%" (x *. 100.) ] in
    (* let content = if i == maxi then [Html.b content] else content in *)
    [%html "<div style='" bg "' class='" cls "'>" content "</div>"]
  in
  let elt =
    [%html "<div class='mnist-pred'><div><canvas></canvas></div>" (List.mapi aux pred) "</div>"]
    |> Tyxml_js.To_dom.of_element
  in
  Ft_js.select elt "canvas" Dom_html.CoerceTo.canvas |> put_digit_to_canvas img;
  elt
