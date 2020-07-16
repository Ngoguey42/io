module Dom_html = Js_of_ocaml.Dom_html
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Typed_array = Js_of_ocaml.Typed_array
module Reactjs = Ft_js.Reactjs

type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type entry = [ `Train_imgs | `Train_labs | `Test_imgs | `Test_labs ]

type status = [ `Unknown | `Check | `Download of int * int | `Ready of uint8_ba | `Store | `Unzip ]

let test_set_sample =
  [
    (0, 9167);
    (1, 7270);
    (2, 8838);
    (3, 6873);
    (4, 3444);
    (5, 3556);
    (6, 161);
    (7, 4658);
    (8, 391);
    (9, 8322);
  ]

let test_set_size = 10000

let train_set_size = 60000

let entries = [ `Train_imgs; `Train_labs; `Test_imgs; `Test_labs ]

let filename_of_entry = function
  | `Train_imgs -> "train-images-idx3-ubyte"
  | `Train_labs -> "train-labels-idx1-ubyte"
  | `Test_imgs -> "t10k-images-idx3-ubyte"
  | `Test_labs -> "t10k-labels-idx1-ubyte"

let url_of_entry entry =
  String.concat "" [ "http://yann.lecun.com/exdb/mnist/"; filename_of_entry entry; ".gz" ]

let _entry_data_of_idb : _ -> _ -> (entry * status -> unit) -> unit Lwt.t =
 fun entry store progress ->
  let open Lwt.Infix in
  let n = filename_of_entry entry in

  progress (entry, `Check);
  Lwt_js.sleep 0.025 >>= fun () ->
  let reshape arr =
    let sub i j a = Bigarray.Genarray.sub_left a i j in
    match entry with
    | `Train_imgs ->
        Typed_array.to_genarray arr
        (* Ft_js.Conv.Uint8.ba_of_ta arr *)
        |> sub 16 (train_set_size * 28 * 28)
        |> Fun.flip Bigarray.reshape [| train_set_size; 28; 28 |]
    | `Train_labs ->
        Typed_array.to_genarray arr
       (* Ft_js.Conv.Uint8.ba_of_ta arr *)
       |> sub 8 train_set_size
        |> Fun.flip Bigarray.reshape [| train_set_size |]
    | `Test_imgs ->
        Typed_array.to_genarray arr
        (* Ft_js.Conv.Uint8.ba_of_ta arr *)
        |> sub 16 (test_set_size * 28 * 28)
        |> Fun.flip Bigarray.reshape [| test_set_size; 28; 28 |]
    | `Test_labs ->
        Typed_array.to_genarray arr
       (* Ft_js.Conv.Uint8.ba_of_ta arr *)
       |> sub 8 test_set_size
        |> Fun.flip Bigarray.reshape [| test_set_size |]
  in
  let arr =
    Ft_js.Idb.get store n >>= function
    | Some arr -> arr |> reshape |> Lwt.return
    | None ->
        Lwt_js.sleep 0.025 >>= fun () ->
        let f i j = progress (entry, `Download (i, j)) in
        Ft_js.array_of_url ~progress:f (url_of_entry entry) >>= fun arr ->
        progress (entry, `Unzip);
        Lwt_js.sleep 0.025 >>= fun () ->
        let arr = Ft_js.decompress_array arr in
        progress (entry, `Store);
        Lwt_js.sleep 0.025 >>= fun () ->
        Ft_js.Idb.set store n arr >>= fun _ -> arr |> reshape |> Lwt.return
  in
  arr >|= fun arr -> progress (entry, `Ready arr)

let get : (entry * status -> unit) -> unit Lwt.t =
 fun progress ->
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
      _entry_data_of_idb `Train_imgs store progress;
      _entry_data_of_idb `Train_labs store progress;
      _entry_data_of_idb `Test_imgs store progress;
      _entry_data_of_idb `Test_labs store progress;
    ]
  in
  Lwt.all promises >|= fun _ -> Ft_js.Idb.close store.db

let put_digit_to_canvas img (canvas : Dom_html.canvasElement Js.t) =
  let img =
    img
    |> (fun x -> Ndarray.reshape x [| 28; 28; 1 |])
    |> (fun x -> Ndarray.repeat x [| 1; 1; 3 |])
    |> Ndarray.pad ~v:255 [ [ 0; 0 ]; [ 0; 0 ]; [ 0; 1 ] ]
    |> Typed_array.from_genarray
    (* |> Ft_js.Conv.Uint8.ta_of_ba *)
  in
  canvas##.width := 28;
  canvas##.height := 28;
  canvas##.style##.height := Js.string "100%";
  canvas##.style##.width := Js.string "100%";
  let ctx = canvas##getContext Dom_html._2d_ in
  let idata = ctx##getImageData 0. 0. 28. 28. in
  Js.Unsafe.meth_call idata##.data "set" [| Js.Unsafe.inject img |] |> ignore;
  ctx##putImageData idata 0. 0.

let b64_url_of_digit img =
  let canvas = Dom_html.createCanvas Dom_html.window##.document in
  put_digit_to_canvas img canvas;
  canvas##toDataURL |> Js.to_string
