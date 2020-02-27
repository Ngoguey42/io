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

module Conv = struct
  (* TypedArray *)
  type uint8_ta = Typed_array.uint8Array Js.t

  type float32_ta = Typed_array.float32Array Js.t

  type arrayBuffer = Typed_array.arrayBuffer Js.t

  type bigstring = Typed_array.Bigstring.t

  (* BigArray *)
  type float32_bag = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

  type uint8_bag = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

  (* Ndarray *)
  type float32_nd = Owl_base_dense_ndarray.S.arr

  type uint8_nd = (char, Bigarray.int8_unsigned_elt) Ndarray.t

  let list_of_ta : ('a, 'b) Typed_array.typedArray Js.t -> 'a list = fun arr ->
    List.init arr##.length (fun i -> Typed_array.unsafe_get arr i)

  module Cast = struct
    module Ta = struct
      let uint8_of_float32 : float32_ta -> uint8_ta =
       fun arr ->
        Js.Unsafe.fun_call Js.Unsafe.global ##. Uint32Array##.from [| Js.Unsafe.inject arr |]

      let float32_of_uint8 : uint8_ta -> float32_ta =
       fun arr ->
        Js.Unsafe.fun_call Js.Unsafe.global ##. Float32Array##.from [| Js.Unsafe.inject arr |]
    end
  end

  module Reinterpret = struct
    module Uint8 = struct
      (* Not using `Typed_array.Bigstring` because we want to keep the shape *)
      external _bag_of_ta : uint8_ta -> uint8_bag = "ft_owljs_uint8_bag_of_ta"

      let nd_of_ta arr =
        let arr : uint8_ta = arr in
        let arr : uint8_bag = _bag_of_ta arr in
        let arr : uint8_nd = arr in
        arr

      external _ta_of_bag : uint8_bag -> uint8_ta = "ft_owljs_uint8_ta_of_bag"

      let ta_of_nd arr =
        let arr : uint8_nd = arr in
        let arr : uint8_bag = arr in
        let arr : uint8_ta = _ta_of_bag arr in
        arr
    end

    module Float32 = struct
      external _bag_of_ta : float32_ta -> float32_bag = "ft_owljs_float32_bag_of_ta"

      let nd_of_ta arr =
        let arr : float32_ta = arr in
        let arr : float32_bag = _bag_of_ta arr in
        let arr : float32_nd = arr in
        arr

      external _ta_of_bag : float32_bag -> float32_ta = "ft_owljs_float32_ta_of_bag"

      let ta_of_nd arr =
        let arr : float32_nd = arr in
        let arr : float32_bag = arr in
        let arr : float32_ta = _ta_of_bag arr in
        arr
    end
  end
end

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
         [%html "<tr><th>" [txt n] "</th><th class=" [n] ">unknown</th></tr>"]
         :: aux tl
    in
    table ~a:[a_class ["mnist-status"]]
          ~thead:[%html "<thead><tr><th colspan='2'>MNIST dataset status</th></tr></thead>"]
      (aux entries)

  let _status_div = lazy (Tyxml_js.To_dom.of_element @@ _create_status_div ())

  let status_div () = Lazy.force _status_div

  let _entry_status_div entry =
    let status_div = status_div () in
    let n = filename_of_entry entry in
    Ft_js.select status_div ("." ^ n) Dom_html.CoerceTo.th

    (* let elt = status_div##querySelector (Js.string ("#" ^ n)) in *)
    (* Js.coerce_opt elt Dom_html.CoerceTo.th (fun _ -> assert false) *)

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
    Lwt.all promises >|= function [ a; b; c; d ] -> (a, b, c, d) | _ -> assert false

  let put_digit_to_canvas img (canvas : Dom_html.canvasElement Js.t) =
    let img =
      img |> Conv.Reinterpret.Uint8.nd_of_ta
      |> (fun x -> Ndarray.reshape x [| 28; 28; 1 |])
      |> (fun x -> Ndarray.repeat x [| 1; 1; 3 |])
      |> Ndarray.pad ~v:(char_of_int 255) [ [ 0; 0 ]; [ 0; 0 ]; [ 0; 1 ] ]
      |> Conv.Reinterpret.Uint8.ta_of_nd
    in
    canvas##.width := 28;
    canvas##.height := 28;
    canvas##.style##.height := Js.string "100%";
    canvas##.style##.width := Js.string "100%";
    let ctx = canvas##getContext Dom_html._2d_ in
    let idata = ctx##getImageData 0. 0. 28. 28. in
    (* Firebug.console##log idata##.data; *)
    Js.Unsafe.meth_call idata##.data "set" [| Js.Unsafe.inject img |] |> ignore;
    ctx##putImageData idata 0. 0.

  let html_pred_overview img lab pred =
    let txt = Format.kasprintf Html.txt in
    let txt' = Format.kasprintf Html.txt in
    let aux i x =
      let bg = "background: " ^ Ft.Color.(Jet.get x |> to_hex_string) in
      let cls = [ (if i == lab then "good-one" else "") ] in
      [%html "<div style='" bg "' class='" cls "'>" [txt "%d" i] "<br/>" [txt' "%.0f%%" (x *. 100.)]"</div>"]
    in
    let elt =
      [%html "<div class='mnist-pred'><div><canvas></canvas></div>" (List.mapi aux pred) "</div>"]
      |> Tyxml_js.To_dom.of_element
    in
    Ft_js.select elt "canvas" Dom_html.CoerceTo.canvas |> put_digit_to_canvas img;
    elt
end
