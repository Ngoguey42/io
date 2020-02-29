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
