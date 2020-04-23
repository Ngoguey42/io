module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Typed_array = Js_of_ocaml.Typed_array

(* TypedArray *)
type uint8_ta = (int, [ `Uint8 ]) Typed_array.typedArray

type float32_ta = (float, [ `Float32 ]) Typed_array.typedArray

type int32_ta = (int, [ `Int32 ]) Typed_array.typedArray

type arrayBuffer = Typed_array.arrayBuffer Js.t

(* BigArray *)
type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

type float32_ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type int32_ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t

let list_of_ta : ('a, 'b) Typed_array.typedArray Js.t -> 'a list =
 fun arr -> List.init arr##.length (fun i -> Typed_array.unsafe_get arr i)

module Ta = struct
  let uint8_of_float32 : float32_ta Js.t -> uint8_ta Js.t =
   fun arr -> Js.Unsafe.fun_call Js.Unsafe.global ##. Uint32Array##.from [| Js.Unsafe.inject arr |]

  let float32_of_uint8 : uint8_ta Js.t -> float32_ta Js.t =
   fun arr -> Js.Unsafe.fun_call Js.Unsafe.global ##. Float32Array##.from [| Js.Unsafe.inject arr |]
end

module Uint8 = struct
  (* Not using `Typed_array.Bigstring` because we want to keep the shape *)
  external ba_of_ta : uint8_ta Js.t -> uint8_ba = "ft_js_uint8_ba_of_ta"

  external ta_of_ba : uint8_ba -> uint8_ta Js.t = "ft_js_uint8_ta_of_ba"
end

module Float32 = struct
  external ba_of_ta : float32_ta Js.t -> float32_ba = "ft_js_float32_ba_of_ta"

  external ta_of_ba : float32_ba -> float32_ta Js.t = "ft_js_float32_ta_of_ba"
end

module Int32 = struct
  external ba_of_ta : int32_ta Js.t -> int32_ba = "ft_js_int32_ba_of_ta"

  external ta_of_ba : int32_ba -> int32_ta Js.t = "ft_js_int32_ta_of_ba"
end
