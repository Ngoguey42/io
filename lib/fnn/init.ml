(* Pieces of code are shamelessly copied from owl *)
module Genarray_ops = struct
  let numel a = Array.fold_left ( * ) 1 (Bigarray.Genarray.dims a)

  let empty kind dims = Bigarray.Genarray.create kind Bigarray.c_layout dims

  let create kind dims value =
    let x = empty kind dims in
    Bigarray.Genarray.fill x value;
    x

  let init kind dims f =
    let varr = empty kind dims in
    let n = numel varr in
    let varr_flat = Bigarray.reshape varr [| n |] |> Bigarray.array1_of_genarray in
    for i = 0 to n - 1 do
      Bigarray.Array1.unsafe_set varr_flat i (f i)
    done;
    varr

  let uniform_rvs rng a b = a +. ((b -. a) *. Random.State.float rng 1.)

  let uniform (type b) :
      (float, b) Bigarray.kind ->
      _ ->
      float ->
      float ->
      int array ->
      (float, b, Bigarray.c_layout) Bigarray.Genarray.t =
   fun kind rng a b dims -> init kind dims (fun _ -> uniform_rvs rng a b)

  let gaussian_rvs rng mu sigma =
    let u1, u2 = (Random.State.float rng 1., Random.State.float rng 1.) in
    let z = sqrt (~-.2. *. log u1) *. sin (2. *. 3.141592653 *. u2) in
    mu +. (sigma *. z)

  let gaussian (type b) :
      (float, b) Bigarray.kind ->
      _ ->
      float ->
      float ->
      int array ->
      (float, b, Bigarray.c_layout) Bigarray.Genarray.t =
   fun kind rng mu sigma dims -> init kind dims (fun _ -> gaussian_rvs rng mu sigma)
end

open Genarray_ops

type _uint8_only =
  [ `Uint8Array of (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

type _int32_only =
  [ `Int32Array of (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

type _int64_only =
  [ `Int64Array of (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

type _float32_only =
  [ `Float32Array of (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

type _float64_only =
  [ `Float64Array of (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

type _int_intersection = [ `IntConstant of int64 ]

type _float_intersection =
  [ `Uniform of float * float
  | `Gaussian of float * float
  | `Standard
  | `Tanh
  | `GlorotUniform
  | `GlorotNormal
  | `LecunNormal
  | `Bilinear (* Upsampling for conv transpose *)
  | `FloatConstant of float ]

type uint8_init = [ _uint8_only | _int_intersection ]

type int32_init = [ _int32_only | _int_intersection ]

type int64_init = [ _int64_only | _int_intersection ]

type float32_init = [ _float32_only | _float_intersection ]

type float64_init = [ _float64_only | _float_intersection ]

type int_init = [ uint8_init | int32_init | int64_init ]

type float_init = [ float32_init | float64_init ]

type t = [ int_init | float_init ]

let calc_fans s =
  let _prod x = Array.fold_left (fun p q -> p * q) 1 x in
  let l = Array.length s in
  let fan_in, fan_out =
    (* for matrices *)
    if l = 2 then (float_of_int s.(0), float_of_int s.(1)) (* for convolution kernels 1d, 2d, 3d *)
    else if l > 2 && l < 6 then
      let s' = Array.sub s 0 (l - 2) in
      let receptive = _prod s' in
      let i = s.(l - 2) * receptive |> float_of_int in
      let o = s.(l - 1) * receptive |> float_of_int in
      (i, o) (* for no specific assumptions *)
    else
      let i_o = _prod s |> float_of_int |> Stdlib.sqrt in
      (i_o, i_o)
  in
  (fan_in, fan_out)

let _scalar_from_int64 : type a b. (a, b) Bigarray.kind -> int64 -> a =
 fun kind v ->
  match kind with
  | Bigarray.Int8_unsigned -> Int64.to_int v
  | Bigarray.Int32 -> Int64.to_int32 v
  | Bigarray.Int64 -> v
  | _ -> invalid_arg "Bad Init.t/Bigarray.kind combination"

let _upcast_array :
    type a b a' b'.
    (a, b) Bigarray.kind -> (a', b', _) Bigarray.Genarray.t -> (a, b, _) Bigarray.Genarray.t =
 fun kind arr ->
  match (kind, Bigarray.Genarray.kind arr) with
  | Bigarray.Int64, Bigarray.Int64 -> arr
  | Bigarray.Int32, Bigarray.Int32 -> arr
  | Bigarray.Int8_unsigned, Bigarray.Int8_unsigned -> arr
  | Bigarray.Float32, Bigarray.Float32 -> arr
  | Bigarray.Float64, Bigarray.Float64 -> arr
  | _, _ -> invalid_arg "Bad Init.t/Bigarray.kind combination"

let _run_float :
    type b.
    Random.State.t ->
    [< _float_intersection ] ->
    (float, b) Bigarray.kind ->
    int array ->
    (float, b, Bigarray.c_layout) Bigarray.Genarray.t =
 fun rng t kind dimensions ->
  let fan_in, fan_out = calc_fans dimensions in
  let r0 = sqrt (1. /. fan_in) in
  let r1 = sqrt (6. /. (fan_in +. fan_out)) in
  let r2 = sqrt (2. /. (fan_in +. fan_out)) in
  match t with
  | `Uniform (a, b) -> uniform kind rng a b dimensions
  | `Gaussian (mu, sigma) -> gaussian kind rng mu sigma dimensions
  | `Standard -> uniform kind rng ~-.r0 r0 dimensions
  | `Tanh -> uniform kind rng ~-.r1 r1 dimensions
  | `GlorotUniform -> uniform kind rng ~-.r1 r1 dimensions
  | `GlorotNormal -> gaussian kind rng 0. r2 dimensions
  | `LecunNormal -> gaussian kind rng 0. r0 dimensions
  | `Bilinear -> failwith "not implemented"
  | `FloatConstant k -> create kind dimensions k

let _run_int :
    type a b.
    Random.State.t ->
    [< _int_intersection ] ->
    (a, b) Bigarray.kind ->
    int array ->
    (a, b, Bigarray.c_layout) Bigarray.Genarray.t =
 fun _ t kind dimensions ->
  match (t, kind) with
  | `IntConstant k, Bigarray.Int8_unsigned
  | `IntConstant k, Bigarray.Int32
  | `IntConstant k, Bigarray.Int64 ->
      create kind dimensions (_scalar_from_int64 kind k)
  | `IntConstant _, _ -> invalid_arg "Bad Init.t/Bigarray.kind combination"

let run :
    type a b.
    ?rng:Random.State.t ->
    [< t ] ->
    (a, b) Bigarray.kind ->
    int array ->
    (a, b, Bigarray.c_layout) Bigarray.Genarray.t =
 fun ?(rng = Random.get_state ()) t kind dimensions ->
  match ((t :> t), kind) with
  | (#_uint8_only as t), Bigarray.Int8_unsigned -> (
      match t with `Uint8Array a -> _upcast_array kind a )
  | (#_int32_only as t), Bigarray.Int32 -> ( match t with `Int32Array a -> _upcast_array kind a )
  | (#_int64_only as t), Bigarray.Int64 -> ( match t with `Int64Array a -> _upcast_array kind a )
  | (#_float32_only as t), Bigarray.Float32 -> (
      match t with `Float32Array a -> _upcast_array kind a )
  | (#_float64_only as t), Bigarray.Float64 -> (
      match t with `Float64Array a -> _upcast_array kind a )
  | (#_float_intersection as t), Bigarray.Float32 -> _run_float rng t kind dimensions
  | (#_float_intersection as t), Bigarray.Float64 -> _run_float rng t kind dimensions
  | (#_int_intersection as t), Bigarray.Int64 -> _run_int rng t kind dimensions
  | _, _ -> invalid_arg "Bad Init.t/Bigarray.kind combination"
