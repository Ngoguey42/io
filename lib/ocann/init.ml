(* Pieces of code are shamelessly copied from owl *)
open struct
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
      (Float.t, b) Bigarray.kind ->
      Int.t ->
      Float.t ->
      Float.t ->
      Int.t array ->
      (Float.t, b, Bigarray.c_layout) Bigarray.Genarray.t =
   fun kind seed a b dims ->
    let rng = Random.State.make [| seed |] in
    init kind dims (fun _ -> uniform_rvs rng a b)

  let gaussian_rvs rng mu sigma =
    let u1, u2 = (Random.State.float rng 1., Random.State.float rng 1.) in
    let z = sqrt (~-.2. *. log u1) *. sin (2. *. 3.141592653 *. u2) in
    mu +. (sigma *. z)

  let gaussian (type b) :
      (Float.t, b) Bigarray.kind ->
      Int.t ->
      Float.t ->
      Float.t ->
      Int.t array ->
      (Float.t, b, Bigarray.c_layout) Bigarray.Genarray.t =
   fun kind seed mu sigma dims ->
    let rng = Random.State.make [| seed |] in
    init kind dims (fun _ -> gaussian_rvs rng mu sigma)

  let calc_fans s =
    let _prod x = Array.fold_left (fun p q -> p * q) 1 x in
    let l = Array.length s in
    let fan_in, fan_out =
      (* for matrices *)
      if l = 2 then (float_of_int s.(0), float_of_int s.(1))
        (* for convolution kernels 1d, 2d, 3d *)
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

  let scalar_from_int64 : type a b. (a, b) Bigarray.kind -> Int64.t -> a =
   fun kind v ->
    match kind with
    | Bigarray.Int8_unsigned -> Int64.to_int v
    | Bigarray.Int32 -> Int64.to_int32 v
    | Bigarray.Int64 -> v
    | _ -> invalid_arg "Bad Init.t/Bigarray.kind combination"

  let upcast_array :
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
end

module Deterministic = struct
  type _uint8_only =
    [ `Uint8_array of (Int.t, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

  type _int32_only =
    [ `Int32_array of (Int32.t, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

  type _int64_only =
    [ `Int64_array of (Int64.t, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

  type _float32_only =
    [ `Float32_array of (Float.t, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

  type _float64_only =
    [ `Float64_array of (Float.t, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t ]

  type _int_intersection = [ `Int_constant of Int64.t ]

  type _float_intersection =
    [ `Uniform_seeded of Float.t * Float.t * Int.t
    | `Gaussian_seeded of Float.t * Float.t * Int.t
    | `Standard_seeded of Int.t
    | `Tanh_seeded of Int.t
    | `Glorot_uniform_seeded of Int.t
    | `Glorot_normal_seeded of Int.t
    | `Lecun_normal_seeded of Int.t
    | `Float_constant of Float.t ]

  type uint8 = [ _uint8_only | _int_intersection ]

  type int32 = [ _int32_only | _int_intersection ]

  type int64 = [ _int64_only | _int_intersection ]

  type float32 = [ _float32_only | _float_intersection ]

  type float64 = [ _float64_only | _float_intersection ]

  type int = [ uint8 | int32 | int64 ]

  type float = [ float32 | float64 ]

  type t = [ int | float ]

  let _run_float :
      type b.
      [< _float_intersection ] ->
      (Float.t, b) Bigarray.kind ->
      Int.t array ->
      (Float.t, b, Bigarray.c_layout) Bigarray.Genarray.t =
   fun t kind dimensions ->
    let fan_in, fan_out = calc_fans dimensions in
    let r0 = sqrt (1. /. fan_in) in
    let r1 = sqrt (6. /. (fan_in +. fan_out)) in
    let r2 = sqrt (2. /. (fan_in +. fan_out)) in
    match t with
    | `Uniform_seeded (a, b, seed) -> uniform kind seed a b dimensions
    | `Gaussian_seeded (mu, sigma, seed) -> gaussian kind seed mu sigma dimensions
    | `Standard_seeded seed -> uniform kind seed ~-.r0 r0 dimensions
    | `Tanh_seeded seed -> uniform kind seed ~-.r1 r1 dimensions
    | `Glorot_uniform_seeded seed -> uniform kind seed ~-.r1 r1 dimensions
    | `Glorot_normal_seeded seed -> gaussian kind seed 0. r2 dimensions
    | `Lecun_normal_seeded seed -> gaussian kind seed 0. r0 dimensions
    | `Float_constant k -> create kind dimensions k

  let _run_int :
      type a b.
      [< _int_intersection ] ->
      (a, b) Bigarray.kind ->
      Int.t array ->
      (a, b, Bigarray.c_layout) Bigarray.Genarray.t =
   fun t kind dimensions ->
    match (t, kind) with
    | `Int_constant k, Bigarray.Int8_unsigned
    | `Int_constant k, Bigarray.Int32
    | `Int_constant k, Bigarray.Int64 ->
        create kind dimensions (scalar_from_int64 kind k)
    | `Int_constant _, _ -> invalid_arg "Bad Init.t/Bigarray.kind combination"

  let run :
      type a b.
      [< t ] -> (a, b) Bigarray.kind -> Int.t array -> (a, b, Bigarray.c_layout) Bigarray.Genarray.t
      =
   fun t kind dimensions ->
    match ((t :> t), kind) with
    | (#_uint8_only as t), Bigarray.Int8_unsigned -> (
        match t with `Uint8_array a -> upcast_array kind a )
    | (#_int32_only as t), Bigarray.Int32 -> (
        match t with `Int32_array a -> upcast_array kind a )
    | (#_int64_only as t), Bigarray.Int64 -> (
        match t with `Int64_array a -> upcast_array kind a )
    | (#_float32_only as t), Bigarray.Float32 -> (
        match t with `Float32_array a -> upcast_array kind a )
    | (#_float64_only as t), Bigarray.Float64 -> (
        match t with `Float64_array a -> upcast_array kind a )
    | (#_float_intersection as t), Bigarray.Float32 -> _run_float t kind dimensions
    | (#_float_intersection as t), Bigarray.Float64 -> _run_float t kind dimensions
    | (#_int_intersection as t), Bigarray.Int64 -> _run_int t kind dimensions
    | _, _ -> invalid_arg "Bad Init.t/Bigarray.kind combination"
end

type _uint8_only = Deterministic._uint8_only

type _int32_only = Deterministic._int32_only

type _int64_only = Deterministic._int64_only

type _float32_only = Deterministic._float32_only

type _float64_only = Deterministic._float64_only

type _int_intersection = Deterministic._int_intersection

type _float_intersection =
  [ Deterministic._float_intersection
  | `Uniform of float * float
  | `Gaussian of float * float
  | `Standard
  | `Tanh
  | `Glorot_uniform
  | `Glorot_normal
  | `Lecun_normal ]

type uint8 = [ _uint8_only | _int_intersection ]

type int32 = [ _int32_only | _int_intersection ]

type int64 = [ _int64_only | _int_intersection ]

type float32 = [ _float32_only | _float_intersection ]

type float64 = [ _float64_only | _float_intersection ]

type int = [ uint8 | int32 | int64 ]

type float = [ float32 | float64 ]

type t = [ int | float ]

let to_deterministic : ?rng:Random.State.t -> [< t ] -> Deterministic.t =
 fun ?rng t ->
  let rng = match rng with None -> Random.get_state () | Some rng -> rng in
  let seed = Random.State.int rng ((2 lsl 29) - 1) in
  match t with
  | `Uniform (a, b) -> `Uniform_seeded (a, b, seed)
  | `Gaussian (mu, sigma) -> `Gaussian_seeded (mu, sigma, seed)
  | `Standard -> `Standard_seeded seed
  | `Tanh -> `Tanh_seeded seed
  | `Glorot_uniform -> `Glorot_uniform_seeded seed
  | `Glorot_normal -> `Glorot_normal_seeded seed
  | `Lecun_normal -> `Lecun_normal_seeded seed
  | #Deterministic.t as t -> t

let unseed : [< t ] -> t = function
  | `Uniform_seeded (a, b, _) -> `Uniform (a, b)
  | `Gaussian_seeded (mu, sigma, _) -> `Gaussian (mu, sigma)
  | `Standard_seeded _ -> `Standard
  | `Tanh_seeded _ -> `Tanh
  | `Glorot_uniform_seeded _ -> `Glorot_uniform
  | `Glorot_normal_seeded _ -> `Glorot_normal
  | `Lecun_normal_seeded _ -> `Lecun_normal
  | t -> t

let unseed_float32 : [< float32 ] -> float32 = function
  | `Uniform_seeded (a, b, _) -> `Uniform (a, b)
  | `Gaussian_seeded (mu, sigma, _) -> `Gaussian (mu, sigma)
  | `Standard_seeded _ -> `Standard
  | `Tanh_seeded _ -> `Tanh
  | `Glorot_uniform_seeded _ -> `Glorot_uniform
  | `Glorot_normal_seeded _ -> `Glorot_normal
  | `Lecun_normal_seeded _ -> `Lecun_normal
  | #float32 as t -> t

let float32_to_deterministic : ?rng:Random.State.t -> [< float32 ] -> Deterministic.float32 =
 fun ?rng t ->
  let rng = match rng with None -> Random.get_state () | Some rng -> rng in
  let seed = Random.State.int rng ((2 lsl 29) - 1) in
  match t with
  | `Uniform (a, b) -> `Uniform_seeded (a, b, seed)
  | `Gaussian (mu, sigma) -> `Gaussian_seeded (mu, sigma, seed)
  | `Standard -> `Standard_seeded seed
  | `Tanh -> `Tanh_seeded seed
  | `Glorot_uniform -> `Glorot_uniform_seeded seed
  | `Glorot_normal -> `Glorot_normal_seeded seed
  | `Lecun_normal -> `Lecun_normal_seeded seed
  | #Deterministic.float32 as t -> t

let run :
    type a b.
    ?rng:Random.State.t ->
    [< t ] ->
    (a, b) Bigarray.kind ->
    Int.t array ->
    (a, b, Bigarray.c_layout) Bigarray.Genarray.t =
 fun ?rng t kind dimensions ->
  let t = to_deterministic ?rng t in
  Deterministic.run t kind dimensions

let dimensions_opt : t -> Int.t array option = function
  | `Float32_array arr -> Some (Bigarray.Genarray.dims arr)
  | `Float64_array arr -> Some (Bigarray.Genarray.dims arr)
  | `Int32_array arr -> Some (Bigarray.Genarray.dims arr)
  | `Int64_array arr -> Some (Bigarray.Genarray.dims arr)
  | `Uint8_array arr -> Some (Bigarray.Genarray.dims arr)
  | _ -> None
