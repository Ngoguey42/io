(*
Axes naming in various documentations
- Here      :`S2       `S1       `S0        `N    `C
- Tfjs (1d) :                     L (length) N     C
- Tfjs (2d+): D         H         W          N     C
- pytorch   : D         H         W          N     C
- keras (1d):                     steps      batch channels
- keras (2d):           rows      cols       batch channels
- keras (3d): conv_dim1 conv_dim2 conv_dim3  batch channels
 *)
module Axis = struct
  type symbolic1d = [ `N ]

  type symbolic2d = [ symbolic1d | `C ]

  type symbolic3d = [ symbolic2d | `S0 ]

  type symbolic4d = [ symbolic3d | `S1 ]

  type symbolic5d = [ symbolic4d | `S2 ]

  type symbolic = symbolic5d

  type absolute = [ `Idx of int ]

  type t = [ symbolic | absolute ]

  let is_symbolic : [< t ] -> bool = function
    | `N -> true
    | `C -> true
    | `S0 -> true
    | `S1 -> true
    | `S2 -> true
    | `Idx _ -> false

  let is_absolute : [< t ] -> bool = function
    | `N -> false
    | `C -> false
    | `S0 -> false
    | `S1 -> false
    | `S2 -> false
    | `Idx _ -> true

  let to_string : [< t ] -> string = function
    | `N -> "n"
    | `C -> "c"
    | `S0 -> "s0"
    | `S1 -> "s1"
    | `S2 -> "s2"
    | `Idx i -> string_of_int i

  let compatible_with_ndim : [< t ] -> int -> bool =
   fun axis ndim ->
    match axis with
    | `Idx i -> i < ndim
    | `N -> 0 < ndim
    | `C -> 1 < ndim
    | `S0 -> 2 < ndim
    | `S1 -> 3 < ndim
    | `S2 -> 4 < ndim

  let symbolic_axes_of_ndim = function
    | 0 -> []
    | 1 -> [ `N ]
    | 2 -> [ `N; `C ]
    | 3 -> [ `N; `C; `S0 ]
    | 4 -> [ `N; `C; `S0; `S1 ]
    | 5 -> [ `N; `C; `S0; `S1; `S2 ]
    | _ -> invalid_arg "Unsupported ndim>5"

  let absolute_axes_of_ndim = function
    | 0 -> []
    | 1 -> [ `Idx 0 ]
    | 2 -> [ `Idx 0; `Idx 1 ]
    | 3 -> [ `Idx 0; `Idx 1; `Idx 2 ]
    | 4 -> [ `Idx 0; `Idx 1; `Idx 2; `Idx 3 ]
    | 5 -> [ `Idx 0; `Idx 1; `Idx 2; `Idx 3; `Idx 4 ]
    | _ -> invalid_arg "Unsupported ndim>5"

  let min_ndim_of_axis = function
    | `Idx i -> i + 1
    | `N -> 1
    | `C -> 2
    | `S0 -> 3
    | `S1 -> 4
    | `S2 -> 5
end

module type SIZE = sig
  type tag = [ `K | `U ]

  type _ t = K : int -> [< tag > `K ] t | U : [< tag > `U ] t

  type any = tag t

  val to_any : _ t -> tag t

  val to_known : _ t -> [ `K ] t

  val to_unknown : _ t -> [ `U ] t

  val to_string : _ t -> string

  val concatenate : 'a t -> 'a t -> 'a t

  val all_broadcastable : 'a t list -> bool

  val broadcast : 'a t -> 'a t -> 'a t

  val add : 'a t -> 'a t -> 'a t

  val sub : 'a t -> 'a t -> 'a t

  val mul : 'a t -> 'a t -> 'a t

  val div : 'a t -> 'a t -> 'a t

  val modulo : 'a t -> 'a t -> 'a t

  module Open : sig
    val to_known : _ t -> [< tag > `K ] t

    val to_unknown : _ t -> [< tag > `U ] t
  end

  module Infix : sig
    val ( + ) : 'a t -> 'a t -> 'a t

    val ( * ) : 'a t -> 'a t -> 'a t

    val ( - ) : 'a t -> 'a t -> 'a t

    val ( / ) : 'a t -> 'a t -> 'a t

    val ( mod ) : 'a t -> 'a t -> 'a t
  end
end

module Size : SIZE = struct
  type tag = [ `K | `U ]

  type _ t = K : int -> [< tag > `K ] t | U : [< tag > `U ] t

  type any = tag t

  let to_string (type a) : a t -> string = function U -> "_" | K i -> string_of_int i

  let to_any (type a) : a t -> [ `K | `U ] t = function K _ as d -> d | U as d -> d

  let to_known (type a) : a t -> [< tag > `K ] t = function K _ as d -> d | U -> failwith "failed"

  let to_unknown (type a) : a t -> [< tag > `U ] t = function
    | U as d -> d
    | K _ -> failwith "failed"

  (** When used with open variant types, all wrong value combinations have compatible types
      e.g.: [> `U] and [> `K] is accepted by the type system
      When using exact variant types, some valid value combinations have incompatible types
      e.g.: [`U] and [ `K | `U] is rejected by the type system
   *)
  let concatenate (type a) : a t -> a t -> a t =
   fun d d' -> match (d, d') with U, U -> d | K i, K i' -> K (i + i') | _, _ -> failwith "failed"

  let all_broadcastable : 'a t list -> bool =
   fun l ->
    List.map to_any l
    |> List.filter_map (function K 1 -> None | s -> Some s)
    |> List.sort_uniq compare |> List.length <= 1

  let broadcast (type a) : a t -> a t -> a t =
   fun size size' ->
    match (size, size') with
    | U, U -> size
    | K i, K i' when i = i' -> size
    | _, K 1 -> size
    | K 1, _ -> size'
    | U, K _ | K _, U | K _, K _ ->
        Printf.sprintf "Can't broadcast %s with %s" (to_string size) (to_string size')
        |> invalid_arg

  let add (type a) : a t -> a t -> a t =
   fun a b -> match (a, b) with K i, K j -> K (i + j) | U, _ -> a | _, U -> b

  let sub (type a) : a t -> a t -> a t =
   fun a b -> match (a, b) with K i, K j -> K (i - j) | U, _ -> a | _, U -> b

  let modulo (type a) : a t -> a t -> a t =
   fun a b -> match (a, b) with K i, K j -> K (i mod j) | U, _ -> a | _, U -> b

  let div (type a) : a t -> a t -> a t =
   fun a b -> match (a, b) with K i, K j -> K (i / j) | U, _ -> a | _, U -> b

  let mul (type a) : a t -> a t -> a t =
   fun a b -> match (a, b) with K i, K j -> K (i * j) | U, _ -> a | _, U -> b

  module Open = struct
    let to_known = to_known

    let to_unknown = to_unknown
  end

  module Infix = struct
    let ( + ) = add

    let ( * ) = mul

    let ( - ) = sub

    let ( / ) = div

    let ( mod ) = modulo
  end
end

module Length = struct
  type tag = [ `L0 | `L1 | `L2 | `L3 | `L4 | `L5 ]

  type _ t =
    | L0 : [< tag > `L0 ] t
    | L1 : [< tag > `L1 ] t
    | L2 : [< tag > `L2 ] t
    | L3 : [< tag > `L3 ] t
    | L4 : [< tag > `L4 ] t
    | L5 : [< tag > `L5 ] t

  let to_any (type a) : a t -> tag t = function
    | L0 as l -> l
    | L1 as l -> l
    | L2 as l -> l
    | L3 as l -> l
    | L4 as l -> l
    | L5 as l -> l

  let to_string : _ t -> string =
   fun shape ->
    match to_any shape with L0 -> "0" | L1 -> "1" | L2 -> "2" | L3 -> "3" | L4 -> "4" | L5 -> "5"

  let to_int : _ t -> int =
   fun shape -> match to_any shape with L0 -> 0 | L1 -> 1 | L2 -> 2 | L3 -> 3 | L4 -> 4 | L5 -> 5
end

module Layout = struct
  type (_, _) t =
    | Sym0d : ([< Length.tag > `L0 ], [< Axis.t ]) t
    | Sym1d : ([< Length.tag > `L1 ], [< Axis.t > `N ]) t
    | Sym2d : ([< Length.tag > `L2 ], [< Axis.t > `N `C ]) t
    | Sym3d : ([< Length.tag > `L3 ], [< Axis.t > `N `C `S0 ]) t
    | Sym4d : ([< Length.tag > `L4 ], [< Axis.t > `N `C `S0 `S1 ]) t
    | Sym5d : ([< Length.tag > `L5 ], [< Axis.t > `N `C `S0 `S1 `S2 ]) t
    | Abs0d : ([< Length.tag > `L0 ], [< Axis.t > `Idx ]) t
    | Abs1d : ([< Length.tag > `L1 ], [< Axis.t > `Idx ]) t
    | Abs2d : ([< Length.tag > `L2 ], [< Axis.t > `Idx ]) t
    | Abs3d : ([< Length.tag > `L3 ], [< Axis.t > `Idx ]) t
    | Abs4d : ([< Length.tag > `L4 ], [< Axis.t > `Idx ]) t
    | Abs5d : ([< Length.tag > `L5 ], [< Axis.t > `Idx ]) t

  let to_any (type a b) : (a, b) t -> (Length.tag, Axis.t) t = function
    | Sym0d as v -> v
    | Sym1d as v -> v
    | Sym2d as v -> v
    | Sym3d as v -> v
    | Sym4d as v -> v
    | Sym5d as v -> v
    | Abs0d as v -> v
    | Abs1d as v -> v
    | Abs2d as v -> v
    | Abs3d as v -> v
    | Abs4d as v -> v
    | Abs5d as v -> v

  let to_string : (_, _) t -> string =
   fun shape ->
    match to_any shape with
    | Sym0d -> "Sym0d"
    | Sym1d -> "Sym1d"
    | Sym2d -> "Sym2d"
    | Sym3d -> "Sym3d"
    | Sym4d -> "Sym4d"
    | Sym5d -> "Sym5d"
    | Abs0d -> "Abs0d"
    | Abs1d -> "Abs1d"
    | Abs2d -> "Abs2d"
    | Abs3d -> "Abs3d"
    | Abs4d -> "Abs4d"
    | Abs5d -> "Abs5d"
end
