open Misc

module type PSHAPE = sig
  type _k = [ `K ] Size.t

  type _p = [ `K | `U ] Size.t

  type _t = Length.tag

  type _s = Size.tag

  type _a = Axis.t

  type[@ocamlformat "disable"] (_, _, _) t =
    | Sym0d_partial:                                ([<_t>`L0], _s,       [<_a]) t
    | Sym0d_total:                                  ([<_t>`L0], [<_s>`K], [<_a]) t
    | Sym1d_partial:<n:_p> ->                       ([<_t>`L1], _s      , [<_a>`N]) t
    | Sym1d_total  :<n:_k> ->                       ([<_t>`L1], [<_s>`K], [<_a>`N]) t
    | Sym2d_partial:<n:_p;c:_p> ->                  ([<_t>`L2], _s      , [<_a>`N `C]) t
    | Sym2d_total  :<n:_k;c:_k> ->                  ([<_t>`L2], [<_s>`K], [<_a>`N `C]) t
    | Sym3d_partial:<n:_p;c:_p;s0:_p> ->            ([<_t>`L3], _s      , [<_a>`N `C `S0]) t
    | Sym3d_total  :<n:_k;c:_k;s0:_k> ->            ([<_t>`L3], [<_s>`K], [<_a>`N `C `S0]) t
    | Sym4d_partial:<n:_p;c:_p;s0:_p;s1:_p> ->      ([<_t>`L4], _s      , [<_a>`N `C `S0 `S1]) t
    | Sym4d_total  :<n:_k;c:_k;s0:_k;s1:_k> ->      ([<_t>`L4], [<_s>`K], [<_a>`N `C `S0 `S1]) t
    | Sym5d_partial:<n:_p;c:_p;s0:_p;s1:_p;s2:_p> ->([<_t>`L5], _s      , [<_a>`N `C `S0 `S1 `S2]) t
    | Sym5d_total  :<n:_k;c:_k;s0:_k;s1:_k;s2:_k> ->([<_t>`L5], [<_s>`K], [<_a>`N `C `S0 `S1 `S2]) t
    | Abs0d_partial:                                ([<_t>`L0], _s,       [<_a>`Idx]) t
    | Abs0d_total:                                  ([<_t>`L0], [<_s>`K], [<_a>`Idx]) t
    | Abs1d_partial:(_p) ->                         ([<_t>`L1], _s      , [<_a>`Idx]) t
    | Abs1d_total  :(_k) ->                         ([<_t>`L1], [<_s>`K], [<_a>`Idx]) t
    | Abs2d_partial:(_p * _p) ->                    ([<_t>`L2], _s      , [<_a>`Idx]) t
    | Abs2d_total  :(_k * _k) ->                    ([<_t>`L2], [<_s>`K], [<_a>`Idx]) t
    | Abs3d_partial:(_p * _p * _p) ->               ([<_t>`L3], _s      , [<_a>`Idx]) t
    | Abs3d_total  :(_k * _k * _k) ->               ([<_t>`L3], [<_s>`K], [<_a>`Idx]) t
    | Abs4d_partial:(_p * _p * _p * _p) ->          ([<_t>`L4], _s      , [<_a>`Idx]) t
    | Abs4d_total  :(_k * _k * _k * _k) ->          ([<_t>`L4], [<_s>`K], [<_a>`Idx]) t
    | Abs5d_partial:(_p * _p * _p * _p * _p) ->     ([<_t>`L5], _s      , [<_a>`Idx]) t
    | Abs5d_total  :(_k * _k * _k * _k * _k) ->     ([<_t>`L5], [<_s>`K], [<_a>`Idx]) t

  type any = (Length.tag, Size.tag, Axis.t) t

  type total = (Length.tag, [ `K ], Axis.t) t

  type absolute_total = (Length.tag, [ `K ], [ `Idx of int ]) t

  (** Constructors *)

  val sym0d_partial : ([ `L0 ], Size.tag, [< Axis.t ]) t

  val sym0d_total : ([ `L0 ], [ `K ], [< Axis.t ]) t

  val sym1d_partial : n:[< Size.tag ] Size.t -> ([ `L1 ], Size.tag, Axis.symbolic1d) t

  val sym1d_total : n:[ `K ] Size.t -> ([ `L1 ], [ `K ], Axis.symbolic1d) t

  val sym2d_partial :
    n:[< Size.tag ] Size.t -> c:[< Size.tag ] Size.t -> ([ `L2 ], Size.tag, Axis.symbolic2d) t

  val sym2d_total : n:[ `K ] Size.t -> c:[ `K ] Size.t -> ([ `L2 ], [ `K ], Axis.symbolic2d) t

  val sym3d_partial :
    n:[< Size.tag ] Size.t ->
    c:[< Size.tag ] Size.t ->
    s0:[< Size.tag ] Size.t ->
    ([ `L3 ], Size.tag, Axis.symbolic3d) t

  val sym3d_total :
    n:[ `K ] Size.t -> c:[ `K ] Size.t -> s0:[ `K ] Size.t -> ([ `L3 ], [ `K ], Axis.symbolic3d) t

  val sym4d_partial :
    n:[< Size.tag ] Size.t ->
    c:[< Size.tag ] Size.t ->
    s0:[< Size.tag ] Size.t ->
    s1:[< Size.tag ] Size.t ->
    ([ `L4 ], Size.tag, Axis.symbolic4d) t

  val sym4d_total :
    n:[ `K ] Size.t ->
    c:[ `K ] Size.t ->
    s0:[ `K ] Size.t ->
    s1:[ `K ] Size.t ->
    ([ `L4 ], [ `K ], Axis.symbolic4d) t

  val sym5d_partial :
    n:[< Size.tag ] Size.t ->
    c:[< Size.tag ] Size.t ->
    s0:[< Size.tag ] Size.t ->
    s1:[< Size.tag ] Size.t ->
    s2:[< Size.tag ] Size.t ->
    ([ `L5 ], Size.tag, Axis.symbolic5d) t

  val sym5d_total :
    n:[ `K ] Size.t ->
    c:[ `K ] Size.t ->
    s0:[ `K ] Size.t ->
    s1:[ `K ] Size.t ->
    s2:[ `K ] Size.t ->
    ([ `L5 ], [ `K ], Axis.symbolic5d) t

  val abs0d_partial : ([ `L0 ], Size.tag, [ `Idx of int ]) t

  val abs0d_total : ([ `L0 ], [ `K ], [ `Idx of int ]) t

  val abs1d_partial : [< Size.tag ] Size.t -> ([ `L1 ], Size.tag, [ `Idx of int ]) t

  val abs1d_total : [ `K ] Size.t -> ([ `L1 ], [ `K ], [ `Idx of int ]) t

  val abs2d_partial :
    [< Size.tag ] Size.t -> [< Size.tag ] Size.t -> ([ `L2 ], Size.tag, [ `Idx of int ]) t

  val abs2d_total : [ `K ] Size.t -> [ `K ] Size.t -> ([ `L2 ], [ `K ], [ `Idx of int ]) t

  val abs3d_partial :
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    ([ `L3 ], Size.tag, [ `Idx of int ]) t

  val abs3d_total :
    [ `K ] Size.t -> [ `K ] Size.t -> [ `K ] Size.t -> ([ `L3 ], [ `K ], [ `Idx of int ]) t

  val abs4d_partial :
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    ([ `L4 ], Size.tag, [ `Idx of int ]) t

  val abs4d_total :
    [ `K ] Size.t ->
    [ `K ] Size.t ->
    [ `K ] Size.t ->
    [ `K ] Size.t ->
    ([ `L4 ], [ `K ], [ `Idx of int ]) t

  val abs5d_partial :
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    [< Size.tag ] Size.t ->
    ([ `L5 ], Size.tag, [ `Idx of int ]) t

  val abs5d_total :
    [ `K ] Size.t ->
    [ `K ] Size.t ->
    [ `K ] Size.t ->
    [ `K ] Size.t ->
    [ `K ] Size.t ->
    ([ `L5 ], [ `K ], [ `Idx of int ]) t

  (** Variant properties  *)

  val layout : ('len, _, 'ax) t -> ('len, 'ax) Layout.t

  val length : ('len, _, _) t -> 'len Length.t

  val is_total : (_, _, _) t -> bool

  val is_partial : (_, _, _) t -> bool

  val is_absolute : (_, _, _) t -> bool

  val is_symbolic : (_, _, _) t -> bool

  val axes : (_, _, 'ax) t -> 'ax list

  val ndim : (_, _, _) t -> int

  (** Casting  *)

  val to_any : (_, _, _) t -> (Length.tag, Size.tag, Axis.t) t

  val to_any_length : (_, 'sz, 'ax) t -> (Length.tag, 'sz, 'ax) t

  val to_any_axes : ('len, 'sz, _) t -> ('len, 'sz, Axis.t) t

  val to_partial : ('len, _, 'ax) t -> ('len, Size.tag, 'ax) t

  val to_total : ('len, _, 'ax) t -> ('len, [ `K ], 'ax) t

  val to_symbolic : ('len, 'sz, 'ax) t -> ('len, 'sz, Axis.symbolic) t

  val to_absolute : ('len, 'sz, 'ax) t -> ('len, 'sz, [ `Idx of int ]) t

  val set : ('len, 'sz, 'ax) t -> 'ax -> 'sz Size.t -> ('len, 'sz, 'ax) t
  (** shape(s) -> shape conversions *)

  val extend :
    ?fill:'sz Size.t -> int -> (_, 'sz, [< Axis.symbolic ]) t -> (Length.tag, 'sz, Axis.symbolic) t

  val pad_left :
    ?fill:'sz Size.t -> int -> (_, 'sz, [ `Idx of int ]) t -> (Length.tag, 'sz, [ `Idx of int ]) t

  val concatenate : ('len, 'sz, 'ax) t -> ('len, 'sz, 'ax) t -> 'ax -> ('len, 'sz, 'ax) t

  val concatenate_all : ('len, 'sz, 'ax) t list -> 'ax -> ('len, 'sz, 'ax) t

  val broadcast : (_, 'sz, _) t -> (_, 'sz, _) t -> (Length.tag, 'sz, Axis.t) t

  val broadcast_all : (_, 'sz, _) t list -> (Length.tag, 'sz, Axis.t) t

  val contract :
    ('ax0 * ([< Axis.t ] as 'ax) option) list ->
    ('ax1 * 'ax option) list ->
    (_, _, 'ax0) t ->
    (_, _, 'ax1) t ->
    (Length.tag, Size.tag, Axis.t) t

  val symbolize :
    [< Axis.symbolic ] list -> ('len, 'sz, [ `Idx of int ]) t -> ('len, 'sz, Axis.symbolic) t

  val desymbolize :
    [< Axis.symbolic ] list -> ('len, 'sz, [< Axis.symbolic ]) t -> ('len, 'sz, [ `Idx of int ]) t

  val reorder :
    ?ndim:int ->
    (([< Axis.t ] as 'ax0) * [< Axis.t ]) list ->
    (_, _, 'ax0) t ->
    (Length.tag, Size.tag, Axis.t) t

  val squeeze :
    ([< Axis.t ] as 'ax) list ->
    ([< Length.tag ], [< Size.tag ], 'ax) t ->
    (Length.tag, Size.tag, Axis.t) t

  (** Dimension getters  *)

  val nth : (_, 'sz, [ `Idx of int ]) t -> int -> 'sz Size.t

  val get : (_, 'sz, 'ax) t -> 'ax -> 'sz Size.t

  (** MISC.  *)

  val numel : (_, 'sz, _) t -> 'sz Size.t

  val equal : (_, _, _) t -> (_, _, _) t -> bool

  val all_equal : (_, _, _) t list -> bool

  val all_broadcastable : (_, _, _) t list -> bool

  val to_int_array : (_, [ `K ], [ `Idx of int ]) t -> int array

  val from_int_array : int array -> (Length.tag, [ `K ], [ `Idx of int ]) t

  val to_list : (_, 'sz, 'ax) t -> ('ax * 'sz Size.t) list

  val to_string : (_, _, _) t -> string

  (** `Open` contains constains shape construction functions and shape conversion functions that
      return open-type shapes. All are aliases of exact-type versions of the parent module except
      `to_length` and `to_layout` that are open-type only.

      The exact ones are useful for pattern matching purposes, the open ones are useful for
      polymorphism purposes.
   *)
  module Open : sig
    (** Constructors *)

    val sym0d_partial : ([< Length.tag > `L0 ], Size.tag, [< Axis.t ]) t

    val sym0d_total : ([< Length.tag > `L0 ], [< Size.tag > `K ], [< Axis.t ]) t

    val sym1d_partial :
      n:[< Size.tag ] Size.t -> ([< Length.tag > `L1 ], Size.tag, [< Axis.t > `N ]) t

    val sym1d_total :
      n:[ `K ] Size.t -> ([< Length.tag > `L1 ], [< Size.tag > `K ], [< Axis.t > `N ]) t

    val sym2d_partial :
      n:[< Size.tag ] Size.t ->
      c:[< Size.tag ] Size.t ->
      ([< Length.tag > `L2 ], Size.tag, [< Axis.t > `N `C ]) t

    val sym2d_total :
      n:[ `K ] Size.t ->
      c:[ `K ] Size.t ->
      ([< Length.tag > `L2 ], [< Size.tag > `K ], [< Axis.t > `N `C ]) t

    val sym3d_partial :
      n:[< Size.tag ] Size.t ->
      c:[< Size.tag ] Size.t ->
      s0:[< Size.tag ] Size.t ->
      ([< Length.tag > `L3 ], Size.tag, [< Axis.t > `N `C `S0 ]) t

    val sym3d_total :
      n:[ `K ] Size.t ->
      c:[ `K ] Size.t ->
      s0:[ `K ] Size.t ->
      ([< Length.tag > `L3 ], [< Size.tag > `K ], [< Axis.t > `N `C `S0 ]) t

    val sym4d_partial :
      n:[< Size.tag ] Size.t ->
      c:[< Size.tag ] Size.t ->
      s0:[< Size.tag ] Size.t ->
      s1:[< Size.tag ] Size.t ->
      ([< Length.tag > `L4 ], Size.tag, [< Axis.t > `N `C `S0 `S1 ]) t

    val sym4d_total :
      n:[ `K ] Size.t ->
      c:[ `K ] Size.t ->
      s0:[ `K ] Size.t ->
      s1:[ `K ] Size.t ->
      ([< Length.tag > `L4 ], [< Size.tag > `K ], [< Axis.t > `N `C `S0 `S1 ]) t

    val sym5d_partial :
      n:[< Size.tag ] Size.t ->
      c:[< Size.tag ] Size.t ->
      s0:[< Size.tag ] Size.t ->
      s1:[< Size.tag ] Size.t ->
      s2:[< Size.tag ] Size.t ->
      ([< Length.tag > `L5 ], Size.tag, [< Axis.t > `N `C `S0 `S1 `S2 ]) t

    val sym5d_total :
      n:[ `K ] Size.t ->
      c:[ `K ] Size.t ->
      s0:[ `K ] Size.t ->
      s1:[ `K ] Size.t ->
      s2:[ `K ] Size.t ->
      ([< Length.tag > `L5 ], [< Size.tag > `K ], [< Axis.t > `N `C `S0 `S1 `S2 ]) t

    val abs0d_partial : ([< Length.tag > `L0 ], Size.tag, [< Axis.t > `Idx ]) t

    val abs0d_total : ([< Length.tag > `L0 ], [< Size.tag > `K ], [< Axis.t > `Idx ]) t

    val abs1d_partial :
      [< Size.tag ] Size.t -> ([< Length.tag > `L1 ], Size.tag, [< Axis.t > `Idx ]) t

    val abs1d_total :
      [ `K ] Size.t -> ([< Length.tag > `L1 ], [< Size.tag > `K ], [< Axis.t > `Idx ]) t

    val abs2d_partial :
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      ([< Length.tag > `L2 ], Size.tag, [< Axis.t > `Idx ]) t

    val abs2d_total :
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      ([< Length.tag > `L2 ], [< Size.tag > `K ], [< Axis.t > `Idx ]) t

    val abs3d_partial :
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      ([< Length.tag > `L3 ], Size.tag, [< Axis.t > `Idx ]) t

    val abs3d_total :
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      ([< Length.tag > `L3 ], [< Size.tag > `K ], [< Axis.t > `Idx ]) t

    val abs4d_partial :
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      ([< Length.tag > `L4 ], Size.tag, [< Axis.t > `Idx ]) t

    val abs4d_total :
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      ([< Length.tag > `L4 ], [< Size.tag > `K ], [< Axis.t > `Idx ]) t

    val abs5d_partial :
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      ([< Length.tag > `L5 ], Size.tag, [< Axis.t > `Idx ]) t

    val abs5d_total :
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      [ `K ] Size.t ->
      ([< Length.tag > `L5 ], [< Size.tag > `K ], [< Axis.t > `Idx ]) t

    (** Casting  *)

    val to_layout : ('len, 'ax) Layout.t -> (_, 'sz, _) t -> ('len, 'sz, 'ax) t

    val to_length : 'len Length.t -> (_, 'sz, 'ax) t -> ('len, 'sz, 'ax) t

    val to_total : ('len, _, 'ax) t -> ('len, [< Size.tag > `K ], 'ax) t

    val to_symbolic : ('len, 'sz, 'ax) t -> ('len, 'sz, [< Axis.t > `N `C `S0 `S1 `S2 ]) t

    val to_absolute : ('len, 'sz, 'ax) t -> ('len, 'sz, [< Axis.t > `Idx ]) t

    (** MISC. *)

    val symbolize :
      [< Axis.symbolic ] list ->
      ('len, 'sz, [ `Idx of int ]) t ->
      ('len, 'sz, [< Axis.t > `N `C `S0 `S1 `S2 ]) t

    val desymbolize :
      [< Axis.symbolic ] list ->
      ('len, 'sz, [< Axis.symbolic ]) t ->
      ('len, 'sz, [< Axis.t > `Idx ]) t

    val from_int_array : int array -> (Length.tag, [< Size.tag > `K ], [< Axis.t > `Idx ]) t

    val extend :
      ?fill:'sz Size.t ->
      int ->
      (_, 'sz, [< Axis.symbolic ]) t ->
      (Length.tag, 'sz, [< Axis.t > `N `C `S0 `S1 `S2 ]) t

    val pad_left :
      ?fill:'sz Size.t ->
      int ->
      (_, 'sz, [ `Idx of int ]) t ->
      (Length.tag, 'sz, [< Axis.t > `Idx ]) t
  end
end
