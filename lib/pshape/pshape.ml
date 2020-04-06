include Misc
include Pshape_sig

module Pshape :
  PSHAPE
    with type _k := [ `K ] Size.t
     and type _p := [ `K | `U ] Size.t
     and type _t := Length.tag
     and type _s := Size.tag
     and type _a := Axis.t = struct
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

  let sym0d_partial = Sym0d_partial

  let sym0d_total = Sym0d_total

  let sym1d_partial : n:[< Size.tag ] Size.t -> _ =
   fun ~n ->
    Sym1d_partial
      (object
         method n = Size.to_any n
      end)

  let sym1d_total : n:[ `K ] Size.t -> _ =
   fun ~n ->
    Sym1d_total
      (object
         method n = n
      end)

  let sym2d_partial : n:[< Size.tag ] Size.t -> c:[< Size.tag ] Size.t -> _ =
   fun ~n ~c ->
    Sym2d_partial
      (object
         method n = Size.to_any n

         method c = Size.to_any c
      end)

  let sym2d_total : n:[ `K ] Size.t -> c:[ `K ] Size.t -> _ =
   fun ~n ~c ->
    Sym2d_total
      (object
         method n = n

         method c = c
      end)

  let sym3d_partial :
      n:[< Size.tag ] Size.t -> c:[< Size.tag ] Size.t -> s0:[< Size.tag ] Size.t -> _ =
   fun ~n ~c ~s0 ->
    Sym3d_partial
      (object
         method n = Size.to_any n

         method c = Size.to_any c

         method s0 = Size.to_any s0
      end)

  let sym3d_total : n:[ `K ] Size.t -> c:[ `K ] Size.t -> s0:[ `K ] Size.t -> _ =
   fun ~n ~c ~s0 ->
    Sym3d_total
      (object
         method n = n

         method c = c

         method s0 = s0
      end)

  let sym4d_partial :
      n:[< Size.tag ] Size.t ->
      c:[< Size.tag ] Size.t ->
      s0:[< Size.tag ] Size.t ->
      s1:[< Size.tag ] Size.t ->
      _ =
   fun ~n ~c ~s0 ~s1 ->
    Sym4d_partial
      (object
         method n = Size.to_any n

         method c = Size.to_any c

         method s0 = Size.to_any s0

         method s1 = Size.to_any s1
      end)

  let sym4d_total : n:[ `K ] Size.t -> c:[ `K ] Size.t -> s0:[ `K ] Size.t -> s1:[ `K ] Size.t -> _
      =
   fun ~n ~c ~s0 ~s1 ->
    Sym4d_total
      (object
         method n = n

         method c = c

         method s0 = s0

         method s1 = s1
      end)

  let sym5d_partial :
      n:[< Size.tag ] Size.t ->
      c:[< Size.tag ] Size.t ->
      s0:[< Size.tag ] Size.t ->
      s1:[< Size.tag ] Size.t ->
      s2:[< Size.tag ] Size.t ->
      _ =
   fun ~n ~c ~s0 ~s1 ~s2 ->
    Sym5d_partial
      (object
         method n = Size.to_any n

         method c = Size.to_any c

         method s0 = Size.to_any s0

         method s1 = Size.to_any s1

         method s2 = Size.to_any s2
      end)

  let sym5d_total :
      n:[ `K ] Size.t ->
      c:[ `K ] Size.t ->
      s0:[ `K ] Size.t ->
      s1:[ `K ] Size.t ->
      s2:[ `K ] Size.t ->
      _ =
   fun ~n ~c ~s0 ~s1 ~s2 ->
    Sym5d_total
      (object
         method n = n

         method c = c

         method s0 = s0

         method s1 = s1

         method s2 = s2
      end)

  let abs0d_partial = Abs0d_partial

  let abs0d_total = Abs0d_total

  let abs1d_partial : [< Size.tag ] Size.t -> _ = fun a -> Abs1d_partial (Size.to_any a)

  let abs1d_total : [ `K ] Size.t -> _ = fun a -> Abs1d_total a

  let abs2d_partial : [< Size.tag ] Size.t -> [< Size.tag ] Size.t -> _ =
   fun a b -> Abs2d_partial (Size.to_any a, Size.to_any b)

  let abs2d_total : [ `K ] Size.t -> [ `K ] Size.t -> _ = fun a b -> Abs2d_total (a, b)

  let abs3d_partial : [< Size.tag ] Size.t -> [< Size.tag ] Size.t -> [< Size.tag ] Size.t -> _ =
   fun a b c -> Abs3d_partial (Size.to_any a, Size.to_any b, Size.to_any c)

  let abs3d_total : [ `K ] Size.t -> [ `K ] Size.t -> [ `K ] Size.t -> _ =
   fun a b c -> Abs3d_total (a, b, c)

  let abs4d_partial :
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      _ =
   fun a b c d -> Abs4d_partial (Size.to_any a, Size.to_any b, Size.to_any c, Size.to_any d)

  let abs4d_total : [ `K ] Size.t -> [ `K ] Size.t -> [ `K ] Size.t -> [ `K ] Size.t -> _ =
   fun a b c d -> Abs4d_total (a, b, c, d)

  let abs5d_partial :
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      [< Size.tag ] Size.t ->
      _ =
   fun a b c d e ->
    Abs5d_partial (Size.to_any a, Size.to_any b, Size.to_any c, Size.to_any d, Size.to_any e)

  let abs5d_total :
      [ `K ] Size.t -> [ `K ] Size.t -> [ `K ] Size.t -> [ `K ] Size.t -> [ `K ] Size.t -> _ =
   fun a b c d e -> Abs5d_total (a, b, c, d, e)

  let layout (type len sz ax) : (len, sz, ax) t -> (len, ax) Layout.t = function
    | Sym0d_partial -> Layout.Sym0d
    | Sym0d_total -> Layout.Sym0d
    | Sym1d_partial _ -> Layout.Sym1d
    | Sym1d_total _ -> Layout.Sym1d
    | Sym2d_partial _ -> Layout.Sym2d
    | Sym2d_total _ -> Layout.Sym2d
    | Sym3d_partial _ -> Layout.Sym3d
    | Sym3d_total _ -> Layout.Sym3d
    | Sym4d_partial _ -> Layout.Sym4d
    | Sym4d_total _ -> Layout.Sym4d
    | Sym5d_partial _ -> Layout.Sym5d
    | Sym5d_total _ -> Layout.Sym5d
    | Abs0d_partial -> Layout.Abs0d
    | Abs0d_total -> Layout.Abs0d
    | Abs1d_partial _ -> Layout.Abs1d
    | Abs1d_total _ -> Layout.Abs1d
    | Abs2d_partial _ -> Layout.Abs2d
    | Abs2d_total _ -> Layout.Abs2d
    | Abs3d_partial _ -> Layout.Abs3d
    | Abs3d_total _ -> Layout.Abs3d
    | Abs4d_partial _ -> Layout.Abs4d
    | Abs4d_total _ -> Layout.Abs4d
    | Abs5d_partial _ -> Layout.Abs5d
    | Abs5d_total _ -> Layout.Abs5d

  let length (type len sz ax) : (len, sz, ax) t -> len Length.t =
    let open Length in
    function
    | Sym0d_partial -> L0
    | Sym0d_total -> L0
    | Sym1d_partial _ -> L1
    | Sym1d_total _ -> L1
    | Sym2d_partial _ -> L2
    | Sym2d_total _ -> L2
    | Sym3d_partial _ -> L3
    | Sym3d_total _ -> L3
    | Sym4d_partial _ -> L4
    | Sym4d_total _ -> L4
    | Sym5d_partial _ -> L5
    | Sym5d_total _ -> L5
    | Abs0d_partial -> L0
    | Abs0d_total -> L0
    | Abs1d_partial _ -> L1
    | Abs1d_total _ -> L1
    | Abs2d_partial _ -> L2
    | Abs2d_total _ -> L2
    | Abs3d_partial _ -> L3
    | Abs3d_total _ -> L3
    | Abs4d_partial _ -> L4
    | Abs4d_total _ -> L4
    | Abs5d_partial _ -> L5
    | Abs5d_total _ -> L5

  let to_partial (type len sz ax) : (len, sz, ax) t -> (len, Size.tag, ax) t = function
    | Sym0d_partial as v -> v
    | Sym0d_total -> sym0d_partial
    | Sym1d_partial _ as v -> v
    | Sym1d_total v -> sym1d_partial ~n:v#n
    | Sym2d_partial _ as v -> v
    | Sym2d_total v -> sym2d_partial ~n:v#n ~c:v#c
    | Sym3d_partial _ as v -> v
    | Sym3d_total v -> sym3d_partial ~n:v#n ~c:v#c ~s0:v#s0
    | Sym4d_partial _ as v -> v
    | Sym4d_total v -> sym4d_partial ~n:v#n ~c:v#c ~s0:v#s0 ~s1:v#s1
    | Sym5d_partial _ as v -> v
    | Sym5d_total v -> sym5d_partial ~n:v#n ~c:v#c ~s0:v#s0 ~s1:v#s1 ~s2:v#s2
    | Abs0d_partial as v -> v
    | Abs0d_total -> sym0d_partial
    | Abs1d_partial _ as v -> v
    | Abs1d_total (Size.K _ as a) -> Abs1d_partial a
    | Abs2d_partial _ as v -> v
    | Abs2d_total ((Size.K _ as a), (Size.K _ as b)) -> Abs2d_partial (a, b)
    | Abs3d_partial _ as v -> v
    | Abs3d_total ((Size.K _ as a), (Size.K _ as b), (Size.K _ as c)) -> Abs3d_partial (a, b, c)
    | Abs4d_partial _ as v -> v
    | Abs4d_total ((Size.K _ as a), (Size.K _ as b), (Size.K _ as c), (Size.K _ as d)) ->
        Abs4d_partial (a, b, c, d)
    | Abs5d_partial _ as v -> v
    | Abs5d_total
        ((Size.K _ as a), (Size.K _ as b), (Size.K _ as c), (Size.K _ as d), (Size.K _ as e)) ->
        Abs5d_partial (a, b, c, d, e)

  let to_any (type len sz ax) : (len, sz, ax) t -> (Length.tag, Size.tag, Axis.t) t = function
    | Sym0d_partial as s -> s
    | Sym0d_total as s -> s
    | Sym1d_total _ as s -> s
    | Sym1d_partial _ as s -> s
    | Sym2d_total _ as s -> s
    | Sym2d_partial _ as s -> s
    | Sym3d_total _ as s -> s
    | Sym3d_partial _ as s -> s
    | Sym4d_total _ as s -> s
    | Sym4d_partial _ as s -> s
    | Sym5d_total _ as s -> s
    | Sym5d_partial _ as s -> s
    | Abs0d_partial as s -> s
    | Abs0d_total as s -> s
    | Abs1d_total _ as s -> s
    | Abs1d_partial _ as s -> s
    | Abs2d_total _ as s -> s
    | Abs2d_partial _ as s -> s
    | Abs3d_total _ as s -> s
    | Abs3d_partial _ as s -> s
    | Abs4d_total _ as s -> s
    | Abs4d_partial _ as s -> s
    | Abs5d_total _ as s -> s
    | Abs5d_partial _ as s -> s

  let to_any_length (type len sz ax) : (len, sz, ax) t -> (Length.tag, sz, ax) t = function
    | Sym0d_partial as s -> s
    | Sym0d_total as s -> s
    | Sym1d_total _ as s -> s
    | Sym1d_partial _ as s -> s
    | Sym2d_total _ as s -> s
    | Sym2d_partial _ as s -> s
    | Sym3d_total _ as s -> s
    | Sym3d_partial _ as s -> s
    | Sym4d_total _ as s -> s
    | Sym4d_partial _ as s -> s
    | Sym5d_total _ as s -> s
    | Sym5d_partial _ as s -> s
    | Abs0d_total as s -> s
    | Abs0d_partial as s -> s
    | Abs1d_total _ as s -> s
    | Abs1d_partial _ as s -> s
    | Abs2d_total _ as s -> s
    | Abs2d_partial _ as s -> s
    | Abs3d_total _ as s -> s
    | Abs3d_partial _ as s -> s
    | Abs4d_total _ as s -> s
    | Abs4d_partial _ as s -> s
    | Abs5d_total _ as s -> s
    | Abs5d_partial _ as s -> s

  let to_any_axes (type len sz ax) : (len, sz, ax) t -> (len, sz, Axis.t) t = function
    | Sym0d_partial as s -> s
    | Sym0d_total as s -> s
    | Sym1d_total _ as s -> s
    | Sym1d_partial _ as s -> s
    | Sym2d_total _ as s -> s
    | Sym2d_partial _ as s -> s
    | Sym3d_total _ as s -> s
    | Sym3d_partial _ as s -> s
    | Sym4d_total _ as s -> s
    | Sym4d_partial _ as s -> s
    | Sym5d_total _ as s -> s
    | Sym5d_partial _ as s -> s
    | Abs0d_total as s -> s
    | Abs0d_partial as s -> s
    | Abs1d_total _ as s -> s
    | Abs1d_partial _ as s -> s
    | Abs2d_total _ as s -> s
    | Abs2d_partial _ as s -> s
    | Abs3d_total _ as s -> s
    | Abs3d_partial _ as s -> s
    | Abs4d_total _ as s -> s
    | Abs4d_partial _ as s -> s
    | Abs5d_total _ as s -> s
    | Abs5d_partial _ as s -> s

  let is_total (type len sz ax) : (len, sz, ax) t -> bool = function
    | Sym0d_partial -> false
    | Sym0d_total -> true
    | Sym1d_partial _ -> false
    | Sym1d_total _ -> true
    | Sym2d_partial _ -> false
    | Sym2d_total _ -> true
    | Sym3d_partial _ -> false
    | Sym3d_total _ -> true
    | Sym4d_partial _ -> false
    | Sym4d_total _ -> true
    | Sym5d_partial _ -> false
    | Sym5d_total _ -> true
    | Abs0d_partial -> false
    | Abs0d_total -> true
    | Abs1d_partial _ -> false
    | Abs1d_total _ -> true
    | Abs2d_partial _ -> false
    | Abs2d_total _ -> true
    | Abs3d_partial _ -> false
    | Abs3d_total _ -> true
    | Abs4d_partial _ -> false
    | Abs4d_total _ -> true
    | Abs5d_partial _ -> false
    | Abs5d_total _ -> true

  let is_partial (type len sz ax) : (len, sz, ax) t -> bool = fun shape -> not (is_total shape)

  let is_absolute (type len sz ax) : (len, sz, ax) t -> bool = function
    | Sym0d_partial -> false
    | Sym0d_total -> false
    | Sym1d_partial _ -> false
    | Sym1d_total _ -> false
    | Sym2d_partial _ -> false
    | Sym2d_total _ -> false
    | Sym3d_partial _ -> false
    | Sym3d_total _ -> false
    | Sym4d_partial _ -> false
    | Sym4d_total _ -> false
    | Sym5d_partial _ -> false
    | Sym5d_total _ -> false
    | Abs0d_partial -> true
    | Abs0d_total -> true
    | Abs1d_partial _ -> true
    | Abs1d_total _ -> true
    | Abs2d_partial _ -> true
    | Abs2d_total _ -> true
    | Abs3d_partial _ -> true
    | Abs3d_total _ -> true
    | Abs4d_partial _ -> true
    | Abs4d_total _ -> true
    | Abs5d_partial _ -> true
    | Abs5d_total _ -> true

  let is_symbolic (type len sz ax) : (len, sz, ax) t -> bool = fun shape -> not (is_absolute shape)

  let to_list (type len sz ax) : (len, sz, ax) t -> (ax * sz Size.t) list = function
    | Sym0d_partial -> []
    | Sym0d_total -> []
    | Sym1d_partial v -> [ (`N, Size.to_any v#n) ]
    | Sym1d_total v -> [ (`N, Size.Open.to_known v#n) ]
    | Sym2d_partial v -> [ (`N, Size.to_any v#n); (`C, Size.to_any v#c) ]
    | Sym2d_total v -> [ (`N, Size.Open.to_known v#n); (`C, Size.Open.to_known v#c) ]
    | Sym3d_partial v -> [ (`N, Size.to_any v#n); (`C, Size.to_any v#c); (`S0, Size.to_any v#s0) ]
    | Sym3d_total v ->
        [
          (`N, Size.Open.to_known v#n); (`C, Size.Open.to_known v#c); (`S0, Size.Open.to_known v#s0);
        ]
    | Sym4d_partial v ->
        [
          (`N, Size.to_any v#n);
          (`C, Size.to_any v#c);
          (`S0, Size.to_any v#s0);
          (`S1, Size.to_any v#s1);
        ]
    | Sym4d_total v ->
        [
          (`N, Size.Open.to_known v#n);
          (`C, Size.Open.to_known v#c);
          (`S0, Size.Open.to_known v#s0);
          (`S1, Size.Open.to_known v#s1);
        ]
    | Sym5d_partial v ->
        [
          (`N, Size.to_any v#n);
          (`C, Size.to_any v#c);
          (`S0, Size.to_any v#s0);
          (`S1, Size.to_any v#s1);
          (`S2, Size.to_any v#s2);
        ]
    | Sym5d_total v ->
        [
          (`N, Size.Open.to_known v#n);
          (`C, Size.Open.to_known v#c);
          (`S0, Size.Open.to_known v#s0);
          (`S1, Size.Open.to_known v#s1);
          (`S2, Size.Open.to_known v#s2);
        ]
    | Abs0d_partial -> []
    | Abs0d_total -> []
    | Abs1d_partial a -> [ (`Idx 0, Size.to_any a) ]
    | Abs1d_total a -> [ (`Idx 0, Size.Open.to_known a) ]
    | Abs2d_partial (a, b) -> [ (`Idx 0, Size.to_any a); (`Idx 1, Size.to_any b) ]
    | Abs2d_total (a, b) -> [ (`Idx 0, Size.Open.to_known a); (`Idx 1, Size.Open.to_known b) ]
    | Abs3d_partial (a, b, c) ->
        [ (`Idx 0, Size.to_any a); (`Idx 1, Size.to_any b); (`Idx 2, Size.to_any c) ]
    | Abs3d_total (a, b, c) ->
        [
          (`Idx 0, Size.Open.to_known a);
          (`Idx 1, Size.Open.to_known b);
          (`Idx 2, Size.Open.to_known c);
        ]
    | Abs4d_partial (a, b, c, d) ->
        [
          (`Idx 0, Size.to_any a);
          (`Idx 1, Size.to_any b);
          (`Idx 2, Size.to_any c);
          (`Idx 3, Size.to_any d);
        ]
    | Abs4d_total (a, b, c, d) ->
        [
          (`Idx 0, Size.Open.to_known a);
          (`Idx 1, Size.Open.to_known b);
          (`Idx 2, Size.Open.to_known c);
          (`Idx 3, Size.Open.to_known d);
        ]
    | Abs5d_partial (a, b, c, d, e) ->
        [
          (`Idx 0, Size.to_any a);
          (`Idx 1, Size.to_any b);
          (`Idx 2, Size.to_any c);
          (`Idx 3, Size.to_any d);
          (`Idx 4, Size.to_any e);
        ]
    | Abs5d_total (a, b, c, d, e) ->
        [
          (`Idx 0, Size.Open.to_known a);
          (`Idx 1, Size.Open.to_known b);
          (`Idx 2, Size.Open.to_known c);
          (`Idx 3, Size.Open.to_known d);
          (`Idx 4, Size.Open.to_known e);
        ]

  let numel (type sz) : (_, sz, _) t -> sz Size.t =
   fun shape ->
    let shape = shape |> to_any_length |> to_any_axes in
    match shape with
    | Abs0d_partial -> Size.K 1 |> Size.to_any
    | Abs0d_total -> Size.K 1
    | Sym0d_partial -> Size.K 1 |> Size.to_any
    | Sym0d_total -> Size.K 1
    | _ -> (
        match to_list shape |> List.map snd with
        | hd :: tl -> List.fold_left Size.mul hd tl
        | _ -> failwith "unreachable" )

  let equal : (_, _, _) t -> (_, _, _) t -> bool =
   fun shape shape' ->
    let shape, shape' = (to_any shape, to_any shape') in
    to_list shape = to_list shape'

  let all_equal : (_, _, _) t list -> bool =
   fun l ->
    match l with [] -> true | hd :: tl -> List.fold_left (fun acc s -> acc && equal hd s) true tl

  let ndim (type len sz ax) : (len, sz, ax) t -> int = fun shape -> length shape |> Length.to_int

  let axes (type len sz ax) : (len, sz, ax) t -> ax list =
   fun shape -> to_list shape |> List.map fst

  let to_string : (_, _, _) t -> string =
   fun shape ->
    let shape = to_any shape in
    if is_absolute shape then
      to_list shape
      |> List.map (fun (_, size) -> Size.to_string size)
      |> String.concat ", "
      (* |> Printf.sprintf "((%s) %s)" (if is_total shape then "t" else "p") *)
      |> Printf.sprintf "(%s)"
    else
      to_list shape
      |> List.map (fun (axis, size) -> Axis.to_string axis ^ "=" ^ Size.to_string size)
      |> String.concat " ; "
      (* |> Printf.sprintf "{(%s) %s}" (if is_total shape then "t" else "p") *)
      |> Printf.sprintf "{%s}"

  let to_total (type len sz ax) : (len, sz, ax) t -> (len, [< Size.tag > `K ], ax) t =
   fun shape ->
    let fail () = to_string shape |> Printf.sprintf "Can't upcast %s to total" |> invalid_arg in
    match shape with
    | Sym0d_total as v -> v
    | Sym0d_partial -> Sym0d_total
    | Sym1d_total _ as v -> v
    | Sym1d_partial v -> ( match v#n with Size.K _ as n -> sym1d_total ~n | _ -> fail () )
    | Sym2d_total _ as v -> v
    | Sym2d_partial v -> (
        match (v#n, v#c) with (Size.K _ as n), (Size.K _ as c) -> sym2d_total ~n ~c | _ -> fail () )
    | Sym3d_total _ as v -> v
    | Sym3d_partial v -> (
        match (v#n, v#c, v#s0) with
        | (Size.K _ as n), (Size.K _ as c), (Size.K _ as s0) -> sym3d_total ~n ~c ~s0
        | _ -> fail () )
    | Sym4d_total _ as v -> v
    | Sym4d_partial v -> (
        match (v#n, v#c, v#s0, v#s1) with
        | (Size.K _ as n), (Size.K _ as c), (Size.K _ as s0), (Size.K _ as s1) ->
            sym4d_total ~n ~c ~s0 ~s1
        | _ -> fail () )
    | Sym5d_total _ as v -> v
    | Sym5d_partial v -> (
        match (v#n, v#c, v#s0, v#s1, v#s2) with
        | (Size.K _ as n), (Size.K _ as c), (Size.K _ as s0), (Size.K _ as s1), (Size.K _ as s2) ->
            sym5d_total ~n ~c ~s0 ~s1 ~s2
        | _ -> fail () )
    | Abs0d_total as v -> v
    | Abs0d_partial -> Abs0d_total
    | Abs1d_total _ as v -> v
    | Abs1d_partial (Size.K _ as a) -> Abs1d_total a
    | Abs2d_total _ as v -> v
    | Abs2d_partial ((Size.K _ as a), (Size.K _ as b)) -> Abs2d_total (a, b)
    | Abs3d_total _ as v -> v
    | Abs3d_partial ((Size.K _ as a), (Size.K _ as b), (Size.K _ as c)) -> Abs3d_total (a, b, c)
    | Abs4d_total _ as v -> v
    | Abs4d_partial ((Size.K _ as a), (Size.K _ as b), (Size.K _ as c), (Size.K _ as d)) ->
        Abs4d_total (a, b, c, d)
    | Abs5d_total _ as v -> v
    | Abs5d_partial
        ((Size.K _ as a), (Size.K _ as b), (Size.K _ as c), (Size.K _ as d), (Size.K _ as e)) ->
        Abs5d_total (a, b, c, d, e)
    | _ -> fail ()

  let _to_layout (type len' ax' len sz ax) :
      (len, ax) Layout.t -> (len', sz, ax') t -> (len, sz, ax) t =
   fun layout shape ->
    match (layout, shape) with
    | Layout.Sym0d, (Sym0d_partial as v) -> v
    | Layout.Sym0d, (Sym0d_total as v) -> v
    | Layout.Sym1d, (Sym1d_partial _ as v) -> v
    | Layout.Sym1d, (Sym1d_total _ as v) -> v
    | Layout.Sym2d, (Sym2d_partial _ as v) -> v
    | Layout.Sym2d, (Sym2d_total _ as v) -> v
    | Layout.Sym3d, (Sym3d_partial _ as v) -> v
    | Layout.Sym3d, (Sym3d_total _ as v) -> v
    | Layout.Sym4d, (Sym4d_partial _ as v) -> v
    | Layout.Sym4d, (Sym4d_total _ as v) -> v
    | Layout.Sym5d, (Sym5d_partial _ as v) -> v
    | Layout.Sym5d, (Sym5d_total _ as v) -> v
    | Layout.Abs0d, (Abs0d_partial as v) -> v
    | Layout.Abs0d, (Abs0d_total as v) -> v
    | Layout.Abs1d, (Abs1d_partial _ as v) -> v
    | Layout.Abs1d, (Abs1d_total _ as v) -> v
    | Layout.Abs2d, (Abs2d_partial _ as v) -> v
    | Layout.Abs2d, (Abs2d_total _ as v) -> v
    | Layout.Abs3d, (Abs3d_partial _ as v) -> v
    | Layout.Abs3d, (Abs3d_total _ as v) -> v
    | Layout.Abs4d, (Abs4d_partial _ as v) -> v
    | Layout.Abs4d, (Abs4d_total _ as v) -> v
    | Layout.Abs5d, (Abs5d_partial _ as v) -> v
    | Layout.Abs5d, (Abs5d_total _ as v) -> v
    | _, _ ->
        Printf.sprintf "Can't upscast %s to layout %s" (to_string shape) (Layout.to_string layout)
        |> invalid_arg

  let _to_length (type len' len sz ax) : len Length.t -> (len', sz, ax) t -> (len, sz, ax) t =
   fun len shape ->
    match (len, shape) with
    | Length.L0, (Sym0d_partial as v) -> v
    | Length.L0, (Sym0d_total as v) -> v
    | Length.L1, (Sym1d_partial _ as v) -> v
    | Length.L1, (Sym1d_total _ as v) -> v
    | Length.L2, (Sym2d_partial _ as v) -> v
    | Length.L2, (Sym2d_total _ as v) -> v
    | Length.L3, (Sym3d_partial _ as v) -> v
    | Length.L3, (Sym3d_total _ as v) -> v
    | Length.L4, (Sym4d_partial _ as v) -> v
    | Length.L4, (Sym4d_total _ as v) -> v
    | Length.L5, (Sym5d_partial _ as v) -> v
    | Length.L5, (Sym5d_total _ as v) -> v
    | Length.L0, (Abs0d_partial as v) -> v
    | Length.L0, (Abs0d_total as v) -> v
    | Length.L1, (Abs1d_partial _ as v) -> v
    | Length.L1, (Abs1d_total _ as v) -> v
    | Length.L2, (Abs2d_partial _ as v) -> v
    | Length.L2, (Abs2d_total _ as v) -> v
    | Length.L3, (Abs3d_partial _ as v) -> v
    | Length.L3, (Abs3d_total _ as v) -> v
    | Length.L4, (Abs4d_partial _ as v) -> v
    | Length.L4, (Abs4d_total _ as v) -> v
    | Length.L5, (Abs5d_partial _ as v) -> v
    | Length.L5, (Abs5d_total _ as v) -> v
    | _, _ ->
        Printf.sprintf "Can't upscast %s to length %s" (to_string shape) (Length.to_string len)
        |> invalid_arg

  let to_symbolic (type len sz ax) : (len, sz, ax) t -> (len, sz, [< Axis.t > `N `C `S0 `S1 `S2 ]) t
      = function
    | Sym0d_total as s -> s
    | Sym0d_partial as s -> s
    | Sym1d_total _ as s -> s
    | Sym1d_partial _ as s -> s
    | Sym2d_total _ as s -> s
    | Sym2d_partial _ as s -> s
    | Sym3d_total _ as s -> s
    | Sym3d_partial _ as s -> s
    | Sym4d_total _ as s -> s
    | Sym4d_partial _ as s -> s
    | Sym5d_total _ as s -> s
    | Sym5d_partial _ as s -> s
    | s -> to_string s |> Printf.sprintf "Can't upcast %s to symbolic" |> invalid_arg

  let to_absolute (type len sz ax) : (len, sz, ax) t -> (len, sz, [< Axis.t > `Idx ]) t = function
    | Abs0d_total as s -> s
    | Abs0d_partial as s -> s
    | Abs1d_total _ as s -> s
    | Abs1d_partial _ as s -> s
    | Abs2d_total _ as s -> s
    | Abs2d_partial _ as s -> s
    | Abs3d_total _ as s -> s
    | Abs3d_partial _ as s -> s
    | Abs4d_total _ as s -> s
    | Abs4d_partial _ as s -> s
    | Abs5d_total _ as s -> s
    | Abs5d_partial _ as s -> s
    | s -> to_string s |> Printf.sprintf "Can't upcast %s to absolute" |> invalid_arg

  (** Since `ax` is an open type, it is possible to pass a non compatible axis wihout type error.
      e.g. When `ax` is [> `N | `C], [> `S0] is accepted but [`S0] is not.
   *)
  let get (type len sz ax) : (len, sz, ax) t -> ax -> sz Size.t =
   fun shape axis -> to_list shape |> List.find (fun (axis', _) -> axis = axis') |> snd

  (* Contract two shapes together. Useful to compute the resulting shape of a tensordot operation.

     The first element of a tuple in `mapping0` is an axis of `shape0`, the second element is either
     `None` to tag an axis for contraction or `Some axis` to state the destination axis in the new
     shape. The length of `mapping0` should be equal to the length of `shape0`. All axes of `shape0`
     should be stated in `mapping0`. The order of the axes to compress is important.

     The same goes for `mapping1` and `shape1`.

     Only known sizes can be contracted. The list of sizes to contract in mapping0 should be equal to
     the one from mapping1.

     Example 0: Matrix dot product. Produces the shape ((t) 5, 9)
     contract
       [ `Idx 0, Some (`Idx 0)
       ; `Idx 1, None ]
       [ `Idx 0, None
       ; `Idx 1, Some (`Idx 1) ]
       (abs2d_total (Size.K 5) (Size.K 7))
       (abs2d_total (Size.K 7) (Size.K 9))

     Example 1: Pointwise fully connected layer (1x1 convolution) from a symbolic and an absolute shape
       to a symbolic one. Produces the shape {(p) n=_, c=9, s0=_, s1=_}
     contract
       [ `N,     Some `N
       ; `C,     None
       ; `S0,    Some `S0
       ; `S1,    Some `S1 ]
       [ `Idx 0, None
       ; `Idx 1, Some `C ]
       (sym4d_partial ~n:Size.U ~c:(Size.K 7) ~s0:Size.U ~s1:Size.U)
       (abs2d_total (Size.K 7) (Size.K 9))
  *)
  let contract :
      ('ax0 * ([< Axis.t ] as 'ax) option) list ->
      ('ax1 * 'ax option) list ->
      (_, _, 'ax0) t ->
      (_, _, 'ax1) t ->
      (Length.tag, Size.tag, Axis.t) t =
   fun mapping0 mapping1 shape0 shape1 ->
    (* Downcast *)
    let mapping0 = List.map (fun (a, b) -> (a, (b :> Axis.t option))) mapping0 in
    let mapping1 = List.map (fun (a, b) -> (a, (b :> Axis.t option))) mapping1 in
    let shape0 = shape0 |> to_partial in
    let shape1 = shape1 |> to_partial in

    (* Check input axes *)
    if List.map fst mapping0 |> List.sort compare <> (axes shape0 |> List.sort compare) then
      invalid_arg "mapping0 should exactly reference all axes of shape0";
    if List.map fst mapping1 |> List.sort compare <> (axes shape1 |> List.sort compare) then
      invalid_arg "mapping1 should exactly reference all axes of shape1";

    (* Check contracted axes *)
    let contracted_axes0 =
      List.filter_map (fun (ax, opt) -> match opt with None -> Some ax | Some _ -> None) mapping0
    in
    let contracted_axes1 =
      List.filter_map (fun (ax, opt) -> match opt with None -> Some ax | Some _ -> None) mapping1
    in
    let contracted_sizes0 = List.map (get shape0) contracted_axes0 in
    let contracted_sizes1 = List.map (get shape1) contracted_axes1 in
    if List.length contracted_axes0 <> List.length contracted_axes1 then
      invalid_arg "mapping0 and mapping1 don't define the same number of axes to contract";
    List.iter2
      (fun s s' ->
        match (s, s') with
        | Size.U, _ | _, Size.U -> invalid_arg "Can't contract an unknown size"
        | _, _ -> if s <> s' then invalid_arg "Can't contract axes with different sizes")
      contracted_sizes0 contracted_sizes1;

    (* List dst axes / sizes *)
    let dst_axes_sizes0 =
      List.filter_map
        (fun (srcax, dstax) ->
          match dstax with None -> None | Some dstax -> Some (dstax, get shape0 srcax))
        mapping0
    in
    let dst_axes_sizes1 =
      List.filter_map
        (fun (srcax, dstax) ->
          match dstax with None -> None | Some dstax -> Some (dstax, get shape1 srcax))
        mapping1
    in
    let dst_axes_sizes = dst_axes_sizes0 @ dst_axes_sizes1 in
    let dst_symbolic =
      match dst_axes_sizes with
      | [] -> invalid_arg "Can't contract to length 0"
      | (#Axis.symbolic, _) :: _ -> true
      | (#Axis.absolute, _) :: _ -> false
    in
    let dst_total =
      List.for_all
        (fun (_, size) -> match size with Size.U -> false | Size.K _ -> true)
        dst_axes_sizes
    in

    (* Build output shape *)
    let f ax =
      match List.find_opt (fun (ax', _) -> ax = ax') dst_axes_sizes with
      | None -> invalid_arg "Some destination axes are missing from mapping"
      | Some (_, size) -> size
    in
    let g ax = f ax |> Size.Open.to_known in
    match (dst_symbolic, dst_total, List.length dst_axes_sizes) with
    | _, _, 0 -> failwith "unreachable"
    | false, true, 1 -> abs1d_total (g (`Idx 0))
    | false, false, 1 -> abs1d_partial (f (`Idx 0))
    | false, true, 2 -> abs2d_total (g (`Idx 0)) (g (`Idx 1))
    | false, false, 2 -> abs2d_partial (f (`Idx 0)) (f (`Idx 1))
    | false, true, 3 -> abs3d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2))
    | false, false, 3 -> abs3d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2))
    | false, true, 4 -> abs4d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2)) (g (`Idx 3))
    | false, false, 4 -> abs4d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2)) (f (`Idx 3))
    | false, true, 5 -> abs5d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2)) (g (`Idx 3)) (g (`Idx 4))
    | false, false, 5 ->
        abs5d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2)) (f (`Idx 3)) (f (`Idx 4))
    | true, true, 1 -> sym1d_total ~n:(g `N)
    | true, false, 1 -> sym1d_partial ~n:(f `N)
    | true, true, 2 -> sym2d_total ~n:(g `N) ~c:(g `C)
    | true, false, 2 -> sym2d_partial ~n:(f `N) ~c:(f `C)
    | true, true, 3 -> sym3d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0)
    | true, false, 3 -> sym3d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0)
    | true, true, 4 -> sym4d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0) ~s1:(g `S1)
    | true, false, 4 -> sym4d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0) ~s1:(f `S1)
    | true, true, 5 -> sym5d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0) ~s1:(g `S1) ~s2:(g `S2)
    | true, false, 5 -> sym5d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0) ~s1:(f `S1) ~s2:(f `S2)
    | _, _, _ -> invalid_arg "Ndim >5 is unsupported "

  (* Reorder the axes of the input shape. `mapping` contains a list of src-axis/dst-axis pairs.
     `src-axis` refers to an axis in the input `shape`, `dst-axis` refers to an axis in the output
     `shape`.

     A missing output axis will be set to `Size.K 1`.

     An input axis `ax` missing from `mapping` is interpreted as having `ax` as a destination too.

     If several input axes are mapped to the same output axis they will all be flattened together
     in the same order that they are stated in `mapping`. The resulting output size will be the
     product of all the input sizes. An error will be thrown if one of the flattend axis is missing
     from `mapping`.

     If `mapping` is omited, it defaults to the input shape's axes in reverse order, hense the
     name of this function.

     If `mapping` is an empty list and `ndim` is omited, `transpose` is the identity function.

     Example 0: Transposing an abs2d shape
     {| transpose
       (abs2d_total (Size.K 5) (Size.K 3)) |}
     Produces the shape (3, 5)

     Example 1: A 7x9 image from abs2d to sym4d
     {| transpose
       [ `Idx 1, `S0   (* Width *)
       ; `Idx 0, `S1 ] (* Height *)
       (abs2d_total (Size.K 7) (Size.K 9)) |}
     Produces the shape {n=1, c=1, s0=9, s1=7}

     Example 2: Inserting two new dimensions at the end of an absolute shape
     {| transpose ~ndim:4 [] (abs2d_partial (Size.U) (Size.K 5)) |}
     Produces the shape (_, 5, 1, 1)

     Example 3: Flatten `C, `S0 and `S1 together in the `channel last` order.
     {| transpose
       [ `S1, `C
       ; `S0, `C
       ; `C, `C ]
       (sym4d_partial ~n:(Size.U) ~c:(Size.K 2) ~s0:(Size.K 3) ~s1:(Size.K 5)) |}
     Produces the shape {n=_, c=30}
  *)
  let transpose :
      ?ndim:int -> ?mapping:('ax0 * [< Axis.t ]) list -> (_, 'sz, 'ax0) t -> (Length.tag, Size.tag, Axis.t) t
      =
   fun ?ndim:len ?mapping shape ->
    (* Cast *)
    let shape = shape |> to_partial in
    let shape_axes = axes shape in
    let mapping = match mapping with
      | Some mapping -> List.map (fun (a, b) -> (a, (b :> Axis.t))) mapping
      | None -> List.combine shape_axes (List.rev shape_axes :> Axis.t list)
    in

    (* List and check axes *)
    let src_axes = List.map fst mapping in
    let dst_axes = List.map snd mapping in
    let missing_src_axes =
      List.filter_map (fun ax -> if List.mem ax src_axes then None else Some ax) shape_axes
    in
    let is_sym_src = is_symbolic shape in
    let is_abs_src = not is_sym_src in
    let is_sym_dst =
      match dst_axes with [] -> is_sym_src | _ -> List.exists Axis.is_symbolic dst_axes
    in
    let is_abs_dst =
      match dst_axes with [] -> is_abs_src | _ -> List.exists Axis.is_absolute dst_axes
    in
    if is_sym_dst = is_abs_dst then
      invalid_arg
        "In transpose: Invalid axes in mapping. Can't decide if output is symbolic or partial";
    List.iter
      (fun ax ->
        if not (List.mem ax shape_axes) then
          "In transpose: Axis " ^ Axis.to_string ax ^ "doesn't exist in input shape" |> invalid_arg)
      src_axes;
    List.iter
      (fun ax ->
        if List.mem (ax :> Axis.t) dst_axes then
          "In transpose: Input axis " ^ Axis.to_string ax
          ^ " can't be flattened with other axes because it is missing from mapping"
          |> invalid_arg)
      missing_src_axes;

    (* Check ndim parameter *)
    let min_len =
      max
        (List.map Axis.min_ndim_of_axis dst_axes |> List.fold_left max 0)
        (List.map Axis.min_ndim_of_axis missing_src_axes |> List.fold_left max 0)
    in
    let len =
      match len with
      | None -> min_len
      | Some len ->
          if min_len > len then
            Printf.sprintf "In transpose: An output axis doesn't fit inside ndim:%d" len
            |> invalid_arg;
          len
    in

    (* Build output shape *)
    let is_total_dst =
      List.for_all
        (fun (_, size) -> match size with Size.U -> false | Size.K _ -> true)
        (to_list shape)
    in
    let f ax =
      match List.find_all (fun (_, dstax) -> ax = dstax) mapping with
      | [] ->
          if List.mem ax (missing_src_axes :> Axis.t list) then get (shape |> to_any) ax
          else Size.K 1
      | l ->
          let srcaxs = List.map fst l in
          let srcsizes = List.map (get shape) srcaxs in
          List.fold_left Size.mul (List.hd srcsizes) (List.tl srcsizes)
    in
    let g ax = f ax |> Size.Open.to_known in
    match (is_sym_dst, is_total_dst, len) with
    | _, _, 0 -> failwith "unreachable"
    | false, true, 1 -> abs1d_total (g (`Idx 0))
    | false, false, 1 -> abs1d_partial (f (`Idx 0))
    | false, true, 2 -> abs2d_total (g (`Idx 0)) (g (`Idx 1))
    | false, false, 2 -> abs2d_partial (f (`Idx 0)) (f (`Idx 1))
    | false, true, 3 -> abs3d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2))
    | false, false, 3 -> abs3d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2))
    | false, true, 4 -> abs4d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2)) (g (`Idx 3))
    | false, false, 4 -> abs4d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2)) (f (`Idx 3))
    | false, true, 5 -> abs5d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2)) (g (`Idx 3)) (g (`Idx 4))
    | false, false, 5 ->
        abs5d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2)) (f (`Idx 3)) (f (`Idx 4))
    | true, true, 1 -> sym1d_total ~n:(g `N)
    | true, false, 1 -> sym1d_partial ~n:(f `N)
    | true, true, 2 -> sym2d_total ~n:(g `N) ~c:(g `C)
    | true, false, 2 -> sym2d_partial ~n:(f `N) ~c:(f `C)
    | true, true, 3 -> sym3d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0)
    | true, false, 3 -> sym3d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0)
    | true, true, 4 -> sym4d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0) ~s1:(g `S1)
    | true, false, 4 -> sym4d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0) ~s1:(f `S1)
    | true, true, 5 -> sym5d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0) ~s1:(g `S1) ~s2:(g `S2)
    | true, false, 5 -> sym5d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0) ~s1:(f `S1) ~s2:(f `S2)
    | _, _, _ -> invalid_arg "Ndim >5 is unsupported "

  let symbolize (type len sz) :
      [< Axis.symbolic ] list ->
      (len, sz, [ `Idx of int ]) t ->
      (len, sz, [< Axis.t > `N `C `S0 `S1 `S2 ]) t =
   fun axs shape ->
    let axs = (axs :> Axis.symbolic list) in
    if ndim shape <> List.length axs then
      invalid_arg "Length of axis list passed to symbolize should match the shape's length";

    let index_in_list l elt =
      l |> List.mapi (fun i elt' -> (i, elt')) |> List.find (fun (_, elt') -> elt = elt') |> fst
    in
    let size_of_nth_symbol shape i =
      let i = List.nth axs i |> index_in_list [ `N; `C; `S0; `S1; `S2 ] in
      if i >= ndim shape then invalid_arg "Incompatible axis passed to symbolize";
      get shape (`Idx i)
    in
    match shape with
    | Sym0d_partial as v -> v
    | Sym0d_total as v -> v
    | Abs0d_partial -> Sym0d_partial
    | Abs0d_total -> Sym0d_total
    | Abs1d_partial _ as shape ->
        let size = size_of_nth_symbol shape in
        sym1d_partial ~n:(size 0)
    | Abs2d_partial _ as shape ->
        let size = size_of_nth_symbol shape in
        sym2d_partial ~n:(size 0) ~c:(size 1)
    | Abs3d_partial _ as shape ->
        let size = size_of_nth_symbol shape in
        sym3d_partial ~n:(size 0) ~c:(size 1) ~s0:(size 2)
    | Abs4d_partial _ as shape ->
        let size = size_of_nth_symbol shape in
        sym4d_partial ~n:(size 0) ~c:(size 1) ~s0:(size 2) ~s1:(size 3)
    | Abs5d_partial _ as shape ->
        let size = size_of_nth_symbol shape in
        sym5d_partial ~n:(size 0) ~c:(size 1) ~s0:(size 2) ~s1:(size 3) ~s2:(size 4)
    | Abs1d_total _ as shape ->
        let size = size_of_nth_symbol shape in
        sym1d_total ~n:(size 0)
    | Abs2d_total _ as shape ->
        let size = size_of_nth_symbol shape in
        sym2d_total ~n:(size 0) ~c:(size 1)
    | Abs3d_total _ as shape ->
        let size = size_of_nth_symbol shape in
        sym3d_total ~n:(size 0) ~c:(size 1) ~s0:(size 2)
    | Abs4d_total _ as shape ->
        let size = size_of_nth_symbol shape in
        sym4d_total ~n:(size 0) ~c:(size 1) ~s0:(size 2) ~s1:(size 3)
    | Abs5d_total _ as shape ->
        let size = size_of_nth_symbol shape in
        sym5d_total ~n:(size 0) ~c:(size 1) ~s0:(size 2) ~s1:(size 3) ~s2:(size 4)

  let desymbolize (type len sz) :
      [< Axis.symbolic ] list -> (len, sz, [< Axis.symbolic ]) t -> (len, sz, [< Axis.t > `Idx ]) t
      =
   fun axs shape ->
    let axs = (axs :> Axis.symbolic list) in

    (* Casts to closed types are not available from inside this module but we need one below *)
    let to_symbolic (type len sz ax) : (len, sz, ax) t -> (len, sz, Axis.symbolic) t =
      to_symbolic
    in
    let shape = to_symbolic shape in

    if ndim shape <> List.length axs then
      invalid_arg "Length of axis list passed to desymbolize should match the shape's length";

    let size_of_nth_dim shape i =
      let ax = List.nth axs i in
      if not (Axis.compatible_with_ndim ax (ndim shape)) then
        Printf.sprintf "axis %s passed to desymbolize is incompatible with input shape %s"
                       (Axis.to_string ax)
                       (to_string shape)
        |> invalid_arg;
      get shape ax
    in
    match shape with
    | Sym0d_partial -> Abs0d_partial
    | Sym0d_total -> Abs0d_total
    | Sym1d_partial _ as shape ->
        let size = size_of_nth_dim shape in
        Abs1d_partial (size 0)
    | Sym2d_partial _ as shape ->
        let size = size_of_nth_dim shape in
        Abs2d_partial (size 0, size 1)
    | Sym3d_partial _ as shape ->
        let size = size_of_nth_dim shape in
        Abs3d_partial (size 0, size 1, size 2)
    | Sym4d_partial _ as shape ->
        let size = size_of_nth_dim shape in
        Abs4d_partial (size 0, size 1, size 2, size 3)
    | Sym5d_partial _ as shape ->
        let size = size_of_nth_dim shape in
        Abs5d_partial (size 0, size 1, size 2, size 3, size 4)
    | Sym1d_total _ as shape ->
        let size = size_of_nth_dim shape in
        Abs1d_total (size 0)
    | Sym2d_total _ as shape ->
        let size = size_of_nth_dim shape in
        Abs2d_total (size 0, size 1)
    | Sym3d_total _ as shape ->
        let size = size_of_nth_dim shape in
        Abs3d_total (size 0, size 1, size 2)
    | Sym4d_total _ as shape ->
        let size = size_of_nth_dim shape in
        Abs4d_total (size 0, size 1, size 2, size 3)
    | Sym5d_total _ as shape ->
        let size = size_of_nth_dim shape in
        Abs5d_total (size 0, size 1, size 2, size 3, size 4)

  let squeeze :
      ([< Axis.t ] as 'ax) list ->
      ([< Length.tag ], [< Size.tag ], 'ax) t ->
      (Length.tag, Size.tag, Axis.t) t =
   fun squeeze_axes shape ->
    let shape = shape |> to_any in
    let squeeze_axes = (squeeze_axes :> Axis.t list) in

    let ndim0 = ndim shape in
    let ndim1 = ndim0 - List.length squeeze_axes in
    let shape_axes = axes shape in
    let remaining_axes =
      List.filter_map (fun ax -> if List.mem ax squeeze_axes then None else Some ax) shape_axes
    in

    if List.for_all (fun ax -> List.mem ax shape_axes) squeeze_axes then
      invalid_arg "In squeeze: Invalid axis";
    if List.exists (fun ax -> get (shape |> to_any) ax <> Size.K 1) squeeze_axes then
      invalid_arg "In squeeze: Can only squeeze size 1 axes";
    if
      is_symbolic shape
      && List.sort_uniq compare remaining_axes
         <> List.sort_uniq compare (Axis.symbolic_axes_of_ndim ndim1)
    then invalid_arg "In squeeze: Can' squeeze those axes of a symbolic shape";

    let is_total_dst =
      List.for_all
        (fun ax -> match get shape ax with Size.U -> false | Size.K _ -> true)
        remaining_axes
    in
    let f ax = get shape ax in
    let g ax = f ax |> Size.Open.to_known in
    match (is_symbolic shape, ndim1, is_total_dst) with
    | false, 0, false -> abs0d_partial
    | false, 0, true -> abs0d_total
    | false, 1, false -> abs1d_partial (f (`Idx 0))
    | false, 1, true -> abs1d_total (g (`Idx 0))
    | false, 2, false -> abs2d_partial (f (`Idx 0)) (f (`Idx 1))
    | false, 2, true -> abs2d_total (g (`Idx 0)) (g (`Idx 1))
    | false, 3, false -> abs3d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2))
    | false, 3, true -> abs3d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2))
    | false, 4, false -> abs4d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2)) (f (`Idx 3))
    | false, 4, true -> abs4d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2)) (g (`Idx 3))
    | false, 5, false ->
        abs5d_partial (f (`Idx 0)) (f (`Idx 1)) (f (`Idx 2)) (f (`Idx 3)) (f (`Idx 4))
    | false, 5, true -> abs5d_total (g (`Idx 0)) (g (`Idx 1)) (g (`Idx 2)) (g (`Idx 3)) (g (`Idx 4))
    | true, 0, false -> sym0d_partial
    | true, 0, true -> sym0d_total
    | true, 1, false -> sym1d_partial ~n:(f `N)
    | true, 1, true -> sym1d_total ~n:(g `N)
    | true, 2, false -> sym2d_partial ~n:(f `N) ~c:(f `C)
    | true, 2, true -> sym2d_total ~n:(g `N) ~c:(g `C)
    | true, 3, false -> sym3d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0)
    | true, 3, true -> sym3d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0)
    | true, 4, false -> sym4d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0) ~s1:(f `S1)
    | true, 4, true -> sym4d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0) ~s1:(g `S1)
    | true, 5, false -> sym5d_partial ~n:(f `N) ~c:(f `C) ~s0:(f `S0) ~s1:(f `S1) ~s2:(f `S2)
    | true, 5, true -> sym5d_total ~n:(g `N) ~c:(g `C) ~s0:(g `S0) ~s1:(g `S1) ~s2:(g `S2)
    | _, _, _ -> failwith "unreachable"

  let to_int_array (type len) : (len, [ `K ], [ `Idx of int ]) t -> int array = function
    | Sym0d_total -> [||]
    | Abs0d_total -> [||]
    | Abs1d_total (Size.K a) -> [| a |]
    | Abs2d_total (Size.K a, Size.K b) -> [| a; b |]
    | Abs3d_total (Size.K a, Size.K b, Size.K c) -> [| a; b; c |]
    | Abs4d_total (Size.K a, Size.K b, Size.K c, Size.K d) -> [| a; b; c; d |]
    | Abs5d_total (Size.K a, Size.K b, Size.K c, Size.K d, Size.K e) -> [| a; b; c; d; e |]

  let from_int_array : int array -> ([> Length.tag ], [< Size.tag > `K ], [< Axis.t > `Idx ]) t =
   fun arr ->
    if Array.exists (Fun.flip ( < ) 0) arr then invalid_arg "A dimension size should be >= 0";
    match arr with
    | [||] -> Abs0d_total
    | [| a |] -> Abs1d_total (Size.K a)
    | [| a; b |] -> Abs2d_total (Size.K a, Size.K b)
    | [| a; b; c |] -> Abs3d_total (Size.K a, Size.K b, Size.K c)
    | [| a; b; c; d |] -> Abs4d_total (Size.K a, Size.K b, Size.K c, Size.K d)
    | [| a; b; c; d; e |] -> Abs5d_total (Size.K a, Size.K b, Size.K c, Size.K d, Size.K e)
    | _ -> invalid_arg "Shapes of length > 2 are not implemented"

  let ndim (type len sz ax) : (len, sz, ax) t -> int = fun shape -> length shape |> Length.to_int

  let nth (type sz) : (_, sz, _) t -> int -> sz Size.t =
   fun shape i -> to_list shape |> Fun.flip List.nth i |> snd

  let set (type len sz ax) : (len, sz, ax) t -> ax -> sz Size.t -> (len, sz, ax) t =
   fun shape axis size ->
    match (shape, axis, size) with
    | Sym1d_partial _, `N, ((Size.U | Size.K _) as n) -> sym1d_partial ~n
    | Sym1d_total _, `N, (Size.K _ as n) -> sym1d_total ~n
    | Sym2d_partial v, `N, ((Size.U | Size.K _) as n) -> sym2d_partial ~n ~c:v#c
    | Sym2d_total v, `N, (Size.K _ as n) -> sym2d_total ~n ~c:v#c
    | Sym2d_partial v, `C, ((Size.U | Size.K _) as c) -> sym2d_partial ~n:v#n ~c
    | Sym2d_total v, `C, (Size.K _ as c) -> sym2d_total ~n:v#n ~c
    | Sym3d_partial v, `N, ((Size.U | Size.K _) as n) -> sym3d_partial ~n ~c:v#c ~s0:v#s0
    | Sym3d_total v, `N, (Size.K _ as n) -> sym3d_total ~n ~c:v#c ~s0:v#s0
    | Sym3d_partial v, `C, ((Size.U | Size.K _) as c) -> sym3d_partial ~n:v#n ~c ~s0:v#s0
    | Sym3d_total v, `C, (Size.K _ as c) -> sym3d_total ~n:v#n ~c ~s0:v#s0
    | Sym3d_partial v, `S0, ((Size.U | Size.K _) as s0) -> sym3d_partial ~n:v#n ~c:v#c ~s0
    | Sym3d_total v, `S0, (Size.K _ as s0) -> sym3d_total ~n:v#n ~c:v#c ~s0
    | Sym4d_partial v, `N, ((Size.U | Size.K _) as n) -> sym4d_partial ~n ~c:v#c ~s0:v#s0 ~s1:v#s1
    | Sym4d_total v, `N, (Size.K _ as n) -> sym4d_total ~n ~c:v#c ~s0:v#s0 ~s1:v#s1
    | Sym4d_partial v, `C, ((Size.U | Size.K _) as c) -> sym4d_partial ~n:v#n ~c ~s0:v#s0 ~s1:v#s1
    | Sym4d_total v, `C, (Size.K _ as c) -> sym4d_total ~n:v#n ~c ~s0:v#s0 ~s1:v#s1
    | Sym4d_partial v, `S0, ((Size.U | Size.K _) as s0) -> sym4d_partial ~n:v#n ~c:v#c ~s0 ~s1:v#s1
    | Sym4d_total v, `S0, (Size.K _ as s0) -> sym4d_total ~n:v#n ~c:v#c ~s0 ~s1:v#s1
    | Sym4d_partial v, `S1, ((Size.U | Size.K _) as s1) -> sym4d_partial ~n:v#n ~c:v#c ~s0:v#s0 ~s1
    | Sym4d_total v, `S1, (Size.K _ as s1) -> sym4d_total ~n:v#n ~c:v#c ~s0:v#s0 ~s1
    | Sym5d_partial v, `N, ((Size.U | Size.K _) as n) ->
        sym5d_partial ~n ~c:v#c ~s0:v#s0 ~s1:v#s1 ~s2:v#s2
    | Sym5d_total v, `N, (Size.K _ as n) -> sym5d_total ~n ~c:v#c ~s0:v#s0 ~s1:v#s1 ~s2:v#s2
    | Sym5d_partial v, `C, ((Size.U | Size.K _) as c) ->
        sym5d_partial ~n:v#n ~c ~s0:v#s0 ~s1:v#s1 ~s2:v#s2
    | Sym5d_total v, `C, (Size.K _ as c) -> sym5d_total ~n:v#n ~c ~s0:v#s0 ~s1:v#s1 ~s2:v#s2
    | Sym5d_partial v, `S0, ((Size.U | Size.K _) as s0) ->
        sym5d_partial ~n:v#n ~c:v#c ~s0 ~s1:v#s1 ~s2:v#s2
    | Sym5d_total v, `S0, (Size.K _ as s0) -> sym5d_total ~n:v#n ~c:v#c ~s0 ~s1:v#s1 ~s2:v#s2
    | Sym5d_partial v, `S1, ((Size.U | Size.K _) as s1) ->
        sym5d_partial ~n:v#n ~c:v#c ~s0:v#s0 ~s1 ~s2:v#s2
    | Sym5d_total v, `S1, (Size.K _ as s1) -> sym5d_total ~n:v#n ~c:v#c ~s0:v#s0 ~s1 ~s2:v#s2
    | Sym5d_partial v, `S2, ((Size.U | Size.K _) as s2) ->
        sym5d_partial ~n:v#n ~c:v#c ~s0:v#s0 ~s1:v#s1 ~s2
    | Sym5d_total v, `S2, (Size.K _ as s2) -> sym5d_total ~n:v#n ~c:v#c ~s0:v#s0 ~s1:v#s1 ~s2
    | Abs1d_partial _, `Idx 0, ((Size.U | Size.K _) as a) -> Abs1d_partial a
    | Abs1d_total _, `Idx 0, (Size.K _ as a) -> Abs1d_total a
    | Abs2d_partial (_, b), `Idx 0, ((Size.U | Size.K _) as a) -> Abs2d_partial (a, b)
    | Abs2d_total (_, b), `Idx 0, (Size.K _ as a) -> Abs2d_total (a, b)
    | Abs2d_partial (a, _), `Idx 1, ((Size.U | Size.K _) as b) -> Abs2d_partial (a, b)
    | Abs2d_total (a, _), `Idx 1, (Size.K _ as b) -> Abs2d_total (a, b)
    | Abs3d_partial (_, b, c), `Idx 0, ((Size.U | Size.K _) as a) -> Abs3d_partial (a, b, c)
    | Abs3d_total (_, b, c), `Idx 0, (Size.K _ as a) -> Abs3d_total (a, b, c)
    | Abs3d_partial (a, _, c), `Idx 1, ((Size.U | Size.K _) as b) -> Abs3d_partial (a, b, c)
    | Abs3d_total (a, _, c), `Idx 1, (Size.K _ as b) -> Abs3d_total (a, b, c)
    | Abs3d_partial (a, b, _), `Idx 2, ((Size.U | Size.K _) as c) -> Abs3d_partial (a, b, c)
    | Abs3d_total (a, b, _), `Idx 2, (Size.K _ as c) -> Abs3d_total (a, b, c)
    | Abs4d_partial (_, b, c, d), `Idx 0, ((Size.U | Size.K _) as a) -> Abs4d_partial (a, b, c, d)
    | Abs4d_total (_, b, c, d), `Idx 0, (Size.K _ as a) -> Abs4d_total (a, b, c, d)
    | Abs4d_partial (a, _, c, d), `Idx 1, ((Size.U | Size.K _) as b) -> Abs4d_partial (a, b, c, d)
    | Abs4d_total (a, _, c, d), `Idx 1, (Size.K _ as b) -> Abs4d_total (a, b, c, d)
    | Abs4d_partial (a, b, _, d), `Idx 2, ((Size.U | Size.K _) as c) -> Abs4d_partial (a, b, c, d)
    | Abs4d_total (a, b, _, d), `Idx 2, (Size.K _ as c) -> Abs4d_total (a, b, c, d)
    | Abs4d_partial (a, b, c, _), `Idx 3, ((Size.U | Size.K _) as d) -> Abs4d_partial (a, b, c, d)
    | Abs4d_total (a, b, c, _), `Idx 3, (Size.K _ as d) -> Abs4d_total (a, b, c, d)
    | Abs5d_partial (_, b, c, d, e), `Idx 0, ((Size.U | Size.K _) as a) ->
        Abs5d_partial (a, b, c, d, e)
    | Abs5d_total (_, b, c, d, e), `Idx 0, (Size.K _ as a) -> Abs5d_total (a, b, c, d, e)
    | Abs5d_partial (a, _, c, d, e), `Idx 1, ((Size.U | Size.K _) as b) ->
        Abs5d_partial (a, b, c, d, e)
    | Abs5d_total (a, _, c, d, e), `Idx 1, (Size.K _ as b) -> Abs5d_total (a, b, c, d, e)
    | Abs5d_partial (a, b, _, d, e), `Idx 2, ((Size.U | Size.K _) as c) ->
        Abs5d_partial (a, b, c, d, e)
    | Abs5d_total (a, b, _, d, e), `Idx 2, (Size.K _ as c) -> Abs5d_total (a, b, c, d, e)
    | Abs5d_partial (a, b, c, _, e), `Idx 3, ((Size.U | Size.K _) as d) ->
        Abs5d_partial (a, b, c, d, e)
    | Abs5d_total (a, b, c, _, e), `Idx 3, (Size.K _ as d) -> Abs5d_total (a, b, c, d, e)
    | Abs5d_partial (a, b, c, d, _), `Idx 4, ((Size.U | Size.K _) as e) ->
        Abs5d_partial (a, b, c, d, e)
    | Abs5d_total (a, b, c, d, _), `Idx 4, (Size.K _ as e) -> Abs5d_total (a, b, c, d, e)
    | _, _, _ ->
        Printf.sprintf "Can't set %s to %s on that axis" (to_string shape) (Size.to_string size)
        |> invalid_arg

  let concatenate (type len sz ax) : (len, sz, ax) t -> (len, sz, ax) t -> ax -> (len, sz, ax) t =
   fun shape shape' axis ->
    let fail () =
      Printf.sprintf "Can't concatenate %s and %s on that axis" (to_string shape) (to_string shape')
      |> invalid_arg
    in
    let l = to_list shape in
    let l' = to_list shape' in
    if List.length l <> List.length l' then fail ();
    match
      List.fold_left2
        (fun acc (ax, size) (ax', size') ->
          if ax <> ax' || (ax <> axis && size <> size') then fail ();
          if ax = axis then Some (Size.concatenate size size') else acc)
        None l l'
    with
    | None -> fail ()
    | Some size -> set shape axis size

  let concatenate_all (type len sz ax) : (len, sz, ax) t list -> ax -> (len, sz, ax) t =
   fun l axis ->
    match l with
    | [] -> invalid_arg "Can't concatenate nothing"
    | hd :: tl -> List.fold_left (fun a b -> concatenate a b axis) hd tl

  let extend (type sz) :
      ?fill:sz Size.t ->
      int ->
      (_, sz, [< Axis.symbolic ]) t ->
      (Length.tag, sz, [< Axis.t > `N `C `S0 `S1 `S2 ]) t =
   fun ?fill count shape ->
    let shape = shape |> to_any_length |> to_symbolic in
    if count < 0 then invalid_arg "Can't extent with a negative dimension count";
    let rec aux : int -> (_, sz, [< Axis.symbolic ]) t -> (_, sz, [< Axis.t > `N `C `S0 `S1 `S2 ]) t
        =
     fun count shape ->
      match (shape, count) with
      | (Sym0d_partial as shape), 0 -> shape
      | (Sym0d_total as shape), 0 -> shape
      | (Sym1d_partial _ as shape), 0 -> shape
      | (Sym1d_total _ as shape), 0 -> shape
      | (Sym2d_partial _ as shape), 0 -> shape
      | (Sym2d_total _ as shape), 0 -> shape
      | (Sym3d_partial _ as shape), 0 -> shape
      | (Sym3d_total _ as shape), 0 -> shape
      | (Sym4d_partial _ as shape), 0 -> shape
      | (Sym4d_total _ as shape), 0 -> shape
      | (Sym5d_partial _ as shape), 0 -> shape
      | (Sym5d_total _ as shape), 0 -> shape
      | Sym0d_partial, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (sym1d_partial ~n:fill)
      | Sym1d_partial v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (sym2d_partial ~n:v#n ~c:fill)
      | Sym2d_partial v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (sym3d_partial ~n:v#n ~c:v#c ~s0:fill)
      | Sym3d_partial v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (sym4d_partial ~n:v#n ~c:v#c ~s0:v#s0 ~s1:fill)
      | Sym4d_partial v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (sym5d_partial ~n:v#n ~c:v#c ~s0:v#s0 ~s1:v#s1 ~s2:fill)
      | Sym0d_total, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (sym1d_total ~n:fill)
      | Sym1d_total v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (sym2d_total ~n:v#n ~c:fill)
      | Sym2d_total v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (sym3d_total ~n:v#n ~c:v#c ~s0:fill)
      | Sym3d_total v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (sym4d_total ~n:v#n ~c:v#c ~s0:v#s0 ~s1:fill)
      | Sym4d_total v, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (sym5d_total ~n:v#n ~c:v#c ~s0:v#s0 ~s1:v#s1 ~s2:fill)
      | Sym5d_total _, _ | Sym5d_partial _, _ -> invalid_arg ""
    in
    aux count shape

  let pad_left (type sz) :
      ?fill:sz Size.t -> int -> (_, sz, [ `Idx of int ]) t -> (Length.tag, sz, [< Axis.t > `Idx ]) t
      =
   fun ?fill count shape ->
    let shape = to_any_length shape in
    if count < 0 then invalid_arg "Can't pad_left with a negative dimension count";
    let rec aux : int -> (_, sz, [ `Idx of int ]) t -> (_, sz, [< Axis.t > `Idx ]) t =
     fun count shape ->
      match (shape, count) with
      | (Abs0d_partial as shape), 0 -> shape
      | (Abs0d_total as shape), 0 -> shape
      | (Abs1d_partial _ as shape), 0 -> shape
      | (Abs1d_total _ as shape), 0 -> shape
      | (Abs2d_partial _ as shape), 0 -> shape
      | (Abs2d_total _ as shape), 0 -> shape
      | (Abs3d_partial _ as shape), 0 -> shape
      | (Abs3d_total _ as shape), 0 -> shape
      | (Abs4d_partial _ as shape), 0 -> shape
      | (Abs4d_total _ as shape), 0 -> shape
      | (Abs5d_partial _ as shape), 0 -> shape
      | (Abs5d_total _ as shape), 0 -> shape
      | Abs0d_partial, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (abs1d_partial fill)
      | Abs1d_partial a, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (abs2d_partial fill a)
      | Abs2d_partial (a, b), _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (abs3d_partial fill a b)
      | Abs3d_partial (a, b, c), _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (abs4d_partial fill a b c)
      | Abs4d_partial (a, b, c, d), _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.to_any in
          aux (count - 1) (abs5d_partial fill a b c d)
      | Abs0d_total, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (abs1d_total fill)
      | Abs1d_total a, _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (abs2d_total fill a)
      | Abs2d_total (a, b), _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (abs3d_total fill a b)
      | Abs3d_total (a, b, c), _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (abs4d_total fill a b c)
      | Abs4d_total (a, b, c, d), _ ->
          let fill = Option.value fill ~default:(Size.K 1) |> Size.Open.to_known in
          aux (count - 1) (abs5d_total fill a b c d)
      | Sym0d_partial, _ | Sym0d_total, _ -> invalid_arg ""
      | Abs5d_total _, _ | Abs5d_partial _, _ -> invalid_arg ""
    in
    aux count shape

  (** Two shapes are broadcastable if one of those three conditions is met for each dimensions:
      - At least one of the two sizes is known and equal to one
      - The two sizes known and equal
      - The two sizes are unknown
      All input shapes should either be symbolic or absolute. If all input shapes are symolic they
      must have the same layout. If all input shapes are absolute, the smallest ones will be
      padded with `Size.K` on the left so they all share the same layout.
   *)
  let all_broadcastable : (_, _, _) t list -> bool =
    let transpose : 'a list list -> 'a list list =
     fun lists ->
      let arrays = List.map Array.of_list lists |> Array.of_list in
      let len = List.length lists in
      match List.map List.length lists |> List.sort_uniq compare with
      | [] -> []
      | _ :: _ :: _ -> failwith "Unreachable"
      | [ len' ] -> List.init len' (fun i -> List.init len (fun j -> arrays.(j).(i)))
    in
    let homogeneous_layout_all_broadcastable l =
      List.map (fun shape -> to_list shape |> List.map snd) l
      |> transpose
      |> List.for_all Size.all_broadcastable
    in
    fun l ->
      let l = List.map to_any l in
      match List.map is_symbolic l |> List.sort_uniq compare with
      | _ :: _ :: _ -> invalid_arg "Can't compare symbolic and absolute shapes"
      | [] -> true
      | [ false ] ->
          let len = List.fold_left (fun acc shape -> max acc (ndim shape)) 0 l in
          List.map (fun shape -> pad_left (len - ndim shape) (to_absolute shape)) l
          |> homogeneous_layout_all_broadcastable
      | [ true ] -> (
          match List.map ndim l |> List.sort_uniq compare with
          | [] | _ :: _ :: _ -> invalid_arg "Can't compare symbolic shapes with different layouts"
          | _ -> homogeneous_layout_all_broadcastable l )

  (**
      When broadcasting two shapes with the same layout the operation will fail if one of those
      three conditions isn't met per axis:
      - At least one of the two sizes is known and equal to one
      - The two sizes are known and equal
      - The two sizes are unknown

      When broadcasting two absolute shapes with a different layout, the smallest one is first
      padded with ones on its left to match the largest's layout.

      When broadcasting two symbolic shapes with a different layout, the smallest one is first
      filled with ones on the missing dimensions to match the largest's layout.

   *)
  let broadcast (type sz) : (_, sz, _) t -> (_, sz, _) t -> (_, sz, _) t =
    let homogeneous_layout_broadcast (type sz) : (_, sz, _) t -> (_, sz, _) t -> (_, sz, _) t =
     fun shape shape' ->
      List.fold_left
        (fun shape (axis, size) -> set shape axis (Size.broadcast size (get shape' axis)))
        shape (to_list shape)
    in
    fun shape shape' ->
      let shape = shape |> to_any_length |> to_any_axes in
      let shape' = shape' |> to_any_length |> to_any_axes in
      match (is_symbolic shape, is_symbolic shape') with
      | false, true | true, false -> invalid_arg ""
      | false, false ->
          let len, len' = (ndim shape, ndim shape') in

          (* Casts to closed types are not available from inside this module but we need one below *)
          let to_absolute (type len sz ax) : (len, sz, ax) t -> (len, sz, Axis.absolute) t =
            to_absolute
          in

          let shape = pad_left (max 0 (len' - len)) (shape |> to_absolute) |> to_any_axes in
          let shape' = pad_left (max 0 (len - len')) (shape' |> to_absolute) |> to_any_axes in
          homogeneous_layout_broadcast shape shape'
      | true, true ->
          let len, len' = (ndim shape, ndim shape') in

          (* Casts to closed types are not available from inside this module but we need one below *)
          let to_symbolic (type len sz ax) : (len, sz, ax) t -> (len, sz, Axis.symbolic) t =
            to_symbolic
          in

          let shape = extend (max 0 (len' - len)) (shape |> to_symbolic) |> to_any_axes in
          let shape' = extend (max 0 (len - len')) (shape' |> to_symbolic) |> to_any_axes in
          homogeneous_layout_broadcast shape shape'

  let broadcast_all : (_, 'sz, _) t list -> (_, 'sz, _) t =
   fun l ->
    match List.map (fun shape -> shape |> to_any_length |> to_any_axes) l with
    | [] -> invalid_arg "Can't broadcast nothing"
    | hd :: tl -> List.fold_left broadcast hd tl

  module Open = struct
    let sym0d_partial = sym0d_partial

    let sym0d_total = sym0d_total

    let sym1d_partial = sym1d_partial

    let sym1d_total = sym1d_total

    let sym2d_partial = sym2d_partial

    let sym2d_total = sym2d_total

    let sym3d_partial = sym3d_partial

    let sym3d_total = sym3d_total

    let sym4d_partial = sym4d_partial

    let sym4d_total = sym4d_total

    let sym5d_partial = sym5d_partial

    let sym5d_total = sym5d_total

    let abs0d_partial = abs0d_partial

    let abs0d_total = abs0d_total

    let abs1d_partial = abs1d_partial

    let abs1d_total = abs1d_total

    let abs2d_partial = abs2d_partial

    let abs2d_total = abs2d_total

    let abs3d_partial = abs3d_partial

    let abs3d_total = abs3d_total

    let abs4d_partial = abs4d_partial

    let abs4d_total = abs4d_total

    let abs5d_partial = abs5d_partial

    let abs5d_total = abs5d_total

    let to_length = _to_length

    let to_layout = _to_layout

    let to_total = to_total

    let to_symbolic = to_symbolic

    let to_absolute = to_absolute

    let symbolize = symbolize

    let desymbolize = desymbolize

    let from_int_array = from_int_array

    let extend = extend

    let pad_left = pad_left
  end
end
include Pshape
