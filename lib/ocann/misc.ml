(** {| memoized_walk get_id f root |} is the result of {| follow root |} with {| follow x |}
    being the result of {| f follow x |} memoized. {| follow |} can be called from inside {| f |}
    in order to traverse data in a depth-first fashion. Memoization avoids visiting the same data
    twice. It doesn't check for loop.
 *)
let memoized_walk : ('a -> int) -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b =
 fun get_id f ->
  let table = Hashtbl.create 100 in
  let rec follow node =
    let id = get_id node in
    match Hashtbl.find_opt table id with
    | Some acc -> acc
    | None ->
        let acc = f follow node in
        Hashtbl.add table id acc;
        acc
  in
  follow

let memoized_walk_map : ('a -> int) -> (('a -> 'b) -> 'a -> 'b) -> 'a list -> 'b list =
 fun get_id f l -> List.map (memoized_walk get_id f) l

let memoized_walk_obj : (((< .. > as 'a) -> 'b) -> 'a -> 'b) -> 'a -> 'b =
 fun f ->
  let table = Hashtbl.create 100 in
  let rec follow node =
    let id = Oo.id node in
    match Hashtbl.find_opt table id with
    | Some acc -> acc
    | None ->
        let acc = f follow node in
        Hashtbl.add table id acc;
        acc
  in
  follow

let memoized_walk_obj_map : (((< .. > as 'a) -> 'b) -> 'a -> 'b) -> 'a list -> 'b list =
 fun f l -> List.map (memoized_walk_obj f) l

module type TENSOR = sig
  type ('a, 'b) t

  val of_ba : ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> ('a, 'b) t

  val to_ba : ('a, 'b) t -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t

  val dimensions : ('a, 'b) t -> int array
end

module type ID = sig
  type t

  val create_default : unit -> t

  val compare : t -> t -> int
end

module type STATE = sig
  val get_state : unit -> Random.State.t
end

module Bigarray_tensor :
  TENSOR with type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t = struct
  type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t

  let of_ba x = x

  let to_ba x = x

  let dimensions = Bigarray.Genarray.dims
end

module String_option_id : ID with type t = string option = struct
  type t = string option

  let create_default () = None

  let compare = compare
end
