
(** {| memoized_walk f root |} is the result of {| follow root |} with {| follow x |}
    being the result of {| f follow x |} memoized. {| follow |} can be called from inside {| f |}
    in order to traverse data in a depth-first fashion. Memoization avoids visiting the same data
    twice. It only works with objects and it doesn't check for loop.
 *)
let memoized_walk : (((< .. > as 'a) -> 'b) -> 'a -> 'b) -> 'a -> 'b =
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

let memoized_walk_map : (((< .. > as 'a) -> 'b) -> 'a -> 'b) -> 'a list -> 'b list =
 fun f l -> List.map (memoized_walk f) l

module type TENSOR = sig
  type ('a, 'b) t

  val of_ba : ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> ('a, 'b) t

  val dimensions : ('a, 'b) t -> int array
end

module type ID = sig
  type t

  val create_default : unit -> t

  val compare : t -> t -> int
end
