module Irmin_mem = Irmin_mem

module List = struct
  let rec string_join sep = function
    | a :: (_ :: _ as tail) -> a ^ sep ^ string_join sep tail
    | [ a ] -> a
    | [] -> ""

  let split s idx =
    let len = String.length s in
    (String.sub s 0 idx, String.sub s idx (len - idx))

  let rec chunk size = function
    | "" -> []
    | s ->
        let len = String.length s in
        if len <= size then [ s ]
        else
          let a, b = split s size in
          a :: chunk size b
end

module Color = struct
  (* From `matplotlib.color.LinearSegmentedColormap`:
     https://github.com/matplotlib/matplotlib/blob/e11582f56d69e9d9a32b7bf2d53af055fd1b1915/lib/matplotlib/colors.py#L684
  *)
  let _get_one xys x =
    let rec aux xys =
      match xys with
      | [ x0; y0 ] :: [ x1; y1 ] :: _ when x >= x0 && x <= x1 ->
          let x_width = x1 -. x0 in
          let x_offset = x -. x0 in
          let y_width = y1 -. y0 in
          let y_offset = x_offset /. x_width *. y_width in
          y_offset +. y0
      | _ :: tail -> aux tail
      | [] -> Printf.sprintf "Unreachable (x=%f)" x |> failwith
    in
    if x < 0. || x > 1. then failwith "Trying to interpolate a color out of range";
    aux xys

  module Jet = struct
    (* From `matplotlib.cm.jet`:
       https://github.com/matplotlib/matplotlib/blob/e11582f56d69e9d9a32b7bf2d53af055fd1b1915/lib/matplotlib/_cm.py?ts=4#L243
    *)
    let red = [ [ 0.; 0. ]; [ 0.35; 0. ]; [ 0.66; 1. ]; [ 0.89; 1. ]; [ 1.; 0.5 ] ]

    let green = [ [ 0.; 0. ]; [ 0.125; 0. ]; [ 0.375; 1. ]; [ 0.64; 1. ]; [ 0.91; 0. ]; [ 1.; 0. ] ]

    let blue = [ [ 0.; 0.5 ]; [ 0.11; 1. ]; [ 0.34; 1. ]; [ 0.65; 0. ]; [ 1.; 0. ] ]

    let get x = `RGB_percent (_get_one red x, _get_one green x, _get_one blue x)
  end

  let to_hex_string = function
    | `RGB_percent (r, g, b) ->
        let f x = int_of_float @@ (x *. 255.) in
        Printf.sprintf "#%02X%02X%02X" (f r) (f g) (f b)
end
