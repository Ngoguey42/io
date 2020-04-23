module Irmin_mem = Irmin_mem
module StdList = List

module List = struct
  let rec string_join sep = function
    | a :: (_ :: _ as tail) -> a ^ sep ^ string_join sep tail
    | [ a ] -> a
    | [] -> ""

  let split s idx =
    let len = String.length s in
    (String.sub s 0 idx, String.sub s idx (len - idx))

  let split3 l =
    List.fold_right (fun (a, b, c) (la, lb, lc) -> (a :: la, b :: lb, c :: lc)) l ([], [], [])

  let _ = assert (split3 [ (0, 2, 4); (1, 3, 5) ] = ([ 0; 1 ], [ 2; 3 ], [ 4; 5 ]))

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
  (* From `matplotlib.color.LinearSegmentedColormap`
   * https://github.com/matplotlib/matplotlib/blob/e11582f56d69e9d9a32b7bf2d53af055fd1b1915/lib/matplotlib/colors.py#L684
   *)
  let _get_one xys x =
    let rec aux xys =
      match xys with
      | (x0, y0) :: (x1, y1) :: _ when x >= x0 && x <= x1 ->
          let x_width = x1 -. x0 in
          let x_offset = x -. x0 in
          let y_width = y1 -. y0 in
          let y_offset = x_offset /. x_width *. y_width in
          y_offset +. y0
      | _ :: tail -> aux tail
      | [] -> failwith "Unreachable"
    in
    if Float.is_nan x then failwith "Trying to interpolate a color with a NaN";
    if x < 0. || x > 1. then failwith "Trying to interpolate a color out of range";
    aux xys

  let to_hex_string = function
    | `RGB_percent (r, g, b) ->
        let f x = int_of_float @@ (x *. 255.) in
        Printf.sprintf "#%02X%02X%02X" (f r) (f g) (f b)

  let unpack_xrgb l =
    let pick i l =
      match (i, l) with
      | `r, (x, v, _, _) | `g, (x, _, v, _) | `b, (x, _, _, v) -> (x, v)
      | _, _ -> failwith "unreachable"
    in
    let map = StdList.map in
    (map (pick `r) l, map (pick `g) l, map (pick `b) l)

  module Jet = struct
    (* From `matplotlib.cm.jet`
     * https://github.com/matplotlib/matplotlib/blob/e11582f56d69e9d9a32b7bf2d53af055fd1b1915/lib/matplotlib/_cm.py?ts=4#L243
     *)
    let red = [ (0., 0.); (0.35, 0.); (0.66, 1.); (0.89, 1.); (1., 0.5) ]

    let green = [ (0., 0.); (0.125, 0.); (0.375, 1.); (0.64, 1.); (0.91, 0.); (1., 0.) ]

    let blue = [ (0., 0.5); (0.11, 1.); (0.34, 1.); (0.65, 0.); (1., 0.) ]

    let get x = `RGB_percent (_get_one red x, _get_one green x, _get_one blue x)
  end

  module Firegrass = struct
    (* Generated using the infos and the .xls contained here:
     * http://www.kennethmoreland.com/color-maps/
     *)
    let red, green, blue =
      unpack_xrgb
        [
          (0.0, 0.758112491, 0.214410284, 0.233365527);
          (0.03125, 0.792973226, 0.278253976, 0.26171667);
          (0.0625, 0.824721715, 0.336893693, 0.292041952);
          (0.09375, 0.853149914, 0.392215859, 0.324355787);
          (0.125, 0.878071007, 0.444956966, 0.358634377);
          (0.15625, 0.899319896, 0.495383632, 0.394815586);
          (0.1875, 0.916753461, 0.543535858, 0.432799404);
          (0.21875, 0.930250628, 0.58933337, 0.472448816);
          (0.25, 0.939712236, 0.632628934, 0.51359092);
          (0.28125, 0.945060749, 0.673238039, 0.556018224);
          (0.3125, 0.946239785, 0.710956895, 0.599490112);
          (0.34375, 0.943213507, 0.745574177, 0.643734476);
          (0.375, 0.935965841, 0.776879188, 0.688449558);
          (0.40625, 0.9244995, 0.804667852, 0.733306049);
          (0.4375, 0.908834783, 0.828747317, 0.777949474);
          (0.46875, 0.889008036, 0.848939615, 0.822002902);
          (0.5, 0.865069663, 0.865084671, 0.865070027);
          (0.53125, 0.819531136, 0.867767935, 0.85308666);
          (0.5625, 0.772179049, 0.866735157, 0.835704507);
          (0.59375, 0.723443873, 0.862046472, 0.813149162);
          (0.625, 0.673741625, 0.853784703, 0.785687648);
          (0.65625, 0.623467383, 0.842054539, 0.753624476);
          (0.6875, 0.572988255, 0.82698154, 0.717297142);
          (0.71875, 0.52263548, 0.808710957, 0.677070968);
          (0.75, 0.472695132, 0.78740642, 0.633333147);
          (0.78125, 0.423396466, 0.763248502, 0.586485662);
          (0.8125, 0.374896113, 0.736433175, 0.536936504);
          (0.84375, 0.327254487, 0.707170194, 0.485088029);
          (0.875, 0.280396428, 0.675681414, 0.431320088);
          (0.90625, 0.234036496, 0.642199062, 0.375962865);
          (0.9375, 0.187513317, 0.60696398, 0.319247349);
          (0.96875, 0.139336649, 0.57022386, 0.261201283);
          (1.0, 0.085438707, 0.532231466, 0.201388188);
        ]

    let get x = `RGB_percent (_get_one red x, _get_one green x, _get_one blue x)
  end
end
