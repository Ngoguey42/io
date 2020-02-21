module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

(* Tools based on owl-base to be able to run in the browser *)

(* Create a signature with type constraints to make Owl_pretty work with Graph *)
module type GRAPH = Owl_neural_graph_sig.Sig
  with type Neuron.Optimise.Algodiff.A.arr := Owl_base_dense_ndarray.D.arr
  with type Neuron.Optimise.Algodiff.A.elt := Owl_base_dense_ndarray.D.elt

module Make_neural (Graph : GRAPH) = struct
  module Algodiff = Graph.Neuron.Optimise.Algodiff

  module Str = struct
    let shape arr =
      Algodiff.Arr.shape arr
      |> Array.map string_of_int
      |> Array.to_list
      |> Ft.List.string_join ", "
      |> Printf.sprintf "(%s)"

    let array arr =
      Algodiff.unpack_arr arr
      |> Owl_pretty.dsnda_to_string
           ~header:false
           ~max_row:1000
           ~max_col:1000
  end

  let to_flat_list arr =
    (* val iter : (elt -> unit) -> arr -> unit *)
    let l = ref [] in
    Algodiff.unpack_arr arr
    |> Owl_base_dense_ndarray.D.iter (fun x -> l := x::!l);
    List.rev !l

end

module Color = struct

  let _get_one xys x =
    let rec aux xys =
      match xys with
      | [x0; y0]::[x1; y1]::_ when x >= x0 && x <= x1 ->
         let x_width = (x1 -. x0) in
         let x_offset = x -. x0 in
         let y_width = (y1 -. y0) in
         let y_offset = x_offset /. x_width *. y_width in
         y_offset +. y0
      | _::tail ->
         aux tail
      | [] ->
         failwith "Unreachable"
    in
    if x < 0. || x > 1. then
      failwith "Trying to interpolate a color out of range";
    aux xys

  module Jet = struct
    let red = [
        [0.; 0.]; [0.35; 0.]; [0.66; 1.]; [0.89; 1.]; [1.; 0.5]
      ]
    let green = [
        [0.; 0.]; [0.125; 0.]; [0.375; 1.]; [0.64; 1.]; [0.91; 0.]; [1.; 0.]
      ]
    let blue = [
        [0.; 0.5]; [0.11; 1.]; [0.34; 1.]; [0.65; 0.]; [1.; 0.]
      ]

    let get x =
      `RGB_percent (_get_one red x, _get_one green x, _get_one blue x)

  end

  let to_hex_string = function
    | `RGB_percent (r, g, b) ->
       let f x = int_of_float @@ x *. 255. in
       Printf.sprintf "#%02X%02X%02X" (f r) (f g) (f b)

end

let create_softmax_div probas =
  let open Html in
  let rec aux l i =
    match l with
    | x::tail ->
       let c = Color.Jet.get x in
       let c = Color.to_hex_string c in
       (* let c = Printf.sprintf "background: %s;" c in *)
       ignore c;
       let t = Printf.sprintf "%.0f%%" (x *. 100.) in
       let t = [i |> string_of_int |> Html.txt; br (); Html.txt t] in
       let style = "width: 34px; height: 34px; text-align: center; color: white;" ^
                     "text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;" ^
                       "font-size: small;"
       in
       let style = Printf.sprintf "%s; background: %s" style c in
       let elt = td ~a:[a_style style] t in
       elt::aux tail (i + 1)
       (* [%html "<td>10%</td>"]::(aux tail) *)

       (* [] *)
       (* (colgroup [col ()])::aux tail *)
       (* (th ~a:[a_style c] [txt "Pourcent"])::(aux tail) *)
    | [] ->
       []
  in
  ignore aux;
  ignore probas;
  table @@ [tr @@ aux probas 0]
  (* [%html "<table><tbody>""</tbody></table>"] *)
(* let d = create_softmax_div [0.1; 0.1; 0.1] *)
