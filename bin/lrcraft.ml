module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

module NeuralD = Owl_neural_generic.Make(Owl_base_dense_ndarray.D)

module Aa = Irm

(* let document = Dom_html.window##.document *)
(* let body = Dom_html.window##.document##.body *)

(* let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x) *)

let network =
  let n =
    NeuralD.Graph.input [|7;7;1|]
    |> NeuralD.Graph.conv2d ~padding:Owl_types.SAME ~act_typ:NeuralD.Graph.Neuron.Activation.Relu [|4;4;1;60|] [|2;2|]
    |> NeuralD.Graph.conv2d ~padding:Owl_types.SAME ~act_typ:NeuralD.Graph.Neuron.Activation.Relu [|4;4;60;10|] [|2;2|]
    |> NeuralD.Graph.conv2d ~padding:Owl_types.SAME ~act_typ:(NeuralD.Graph.Neuron.Activation.Softmax 3) [|4;4;10;10|] [|2;2|]
    |> NeuralD.Graph.get_network
  in
  NeuralD.Graph.init n;
  n

let print_arr = NeuralD.Algodiff.A.print ~max_row:1000 ~max_col:1000
let print_arr_ad x = print_arr @@ NeuralD.Algodiff.unpack_arr x

let x =
  (* let open NeuralD.Algodiff.Maths in *)
  (* NeuralD.Algodiff.Arr.zeros [|1;7;7;1|] + (F 1.) *)
  NeuralD.Algodiff.Arr.uniform ~a:~-.1. ~b:1. [|2;7;7;1|]

let shp = NeuralD.Algodiff.Arr.shape x
let y, y1 = NeuralD.Graph.forward network x

module Ft = struct
  module List = struct
    let rec string_join sep = function
      | a::(_::_ as tail) -> a ^ sep ^ string_join sep tail
      | [a] -> a
      | [] -> ""
  end
end

module Str = struct
  let shape arr =
    NeuralD.Algodiff.Arr.shape arr
    |> Array.map string_of_int
    |> Array.to_list
    |> Ft.List.string_join ", "
    |> Printf.sprintf "(%s)"

  let array arr =
    NeuralD.Algodiff.unpack_arr arr
    |> Owl_pretty.dsnda_to_string
         ~header:false
         ~max_row:1000
         ~max_col:1000
end

let _ =

  Firebug.console##log "x";
  Firebug.console##log (Js.array @@ NeuralD.Algodiff.Arr.shape x);

  Firebug.console##log "y";
  Firebug.console##log (Js.array @@ NeuralD.Algodiff.Arr.shape y);
  print_arr_ad y;

  (* Firebug.console##log "y1"; *)
  (* Firebug.console##log (Array.length y1); *)
  (* for i=0 to Array.length y1 - 1 do *)
  (*   let a = y1.(i) in *)
  (*   let len = Array.length a in *)
  (*   Printf.eprintf "> %d %d\n%!" i len; *)
  (*   for j=0 to len - 1 do *)
  (*     let b = a.(j) in *)
  (*     (\* print_arr_ad b *\) *)
  (*     Firebug.console##log (Js.array @@ NeuralD.Algodiff.Arr.shape b); *)
  (*   done *)
  (* done; *)
  ()

let create_content () =
  let l = [
      Str.shape y |> Printf.sprintf "y shape: %s" |> Html.txt;
      [%html "<br/>"];
      Str.shape y |> Printf.sprintf "y shape: %s" |> Html.txt;
      [%html "<br/>"];
      Printf.sprintf "y" |> Html.txt;
      Str.array y |> Html.txt;
    ] in
  Html.div l
