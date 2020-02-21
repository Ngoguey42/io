module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

module Graph = Owl_neural_generic.Make_Embedded(Owl_base_dense_ndarray.D)
module Ft_neural = Ft_js.Make_neural(Graph)

(* let document = Dom_html.window##.document *)
(* let body = Dom_html.window##.document##.body *)

(* let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x) *)

let network =
  let n =
    Graph.input [|7;7;1|]
    |> Graph.conv2d ~padding:Owl_types.SAME ~act_typ:Graph.Neuron.Activation.Relu [|4;4;1;60|] [|2;2|]
    |> Graph.conv2d ~padding:Owl_types.SAME ~act_typ:Graph.Neuron.Activation.Relu [|4;4;60;10|] [|2;2|]
    |> Graph.conv2d ~padding:Owl_types.SAME ~act_typ:(Graph.Neuron.Activation.Softmax 3) [|4;4;10;10|] [|2;2|]
    |> Graph.get_network
  in
  Graph.init n;
  n

let print_arr = Graph.Neuron.Optimise.Algodiff.A.print ~max_row:1000 ~max_col:1000
let print_arr_ad x = print_arr @@ Graph.Neuron.Optimise.Algodiff.unpack_arr x

let x =
  (* let open Graph.Neuron.Optimise.Algodiff.Maths in *)
  (* Graph.Neuron.Optimise.Algodiff.Arr.zeros [|1;7;7;1|] + (F 1.) *)
  Graph.Neuron.Optimise.Algodiff.Arr.uniform ~a:~-.1. ~b:1. [|1;7;7;1|]

let shp = Graph.Neuron.Optimise.Algodiff.Arr.shape x
let y, y1 = Graph.forward network x

let _ =
  Firebug.console##log "x";
  Firebug.console##log (Js.array @@ Graph.Neuron.Optimise.Algodiff.Arr.shape x);

  Firebug.console##log "y";
  Firebug.console##log (Js.array @@ Graph.Neuron.Optimise.Algodiff.Arr.shape y);
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
  (*     Firebug.console##log (Js.array @@ Graph.Neuron.Optimise.Algodiff.Arr.shape b); *)
  (*   done *)
  (* done; *)
  ()

let create_content () =
  let l = [
      Ft_neural.Str.shape x |> Printf.sprintf "x shape: %s" |> Html.txt;
      [%html "<br/>"];
      Ft_neural.Str.shape y |> Printf.sprintf "y shape: %s" |> Html.txt;
      [%html "<br/>"];
      Printf.sprintf "y" |> Html.txt;
      Ft_neural.Str.array y |> Html.txt;
      y |> Ft_neural.to_flat_list
      |> Ft_js.create_softmax_div;
    ] in
  Html.div l
