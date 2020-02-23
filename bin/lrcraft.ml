module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Graph = Owl_neural_generic.Make_Embedded (Owl_base_dense_ndarray.D)
module Ft_neural = Ft_owlbase.Make_neural (Graph)

(* let document = Dom_html.window##.document *)
let body = Dom_html.window##.document##.body

let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x)
let print_arr = Graph.Neuron.Optimise.Algodiff.A.print ~max_row:1000 ~max_col:1000
let print_arr_ad x = print_arr @@ Graph.Neuron.Optimise.Algodiff.unpack_arr x

let test_owl () =
  let w = 28 in
  let open Ft_neural.Network_builder in
  let n =
    input2d 28 28 1
    |> conv2d (`One 4) false (`One 2) 20
    |> relu
    |> conv2d (`One 3) false (`One 2) 20
    |> relu
    |> conv2d (`One 4) false (`One 2) 10
    |> max_pool2d (`One 2) (`One 2)
    |> softmax2d |> get_network
  in
  Graph.init n;
  let x = Graph.Neuron.Optimise.Algodiff.Arr.uniform ~a:~-.1. ~b:1. [| 1; w; w; 1 |] in
  let y, _ = Graph.forward n x in
  x, y

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

let main () =
  let open Lwt.Infix in
  let x, y = test_owl () in
  Dom.appendChild body @@ Ft_owljs.Mnist.status_div ();
  let l =
    [
      Ft_neural.Str.shape x |> Printf.sprintf "x shape: %s" |> Html.txt;
      [%html "<br/>"];
      Ft_neural.Str.shape y |> Printf.sprintf "y shape: %s" |> Html.txt;
      [%html "<br/>"];
      Printf.sprintf "y" |> Html.txt;
      y |> Ft_neural.to_flat_list |> Ft_js.create_softmax_div;
    ]
  in
  display @@ Html.div l;

  Ft_owljs.Mnist.get () >>= fun data ->
  ignore data;

  Lwt.return ()
