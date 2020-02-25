module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Graph = Owl_neural_generic.Make_Embedded (Owl_base_dense_ndarray.S)
module Ft_neural = Ft_owlbase.Make_neural (Graph)

let body = Dom_html.window##.document##.body

let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x)

let print_arr = Graph.Neuron.Optimise.Algodiff.A.print ~max_row:1000 ~max_col:1000

let print_arr_ad x = print_arr @@ Graph.Neuron.Optimise.Algodiff.unpack_arr x

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

let test_owl x =
  (* let w = 28 in *)
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
  (* let x = Graph.Neuron.Optimise.Algodiff.Arr.uniform ~a:~-.1. ~b:1. [| 1; w; w; 1 |] in *)
  let y, _ = Graph.forward n x in
  y

let main () =
  let open Lwt.Infix in
  Dom.appendChild body @@ Ft_owljs.Mnist.status_div ();

  Ft_owljs.Mnist.get () >>= fun (train_imgs, train_labs, test_imgs, test_labs) ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  let pred_and_show img lab =
    let x =
      img
      |> Ft_owljs.Conv.Cast.Ta.float32_of_uint8
      |> Ft_owljs.Conv.Reinterpret.Float32.nd_of_ta
      |> (fun x -> Owl_base_dense_ndarray_generic.reshape x [| 1; 28; 28; 1 |])
      |> Graph.Neuron.Optimise.Algodiff.pack_arr
    in
    let y = test_owl x in
    (* let l = *)
    (*   [ *)
    (*     Ft_neural.Str.shape x |> Printf.sprintf "x shape: %s" |> Html.txt; *)
    (*     [%html "<br/>"]; *)
    (*     Ft_neural.Str.shape y |> Printf.sprintf "y shape: %s" |> Html.txt; *)
    (*     [%html "<br/>"]; *)
    (*     (\* y |> Ft_neural.to_flat_list |> Ft_owljs.Mnist.create_softmax_div; *\) *)
    (*   ] *)
    (* in *)
    (* display @@ Html.div l; *)
    Ft_owljs.Mnist.truc img lab (Ft_neural.to_flat_list y)
    |> Dom.appendChild body;
  in

  let slice a b arr =
    Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
  in
  for i = 0 to 10 do
    let img = test_imgs |> slice (16 + 28 * 28 * (i)) (16 + 28 * 28 * (i + 1)) in
    let lab = Js_of_ocaml.Typed_array.unsafe_get test_labs (8 + i) in
    pred_and_show img lab;
  done;


  Lwt.return ()
