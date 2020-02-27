module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Graph = Owl_neural_generic.Make_Embedded (Owl_base_dense_ndarray.S)
module Algodiff = Graph.Neuron.Optimise.Algodiff
module Ft_neural = Ft_owlbase.Make_neural (Graph)
module Ndarray = Owl_base_dense_ndarray_generic
module Typed_array = Js_of_ocaml.Typed_array
let body = Dom_html.window##.document##.body

let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x)

let print_arr = Algodiff.A.print ~max_row:1000 ~max_col:1000

let print_arr_ad x = print_arr @@ Algodiff.unpack_arr x

(* Firebug.console##log "y1"; *)
(* Firebug.console##log (Array.length y1); *)
(* for i=0 to Array.length y1 - 1 do *)
(*   let a = y1.(i) in *)
(*   let len = Array.length a in *)
(*   Printf.eprintf "> %d %d\n%!" i len; *)
(*   for j=0 to len - 1 do *)
(*     let b = a.(j) in *)
(*     (\* print_arr_ad b *\) *)
(*     Firebug.console##log (Js.array @@ Algodiff.Arr.shape b); *)
(*   done *)
(* done; *)

let float' x = Ndarray.create Bigarray.Float32 [| |] x |> Algodiff.pack_arr
let int' x = Ndarray.create Bigarray.Float32 [| |] (float_of_int x) |> Algodiff.pack_arr

let cross_entropy_of_softmaxed : int list -> 'a -> 'a = fun labs pred ->
  let shape = Algodiff.Arr.shape pred in
  let b, h, w, c = match shape with
    | [| b; h; w; c |] -> b, h, w, c
    | _ -> assert false
  in
  assert (h == 1);
  assert (w == 1);
  assert (c == 10);

  let onehot lab = Ndarray.init Bigarray.Float32 [|1; h; w; c|] (fun i -> if i == lab then 1. else 0.) in

  let truth = List.map onehot labs |> Array.of_list |> Ndarray.concatenate ~axis:0 |> Algodiff.pack_arr in
  (* print_endline @@ Ft_neural.Str.shape pred; *)
  (* print_endline @@ Ft_neural.Str.shape truth; *)

  Algodiff.Maths.(
    pred
    |> max2 (float' 1e-10)
    |> log
    |> neg
    |> ( * ) truth
    |> sum ~axis:3
    |> (fun x -> assert (Array.length @@ Algodiff.Arr.shape x == 4); x)
    |> (fun x -> (sum ~axis:0 x) / (int' b))
    |> (fun x -> assert (Array.length @@ Algodiff.Arr.shape x == 4); x)
    |> (fun x -> reshape x [| 1 |])
  )


module Adam = struct
  type t = {
      beta1: float;
      beta2: float;
      step: int;
      rgrad: Algodiff.t array array; (* Running gradient *)
      rgrad_sq: Algodiff.t array array; (* Running gradient squared *)
    }

  let create ~beta1 ~beta2 nn =
    let aux node =
      match Graph.(node.neuron) with
      (* Initialize with scalars and let the first `apply` call broadcast to the right shape *)
      | Graph.Neuron.Conv2D _ -> [| float' 0.; float' 0. |] (* First for kernel, second for bias *)
      | _ -> [| |]
    in

    let rgrad = Array.map aux Graph.(nn.topo) in
    let rgrad_sq = Array.map aux Graph.(nn.topo) in
    {beta1; beta2; step=0; rgrad; rgrad_sq}

  let apply : t -> float -> _ array array -> (t * _ array array) =
    fun ({beta1; beta2; step; rgrad; rgrad_sq} as r) lr grad ->
    let step = step + 1 in
    let correction1 = float' (1. -. (beta1 ** (float_of_int step))) in
    let correction2 = float' (1. -. (beta2 ** (float_of_int step))) in

    let f scale a b =
      let scale, scale' = float' scale, float' (1. -. scale) in
      Algodiff.Maths.(a * scale + b * scale')
    in
    let rgrad = Owl_utils.aarr_map2 (f beta1) rgrad grad in
    let rgrad_sq = Owl_utils.aarr_map2 (f beta2) rgrad_sq (Owl_utils.aarr_map Algodiff.Maths.sqr grad) in

    let f rgrad rgrad_sq =
      let open Algodiff.Maths in
      let rgrad = rgrad / correction1 in
      let rgrad_sq = (sqrt rgrad_sq) / (sqrt correction2) in
      rgrad / (rgrad_sq + float' 1e-10) * float' lr |> neg
    in
    let updates = Owl_utils.aarr_map2 f rgrad rgrad_sq in

    { r with step; rgrad; rgrad_sq }, updates

end

let test_owl nn optim labs xs =
  let lr = 1e-3 in

  let y, _ = Graph.forward nn xs in
  let loss = cross_entropy_of_softmaxed labs y in
  let weights, grads = Graph.backward nn loss in
  let loss = Algodiff.primal' loss in

  Printf.printf "loss: %s\n" @@ Ft_neural.Str.array loss;

  (* Printf.eprintf "**************************************************\n%!"; *)
  (* let f a = *)
  (*   Firebug.console##log (Js.array @@ Algodiff.Arr.shape a); *)

  (* in *)
  (* Owl_utils.aarr_map f grads |> ignore; *)
  (* Printf.eprintf "**************************************************\n%!"; *)
  let optim, updates = Adam.apply optim lr grads in

  let weights = Owl_utils.aarr_map2 (fun w u -> Algodiff.Maths.(w + u)) weights updates in
  Graph.update nn weights;
  (* ~lr:1e-4 *)
  (* for i = 0 to Array.length grads - 1 do *)
  (*   let grads' = grads.(i) in *)
  (*   let len = Array.length grads' in *)
  (*   Printf.eprintf "> %d %d\n%!" i len; *)
  (*   for j = 0 to len - 1 do *)
  (*     let grads'' = grads'.(j) in *)
  (*     Firebug.console##log (Js.array @@ Algodiff.Arr.shape grads''); *)
  (*   done *)
  (* done; *)



  ignore (weights, grads, loss, optim, lr, updates);
  optim, y

let main () =
  let open Lwt.Infix in
  Dom.appendChild body @@ Ft_owljs.Mnist.status_div ();

  Ft_owljs.Mnist.get () >>= fun (train_imgs, train_labs, test_imgs, test_labs) ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  let slice a b arr =
    Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
  in

  let open Ft_neural.Network_builder in
  let nn =
    input2d 28 28 1
    |> conv2d (`One 4) false (`One 2) 20
    |> relu
    |> conv2d (`One 3) false (`One 2) 20
    |> relu
    |> conv2d (`One 4) false (`One 2) 10
    |> max_pool2d (`One 2) (`One 2)
    |> softmax2d |> get_network
  in
  Graph.init nn;
  let optim = Adam.create ~beta1:0.9 ~beta2:0.999 nn in

  let rec aux optim = function
    | 1000 -> Lwt.return optim
    | i ->
       (* optim >>= fun optim -> *)
       let j = 0 in
       let batch_size = 300 in
       let imgs = test_imgs |> slice (16 + 28 * 28 * (j)) (16 + 28 * 28 * (j + batch_size)) in
       let labs =
         test_labs
         |> slice (8 + (j)) (8 + (j + batch_size))
         |> Ft_owljs.Conv.list_of_ta
       in

       let xs =
         imgs
         |> Ft_owljs.Conv.Cast.Ta.float32_of_uint8
         |> Ft_owljs.Conv.Reinterpret.Float32.nd_of_ta
         |> (fun x -> Ndarray.reshape x [| batch_size; 28; 28; 1 |])
         |> Algodiff.pack_arr
       in
       let optim, ys = test_owl nn optim labs xs in

       let lab = List.hd labs in
       let img = imgs |> slice 0 (28 * 28) in
       let y = Ndarray.get_slice [[0; 0]] (Algodiff.unpack_arr ys) |> Algodiff.pack_arr in
       Ft_owljs.Mnist.html_pred_overview img lab (Ft_neural.to_flat_list y)
       |> Dom.appendChild body;

       Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun _ ->

       aux optim (i + 1)
  in

  let time = (new%js Js.date_now)##valueOf /. 1000. in
  aux optim 0 >>= fun _ ->
  let time' = (new%js Js.date_now)##valueOf /. 1000. in
  Printf.eprintf "%f\n%!" (time' -. time);

  Lwt.return ()
