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

(* ********************************************************************************************** *)
(* Features:
    - Most classic way of tackling the problem, 3x3 stride:2 convolutions with batch_norm

   Successive widths: 28, 14, 7, 4
 *)
let[@ocamlformat "disable"] encoder_padding_batchnorm (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 10) (3, 3) ~s:(2, 2) ~b:`Same (* pooling *) |> bias |> batch_norm |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~s:(2, 2) ~b:`Same (* pooling *) |> bias |> batch_norm |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~s:(2, 2) ~b:`Same (* pooling *) |> bias |> batch_norm |> relu
  |> Fnn.downcast

(* Features:
    - No padding
    - For stride 2 convs:
    - 4x4 kernel if input has even spatial size
    - 3x3 kernel is input has odd spatial size

   Successive widths: 28, 13, 6, 4
 *)
let[@ocamlformat "disable"] encoder_pooling (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 12) (4, 4) ~s:(2, 2) ~b:`Assert_fit (* pooling *) |> bias |> relu
  |> conv2d ~o (`Full 25) (3, 3) ~s:(2, 2) ~b:`Assert_fit (* pooling *) |> bias |> relu
  |> conv2d ~o (`Full 50) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
  |> Fnn.downcast

(* Residual network inspired by mobilenet v2:
    - Inverted bottleneck: the residual branch is a projection instead of being an expansion
    - No `relu` on the residual branch
    - No `relu` between `sum` and `conv` to avoid washing away the projected features
    - Expansion and projections with 1x1 kernels, transformations with depthwise kernels
    - The very first conv is an expansion
    But with those diffs:
    - Use bias
    - No padding (requires explicit cropping on the residual branch)
    - Stride 2 poolings with 4x4 and 2x2 kernels (less checkerboard effects than odd kernels)

   Successive widths: 28, 14, 6, 4
   *)
let[@ocamlformat "disable"] encoder_mobilenet (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 18) (2, 2) ~s:(2, 2) ~b:`Assert_fit |> bias (* expansion (pooling) *)
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> conv2d ~o (`Full 6) (2, 2) ~s:(2, 2) ~b:`Assert_fit |> bias (* projection (pooling) *)
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Depthwise 1) (4, 4) ~s:(2, 2) ~b:`Assert_fit |> bias (* transformation (pooling) *)
          |> relu |> conv2d ~o (`Full 6) (1, 1) ~b:`Assert_fit |> bias (* projection *)
          |> Fnn.downcast
     ])
  |> sum
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Full 18) (1, 1) ~b:`Assert_fit |> bias (* expansion *)
          |> relu |> conv2d ~o (`Depthwise 1) (3, 3) ~b:`Assert_fit |> bias (* transformation *)
          |> relu |> conv2d ~o (`Full 6) (1, 1) ~b:`Assert_fit |> bias (* projection *)
          |> Fnn.downcast
     ])
  |> sum
  |> Fnn.downcast

(* Features:
   - No stride, only 3x3 3-dilated convolutions

   A 3x3 3-dilated kernel applied on a 10x10 array outputs a 4x4 array:
       |0 1 2 3 4 5 6 7 8 9|
       [-     -     -]     |
       | [-     -     -]   |
       |   [-     -     -] |
       |     [-     -     -]

   Successive widths: 28, 22, 16, 10, 4
 *)
let[@ocamlformat "disable"] encoder_dilatedconvs (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> Fnn.downcast

(* Features:
   - One 16x16 4-strided convolution

   A 16x16 4-strided kernel applied on a 28x28 array outputs a 4x4 array:
       |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7|
       [- - - - - - - - - - - - - - - -]                       |
       |       [- - - - - - - - - - - - - - - -]               |
       |               [- - - - - - - - - - - - - - - -]       |
       |                       [- - - - - - - - - - - - - - - -]

   Successive widths: 28, 4
 *)
let[@ocamlformat "disable"] encoder_oneconv (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  (* let batch_norm = batch_norm ~affine:false in (\* TODO: Remove line *\) *)
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  (* |> conv2d ~o (`Full 50) (16, 16) ~s:(4, 4) ~b:`Assert_fit |> bias *)
  |> conv2d ~o (`Full 50) (16, 16) ~s:(6, 6) ~b:`Assert_fit |> bias
  (* |> conv2d ~o (`Full 50) (16, 16) ~s:(3, 3) ~b:`Assert_fit |> bias *)
  (* |> batch_norm ~affine:false *)
  |> relu
  |> Fnn.downcast

(* ********************************************************************************************** *)

let _main_nn train_imgs train_labs test_imgs test_labs =
  ignore (train_imgs, train_labs, test_imgs, test_labs);
  let rng = Random.State.make [| 42 |] in
  let builder = Fnn.create_builder ~rng () in

  let open Lwt.Infix in
  (* let o = `Sgd in *)
  let o = `Adam (0.9, 0.999, 1e-4) in
  let encoders, decoder =
    (* let module Builder = (val builder) in *)
    let module Builder = (val builder) in
    let open Builder in
    let open Pshape.Size in
    Printf.eprintf "Building encoder(s)...\n%!";
    let encoders = [
        (* encoder_padding_batchnorm builder o; *)
        (* encoder_pooling builder o; *)
        (* encoder_mobilenet builder o; *)
        (* encoder_dilatedconvs (); *)
        encoder_oneconv builder o;
      ]
    in
    Printf.eprintf "Building decoder...\n%!";

    let decoder =
      let c =
        List.map (fun net -> Pshape.get net#out_shape `C) encoders
        |> List.fold_left add (K 0)
      in
      let w = 3 in
      input (Pshape.sym4d_partial ~n:U ~c ~s0:(K w) ~s1:(K w)) `Float32

      (* |> conv2d ~o ~id:(Some "classif") (`Full 10) (w, w) ~b:`Assert_fit |> bias |> transpose ~mapping:[`C, `C; `S0, `C; `S1, `C] *)
      (* |> conv2d ~o ~id:(Some "classif") (`Full 10) (1, 1) ~b:`Assert_fit |> bias |> maxpool2d ~b:`Assert_fit (w, w) |> transpose ~mapping:[`C, `C; `S0, `C; `S1, `C] *)
      (* |> conv2d ~o (`Depthwise 1) (3, 3) ~b:`Assert_fit |> bias *)

      (* Classify usingg a maxpool and a fully connected layer *)
      (* |> maxpool2d ~b:`Assert_fit (w, w) |> transpose ~mapping:[`C, `C; `S0, `C; `S1, `C] |> dense ~o:`Sgd [`C, 10] ~id:(Some "classif") |> bias *)

      (* Classify and flatten using flatten and fully-connected *)
      |> transpose ~mapping:[`C, `C; `S0, `C; `S1, `C] |> dense ~o:`Sgd [`C, 10] ~id:(Some "classif") |> bias

      |> softmax `C
      |> Fnn.downcast
    in
    Printf.eprintf "...done building\n%!";
    encoders, decoder
  in

  let module Backend = (val Ft_cnnjs.get_backend `Tfjs_webgl) in
  (* let module Backend = (val Ft_cnnjs.get_backend `Tfjs_cpu) in *)

  let rng = Random.State.make [| 42 |] in
  let batch_count, batch_size = 1000, 1000 in
  (* let batch_count, batch_size = 2, 10 in *)
  let get_data _ =
    let indices = Array.init batch_size (fun _ -> Random.State.int rng 60000) in
    let imgs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_imgs i (1)) indices
      |> Ndarray.concatenate ~axis:0
    in
    let labs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_labs i (1)) indices
      |> Ndarray.concatenate ~axis:0
    in
    (imgs, labs)
  in
  let get_lr i =
    1e-3 *. (1. -. (float_of_int i) /. (float_of_int batch_count))
  in

  Backend.train ~progress:(fun _ -> ()) ~verbose:true ~batch_count ~get_lr ~get_data ~encoders ~decoder
  >>= fun (encoders, decoder) ->
  ignore encoders;
  ignore decoder;
  Lwt.return ()

(* ********************************************************************************************** *)

let owl_train_pass nn optim xs labs =
  let lr = 1e-3 in
  (* Printf.eprintf "forward...\n%!"; *)
  let y, _ = Graph.forward nn xs in
  (* Printf.eprintf "loss...\n%!"; *)
  let loss = Ft_neural.cross_entropy_of_softmaxed labs y in
  (* Printf.eprintf "backward...\n%!"; *)
  let weights, grads = Graph.backward nn loss in
  let loss = Algodiff.primal' loss in
  Printf.printf "loss: %s\n%!" @@ Ft_neural.Str.array loss;
  (* let optim, updates = Ft_neural.SGD.apply optim lr grads in *)
  let optim, updates = Ft_neural.Adam.apply optim lr grads in
  let weights = Owl_utils.aarr_map2 (fun w u -> Algodiff.Maths.(w + u)) weights updates in
  Graph.update nn weights;
  (optim, y)

let _main_owld train_imgs train_labs test_imgs test_labs =
  let open Lwt.Infix in
  ignore (train_imgs, train_labs, test_imgs, test_labs);


  let slice a b arr =
    Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
  in

  let nn =
    let open Ft_neural.Network_builder in
    let x = input2d 28 28 1 in
    let nn =
      x
      |> conv2d (`One 4) false (`One 2) 10
      |> relu
      |> conv2d (`One 3) false (`One 2) 10
      |> relu
      |> conv2d (`One 3) false (`One 1) 10
      |> relu
      |> conv2d (`One 3) false (`One 1) 10
      |> max_pool2d (`One 2) (`One 2)
      |> softmax2d
    in
    (* let x' = input2d 2 2 10 in *)
    (* let nn' = *)
    (*   x' *)
    (* in *)
    let nn = get_network nn in
    nn
  in
  Printf.eprintf "b\n%!";

  Graph.print nn;

  Graph.init nn;
  Printf.eprintf "c\n%!";
  (* let optim = () in *)
  let optim = Ft_neural.Adam.create ~beta1:0.9 ~beta2:0.999 nn in

  let rec aux optim = function
    (* | 1 -> Lwt.return optim *)
    | 500 -> Lwt.return optim
    | i ->
       Printf.printf "> %d\n%!" i;
       let time = (new%js Js.date_now)##valueOf /. 1000. in

       let j = Random.int 55000 in
       (* let batch_size = 5 in *)
       let batch_size = 600 in
       let imgs = train_imgs |> slice (16 + (28 * 28 * j)) (16 + (28 * 28 * (j + batch_size))) in
       let labs = train_labs |> slice (8 + j) (8 + (j + batch_size)) |> Ft_js.Conv.list_of_ta in
       let xs =
         imgs |> Ft_js.Conv.Ta.float32_of_uint8 |> Ft_js.Conv.Float32.ba_of_ta
         |> (fun x -> Ndarray.reshape x [| batch_size; 28; 28; 1 |])
         |> Algodiff.pack_arr
       in
       let optim, ys = owl_train_pass nn optim xs labs in
       ignore ys;

       let j = 0 in
       let batch_size = 3 in
       let imgs = test_imgs |> slice (16 + (28 * 28 * j)) (16 + (28 * 28 * (j + batch_size))) in
       let labs = test_labs |> slice (8 + j) (8 + (j + batch_size)) |> Ft_js.Conv.list_of_ta in
       let xs =
         imgs |> Ft_js.Conv.Ta.float32_of_uint8 |> Ft_js.Conv.Float32.ba_of_ta
         |> (fun x -> Ndarray.reshape x [| batch_size; 28; 28; 1 |])
         |> Algodiff.pack_arr
       in
       let ys = Graph.run xs nn in

       for j = 0 to batch_size - 1 do
         if i >= 4 then
           Ft_js.select body ".mnist-pred" Dom_html.CoerceTo.div |> Dom.removeChild body;
         let lab = List.nth labs j in
         let img = imgs |> slice (28 * 28 * j) (28 * 28 * (j + 1)) in
         let y = Ndarray.get_slice [ [ j; j ] ] (Algodiff.unpack_arr ys) |> Algodiff.pack_arr in
         Ft_cnnjs.Mnist.html_pred_overview img lab (Ft_neural.to_flat_list y)
         |> Dom.appendChild body
       done;

       (* Dom_html.createBr Dom_html.window##.document |> Dom.appendChild body; *)
       let time' = (new%js Js.date_now)##valueOf /. 1000. in
       Printf.printf "> %d %fsec\n%!" i (time' -. time);

       Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun _ -> aux optim (i + 1)
  in

  aux optim 0 >>= fun _ -> Lwt.return ()

(* ********************************************************************************************** *)

let main () =
  let open Lwt.Infix in
  Dom.appendChild body @@ Ft_cnnjs.Mnist.status_div ();

  Ft_cnnjs.Mnist.get () >>= fun (train_imgs, train_labs, test_imgs, test_labs) ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  _main_nn train_imgs train_labs test_imgs test_labs >>= fun _ ->
  (* _main_owld train_imgs train_labs test_imgs test_labs >>= fun _ -> *)

  (* Ft_cnnjs.Mnist_tfjs.main train_imgs train_labs test_imgs test_labs >>= fun _ -> *)
  (* Ft_owljs.Tf.main train_imgs train_labs test_imgs test_labs >>= fun _ -> *)
  (* Js.Unsafe.fun_call Js.Unsafe.global##.ft_tf_test_ [| *)
  (*                      train_imgs|> Js.Unsafe.inject;  train_labs|> Js.Unsafe.inject;  test_imgs|> Js.Unsafe.inject;  test_labs|> Js.Unsafe.inject; *)
  (*                    |] *)
  (* |> Ft_js.wrap_promise >>= fun _ -> *)
  Printf.eprintf "Done\n%!";
  Lwt.return ()
