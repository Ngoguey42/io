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

let _main_nn train_imgs train_labs test_imgs test_labs =
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  let open Lwt.Infix in
  let optimizer = `Adam (0.9, 0.999, 1e-10) in
  let encoders, decoder =
    let open Ft_cnnjs.Nn.Builder in
    Printf.eprintf "Building encoder(s)...\n%!";
    let encoders = [
        input2d 1
        |> conv2d (`One 4) false (`One 2) 10 `Tanh optimizer
        |> relu
        |> conv2d (`One 3) false (`One 2) 10 `Tanh optimizer
        |> fork (fun up -> [
            up
            |> relu
            |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer
          ; up
            |> relu
            |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer
        ])
        |> add
        |> relu |> finalize;
        (* input2d 1 *)
        (* |> conv2d (`One 4) false (`One 2) 10 `Tanh optimizer *)
        (* |> relu *)
        (* |> conv2d (`One 3) false (`One 2) 10 `Tanh optimizer *)
        (* |> relu *)
        (* |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer *)
        (* |> relu |> finalize; *)
      ]
    in
    Printf.eprintf "Building decoder...\n%!";
    let decoder =
      input2d (10 * List.length encoders)
      |> conv2d (`One 3) false (`One 1) 10 `Tanh optimizer
      |> maxpool2d (`One 2) (`One 2)
      |> softmax |> finalize
    in
    Printf.eprintf "...done building\n%!";
    encoders, decoder
  in
  let module Backend = (val Ft_cnnjs.get_backend `Tfjs_cpu) in

  let get_data _ =
    (* TODO: Real random  *)
    let slice a b arr =
      Js.Unsafe.meth_call arr "slice" [| Js.Unsafe.inject a; Js.Unsafe.inject b |]
    in
    let j = Random.int 50000 in
    let batch_size = 5 in
    let imgs = train_imgs |> slice (16 + (28 * 28 * j)) (16 + (28 * 28 * (j + batch_size))) in
    let labs = train_labs |> slice (8 + j) (8 + (j + batch_size)) in
    (imgs, labs)
  in
  let get_lr _ = 1e-3 in

  print_string (Ft_cnnjs.Nn.String.of_network (List.hd encoders));

  (* print_string (Ft_cnnjs.Nn.String.of_network decoder); *)
  Backend.train ~progress:(fun _ -> ()) ~verbose:true ~batch_count:1 ~get_lr ~get_data ~encoders ~decoder
  >>= fun (encoders, decoder) ->
  print_string (Ft_cnnjs.Nn.String.of_network (List.hd encoders));

  (* Backend.train ~progress:(fun _ -> ()) ~verbose:true ~batch_count:4 ~get_lr ~get_data ~encoders ~decoder *)
  (* >>= fun (encoders, decoder) -> *)
  (* print_string (Ft_cnnjs.Nn.String.of_network (List.hd encoders)); *)

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
