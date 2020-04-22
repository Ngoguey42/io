module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic
module Typed_array = Js_of_ocaml.Typed_array
module Reactjs =  Ft_js.Reactjs
module Lwt_js = Js_of_ocaml_lwt.Lwt_js

let body = Dom_html.window##.document##.body

let display x = Dom.appendChild body (Tyxml_js.To_dom.of_element x)

let _main_nn train_imgs train_labs test_imgs test_labs =
  let open Lwt.Infix in
  ignore (train_imgs, train_labs, test_imgs, test_labs);
  let encoders, decoder = Ft_cnnjs.Fnn_archi.create_nn (Random.State.make [| 42 |]) in

  let module Backend = (val Ft_cnnjs.get_backend `Tfjs_webgl) in

  let rng = Random.State.make [| 42 |] in
  let batch_count, batch_size = 2, 10 in
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
  ignore (encoders, decoder);
  Lwt.return ()

(* ********************************************************************************************** *)

let main () =
  let open Lwt.Infix in

  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild body container;

  let events, send_event = React.E.create () in
  Reactjs.render (Reactjs.Jsx.of_make Ft_cnnjs.Mnist.make send_event) container;

  Lwt_react.E.next events >>= fun (train_imgs, train_labs, test_imgs, test_labs) ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  Lwt_js.sleep 0.01 >>= fun () ->

  _main_nn train_imgs train_labs test_imgs test_labs >>= fun _ ->

  Printf.eprintf "Done\n%!";
  Lwt.return ()
