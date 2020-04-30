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

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in

  Printf.printf "loading\n%!";
  Ft_js.Scripts.import `Tfjs >>= fun () ->
  Printf.printf "loaded tfjs\n%!";
  Ft_js.Scripts.import `Cryptojs >>= fun () ->
  Ft_js.Scripts.import `Pako >>= fun () ->
  Ft_js.Scripts.import `Reactjs >>= fun () ->

  (* ************************************************************************ *)
  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  let lwt, lwt' = Lwt.wait () in
  let send_event res = Lwt.wakeup lwt' res in

  Dom.appendChild body container;
  Reactjs.render (Reactjs.Jsx.of_constructor Ft_cnnjs.Mnist.construct send_event) container;
  lwt >>= fun (train_imgs, train_labs, test_imgs, test_labs) ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  (* ************************************************************************ *)
  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  let lwt, lwt' = Lwt.wait () in
  let send_event res = Lwt.wakeup lwt' res in
  let params =
    (train_imgs, train_labs, test_imgs, test_labs),
    Ft_cnnjs.Training.({
                        (* backend = `Tfjs_cpu; *)
                        (* batch_size = 50; *)
                        backend = `Tfjs_webgl;
                        batch_size = 5000;

                        lr = `Down (1e-3, 0.);
                        batch_count = 15;
                        seed = 42;
                        verbose = true;

                    }),
    Ft_cnnjs.Fnn_archi.create_nn (Random.State.make [| 42 |]),
    send_event
  in

  Dom.appendChild body container;
  Reactjs.render (Reactjs.Jsx.of_constructor Ft_cnnjs.Training.construct params) container;
  lwt >|= function
  | `Crash exn -> raise exn
  | `End -> ()
  | `Abort -> ()

  (* ************************************************************************ *)
