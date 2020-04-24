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

let main () =
  let open Lwt.Infix in

  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  let events, send_event = React.E.create () in
  Dom.appendChild body container;
  Reactjs.render (Reactjs.Jsx.of_make Ft_cnnjs.Mnist.make send_event) container;
  Lwt_react.E.next events >>= fun (train_imgs, train_labs, test_imgs, test_labs) ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  let events, send_event = React.E.create () in
  let send_event : 'a -> unit = send_event in
  let params =
    (train_imgs, train_labs, test_imgs, test_labs),
    Ft_cnnjs.Training.({
                        backend = `Tfjs_webgl;
                        lr = `Down (1e-3, 0.);
                        batch_count = 100;
                        batch_size = 5000;
                        seed = 42;
                        verbose = true;

                    }),
    Ft_cnnjs.Fnn_archi.create_nn (Random.State.make [| 42 |]),
    send_event
  in
  Dom.appendChild body container;
  Reactjs.render (Reactjs.Jsx.of_make Ft_cnnjs.Training.make params) container;
  Lwt_react.E.next events >|= function
  | `Crash exn -> raise exn
  | `End -> ()
  | `Abort -> ()
