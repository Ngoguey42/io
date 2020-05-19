open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
  module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
  module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray_generic
  module Typed_array = Js_of_ocaml.Typed_array
  module Reactjs = Ft_js.Reactjs
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
end

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in
  let textdiv = [%html "<div class='textdiv'></div>"] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild body textdiv;

  [%html "<div id='chart'></div>"] |> Tyxml_js.To_dom.of_element |> Dom.appendChild textdiv;

  (* ************************************************************************ *)
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Ft_js.import_js "https://cdn.plot.ly/plotly-latest.min.js" >>= fun () ->
  (* ************************************************************************ *)
  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  let lwt, lwt' = Lwt.wait () in
  let send_event res = Lwt.wakeup lwt' res in
  Dom.appendChild textdiv container;
  Reactjs.render (Reactjs.Jsx.of_constructor Ressources.construct_react_table send_event) container;
  lwt >>= fun (train_imgs, train_labs, test_imgs, test_labs) ->
  ignore (train_imgs, train_labs, test_imgs, test_labs);

  (* ************************************************************************ *)
  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  Dom.appendChild textdiv container;
  Reactjs.render
    (Reactjs.Jsx.of_constructor Ft_cnnjs.Network_construction.construct_react_component send_event)
    container;

  (* Lwt.return () *)

  (* ************************************************************************ *)
  let container = Html.div [] |> Tyxml_js.To_dom.of_element in
  let lwt, lwt' = Lwt.wait () in
  let send_event res = Lwt.wakeup lwt' res in
  let params =
    Ft_cnnjs.Training.
      {
        db = (train_imgs, train_labs, test_imgs, test_labs);
        networks = Ft_cnnjs.Fnn_archi.create_nn (Random.State.make [| 42 |]);
        config =
          {
            (* backend = `Tfjs_cpu; *)
            (* batch_size = 50; *)
            backend = `Tfjs_webgl;
            batch_size = 200;
            lr = `Down (1e-3, 0.);
            batch_count = 4;
            seed = 42;
            verbose = true;
          };
      }
  in
  Dom.appendChild textdiv container;
  Reactjs.(
    Jsx.of_constructor Ft_cnnjs.Training.construct (params, send_event) |> Fun.flip render container);
  lwt >|= function `Crash exn -> raise exn | `End -> () | `Abort -> ()

(* ************************************************************************ *)
