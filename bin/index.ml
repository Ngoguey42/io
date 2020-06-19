module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

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
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

type card_data = { href : string; title : string; description : string; date : string }

let cards_data =
  (* From oldes to newest *)
  [
    {
      href = "cinquante.html";
      title = "Cinquante";
      description = "JavaScript game based on the <cite>MNIST</cite> dataset.";
      date = "MAY 16, 2020";
    };
    {
      href = "reactjs-wrapper.html";
      title = "Reactjs wrapper";
      description =
        "<cite>Js_of_ocaml</cite> wrapper for <cite>Reactjs</cite> using <cite>React</cite>(ml).";
      date = "JUN 17, 2020";
    };
    {
      href = "mnist-jsoo.html";
      title = "mnist-jsoo";
      description = "Create, train and compare artificial neural networks on <cite>MNIST<cite>.";
      date = "JUN 18, 2020";
    };
    {
      href = "ocann0.html";
      title = "OCaNN";
      description = "A computation-agnostic neural network abstraction tailored for OCaml.";
      date = "JUN 24, 2020";
    };
  ]

let jsx_of_card { href; title; description; date } =
  let open Reactjs.Jsx in
  [
    of_string title >> of_bootstrap "Card.Title";
    [
      of_tag "span" ~inner_html:description [];
      date |> of_string >> of_tag "small" ~as_:"div" ~classes:[ "text-muted" ];
    ]
    |> of_bootstrap "Card.Text";
  ]
  |> of_bootstrap "Card.Body"
  >> of_bootstrap "Card" ~as_:"a" ~href
  >> of_bootstrap "Col" ~md_span:4 ~sm_span:6 ~classes:[ "index-card-holder" ]

let construct_cards () =
  Printf.printf "> Component - cards | construct\n%!";
  let render () =
    let open Reactjs.Jsx in
    Printf.printf "> Component - cards | render\n%!";

    [
      (* "Blog" *)
      (* |> of_string *)
      (* >> of_tag "h1" *)
      (* >> of_tag "div" ~classes:["bigbox-title"]; *)
      [
        (* "Blog" *)
        (* |> of_string *)
        (* >> of_tag "h1"; *)
        (* of_tag *)
        List.rev cards_data |> List.map jsx_of_card |> of_bootstrap "Row"
        >> of_bootstrap "Container";
      ]
      |> of_tag "div" ~classes:[ "bigbox1" ];
    ]
    |> of_react "Fragment"
  in
  Reactjs.construct render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in

  let div =
    [%html "<div><div style='text-align: center;'>loading</div></div>"]
    |> Tyxml_js.To_dom.of_element
  in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Reactjs.render (Reactjs.Jsx.of_constructor construct_cards ()) div;
  Lwt.return ()
