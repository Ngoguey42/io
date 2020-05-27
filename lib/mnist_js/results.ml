open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

(* open Types *)

let construct_results _ =
  Printf.printf "> construct component: results\n%!";

  (* let params, on_completion = props in *)
  let render _ =
    Printf.printf "> Results.render | render\n%!";
    let open Reactjs.Jsx in
    let tbody =
      [
        of_string "Chart" >> of_bootstrap "Col" ~sm:12;
        of_string "Images" >> of_bootstrap "Col" ~sm:12;
      ]
      |> of_bootstrap "Row" >> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr"
      >> of_tag "tbody"
    in
    let thead = of_string "Results" >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~class_:[ "mnist-panel" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct render
