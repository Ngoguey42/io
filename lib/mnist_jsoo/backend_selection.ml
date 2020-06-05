open struct
  module Js = Js_of_ocaml.Js
  module Reactjs = Ft_js.Reactjs
end

let backends = [ `Tfjs_webgl; `Tfjs_cpu; `Owl_algodiff_cpu ]

let option_of_idx = function
  | 0 -> (`Tfjs_webgl, true)
  | 1 -> (`Tfjs_wasm, true)
  | 2 -> (`Tfjs_cpu, true)
  | 3 -> (`Tfjs_webgl, false)
  | 4 -> (`Tfjs_wasm, false)
  | 5 -> (`Tfjs_cpu, false)
  | 6 -> (`Owl_algodiff_cpu, true)
  | 7 -> (`Owl_algodiff_cpu, false)
  | _ -> failwith "Unknown computation option"

let idx_of_option = function
  | `Tfjs_webgl, true -> 0
  | `Tfjs_wasm, true -> 1
  | `Tfjs_cpu, true -> 2
  | `Tfjs_webgl, false -> 3
  | `Tfjs_wasm, false -> 4
  | `Tfjs_cpu, false -> 5
  | `Owl_algodiff_cpu, true -> 6
  | `Owl_algodiff_cpu, false -> 7

let name_of_backend = function
  | `Tfjs_webgl -> "TensorFlow.js WebGL"
  | `Tfjs_wasm -> "TensorFlow.js WASM"
  | `Tfjs_cpu -> "TensorFlow.js CPU"
  | `Owl_algodiff_cpu -> "Owl Algodiff CPU"

let description_of_backend = function
  | `Tfjs_webgl ->
      {|
JavaScript version of <cite>Google</cite>'s tensor processing library. This backend uses the
<a href="https://en.wikipedia.org/wiki/WebGL"><cite>WebGL</cite></a> API of your browser to
access the <cite>GPU</cite> of your device. A <cite>GPU</cite> specializes in
<a href="https://en.wikipedia.org/wiki/SIMD"><cite>SIMD</cite></a> computing, which is perfect
for tensor processing.
|}
  | `Tfjs_wasm -> ""
  | `Tfjs_cpu ->
      {|
JavaScript version of <cite>Google</cite>'s tensor processing library. This backend processes the
tensors with single-threaded JavaScript code.
|}
  | `Owl_algodiff_cpu ->
      {|
<a href="http://ocaml.xyz/"><cite>Owl</cite></a> is <strike>an</strike> the OCaml scientific
computing library. Among other features it comprises an
<a href="https://en.wikipedia.org/wiki/Automatic_differentiation">automatic differentiation</a>
module (<cite>Algodiff</cite>) and several tensor processing backends. <Cite>Owl</Cite>'s native
OCaml backend transpiled to JavaScript is used here.
|}

let construct_backend_selection : _ Reactjs.constructor =
 fun (fire_upstream_event, tabidx, _) ->
  Printf.printf "> Component - backend_selection | construct\n%!";

  let on_change ev =
    ev##.target##.value |> Js.to_string |> int_of_string |> option_of_idx |> fire_upstream_event
  in
  let render (_, _, enabled) =
    Printf.printf "> Component - backend_selection | render\n%!";
    let open Reactjs.Jsx in
    let tbody =
      backends
      |> List.map (fun backend ->
             let n = name_of_backend backend in
             let d = description_of_backend backend in
             let create_check ww =
               of_bootstrap "Form.Check"
                 ~label:(if ww then "from Web Worker" else "Foreground")
                 ~id:(Printf.sprintf "selecting-backend-tab%d-%s-%b" tabidx n ww)
                 ~name:(Printf.sprintf "selecting-backend-tab%d" tabidx)
                 ~value:(idx_of_option (backend, ww) |> string_of_int)
                 ~type_:"radio"
                 ~style:[ ("color", "#007bff"); ("fontSize", "0.875em") ]
                 ~on_change ~inline:true ~disabled:(not enabled) []
             in
             [
               of_bootstrap "Form.Label" [ of_string n ] ~style:[ ("display", "block") ];
               create_check false;
               create_check true;
               of_bootstrap "Form.Text" ~classes:[ "text-muted" ] ~inner_html:d [];
             ]
             |> of_bootstrap "Form.Group" >> of_bootstrap "Col" ~md_span:6)
      |> of_bootstrap "Row" ~no_gutters:true
      >> of_bootstrap "Form" >> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr"
      >> of_tag "tbody"
    in
    let thead = of_string "Backend Selection" >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct render
