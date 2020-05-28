open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

open Types

let create_evaluating_signal tabsignal tabevents =
  let signal_latest_batch_end =
    React.S.sample
      (fun ev s ->
        match (s, ev) with
        | Evaluating _, Evaluation_event (`Batch_end i) -> Some i
        | Evaluating _, _ -> None
        | _, _ -> Some 0)
      tabevents tabsignal
    |> React.E.fmap Fun.id |> React.S.hold 0
  in
  let signal_evaluating =
    React.S.map
      (function
        | Evaluating s -> `On ((10000 + s.config.batch_size - 1) / s.config.batch_size) | _ -> `Off)
      tabsignal
  in
  React.S.l2
    (fun a b -> match (a, b) with `Off, _ -> `Off | `On bs, i -> `On (i, bs))
    signal_evaluating signal_latest_batch_end

let construct_results (tabsignal, tabevents) =
  Printf.printf "> construct component: results\n%!";
  let stat_events =
    React.E.fmap (function
      | Evaluation_event (`Outcome (`End stats)) -> Some (`Test_stats stats)
      | _ -> None)
  in
  ignore stat_events;
  let signal_evaluating = create_evaluating_signal tabsignal tabevents in

  let render _ =
    Printf.printf "> Results | render\n%!";
    let open Reactjs.Jsx in
    let tbody =
      [
        of_string "Chart" >> of_bootstrap "Col" ~sm:12;
        of_string "Images" >> of_bootstrap "Col" ~sm:12;
      ]
      |> of_bootstrap "Row" >> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr"
      >> of_tag "tbody"
    in
    let badges =
      match React.S.value signal_evaluating with
      | `Off -> []
      | `On (0, _) ->
          [
            "Evaluating" |> of_string
            >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ];
            "Allocating" |> of_string
            >> of_bootstrap "Badge" ~variant:"warning" ~style:[ ("marginLeft", "6px") ];
          ]
      | `On (i, bs) ->
          let i = float_of_int i in
          let bs = float_of_int bs in
          [
            "Evaluating" |> of_string
            >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ];
            Printf.sprintf "%.0f%%" ((i +. 1.) /. bs *. 100.)
            |> of_string
            >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ];
          ]
    in
    let thead = [ of_string "Results" ] @ badges |> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~class_:[ "mnist-panel" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct ~signal:signal_evaluating render
