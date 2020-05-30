open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

open Types

let create_evaluating_signal tabsignal tabevents =
  let latest_batch_end_idx =
    React.S.sample
      (fun ev s ->
        match (s, ev) with
        | Evaluating _, Evaluation_event (`Batch_end i) -> Some i
        | Evaluating _, _ -> None
        | _, _ -> Some 0)
      tabevents tabsignal
    |> React.E.fmap Fun.id |> React.S.hold 0
  in
  let evaluating =
    React.S.map
      (function
        | Evaluating s -> `On ((Mnist.test_set_size + s.config.batch_size - 1) / s.config.batch_size) | _ -> `Off)
      tabsignal
  in
  React.S.l2
    (fun a b -> match (a, b) with `Off, _ -> `Off | `On bs, i -> `On (i, bs))
    evaluating latest_batch_end_idx

let jsx_of_test_set_sample test_set_sample_urls probas =
  let open Reactjs.Jsx in
  let aux digit url =
    let probas = List.init 10 (fun i -> Ndarray.get probas [| digit; i |]) in
    let img = of_tag "img" ~src:url [] in
    let probas =
      probas
      |> List.mapi (fun i p ->
             let bg = Ft.Color.(Firegrass2.get p |> to_hex_string) in
             let content = i |> string_of_int |> of_string in
             let class_ = if i = digit then [ "good-one" ] else [] in
             content >> of_tag "div" ~class_ ~style:[ ("background", bg) ])
    in
    let md_order = (digit / 5) + (digit mod 5 * 2) in
    [ img ] @ probas |> of_bootstrap "Col" ~class_:[ "mnist-pred" ] ~md_span:6 ~md_order
  in

  [
    of_string "Test-set sample"
    >> of_bootstrap "Col" ~class_:[ "mnist-pred" ] ~md_span:12 ~as_:"h5"
    >> of_bootstrap "Row";
    List.mapi aux test_set_sample_urls |> of_bootstrap "Row" ~no_gutters:true;
  ]
  |> of_bootstrap "Container"

let construct_results ((test_imgs, _), tabsignal, tabevents) =
  Printf.printf "> Component - results | construct\n%!";
  let signal_evaluating = create_evaluating_signal tabsignal tabevents in

  let test_set_sample_urls =
    List.map
      (fun (_, idx) -> Ndarray.get_slice [ [ idx ]; []; [] ] test_imgs)
      Mnist.test_set_sample
    |> List.map Mnist.b64_url_of_digit
  in
  let test_stats_events =
    React.E.fmap
      (function Evaluation_event (`Outcome (`End stats)) -> Some stats | _ -> None)
      tabevents
  in

  let test_set_sample_signal =
    test_stats_events
    |> React.E.map (fun stats -> Some stats.test_set_sample_probas)
    |> React.S.hold ~eq:( == ) None
  in

  let render _ =
    Printf.printf "> Results | render\n%!";
    let open Reactjs.Jsx in
    let digits =
      match React.S.value test_set_sample_signal with
      | None -> of_string ""
      | Some probas -> jsx_of_test_set_sample test_set_sample_urls probas
    in
    let tbody =
      [
        of_string "Statistics"
        >> of_bootstrap "Col" ~class_:[ "mnist-pred" ] ~md_span:12 ~as_:"h5"
             ~style:[ ("display", "flex"); ("justifyContent", "center") ]
        >> of_bootstrap "Row";
        digits;
      ]
      |> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr" >> of_tag "tbody"
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

  Reactjs.construct ~signal:signal_evaluating ~signal:test_set_sample_signal render
