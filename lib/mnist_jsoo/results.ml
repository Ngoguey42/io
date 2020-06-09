open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

open Types

let create_evaluating_signal tabsignal tabevents =
  let evaluating_signal =
    React.S.map
      (function
        | Evaluating s ->
            let batch_count =
              (Mnist.test_set_size + s.config.batch_size - 1) / s.config.batch_size
            in
            `On (batch_count, s.config.backend)
        | _ -> `Off)
      tabsignal
  in
  let batch_begin_signal =
    tabevents
    |> React.E.fmap (function
         | Evaluation_event (`Batch_begin i) -> Some (Some i)
         | Evaluation_event (`Outcome _) -> Some None
         | _ -> None)
    |> React.S.hold None
  in
  let batch_end_signal =
    tabevents
    |> React.E.fmap (function
         | Evaluation_event (`Batch_end i) -> Some (Some i)
         | Evaluation_event (`Outcome _) -> Some None
         | _ -> None)
    |> React.S.hold None
  in
  React.S.l3
    (fun ev bbeg bend ->
      match (ev, bbeg, bend) with
      | `Off, _, _ -> `Off
      | `On (_, _), None, None | `On (_, `Tfjs_webgl), Some 0, None -> `Allocating
      | `On (_, _), Some 0, None -> `On 0.0
      | `On (batch_count, _), Some _, Some i ->
          `On ((float_of_int i +. 1.) /. float_of_int batch_count)
      | `On _, None, Some _ -> failwith "results.create_evaluating_signal@unreachable 0"
      | `On _, Some _, None -> failwith "results.create_evaluating_signal@unreachable 1")
    evaluating_signal batch_begin_signal batch_end_signal

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
             let classes = if i = digit then [ "good-one" ] else [] in
             content >> of_tag "div" ~classes ~style:[ ("background", bg) ])
    in
    let md_order = (digit / 5) + (digit mod 5 * 2) in
    [ img ] @ probas |> of_bootstrap "Col" ~classes:[ "mnist-pred" ] ~md_span:6 ~md_order
  in

  [
    of_string "Test-set sample"
    >> of_bootstrap "Col" ~classes:[ "mnist-pred" ] ~md_span:12 ~as_:"h5"
    >> of_bootstrap "Row";
    List.mapi aux test_set_sample_urls |> of_bootstrap "Row" ~no_gutters:true;
  ]
  |> of_bootstrap "Container"

let construct_results ((test_imgs, _), tabshownsignal, tabsignal, tabevents) =
  Printf.printf "> Component - results | construct\n%!";

  let signal_evaluating = create_evaluating_signal tabsignal tabevents in
  let test_set_sample_urls =
    List.map (fun (_, idx) -> Ndarray.get_slice [ [ idx ]; []; [] ] test_imgs) Mnist.test_set_sample
    |> List.map Mnist.b64_url_of_digit
  in
  let test_set_sample_signal =
    tabevents
    |> React.E.fmap (function
         | Evaluation_event (`Outcome (`End stats)) -> Some (Some stats.test_set_sample_probas)
         | _ -> None)
    |> React.S.hold ~eq:( == ) None
  in
  let plotly_ref = Reactjs.create_ref () in

  let render _ =
    Printf.printf "> Results | render\n%!";
    let open Reactjs.Jsx in
    let chart =
      let title = of_string "Statistics" >> of_tag "h5" in
      let chart =
        of_tag "div" ~ref:plotly_ref ~id:"my-div"
          ~style:[ ("width", "686px"); ("height", "350px") ]
          []
      in
      of_tag "div" ~style:[ ("textAlign", "center") ] [ title; chart ]
    in
    let digits =
      match React.S.value test_set_sample_signal with
      | None -> of_string ""
      | Some probas -> jsx_of_test_set_sample test_set_sample_urls probas
    in
    let tbody =
      [ chart; digits ] |> of_bootstrap "Container" >> of_tag "th" >> of_tag "tr" >> of_tag "tbody"
    in
    let badges =
      match React.S.value signal_evaluating with
      | `Off -> []
      | `Allocating ->
          [
            "Evaluating" |> of_string
            >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ];
            "Allocating" |> of_string
            >> of_bootstrap "Badge" ~variant:"warning" ~style:[ ("marginLeft", "6px") ];
          ]
      | `On prog ->
          [
            "Evaluating" |> of_string
            >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ];
            Printf.sprintf "%.0f%%" (prog *. 100.)
            |> of_string
            >> of_bootstrap "Badge" ~variant:"info" ~style:[ ("marginLeft", "6px") ];
          ]
    in
    let thead =
      [ of_string "Results board" ] @ badges |> of_tag "th" >> of_tag "tr" >> of_tag "thead"
    in
    of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in
  let mount () =
    match plotly_ref##.current |> Js.Opt.to_option with
    | None -> failwith "unreachable. React.ref failed"
    | Some elt -> Chart.routine elt tabshownsignal tabsignal tabevents
  in

  Reactjs.construct ~signal:signal_evaluating ~signal:test_set_sample_signal ~mount render
