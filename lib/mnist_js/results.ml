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
        | Evaluating s -> `On ((10000 + s.config.batch_size - 1) / s.config.batch_size) | _ -> `Off)
      tabsignal
  in
  React.S.l2
    (fun a b -> match (a, b) with `Off, _ -> `Off | `On bs, i -> `On (i, bs))
    evaluating latest_batch_end_idx

let jsx_of_marked_digits marked_digits_urls probas =
  let open Reactjs.Jsx in
  (* let _, maxi, _ = *)
  (*   List.fold_left *)
  (*     (fun (i, i', v') v -> if v > v' then (i + 1, i, v) else (i + 1, i', v')) *)
  (*     (0, 0, 0.) pred *)
  (* in *)

  let aux digit url=
    let probas = List.init 10 (fun i ->
                              Ndarray.get probas [|digit; i|]
                            )
    in
    (* let probas = Ndarray.get_slice [[digit]; []] probas in *)
    let probas =
      probas
      (* |> Ndarray.to_array *)
      |> List.map (fun p ->
          Printf.sprintf "%.0f%%, " (p *. 100.) |> of_string
        )
    in
    [
      of_tag "img" ~src:url [];
    ] @ probas
    |> of_bootstrap "Col" ~sm:6
  in
  List.mapi aux marked_digits_urls
  |> of_bootstrap "Row" ~style:["fontSize", "small"]  ~no_gutters:true
  >> of_bootstrap "Container"


  (* (\* let txt = Format.kasprintf Html.txt in *\) *)
  (* (\* let txt' = Format.kasprintf Html.txt in *\) *)
  (* let aux i x = *)
  (*   let bg = "background: " ^ Ft.Color.(Firegrass.get x |> to_hex_string) in *)
  (*   let cls = [] in *)
  (*   let cls = if i == lab then "good-one" :: cls else cls in *)
  (*   let cls = if i == maxi then "highest-one" :: cls else "not-highest-one" :: cls in *)
  (*   let content = [ txt "%d" i; Html.br (); txt' "%.0f%%" (x *. 100.) ] in *)
  (*   (\* let content = if i == maxi then [Html.b content] else content in *\) *)
  (*   [%html "<div style='" bg "' class='" cls "'>" content "</div>"] *)
  (* in *)
  (* let elt = *)
  (*   [%html "<div class='mnist-pred'><div><canvas></canvas></div>" (List.mapi aux pred) "</div>"] *)
  (*   |> Tyxml_js.To_dom.of_element *)
  (* in *)
  (* (\* Ft_js.select elt "canvas" Dom_html.CoerceTo.canvas |> put_digit_to_canvas img; *\) *)
  (* (\* elt *\) *)

let construct_results ((test_imgs, _), tabsignal, tabevents) =
  Printf.printf "> construct component: results\n%!";
  let signal_evaluating = create_evaluating_signal tabsignal tabevents in

  let marked_digits_urls =
    List.map
      (fun (_, idx) -> Ndarray.get_slice [ [ idx ]; []; [] ] test_imgs)
      Constants.marked_digits
    |> List.map Mnist.b64_url_of_digit
    (* |> List.iter print_endline *)
  in
  ignore marked_digits_urls;

  let test_stats_events =
    React.E.fmap (function
      | Evaluation_event (`Outcome (`End stats)) -> Some stats
      | _ -> None) tabevents
  in

  let marked_digits_signal =
    test_stats_events
    |> React.E.map (fun stats -> Some stats.marked_digits_probas)
    |> React.S.hold ~eq:( == ) None
  in

  let render _ =
    Printf.printf "> Results | render\n%!";
    let open Reactjs.Jsx in
    let digits =
      match React.S.value marked_digits_signal with
      | None -> of_string ""
      | Some probas -> jsx_of_marked_digits marked_digits_urls probas
    in
    let tbody =
      [
        of_string "Chart" >> of_bootstrap "Col" ~sm:12;
        digits;
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

  Reactjs.construct ~signal:signal_evaluating ~signal:marked_digits_signal render
