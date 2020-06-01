[@@@ocamlformat "disable"]

module Reactjs = Ft_js.Reactjs

(* snip-before *)
let construct_button (name, operation, fire_operation) =
  let on_click ev =
    fire_operation operation;
    ev##preventDefault
  in
  let render _ =
    let open Reactjs.Jsx in
    let classes = [ "btn"; "btn-primary" ] in
    let style = [ ("margin", "2px") ] in
    of_tag "button" ~on_click ~classes ~style [ of_string name ]
  in
  Reactjs.construct render

let construct_component () =
  let operation_events, fire_operation = React.E.create () in
  let fire_operation : (float -> float) -> unit = fire_operation in
  let value_signal = React.S.accum operation_events 42.0 in
  let ops =
    [
      ("+2", ( +. ) 2.0);
      ("-2", fun v -> v -. 2.0);
      ("x2", ( *. ) 2.0);
      ("/2", fun v -> v /. 2.0);
    ]
  in
  let render () =
    let open Reactjs.Jsx in
    let current_value =
      React.S.value value_signal
      |> Printf.sprintf "signal's current value: %f"
      |> of_string >> of_tag "h3"
    in
    let buttons =
      List.map
        (fun (n, op) -> of_constructor construct_button (n, op, fire_operation))
        ops
      |> of_tag "div"
    in
    [ current_value; buttons ]
    |> of_tag "div" ~style:[ ("textAlign", "center") ]
  in
  Reactjs.construct ~signal:value_signal render
(* snip-after *)
