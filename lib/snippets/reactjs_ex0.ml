[@@@ocamlformat "disable"]

module Reactjs = Ft_js.Reactjs

(* snip-before *)
let render_operation (name, operation, fire_operation) =
  let open Reactjs.Jsx in
  let on_click ev =
    fire_operation operation;
    ev##preventDefault
  in
  let classes = [ "btn"; "btn-primary" ] in
  let style = [ ("margin", "2px") ] in
  of_tag "button" ~on_click ~classes ~style [ of_string name ]

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
    let value =
      of_tag "h3" [ React.S.value value_signal |> string_of_float |> of_string ]
    in
    let buttons =
      List.map
        (fun (name, operation) ->
          of_render render_operation (name, operation, fire_operation))
        ops
      |> of_tag "div"
    in
    [ value; buttons ] |> of_tag "div" ~style:[ ("textAlign", "center") ]
  in
  Reactjs.construct ~signal:value_signal render
(* snip-after *)
