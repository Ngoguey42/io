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

(** [construct_button ()] may be rendered to DOM using [Reactjs.render]. *)
let construct_component () =
  let operation_events, fire_operation = React.E.create () in
  let fire_operation : (float -> float) -> unit = fire_operation in
  let value_signal = React.S.accum operation_events 42.0 in
  let ops =
    [ ("+2", ( +. ) 2.0); ("-2", fun v -> v -. 2.0); ("x2", ( *. ) 2.0); ("/2", fun v -> v /. 2.0) ]
  in

  let render () =
    let open Reactjs.Jsx in
    let value =
      React.S.value value_signal
      |> Printf.sprintf "signal's current value: %f"
      |> of_string >> of_tag "h3"
    in
    let buttons =
      List.map (fun (name, operation) -> (name, operation, fire_operation)) ops
      |> List.map (of_constructor construct_button)
      |> of_tag "div"
    in
    [ value; buttons ] |> of_tag "div" ~style:[ ("textAlign", "center") ]
  in

  let unmount () = React.E.stop ~strong:true operation_events in

  Reactjs.construct ~signal:value_signal ~unmount render

(* snip-after *)
