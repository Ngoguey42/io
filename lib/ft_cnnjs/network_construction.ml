open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
end

let encoder_channels_descr = "Channel count out of the encoder when the tensor is down to width=height=3"

let seed_description =
  "RNG seed to initialize the network's weights and sample the digits during the training."

let construct_int_input : (string * string * int * int) Reactjs.constructor =
 fun (name, description, default, min) ->
  let signal, set_val = React.S.create None in
  let on_change ev =
    let txt = ev##.target##.value |> Js.to_string in
    let is_valid_char = function '0' .. '9' -> true | '-' -> true | _ -> false in
    if String.length txt = 0 then set_val None
    else if txt |> String.to_seq |> List.of_seq |> List.for_all is_valid_char then
      let v = int_of_string txt in
      if v >= min then set_val (Some v)
  in
  let render _ =
    let open Reactjs.Jsx in
    let control =
      match React.S.value signal with
      | None ->
          of_bootstrap "Form.Control" ~placeholder:(string_of_int default) ~value:"" ~type_:"number"
            ~on_change []
      | Some value ->
          of_bootstrap "Form.Control" ~value:(string_of_int value) ~type_:"number" ~on_change []
    in
    of_bootstrap "Form.Group"
      [
        of_bootstrap "Form.Label" [ of_string name ];
        control;
        of_bootstrap "Form.Text" ~class_:[ "text-muted" ] [ of_string description ];
      ]
  in
  Reactjs.construct ~signal render

let construct_react_component : _ Reactjs.constructor =
 fun fire_upstream_event ->
  ignore fire_upstream_event;
  let render _ =
    let open Reactjs.Jsx in
    let submit = of_bootstrap "Button" ~type_:"submit" [ of_string "Create Network" ] in
    (* [encoder, decoder, encoder_channels, seed] *)
    [
      of_bootstrap "Form.Control" ~placeholder:"encoder" [];
      of_bootstrap "Form.Control" ~placeholder:"decoder" [];
      of_constructor construct_int_input ("Encoder Channels", encoder_channels_descr, 25, 1);
      of_constructor construct_int_input ("Seed", seed_description, 42, min_int);
      (* of_bootstrap "Form.Control" ~placeholder:"seed" *)
      (* of_bootstrap "Form.Control" ~placeholder:v ~type_:"int" ~min:1. ~step:1. *)
      (* "encoder"; "decoder"; "encoder_channels"; "seed" *)
    ]
    |> List.map (fun v -> of_bootstrap "Col" ~sm:6 [ v ])
    |> (fun l -> l @ [ submit ])
    |> of_bootstrap "Form.Row"
    |> (fun v -> [ v ])
    |> of_bootstrap "Form"
  in

  Reactjs.construct render
