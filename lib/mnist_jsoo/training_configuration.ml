open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
  module Regexp = Js_of_ocaml.Regexp
end

(* Conf ***************************************************************************************** *)
type lr = [ `Flat of float | `Down of float * float ]

type lr_tag = [ `Flat | `Down ] [@@deriving enum]

type raw_conf = {
  lr_tag : lr_tag;
  lr_begin : float;
  lr_end : float;
  batch_size : Int64.t;
  batch_count : Int64.t;
}

type derived_conf = { lr : lr; batch_size : int; batch_count : int }

module type ENTRY = sig
  type t

  val default : t

  val string_of_dconf : derived_conf -> string

  val update_rconf : t -> raw_conf -> raw_conf

  val name : string

  val description : string
end

module type INT = sig
  include ENTRY with type t = Int64.t
end

module type FLOAT = sig
  include ENTRY with type t = float
end

module type ENUM = sig
  include ENTRY

  val values : t list

  val to_string : t -> string

  val of_string : string -> t

  val name_of_tag : t -> string

  val float_submodules_of_dconf : derived_conf -> (module FLOAT) list
end

module Batch_size = struct
  type t = Int64.t

  (* let default = Int64.of_int 1 *)

  let default = Int64.of_int 500

  let string_of_dconf : derived_conf -> string = fun c -> Printf.sprintf "%d" c.batch_size

  let update_rconf : t -> raw_conf -> raw_conf = fun batch_size rconf -> { rconf with batch_size }

  let name = "Batch Size"

  let description =
    {|
Number of images inside a training batch. Use:
<ul>
<li><code>1</code> for <cite>Stochastic Gradient Descent</cite>,</li>
<li><code>60000</code> (the size of the dataset) for <cite>Gradient Descent</cite>,</li>
<li>any value in-between for <cite>Mini-Batch Gradient Descent</cite>.</li>
</ul>


|}
end

module Batch_count = struct
  type t = Int64.t

  (* let default = Int64.of_int 1 *)

  let default = Int64.of_int 20

  let string_of_dconf : derived_conf -> string = fun c -> Printf.sprintf "%d" c.batch_count

  let update_rconf : t -> raw_conf -> raw_conf = fun batch_count rconf -> { rconf with batch_count }

  let name = "Batch Count"

  let description = "Number of successive training batches."
end

module Lr_begin = struct
  type t = float

  let default = 1e-3

  let string_of_dconf : derived_conf -> string = fun _ -> ""

  let update_rconf : t -> raw_conf -> raw_conf = fun lr_begin rconf -> { rconf with lr_begin }

  let name = "Beginning"

  let description = ""
end

module Lr_end = struct
  type t = float

  let default = 0.

  let string_of_dconf : derived_conf -> string = fun _ -> ""

  let update_rconf : t -> raw_conf -> raw_conf = fun lr_end rconf -> { rconf with lr_end }

  let name = "End"

  let description = ""
end

module Lr = struct
  type t = lr_tag

  let values = List.init (max_lr_tag + 1) lr_tag_of_enum |> List.map Option.get

  let to_string v = lr_tag_to_enum v |> string_of_int

  let of_string v =
    match int_of_string v |> lr_tag_of_enum with Some v -> v | None -> failwith "unreachable"

  let default = `Flat

  let string_of_dconf : derived_conf -> string =
   fun c ->
    match c.lr with
    | `Down (lr0, lr1) -> Printf.sprintf "Down %.1e\u{2192}%.1e" lr0 lr1
    | `Flat lr0 -> Printf.sprintf "Flat %.1e" lr0

  let update_rconf : t -> raw_conf -> raw_conf = fun lr_tag rconf -> { rconf with lr_tag }

  let name = "Learning Rate"

  let description =
    {|
<a href="https://en.wikipedia.org/wiki/Learning_rate"><cite>Learning rate</cite><a> is the name
given to the scaling factor of the parameter updates performed by the optimizer.
Performing several trainings using the <cite>Down</cite> learning rate will get you a
<cite>cyclical learning rate</cite>, a famous technique that gives good results.
|}

  let name_of_tag : t -> string = function `Flat -> "Flat" | `Down -> "Down"

  let float_submodules_of_dconf : derived_conf -> (module FLOAT) list =
   fun dconf ->
    match dconf.lr with
    | `Down _ -> [ (module Lr_begin); (module Lr_end) ]
    | `Flat _ -> [ (module Lr_begin) ]
end

let dconf_of_rconf : raw_conf -> derived_conf =
 fun c ->
  let lr_begin = max 1e-10 c.lr_begin in
  let lr_end = min (max 0. c.lr_end) lr_begin in
  let lr = match c.lr_tag with `Flat -> `Flat lr_begin | `Down -> `Down (lr_begin, lr_end) in
  let batch_count = max Int64.one c.batch_count |> Int64.to_int in
  let batch_size = max Int64.one c.batch_size |> Int64.to_int in
  { lr; batch_size; batch_count }

(* React components ***************************************************************************** *)
let construct_int_input :
    ((module INT) * ((raw_conf -> raw_conf) -> unit) * derived_conf React.signal * bool)
    Reactjs.constructor =
 fun ((module M), update_rconf, dconf_signal, _) ->
  let signal = React.S.map M.string_of_dconf dconf_signal in
  let on_change ev =
    let newtxt = ev##.target##.value |> Js.to_string in
    if String.length newtxt = 0 then M.default |> M.update_rconf |> update_rconf
    else
      match Regexp.string_match (Regexp.regexp "[-+]?[0-9]+") newtxt 0 with
      | None -> ()
      | Some _ -> (
          match Int64.of_string newtxt with
          | v -> v |> M.update_rconf |> update_rconf
          | exception Failure _ -> () )
  in
  let render (_, _, _, enabled) =
    let open Reactjs.Jsx in
    let s = React.S.value signal in
    of_bootstrap "Form.Group"
      [
        of_bootstrap "Form.Label" [ Printf.sprintf "%s (%s)" M.name s |> of_string ];
        of_bootstrap "Form.Control" ~placeholder:(Int64.to_string M.default) ~disabled:(not enabled)
          ~size:"sm" ~type_:"number" ~on_change [];
        of_bootstrap "Form.Text" ~classes:[ "text-muted" ] ~inner_html:M.description [];
      ]
  in
  Reactjs.construct ~signal render

let construct_float_input :
    ((module FLOAT) * ((raw_conf -> raw_conf) -> unit) * derived_conf React.signal * bool * bool)
    Reactjs.constructor =
 fun ((module M), update_rconf, dconf_signal, _, _) ->
  let signal = React.S.map M.string_of_dconf dconf_signal in
  let on_change ev =
    let newtxt = ev##.target##.value |> Js.to_string in
    if String.length newtxt = 0 then M.default |> M.update_rconf |> update_rconf
    else
      match float_of_string newtxt with
      | v -> v |> M.update_rconf |> update_rconf
      | exception Failure _ -> ()
  in
  let render (_, _, _, minimal, enabled) =
    let open Reactjs.Jsx in
    let s = React.S.value signal in
    if minimal then
      [
        of_bootstrap "InputGroup.Text" [ M.name |> of_string ] >> of_bootstrap "InputGroup.Prepend";
        of_bootstrap "Form.Control" ~placeholder:(string_of_float M.default) ~disabled:(not enabled)
          ~type_:"number" ~on_change [];
      ]
      |> of_bootstrap "InputGroup" ~size:"sm"
    else
      [
        of_bootstrap "Form.Label" [ Printf.sprintf "%s (%s)" M.name s |> of_string ];
        of_bootstrap "Form.Control" ~placeholder:(string_of_float M.default) ~disabled:(not enabled)
          ~size:"sm" ~type_:"number" ~on_change [];
        of_bootstrap "Form.Text" ~classes:[ "text-muted" ] ~inner_html:M.description [];
      ]
      |> of_bootstrap "Form.Group"
  in

  Reactjs.construct ~signal render

let construct_select :
    ((module ENUM) * ((raw_conf -> raw_conf) -> unit) * derived_conf React.signal * bool)
    Reactjs.constructor =
 fun ((module M), update_rconf, dconf_signal, _) ->
  let signal = React.S.map M.string_of_dconf dconf_signal in
  let on_change ev =
    let v = ev##.target##.value |> Js.to_string |> M.of_string in
    update_rconf (M.update_rconf v)
  in
  let render (_, _, _, enabled) =
    let open Reactjs.Jsx in
    let dconf = React.S.value dconf_signal in
    let s = React.S.value signal in
    let control =
      M.values
      |> List.map (fun v -> of_tag "option" ~value:(M.to_string v) [ of_string (M.name_of_tag v) ])
      |> of_bootstrap "Form.Control" ~as_:"select" ~disabled:(not enabled) ~on_change ~size:"sm"
           ~default_value:(M.to_string M.default)
    in
    let submodules = M.float_submodules_of_dconf dconf in
    let submodules =
      let md_span = 12 / List.length submodules in
      List.map
        (fun v ->
          of_constructor construct_float_input (v, update_rconf, dconf_signal, true, enabled)
          >> of_bootstrap "Col" ~md_span)
        submodules
    in
    ignore submodules;

    [
      of_bootstrap "Form.Label" [ Printf.sprintf "%s (%s)" M.name s |> of_string ]
      >> of_bootstrap "Col" ~md_span:12;
      control >> of_bootstrap "Col" ~md_span:12;
    ]
    @ submodules
    @ [
        of_bootstrap "Form.Text" ~classes:[ "text-muted" ] ~inner_html:M.description []
        >> of_bootstrap "Col" ~md_span:12;
      ]
    |> of_bootstrap "Row" ~no_gutters:true
    >> of_bootstrap "Form.Group"
  in
  Reactjs.construct ~signal render

let construct_training_config : _ Reactjs.constructor =
 fun (fire_upstream_event, _) ->
  Printf.printf "> Component - training_configuration | construct\n%!";
  let rconf_signal, update_rconf =
    React.S.create
      {
        lr_tag = Lr.default;
        batch_count = Batch_count.default;
        batch_size = Batch_size.default;
        lr_begin = Lr_begin.default;
        lr_end = Lr_end.default;
      }
  in

  let dconf_signal = React.S.map dconf_of_rconf rconf_signal in

  let update_rconf updater =
    let rconf = React.S.value rconf_signal in
    let rconf = updater rconf in
    update_rconf rconf
  in

  let on_click ev =
    ev##preventDefault;
    let dconf = React.S.value dconf_signal in
    fire_upstream_event (dconf.lr, dconf.batch_size, dconf.batch_count)
  in

  let render (_, enabled) =
    let open Reactjs.Jsx in
    let dconf = React.S.value dconf_signal in
    ignore dconf;
    let button =
      of_string "Train"
      >> of_bootstrap ~disabled:(not enabled) ~on_click "Button" ~type_:"submit" ~size:"lg"
      >> of_tag "div" ~style:[ ("display", "flex"); ("alignItems", "flex-end") ]
    in
    let select m = of_constructor construct_select (m, update_rconf, dconf_signal, enabled) in
    let input m = of_constructor construct_int_input (m, update_rconf, dconf_signal, enabled) in
    let tbody =
      [
        [ select (module Lr); button ]
        |> of_bootstrap "Col" ~xs_span:12 ~xs_order:1 ~md_span:6 ~md_order:0;
        [ input (module Batch_size); input (module Batch_count) ]
        |> of_bootstrap "Col" ~xs_span:12 ~xs_order:0 ~md_span:6 ~md_order:1;
      ]
      |> of_bootstrap "Row" >> of_bootstrap "Form" >> of_tag "th" >> of_tag "tr" >> of_tag "tbody"
    in
    let thead =
      of_string "Training Configuration" >> of_tag "th" >> of_tag "tr" >> of_tag "thead"
    in
    of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct ~signal:dconf_signal render
