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
type encoder_tag = [ `Fc | `Oneconv | `Twoconv | `Threeconv | `Fourconvres ] [@@deriving enum]

type decoder_tag = [ `Maxpool_fc | `Fc ] [@@deriving enum]

type optimizer_tag = [ `Adam0 | `Sgd ] [@@deriving enum]

type raw_conf = {
  encoder_tag : encoder_tag;
  parameters : Int64.t;
  decoder_tag : decoder_tag;
  seed : Int64.t;
  optimizer_tag : optimizer_tag;
}

type derived_conf = {
  encoder_tag : encoder_tag;
  decoder_tag : decoder_tag;
  parameters : int;
  filters : int;
  seed : int;
  optimizer_tag : optimizer_tag;
  encoder_nn : Fnn.network;
  decoder_nn : Fnn.network;
}

module type ENTRY = sig
  type t

  val default : t

  val update_rconf : t -> raw_conf -> raw_conf

  val name : string

  val description : string
end

module type ENUM = sig
  include ENTRY

  val values : t list

  val to_string : t -> string

  val of_string : string -> t

  val name_of_tag : t -> string
end

module type INT = sig
  include ENTRY with type t = Int64.t

  val of_dconf : derived_conf -> t
end

module Seed = struct
  type t = Int64.t

  let default = Int64.of_int 42

  let of_dconf : derived_conf -> t = fun c -> Int64.of_int c.seed

  let update_rconf : t -> raw_conf -> raw_conf = fun seed rconf -> { rconf with seed }

  let name = "Seed"

  let description =
    "PRNG seed to initialize the network's weights and order the images during training."
end

module Parameters = struct
  type t = Int64.t

  let default = Int64.of_int 10000

  let of_dconf : derived_conf -> t = fun c -> Int64.of_int c.parameters

  let update_rconf : t -> raw_conf -> raw_conf = fun parameters rconf -> { rconf with parameters }

  let name = "Parameters"

  let description = "Number of float32 trainable parameters owned by the network."
end

module Encoder = struct
  type t = encoder_tag

  let values = List.init (max_encoder_tag + 1) encoder_tag_of_enum |> List.map Option.get

  let to_string v = encoder_tag_to_enum v |> string_of_int

  let of_string v =
    match int_of_string v |> encoder_tag_of_enum with Some v -> v | None -> failwith "unreachable"

  let default = `Oneconv

  let of_dconf : derived_conf -> t = fun c -> c.encoder_tag

  let update_rconf : t -> raw_conf -> raw_conf = fun encoder_tag rconf -> { rconf with encoder_tag }

  let name = "Encoder"

  let description = "First part of the network. Takes a batch of 28x28x1 float32 images."

  let module_of_tag : t -> (module Network_architectures.ENCODER) = function
    | `Fc -> (module Network_architectures.Fc_encoder)
    | `Oneconv -> (module Network_architectures.Oneconv)
    | `Twoconv -> (module Network_architectures.Twoconv)
    | `Threeconv -> (module Network_architectures.Threeconv)
    | `Fourconvres -> (module Network_architectures.Fourconvres)

  let name_of_tag t : string =
    let module M = (val module_of_tag t) in
    M.short_description

  let code_of_tag t f : string =
    let module M = (val module_of_tag t) in
    M.code f

  let network_of_tag t b o f : Fnn.network =
    let module M = (val module_of_tag t) in
    M.create b o f
end

module Decoder = struct
  type t = decoder_tag

  let values = List.init (max_decoder_tag + 1) decoder_tag_of_enum |> List.map Option.get

  let to_string v = decoder_tag_to_enum v |> string_of_int

  let of_string v =
    match int_of_string v |> decoder_tag_of_enum with Some v -> v | None -> failwith "unreachable"

  let default = `Fc

  let of_dconf : derived_conf -> t = fun c -> c.decoder_tag

  let update_rconf : t -> raw_conf -> raw_conf = fun decoder_tag rconf -> { rconf with decoder_tag }

  let name = "Decoder"

  let description =
    "Second part of the network. Outputs a batch of 1x1x10 float32 predictions that sum to 1 for \
     each image."

  let module_of_tag : t -> (module Network_architectures.DECODER) = function
    | `Fc -> (module Network_architectures.Fc_decoder)
    | `Maxpool_fc -> (module Network_architectures.Maxpool_fc)

  let name_of_tag t : string =
    let module M = (val module_of_tag t) in
    M.short_description

  let code_of_tag t w f : string =
    let module M = (val module_of_tag t) in
    M.code w f

  let network_of_tag t b o w f : Fnn.network =
    let module M = (val module_of_tag t) in
    M.create b o w f
end

module Optimizer = struct
  type t = optimizer_tag

  let values = List.init (max_optimizer_tag + 1) optimizer_tag_of_enum |> List.map Option.get

  let to_string v = optimizer_tag_to_enum v |> string_of_int

  let of_string v =
    match int_of_string v |> optimizer_tag_of_enum with
    | Some v -> v
    | None -> failwith "unreachable"

  let name_of_tag = function
    | `Adam0 -> "ADaptive Moment Estimation (ADAM)"
    | `Sgd -> "Stochastic Gradient Descent (SGD)"

  let default = `Sgd

  let of_dconf : derived_conf -> t = fun c -> c.optimizer_tag

  let update_rconf : t -> raw_conf -> raw_conf =
   fun optimizer_tag rconf -> { rconf with optimizer_tag }

  let name = "Optimizer"

  let description = "Algorithm used to update a parameter given the gradient of that parameter."

  let optimizer_of_tag = function `Adam0 -> `Adam (0.9, 0.999, 1e-8) | `Sgd -> `Sgd

  let code_of_tag = function `Adam0 -> "`Adam (0.9, 0.999, 1e-8)" | `Sgd -> "`Sgd"
end

(* ********************************************************************************************** *)
let tooltip_chunks =
  [|
    "(* OCaml code to create the Fnn.network object *)\nlet o = ";
    " in\nlet rng = Random.State.make [| ";
    {| |] in
let open (val Fnn.create_builder ~rng ()) in
let open Pshape.Size in

input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
(* encoder *)
|};
    "(* decoder *)\n";
  |]

let jsx_of_line =
  let rec clean_string_of_raw_char_list = function
    | ' ' :: tl -> "\u{a0}" ^ clean_string_of_raw_char_list tl
    | tl ->
        let tl = Array.of_list tl in
        String.init (Array.length tl) (fun i -> tl.(i))
  in
  fun s ->
    s |> String.to_seq |> List.of_seq |> clean_string_of_raw_char_list |> Reactjs.Jsx.of_string

let jsx_of_code s =
  let open Reactjs.Jsx in
  let rec aux = function
    | [] -> []
    | [ hd ] -> [ jsx_of_line hd ]
    | hd :: tl -> jsx_of_line hd :: of_tag "br" [] :: aux tl
  in
  String.split_on_char '\n' s |> aux

let code_of_dconf dconf =
  let (Pshape.Size.K w) = Pshape.get dconf.encoder_nn#out_shape `S0 |> Pshape.Size.to_known in
  let opti = Optimizer.code_of_tag dconf.optimizer_tag in
  let ecode = Encoder.code_of_tag dconf.encoder_tag dconf.filters in
  let dcode = Decoder.code_of_tag dconf.decoder_tag w dconf.filters in
  tooltip_chunks.(0) ^ opti ^ tooltip_chunks.(1) ^ string_of_int dconf.seed ^ tooltip_chunks.(2)
  ^ ecode ^ tooltip_chunks.(3) ^ dcode

let dconf_of_rconf : raw_conf -> derived_conf =
 fun conf ->
  let seed =
    if Int64.compare conf.seed (Int64.of_int 0) <= 0 then 0
    else if Int64.compare (Int64.of_int ((2 lsl 29) - 1)) conf.seed <= 0 then (2 lsl 29) - 1
    else Int64.to_int conf.seed
  in
  let parameters =
    if Int64.compare conf.parameters (Int64.of_int 1) <= 0 then 1
    else if Int64.compare (Int64.of_int max_int) conf.parameters <= 0 then max_int
    else Int64.to_int conf.parameters
  in

  let networks_of_filters f =
    let o = Optimizer.optimizer_of_tag conf.optimizer_tag in
    let rng = Random.State.make [| seed |] in
    let make_builder () = Fnn.create_builder ~rng () in
    let builder = make_builder () in
    let e = Encoder.network_of_tag conf.encoder_tag builder o f in
    let (Pshape.Size.K w) = Pshape.get e#out_shape `S0 |> Pshape.Size.to_known in
    let (Pshape.Size.K f') = Pshape.get e#out_shape `C |> Pshape.Size.to_known in
    let d = Decoder.network_of_tag conf.decoder_tag builder o w f' in
    (e, d)
  in

  let minf = 1 in
  let maxp = 1000000 in

  let emin, dmin = networks_of_filters minf in
  let pmin = Fnn.parameters [ emin; dmin ] |> List.fold_left (fun acc nn -> acc + nn#numel) 0 in

  (* Since Fnn doesn't allocate tensors on fresh networks we can search the number of filters by
     bisection.
  *)
  let rec find_closest_network ((ileft, vleft, _) as left) right =
    assert (vleft <= maxp);
    match right with
    | Some ((iright, vright, _) as right) when ileft + 1 = iright ->
        (* Stop *)
        assert (ileft < iright);
        assert (vleft <= vright);
        if vright > maxp then left
        else if parameters - vleft <= vright - parameters then left
        else right
    | Some ((iright, vright, _) as right) ->
        (* Dichotomy *)
        assert (ileft < iright);
        assert (vleft <= vright);
        let imid = (ileft + iright) / 2 in
        assert (imid <> ileft);
        assert (imid <> iright);
        let emid, dmid = networks_of_filters imid in
        let vmid =
          Fnn.parameters [ emid; dmid ] |> List.fold_left (fun acc nn -> acc + nn#numel) 0
        in
        let mid = (imid, vmid, (emid, dmid)) in
        if vmid >= maxp then find_closest_network left (Some mid)
        else if vmid <= parameters then find_closest_network mid (Some right)
        else find_closest_network left (Some mid)
    | None ->
        (* Looking for right bound *)
        let imid = ileft * 2 in
        assert (imid > ileft);
        (* Failsafe for int overflow *)
        let emid, dmid = networks_of_filters imid in
        let vmid =
          Fnn.parameters [ emid; dmid ] |> List.fold_left (fun acc nn -> acc + nn#numel) 0
        in
        let mid = (imid, vmid, (emid, dmid)) in
        if vmid = vleft then find_closest_network left (Some mid)
        else if vmid >= maxp then find_closest_network left (Some mid)
        else if vmid <= parameters then find_closest_network mid None
        else find_closest_network left (Some mid)
  in

  let filters, parameters, (encoder_nn, decoder_nn) =
    find_closest_network (minf, pmin, (emin, dmin)) None
  in

  {
    encoder_tag = conf.encoder_tag;
    decoder_tag = conf.decoder_tag;
    seed;
    filters;
    parameters;
    encoder_nn;
    decoder_nn;
    optimizer_tag = conf.optimizer_tag;
  }

(* React components ***************************************************************************** *)
let construct_int_input :
    ((module INT) * ((raw_conf -> raw_conf) -> unit) * derived_conf React.signal * bool)
    Reactjs.constructor =
 fun ((module M), update_rconf, dconf_signal, _) ->
  let signal = React.S.map M.of_dconf dconf_signal in
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
    let v = React.S.value signal in
    of_bootstrap "Form.Group"
      [
        of_bootstrap "Form.Label" [ Printf.sprintf "%s (%Ld)" M.name v |> of_string ];
        of_bootstrap "Form.Control" ~placeholder:(Int64.to_string M.default) ~disabled:(not enabled)
          ~size:"sm" ~type_:"number" ~on_change [];
        of_bootstrap "Form.Text" ~class_:[ "text-muted" ] [ of_string M.description ];
      ]
  in
  Reactjs.construct ~signal render

let construct_select : ((module ENUM) * ((raw_conf -> raw_conf) -> unit) * bool) Reactjs.constructor
    =
 fun ((module M), update_rconf, _) ->
  let on_change ev =
    let v = ev##.target##.value |> Js.to_string |> M.of_string in
    update_rconf (M.update_rconf v)
  in
  let render (_, _, enabled) =
    let open Reactjs.Jsx in
    let control =
      M.values
      |> List.map (fun v -> of_tag "option" ~value:(M.to_string v) [ of_string (M.name_of_tag v) ])
      |> of_bootstrap "Form.Control" ~as_:"select" ~disabled:(not enabled) ~on_change ~size:"sm"
           ~default_value:(M.to_string M.default)
    in
    of_bootstrap "Form.Group"
      [
        of_bootstrap "Form.Label" [ of_string M.name ];
        control;
        of_bootstrap "Form.Text" ~class_:[ "text-muted" ] [ of_string M.description ];
      ]
  in
  Reactjs.construct render

let construct_react_component : _ Reactjs.constructor =
 fun (fire_upstream_event, _) ->
  Printf.printf "> construct component: network_creation\n%!";
  let rconf_signal, update_rconf =
    React.S.create
      {
        encoder_tag = Encoder.default;
        parameters = Parameters.default;
        decoder_tag = Decoder.default;
        seed = Seed.default;
        optimizer_tag = Optimizer.default;
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
    fire_upstream_event (dconf.encoder_nn, dconf.decoder_nn, dconf.seed)
  in

  let render (_, enabled) =
    let open Reactjs.Jsx in
    let dconf = React.S.value dconf_signal in
    let tt =
      of_tag "div"
        ~class_:[ "text-monospace"; "text-left"; "small" ]
        (jsx_of_code (code_of_dconf dconf))
      >> of_bootstrap "Tooltip" ~id:"network-code"
    in
    let button =
      [
        of_string "Create"
        >> of_bootstrap ~disabled:(not enabled) ~on_click "Button" ~type_:"submit" ~size:"lg";
        of_string "?"
        >> of_bootstrap
             ~on_click:(fun ev -> ev##preventDefault)
             "Button" ~type_:"submit" ~class_:[ "btn-info"; "code-overlay" ]
        >> of_bootstrap "OverlayTrigger" ~placement:"right" ~overlay:tt;
      ]
      |> of_bootstrap "Col" ~sm:6 ~style:[ ("display", "flex"); ("alignItems", "flex-end") ]
    in
    let select m =
      of_constructor construct_select (m, update_rconf, enabled) >> of_bootstrap "Col" ~sm:6
    in
    let input m =
      of_constructor construct_int_input (m, update_rconf, dconf_signal, enabled)
      >> of_bootstrap "Col" ~sm:6
    in
    let tbody =
      [
        select (module Encoder);
        select (module Decoder);
        input (module Parameters);
        input (module Seed);
        button;
        select (module Optimizer);
      ]
      |> of_bootstrap "Row" >> of_bootstrap "Form" >> of_tag "th" >> of_tag "tr" >> of_tag "tbody"
    in
    let thead = of_string "Network Creation" >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
    of_bootstrap "Table" ~class_:[ "mnist-panel" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct ~signal:dconf_signal render
