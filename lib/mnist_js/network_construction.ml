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

(* Encoders ************************************************************************************* *)
type encoder_tag = [ `Zeroconv | `Oneconv | `Twoconv | `Threeconv | `Threeconvres ]
[@@deriving enum]

module type ENCODER = sig
  val create : (module Fnn.BUILDER) -> Fnn.optimizer_conf -> int -> Fnn.network

  val code : int -> string

  val description : string
end

module Noop : ENCODER = struct
  let create (module Builder : Fnn.BUILDER) _ _ : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32 |> Fnn.downcast

  let code _ = ""

  let description = "No encoder, only decoder"
end

module Oneconv : ENCODER = struct
  let create (module Builder : Fnn.BUILDER) o f : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> conv2d ~o (`Full f) (16, 16) ~s:(6, 6) ~b:`Assert_fit
    |> bias |> relu |> Fnn.downcast

  let code f =
    Printf.sprintf "|> conv2d ~o (`Full %d) (16, 16) ~s:(6, 6) ~b:`Assert_fit |> bias |> relu\n" f

  let description = "A single 16x16(s6) convolution"
end

module Twoconv : ENCODER = struct
  let create (module Builder : Fnn.BUILDER) o f : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit
    |> bias |> relu
    |> conv2d ~o (`Full f) (3, 3) ~s:(3, 3) ~b:`Assert_fit
    |> bias |> relu |> Fnn.downcast

  let code f =
    Printf.sprintf
      "|> conv2d ~o (`Full %d) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu\n\
       |> conv2d ~o (`Full %d) (3, 3) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu\n"
      f f

  let description = "One 4x4(s3) and one 3x3(s3)"
end

module Threeconv = struct
  let create (module Builder : Fnn.BUILDER) o f : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit
    |> bias |> relu
    |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit
    |> bias |> relu
    |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit
    |> bias |> relu |> Fnn.downcast

  let code f =
    Printf.sprintf
      {||> conv2d ~o (`Full %d) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu
|> conv2d ~o (`Full %d) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
|> conv2d ~o (`Full %d) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
|}
      f f f

  let description = "One 4x4(s3) and two 4x4(s1)"
end

module Threeconvres : ENCODER = struct
  let create (module Builder : Fnn.BUILDER) o f : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    (* |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu *)
    (* |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu *)
    (* |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu *)
    (* |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu *)
    |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit
    |> bias
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Fnn.downcast;
           up |> relu |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast;
         ])
    |> sum
    (* |> batch_norm *)
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Fnn.downcast;
           up |> relu |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast;
         ])
    |> sum
    (* |> batch_norm *)
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Fnn.downcast;
           up |> relu |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast;
         ])
    (* (\* |> (fun up -> *\) *)
    (* (\*      [ *\) *)
    (* (\*        up |> cropping2d [ 1; 2; 1; 2 ] |> Fnn.downcast; *\) *)
    (* (\*        up |> relu |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast; *\) *)
    (* (\*      ]) *\) *)
    (* (\* |> sum *\) *)
    (* (\* |> (fun up -> *\) *)
    (* (\*      [ *\) *)
    (* (\*        up |> cropping2d [ 2; 1; 2; 1 ] |> Fnn.downcast; *\) *)
    (* (\*        up |> relu |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast; *\) *)
    (* (\*      ]) *\) *)
    |> sum
    (* (\* |> batch_norm *\) *)
    |> Fnn.downcast

  let code f =
    Printf.sprintf
      ( "|> conv2d ~o (`Full %d) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias\n|> (fun up -> [\n"
      ^^ "\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}up |> cropping2d [ 1; 2; 1; 2 ] |> \
          Fnn.downcast\n"
      ^^ "\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}; up |> relu |> conv2d ~o (`Full %d) (4, 4) ~s:(1, \
          1) ~b:`Assert_fit\n"
      ^^ "\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}|> bias |> Fnn.downcast\n"
      ^^ "\u{a0}\u{a0}\u{a0}])\n" ^^ "|> sum\n" ^^ "|> (fun up -> [\n"
      ^^ "\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}up |> cropping2d [ 2; 1; 2; 1 ] |> \
          Fnn.downcast\n"
      ^^ "\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}; up |> relu |> conv2d ~o (`Full %d) (4, 4) ~s:(1, \
          1) ~b:`Assert_fit\n"
      ^^ "\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}\u{a0}|> bias |> Fnn.downcast\n"
      ^^ "\u{a0}\u{a0}\u{a0}])\n" ^^ "|> sum\n" )
      f f f

  let description = "One 4x4(s3) and two residual 4x4(s1)"
end

(* Decoders ************************************************************************************* *)
type decoder_tag = [ `Maxpool_fc | `Fc ] [@@deriving enum]

module type DECODER = sig
  val tag : decoder_tag

  val create : (module Fnn.BUILDER) -> int -> int -> Fnn.network

  val code : int -> int -> string

  val description : string
end

module Maxpool_fc : DECODER = struct
  let tag = `Maxpool_fc

  let create (module Builder : Fnn.BUILDER) w f : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K f) ~s0:(K w) ~s1:(K w)) `Float32
    |> maxpool2d ~b:`Assert_fit (w, w)
    |> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ]
    |> dense ~o:(`Adam (0.9, 0.999, 1e-4)) [ (`C, 10) ] ~id:(Some "classif")
    (* |> dense ~o:`Sgd [ (`C, 10) ] ~id:(Some "classif") *)
    |> bias
    |> softmax `C
    |> Fnn.downcast

  let code w _ =
    Printf.sprintf
      {||> maxpool2d ~b:`Assert_fit (%d, %d)
|> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ]
|> dense ~o:`Sgd [ (`C, 10) ] |> bias |> softmax `C
|}
      w w

  let description = "Max-pooling and fully-connected"
end

module Fc : DECODER = struct
  let tag = `Fc

  let create (module Builder : Fnn.BUILDER) w f : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K f) ~s0:(K w) ~s1:(K w)) `Float32
    |> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ]
    |> dense ~o:(`Adam (0.9, 0.999, 1e-4)) [ (`C, 10) ] ~id:(Some "classif")
    (* |> dense ~o:`Sgd [ (`C, 10) ] ~id:(Some "classif") *)
    |> bias
    |> softmax `C
    |> Fnn.downcast

  let code _ _ =
    {||> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ]
|> dense ~o:`Sgd [ (`C, 10) ] |> bias |> softmax `C
|}

  let description = "Fully-connected"
end

(* Conf ***************************************************************************************** *)
type raw_conf = {
  encoder_tag : encoder_tag;
  parameters : Int64.t;
  decoder_tag : decoder_tag;
  seed : Int64.t;
}

type derived_conf = {
  encoder_tag : encoder_tag;
  decoder_tag : decoder_tag;
  parameters : int;
  filters : int;
  seed : int;
  encoder_nn : Fnn.network;
  decoder_nn : Fnn.network;
}

module type ENTRY = sig
  type t

  val default : t

  val of_dconf : derived_conf -> t

  val update_rconf : t -> raw_conf -> raw_conf

  val name : string

  val description : string
end

module type ENUM = sig
  include ENTRY

  val values : t list

  val to_string : t -> string

  val of_string : string -> t

  val to_name : t -> string
end

module type INT = sig
  include ENTRY with type t = Int64.t
end

module Seed = struct
  type t = Int64.t

  let default = Int64.of_int 42

  let of_dconf : derived_conf -> t = fun c -> Int64.of_int c.seed

  let update_rconf : t -> raw_conf -> raw_conf = fun seed rconf -> { rconf with seed }

  let name = "Seed"

  let description =
    "RNG seed to initialize the network's weights and sample the digits during training."
end

module Parameters = struct
  type t = Int64.t

  let default = Int64.of_int 10000

  let of_dconf : derived_conf -> t = fun c -> Int64.of_int c.parameters

  let update_rconf : t -> raw_conf -> raw_conf = fun parameters rconf -> { rconf with parameters }

  let name = "Parameters"

  let description = "Network's trainable parameter count"
end

module Encoder = struct
  type t = encoder_tag

  let values = List.init (max_encoder_tag + 1) encoder_tag_of_enum |> List.map Option.get

  let to_string v = encoder_tag_to_enum v |> string_of_int

  let of_string v =
    match int_of_string v |> encoder_tag_of_enum with Some v -> v | None -> failwith "unreachable"

  let to_name = function
    | `Zeroconv -> "No encoder, only decoder"
    | `Oneconv -> "A single 16x16(s6) convolution"
    | `Twoconv -> "One 4x4(s3) and one 3x3(s3)"
    | `Threeconv -> "One 4x4(s3) and two 4x4(s1)"
    | `Threeconvres -> "One 4x4(s3) and two residual 4x4(s1)"

  let default = `Oneconv

  let of_dconf : derived_conf -> t = fun c -> c.encoder_tag

  let update_rconf : t -> raw_conf -> raw_conf = fun encoder_tag rconf -> { rconf with encoder_tag }

  let name = "Encoder"

  let description = "First part of the network"
end

module Decoder = struct
  type t = decoder_tag

  let modules : (module DECODER) list = [ (module Maxpool_fc); (module Fc) ]

  let values = List.init (max_decoder_tag + 1) decoder_tag_of_enum |> List.map Option.get

  let to_string v = decoder_tag_to_enum v |> string_of_int

  let of_string v =
    match int_of_string v |> decoder_tag_of_enum with Some v -> v | None -> failwith "unreachable"

  let to_name = function
    | `Maxpool_fc -> "Max-pooling and fully-connected"
    | `Fc -> "Fully-connected"

  let default = `Fc

  let of_dconf : derived_conf -> t = fun c -> c.decoder_tag

  let update_rconf : t -> raw_conf -> raw_conf = fun decoder_tag rconf -> { rconf with decoder_tag }

  let name = "Decoder"

  let description = "Second part of the network"
end

(* ********************************************************************************************** *)
let tooltip_chunks =
  [|
    {|(* OCaml code to create the Fnn.network object *)
let rng = Random.State.make [| |};
    {| |] in
let open (val Fnn.create_builder ~rng ()) in
let open Pshape.Size in
let o = `Adam (0.9, 0.999, 1e-4) in

input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
(* encoder *)
|};
    {|(* decoder *)
|};
  |]

let sub_char_to_tag s char tag =
  let open Reactjs.Jsx in
  let rec aux = function
    | [] -> []
    | [ hd ] -> [ of_string hd ]
    | hd :: tl -> of_string hd :: of_tag tag [] :: aux tl
  in
  String.split_on_char char s |> aux

let tooltip_text_of_dconf dconf =
  let module D =
  ( val match dconf.decoder_tag with
        | `Maxpool_fc -> (module Maxpool_fc : DECODER)
        | `Fc -> (module Fc) )
  in
  let module E =
  ( val match dconf.encoder_tag with
        | `Zeroconv -> (module Noop : ENCODER)
        | `Oneconv -> (module Oneconv)
        | `Twoconv -> (module Twoconv)
        | `Threeconv -> (module Threeconv)
        | `Threeconvres -> (module Threeconvres) )
  in
  let (Pshape.Size.K w) = Pshape.get dconf.encoder_nn#out_shape `S0 |> Pshape.Size.to_known in
  let ecode = E.code dconf.filters in
  let dcode = D.code w dconf.filters in
  tooltip_chunks.(0) ^ string_of_int dconf.seed ^ tooltip_chunks.(1) ^ ecode ^ tooltip_chunks.(2)
  ^ dcode

let dconf_of_rconf : raw_conf -> derived_conf =
 fun conf ->
  let module D =
  ( val match conf.decoder_tag with
        | `Maxpool_fc -> (module Maxpool_fc : DECODER)
        | `Fc -> (module Fc) )
  in
  let module E =
  ( val match conf.encoder_tag with
        | `Zeroconv -> (module Noop : ENCODER)
        | `Oneconv -> (module Oneconv)
        | `Twoconv -> (module Twoconv)
        | `Threeconv -> (module Threeconv)
        | `Threeconvres -> (module Threeconvres) )
  in
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
    let o = `Adam (0.9, 0.999, 1e-4) in
    let rng = Random.State.make [| seed |] in
    let make_builder () = Fnn.create_builder ~rng () in
    let builder = make_builder () in
    let e = E.create builder o f in
    let (Pshape.Size.K w) = Pshape.get e#out_shape `S0 |> Pshape.Size.to_known in
    let (Pshape.Size.K f') = Pshape.get e#out_shape `C |> Pshape.Size.to_known in
    let d = D.create builder w f' in
    (e, d)
  in

  let minf = 1 in
  let maxp = 1000000 in

  let emin, dmin = networks_of_filters minf in
  let pmin = Fnn.parameters [ emin; dmin ] |> List.fold_left (fun acc nn -> acc + nn#numel) 0 in

  let rec find_closest_network ((ileft, vleft, _) as left) right =
    match right with
    | Some ((iright, vright, _) as right) when ileft + 1 = iright ->
        (* Stop *)
        assert (ileft < iright);
        assert (vleft <= vright);
        if parameters - vleft <= vright - parameters then left else right
    | Some ((iright, vright, _) as right) ->
        (* Dichotomy *)
        assert (ileft < iright);
        assert (vleft <= vright);
        let imid = (ileft + iright) / 2 in
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
  }

(* React components ***************************************************************************** *)
let construct_uncontrolled_int_input :
    ((module INT) * ((raw_conf -> raw_conf) -> unit) * derived_conf React.signal * bool)
    Reactjs.constructor =
 fun ((module M), update_rconf, dconf_signal, _) ->
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
    let v = M.of_dconf (React.S.value dconf_signal) in
    of_bootstrap "Form.Group"
      [
        of_bootstrap "Form.Label" [ Printf.sprintf "%s (%Ld)" M.name v |> of_string ];
        of_bootstrap "Form.Control" ~placeholder:(Int64.to_string M.default) ~disabled:(not enabled)
          ~type_:"number" ~on_change [];
        of_bootstrap "Form.Text" ~class_:[ "text-muted" ] [ of_string M.description ];
      ]
  in
  Reactjs.construct ~signal:dconf_signal render

let construct_uncontrolled_select :
    ((module ENUM) * ((raw_conf -> raw_conf) -> unit) * bool) Reactjs.constructor =
 fun ((module M), update_rconf, _) ->
  let on_change ev =
    let v = ev##.target##.value |> Js.to_string |> M.of_string in
    update_rconf (M.update_rconf v)
  in
  let render (_, _, enabled) =
    let open Reactjs.Jsx in
    let control =
      M.values
      |> List.map (fun v -> of_tag "option" ~value:(M.to_string v) [ of_string (M.to_name v) ])
      |> of_bootstrap "Form.Control" ~as_:"select" ~disabled:(not enabled) ~on_change
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
  ignore fire_upstream_event;

  let rconf0 =
    {
      encoder_tag = Encoder.default;
      parameters = Parameters.default;
      decoder_tag = Decoder.default;
      seed = Seed.default;
    }
  in
  let rconf_signal, update_rconf = React.S.create rconf0 in
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
      let ( |> ) v f = f [ v ] in
      of_tag "div"
        ~class_:[ "text-monospace"; "text-left"; "small" ]
        (sub_char_to_tag (tooltip_text_of_dconf dconf) '\n' "br")
      |> of_bootstrap "Tooltip" ~id:"tooltip-right"
    in
    let button =
      (* https://stackoverflow.com/a/61659811 *)
      let ( |> ) v f = f [ v ] in
      of_string "Create"
      |> of_bootstrap ~disabled:(not enabled) ~on_click "Button" ~type_:"submit"
      |> of_tag "span"
      |> of_bootstrap "OverlayTrigger" ~placement:"right" ~overlay:tt
    in
    let groups =
      [
        of_constructor construct_uncontrolled_select ((module Encoder), update_rconf, enabled);
        of_constructor construct_uncontrolled_select ((module Decoder), update_rconf, enabled);
        of_constructor construct_uncontrolled_int_input
          ((module Parameters), update_rconf, dconf_signal, enabled);
        of_constructor construct_uncontrolled_int_input
          ((module Seed), update_rconf, dconf_signal, enabled);
        button;
      ]
      |> List.map (fun v -> of_bootstrap "Col" ~sm:6 [ v ])
    in
    let tbody =
      let ( |> ) v f = f [ v ] in
      of_bootstrap "Row" groups |> of_bootstrap "Form" |> of_tag "th" |> of_tag "tr"
      |> of_tag "tbody"
    in
    let thead =
      let ( |> ) v f = f [ v ] in
      of_string "Network Creation" |> of_tag "th" |> of_tag "tr" |> of_tag "thead"
    in
    of_bootstrap "Table" ~class_:[ "mnist-panel" ] ~bordered:true ~size:"sm" [ thead; tbody ]
  in

  Reactjs.construct ~signal:dconf_signal render
