open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
end

(* Encoders ************************************************************************************* *)
type encoder = [ `ZeroConv | `OneConv | `TwoConv | `ThreeConv | `ThreeConvRes ]
[@@deriving yojson, enum]

module type ENCODER = sig
  val create : (module Fnn.BUILDER) -> Fnn.optimizer -> int -> Fnn.network

  val param_count : int -> int

  val code : int -> string

  val description : string
end

module Noop : ENCODER = struct
  let create (module Builder : Fnn.BUILDER) _ _ : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32 |> Fnn.downcast

  let param_count _ = 0

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

  let param_count f =
    let total = 0 in
    let k, f0, f1 = (16, 1, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    total

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

  let param_count f =
    let total = 0 in
    let k, f0, f1 = (4, 1, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    let k, f0, f1 = (3, f, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    total

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

  let param_count f =
    let total = 0 in
    let k, f0, f1 = (4, 1, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    let k, f0, f1 = (4, f, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    let k, f0, f1 = (4, f, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    total

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
    |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit
    |> bias
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Fnn.downcast;
           up |> relu |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast;
         ])
    |> sum
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Fnn.downcast;
           up |> relu |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast;
         ])
    |> sum |> Fnn.downcast

  let param_count f =
    let total = 0 in
    let k, f0, f1 = (4, 1, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    let k, f0, f1 = (4, f, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    let k, f0, f1 = (4, f, f) in
    let total = total + (k * k * f0 * f1) + f1 in
    total

  let code f =
    Printf.sprintf
      {||> conv2d ~o (`Full %d) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias
|> (fun up -> [
        up |> cropping2d [1] |> Fnn.downcast
      ; up |> relu |> conv2d ~o (`Full %d) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast
   ])
|> sum
|> (fun up -> [
        up |> cropping2d [1] |> Fnn.downcast
      ; up |> relu |> conv2d ~o (`Full %d) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> Fnn.downcast
   ])
|> sum
|}
      f f f

  let description = "One 4x4(s3) and two residual 4x4(s1)"
end

(* Decoders ************************************************************************************* *)
type decoder_tag = [ `Maxpool_fc | `Fc ] [@@deriving enum]

module type DECODER = sig
  val tag : decoder_tag

  val create : (module Fnn.BUILDER) -> int -> int -> Fnn.network

  val param_count : int -> int -> int

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
    |> dense ~o:`Sgd [ (`C, 10) ] ~id:(Some "classif")
    |> bias
    |> softmax `C
    |> Fnn.downcast

  let param_count _ f = 1 * 1 * f * 10

  let code _ _ = ""

  let description = "Max-pooling and fully-connected"
end

module Fc : DECODER = struct
  let tag = `Fc

  let create (module Builder : Fnn.BUILDER) w f : Fnn.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K f) ~s0:(K w) ~s1:(K w)) `Float32
    |> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ]
    |> dense ~o:`Sgd [ (`C, 10) ] ~id:(Some "classif")
    |> bias
    |> softmax `C
    |> Fnn.downcast

  let param_count w f = w * w * f * 10

  let code _ _ = ""

  let description = "Fully-connected"
end

(* Conf ***************************************************************************************** *)
type conf = { encoder : encoder; encoding_parameters : int; decoder : decoder_tag; seed : int }

module type ENTRY = sig
  type t

  val default : t

  val of_conf : conf -> t

  val update_conf : t -> conf -> conf

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
  include ENTRY with type t = int

  val min : int
end

module Seed = struct
  type t = int

  let min = min_int

  let default = 42

  let of_conf c = c.seed

  let update_conf seed conf = { conf with seed }

  let name = "Seed"

  let description =
    "RNG seed to initialize the network's weights and sample the digits during training."
end

module Encoding_parameters = struct
  type t = int

  let min = 1

  let default = 2000

  let of_conf c = c.encoding_parameters

  let update_conf encoding_parameters conf = { conf with encoding_parameters }

  let name = "Decoder Channels"

  let description = "Trainable parameter count in encoder"
end

module Encoder = struct
  type t = encoder

  let values = List.init (max_encoder + 1) encoder_of_enum |> List.map Option.get

  let to_string v = encoder_to_yojson v |> Yojson.Safe.to_string

  let of_string v =
    match Yojson.Safe.from_string v |> encoder_of_yojson with
    | Ok v -> v
    | Error _ -> failwith "unreachable"

  let to_name = function
    | `ZeroConv -> "No encoder, only decoder"
    | `OneConv -> "A single 16x16(s6) convolution"
    | `TwoConv -> "One 4x4(s3) and one 3x3(s3)"
    | `ThreeConv -> "One 4x4(s3) and two 4x4(s1)"
    | `ThreeConvRes -> "One 4x4(s3) and two residual 4x4(s1)"

  let default = `OneConv

  let of_conf c = c.encoder

  let update_conf encoder conf = { conf with encoder }

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

  let default = `Maxpool_fc

  let of_conf c = c.decoder

  let update_conf decoder conf = { conf with decoder }

  let name = "Decoder"

  let description = "Second part of the network"
end

(* ********************************************************************************************** *)
let tooltip_chunks =
  [
    {|(* OCaml code to create the Fnn.network object *)
let rng = Random.State.make [| |};
    {| |] in
let open (val Fnn.create_builder ~rng ()) in
let open Pshape.Size in

let o = `Adam (0.9, 0.999, 1e-4) in
input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32

(* encoder *)
|};
    {|
(* decoder *)
|};
  ]

(* React components ***************************************************************************** *)
let construct_int_input : ((module INT) * ((conf -> conf) -> unit)) Reactjs.constructor =
 fun ((module M), update_conf) ->
  let signal, set_val = React.S.create None in
  let on_change ev =
    let txt = ev##.target##.value |> Js.to_string in
    let is_valid_char = function '0' .. '9' -> true | '-' -> true | _ -> false in
    if String.length txt = 0 then (
      set_val None;
      update_conf (M.update_conf M.default) )
    else if txt |> String.to_seq |> List.of_seq |> List.for_all is_valid_char then
      let v = int_of_string txt in
      if v >= M.min then (
        set_val (Some v);
        update_conf (M.update_conf v) )
  in
  let render _ =
    let open Reactjs.Jsx in
    let control =
      match React.S.value signal with
      | None ->
          of_bootstrap "Form.Control" ~placeholder:(string_of_int M.default) ~value:""
            ~type_:"number" ~on_change []
      | Some value ->
          of_bootstrap "Form.Control" ~value:(string_of_int value) ~type_:"number" ~on_change []
    in
    of_bootstrap "Form.Group"
      [
        of_bootstrap "Form.Label" [ of_string M.name ];
        control;
        of_bootstrap "Form.Text" ~class_:[ "text-muted" ] [ of_string M.description ];
      ]
  in
  Reactjs.construct ~signal render

let construct_select : ((module ENUM) * ((conf -> conf) -> unit)) Reactjs.constructor =
 fun ((module M), update_conf) ->
  let on_change ev =
    let v = ev##.target##.value |> Js.to_string |> M.of_string in
    update_conf (M.update_conf v)
  in
  let render _ =
    let open Reactjs.Jsx in
    let control =
      M.values
      |> List.map (fun v -> of_tag "option" ~value:(M.to_string v) [ of_string (M.to_name v) ])
      |> of_bootstrap "Form.Control" ~as_:"select" ~on_change ~default_value:(M.to_string M.default)
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
 fun fire_upstream_event ->
  ignore fire_upstream_event;

  let config_signal, update_conf =
    React.S.create
      {
        encoder = Encoder.default;
        encoding_parameters = Encoding_parameters.default;
        decoder = Decoder.default;
        seed = Seed.default;
      }
  in
  let update_conf updater =
    let conf = React.S.value config_signal in
    let conf = updater conf in
    update_conf conf
  in

  let render _ =
    let open Reactjs.Jsx in
    let groups =
      [
        of_constructor construct_select ((module Encoder), update_conf);
        of_constructor construct_select ((module Decoder), update_conf);
        of_constructor construct_int_input ((module Encoding_parameters), update_conf);
        of_constructor construct_int_input ((module Seed), update_conf);
        of_bootstrap "Button" ~type_:"submit" [ of_string "Create" ];
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

  Reactjs.construct render
