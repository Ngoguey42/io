(* Encoders ************************************************************************************* *)

module type ENCODER = sig
  val create :
    (module Ocann.Default.BUILDER) -> Ocann.Default.optimizer_conf -> int -> Ocann.Default.network

  val code : int -> string

  val short_description : string
end

module Fc_encoder : ENCODER = struct
  let create (module Builder : Ocann.Default.BUILDER) o f : Ocann.Default.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> Ocann.Default.downcast
    |> conv2d ~o (`Full f) (28, 28) ~s:(1, 1) ~b:`Assert_fit
    |> Ocann.Default.downcast

  let code f =
    Printf.sprintf
      "|> conv2d ~o (`Full %d) (28, 28) ~s:(1, 1) ~b:`Assert_fit (* Equivalent to a dense layer *)\n"
      f

  let short_description = "A single fully-connected layer"
end

module Oneconv : ENCODER = struct
  let create (module Builder : Ocann.Default.BUILDER) o f : Ocann.Default.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> conv2d ~o (`Full f) (16, 16) ~s:(6, 6) ~b:`Assert_fit
    |> bias |> relu |> Ocann.Default.downcast

  let code f =
    Printf.sprintf "|> conv2d ~o (`Full %d) (16, 16) ~s:(6, 6) ~b:`Assert_fit |> bias |> relu\n" f

  let short_description = "A single 16x16(s6) convolution"
end

module Twoconv : ENCODER = struct
  let create (module Builder : Ocann.Default.BUILDER) o f : Ocann.Default.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit
    |> bias |> relu
    |> conv2d ~o (`Full f) (3, 3) ~s:(3, 3) ~b:`Assert_fit
    |> bias |> relu |> Ocann.Default.downcast

  let code f =
    Printf.sprintf
      "|> conv2d ~o (`Full %d) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu\n\
       |> conv2d ~o (`Full %d) (3, 3) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu\n"
      f f

  let short_description = "One 4x4(s3) and one 3x3(s3)"
end

module Threeconv = struct
  let create (module Builder : Ocann.Default.BUILDER) o f : Ocann.Default.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit
    |> bias |> relu
    |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit
    |> bias |> relu
    |> conv2d ~o (`Full f) (4, 4) ~s:(1, 1) ~b:`Assert_fit
    |> bias |> relu |> Ocann.Default.downcast

  let code f =
    Printf.sprintf
      {||> conv2d ~o (`Full %d) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu
|> conv2d ~o (`Full %d) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
|> conv2d ~o (`Full %d) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
|}
      f f f

  let short_description = "One 4x4(s3) and two 4x4(s1)"
end

module Fourconvres : ENCODER = struct
  let create (module Builder : Ocann.Default.BUILDER) o f : Ocann.Default.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
    |> conv2d ~o (`Full f) (4, 4) ~s:(3, 3) ~b:`Assert_fit
    |> bias
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Ocann.Default.downcast;
           up |> relu
           |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit
           |> bias |> Ocann.Default.downcast;
         ])
    |> sum
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Ocann.Default.downcast;
           up |> relu
           |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit
           |> bias |> Ocann.Default.downcast;
         ])
    |> sum
    |> (fun up ->
         [
           up |> cropping2d [ 1 ] |> Ocann.Default.downcast;
           up |> relu
           |> conv2d ~o (`Full f) (3, 3) ~s:(1, 1) ~b:`Assert_fit
           |> bias |> Ocann.Default.downcast;
         ])
    |> sum |> Ocann.Default.downcast

  let code f =
    Printf.sprintf
      {||> conv2d ~o (`Full %d) (4, 4) ~s:(3, 3) ~b:`Assert_fit
|> bias
|> (fun up ->
     [
       up |> cropping2d [ 1 ] |> Ocann.Default.downcast
     ; up |> relu |> conv2d ~o (`Full %d) (3, 3) ~s:(1, 1) ~b:`Assert_fit
       |> bias |> Ocann.Default.downcast
     ])
|> sum
|> (fun up ->
     [
       up |> cropping2d [ 1 ] |> Ocann.Default.downcast
     ; up |> relu |> conv2d ~o (`Full %d) (3, 3) ~s:(1, 1) ~b:`Assert_fit
       |> bias |> Ocann.Default.downcast
     ])
|> sum
|> (fun up ->
     [
       up |> cropping2d [ 1 ] |> Ocann.Default.downcast
     ; up |> relu |> conv2d ~o (`Full %d) (3, 3) ~s:(1, 1) ~b:`Assert_fit
       |> bias |> Ocann.Default.downcast
     ])
|> sum
|}
      f f f f

  let short_description = "One 4x4(s3) and three residual 3x3(s1)"
end

(* Decoders ************************************************************************************* *)
module type DECODER = sig
  val create :
    (module Ocann.Default.BUILDER) ->
    Ocann.Default.optimizer_conf ->
    int ->
    int ->
    Ocann.Default.network

  val code : int -> int -> string

  val short_description : string
end

module Maxpool_fc : DECODER = struct
  let create (module Builder : Ocann.Default.BUILDER) o w f : Ocann.Default.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K f) ~s0:(K w) ~s1:(K w)) `Float32
    |> maxpool2d ~b:`Assert_fit (w, w)
    |> transpose ~mapping:[ (`S1, `C); (`S0, `C); (`C, `C) ]
    (* |> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ] *)
    |> dense ~o [ (`C, 10) ] ~id:(Some "classif")
    |> bias
    |> softmax `C
    |> Ocann.Default.downcast

  let code w _ =
    Printf.sprintf
      {||> maxpool2d ~b:`Assert_fit (%d, %d)
|> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ]
|> dense ~o [ (`C, 10) ] |> bias |> softmax `C
|}
      w w

  let short_description = "Max-pooling and fully-connected"
end

module Fc_decoder : DECODER = struct
  let create (module Builder : Ocann.Default.BUILDER) o w f : Ocann.Default.network =
    let open Builder in
    let open Pshape.Size in
    input (Pshape.sym4d_partial ~n:U ~c:(K f) ~s0:(K w) ~s1:(K w)) `Float32
    |> transpose ~mapping:[ (`S1, `C); (`S0, `C); (`C, `C) ]
    |> dense ~o [ (`C, 10) ] ~id:(Some "classif")
    |> bias
    |> softmax `C
    |> Ocann.Default.downcast

  let code _ _ =
    {||> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ]
|> dense ~o [ (`C, 10) ] |> bias |> softmax `C
|}

  let short_description = "Fully-connected"
end
