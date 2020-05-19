(* ********************************************************************************************** *)
(* Features:
    - Most classic way of tackling the problem, 3x3 stride:2 convolutions with batch_norm

   Successive widths: 28, 14, 7, 4
 *)
let[@ocamlformat "disable"] encoder_padding_batchnorm (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 10) (3, 3) ~s:(2, 2) ~b:`Same (* pooling *) |> bias |> batch_norm |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~s:(2, 2) ~b:`Same (* pooling *) |> bias |> batch_norm |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~s:(2, 2) ~b:`Same (* pooling *) |> bias |> batch_norm |> relu
  |> Fnn.downcast

(* Features:
    - No padding
    - For stride 2 convs:
    - 4x4 kernel if input has even spatial size
    - 3x3 kernel is input has odd spatial size

   Successive widths: 28, 13, 6, 4
 *)
let[@ocamlformat "disable"] encoder_pooling (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 12) (4, 4) ~s:(2, 2) ~b:`Assert_fit (* pooling *) |> bias |> relu
  |> conv2d ~o (`Full 25) (3, 3) ~s:(2, 2) ~b:`Assert_fit (* pooling *) |> bias |> relu
  |> conv2d ~o (`Full 50) (3, 3) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
  |> Fnn.downcast

(* Residual network inspired by mobilenet v2:
    - Inverted bottleneck: the residual branch is a projection instead of being an expansion
    - No `relu` on the residual branch
    - No `relu` between `sum` and `conv` to avoid washing away the projected features
    - Expansion and projections with 1x1 kernels, transformations with depthwise kernels
    - The very first conv is an expansion
    But with those diffs:
    - Use bias
    - No padding (requires explicit cropping on the residual branch)
    - Stride 2 poolings with 4x4 and 2x2 kernels (less checkerboard effects than odd kernels)

   Successive widths: 28, 14, 6, 4
   *)
let[@ocamlformat "disable"] encoder_mobilenet (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 100) (2, 2) ~s:(2, 2) ~b:`Assert_fit |> bias (* expansion (pooling) *)
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> conv2d ~o (`Full 50) (2, 2) ~s:(2, 2) ~b:`Assert_fit |> bias (* projection (pooling) *)
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Depthwise 1) (4, 4) ~s:(2, 2) ~b:`Assert_fit |> bias (* transformation (pooling) *)
          |> relu |> conv2d ~o (`Full 50) (1, 1) ~b:`Assert_fit |> bias (* projection *)
          |> Fnn.downcast
     ])
  |> sum
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Full 100) (1, 1) ~b:`Assert_fit |> bias (* expansion *)
          |> relu |> conv2d ~o (`Depthwise 1) (3, 3) ~b:`Assert_fit |> bias (* transformation *)
          |> relu |> conv2d ~o (`Full 50) (1, 1) ~b:`Assert_fit |> bias (* projection *)
          |> Fnn.downcast
     ])
  |> sum
  |> Fnn.downcast

let[@ocamlformat "disable"] encoder_mobilenet_bis (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 20) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias (* expansion (pooling) *)
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Full 200) (1, 1) ~b:`Assert_fit |> bias (* expansion *)
          |> relu |> conv2d ~o (`Depthwise 1) (3, 3) ~b:`Assert_fit |> bias (* transformation *)
          |> relu |> conv2d ~o (`Full 20) (1, 1) ~b:`Assert_fit |> bias (* projection *)
          |> Fnn.downcast
     ])
  |> sum
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Full 200) (1, 1) ~b:`Assert_fit |> bias (* expansion *)
          |> relu |> conv2d ~o (`Depthwise 1) (3, 3) ~b:`Assert_fit |> bias (* transformation *)
          |> relu |> conv2d ~o (`Full 20) (1, 1) ~b:`Assert_fit |> bias (* projection *)
          |> Fnn.downcast
     ])
  |> sum
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Full 200) (1, 1) ~b:`Assert_fit |> bias (* expansion *)
          |> relu |> conv2d ~o (`Depthwise 1) (3, 3) ~b:`Assert_fit |> bias (* transformation *)
          |> relu |> conv2d ~o (`Full 20) (1, 1) ~b:`Assert_fit |> bias (* projection *)
          |> Fnn.downcast
     ])
  |> sum

  |> Fnn.downcast

(* Features:
   - No stride, only 3x3 3-dilated convolutions

   A 3x3 3-dilated kernel applied on a 10x10 array outputs a 4x4 array:
       |0 1 2 3 4 5 6 7 8 9|
       [-     -     -]     |
       | [-     -     -]   |
       |   [-     -     -] |
       |     [-     -     -]

   Successive widths: 28, 22, 16, 10, 4
 *)
let[@ocamlformat "disable"] encoder_dilatedconvs (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:U ~s1:U) `Float32
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 10) (3, 3) ~d:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> Fnn.downcast

(* Features:
   - One 16x16 4-strided convolution

   A 16x16 4-strided kernel applied on a 28x28 array outputs a 4x4 array:
       |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7|
       [- - - - - - - - - - - - - - - -]                       |
       |       [- - - - - - - - - - - - - - - -]               |
       |               [- - - - - - - - - - - - - - - -]       |
       |                       [- - - - - - - - - - - - - - - -]

   Successive widths: 28, 4
 *)

let[@ocamlformat "disable"] encoder_noop (module Builder : Fnn.BUILDER) _ : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
  |> Fnn.downcast

let[@ocamlformat "disable"] encoder_oneconv (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
  |> conv2d ~o (`Full 50) (16, 16) ~s:(6, 6) ~b:`Assert_fit |> bias |> relu
  |> Fnn.downcast

let[@ocamlformat "disable"] encoder_twoconv (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
  |> conv2d ~o (`Full 50) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 50) (3, 3) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> Fnn.downcast

let[@ocamlformat "disable"] encoder_3conv (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
  |> conv2d ~o (`Full 50) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 50) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
  |> conv2d ~o (`Full 50) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias |> relu
  |> Fnn.downcast

let[@ocamlformat "disable"] encoder_3conv_res (module Builder : Fnn.BUILDER) o : Fnn.network =
  let open Builder in
  let open Pshape.Size in
  input (Pshape.sym4d_partial ~n:U ~c:(K 1) ~s0:(K 28) ~s1:(K 28)) `Float32
  |> conv2d ~o (`Full 50) (4, 4) ~s:(3, 3) ~b:`Assert_fit |> bias
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Full 50) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias
          |> Fnn.downcast
     ])
  |> sum
  |> (fun up -> [
          up
          |> cropping2d [1]
          |> Fnn.downcast
        ; up
          |> relu |> conv2d ~o (`Full 50) (4, 4) ~s:(1, 1) ~b:`Assert_fit |> bias
          |> Fnn.downcast
     ])
  |> sum
  |> Fnn.downcast

(* ********************************************************************************************** *)

let[@ocamlformat "disable"] create_nn rng =
  let builder = Fnn.create_builder ~rng () in

  (* let o = `Sgd in *)
  let o = `Adam (0.9, 0.999, 1e-4) in

  let module Builder = (val builder) in
  let open Builder in
  let open Pshape.Size in
  Printf.eprintf "Building encoder(s)...\n%!";
  let encoders =
    [
      (* encoder_padding_batchnorm builder o; *)
      (* encoder_pooling builder o; *)
      (* encoder_mobilenet_bis builder o; *)
      (* encoder_dilatedconvs (); *)
      (* encoder_noop builder o; *)
      (* encoder_oneconv builder o; *)
      encoder_twoconv builder o;
      (* encoder_3conv builder o; *)
    ]
  in
  Printf.eprintf "encoder: %s\n%!" (List.hd encoders)#to_string;

  let decoder =
    let c =
      List.map (fun net -> Pshape.get net#out_shape `C) encoders |> List.fold_left add (K 0)
    in
    let w = 3 in
    input (Pshape.sym4d_partial ~n:U ~c ~s0:(K w) ~s1:(K w)) `Float32

    (* |> conv2d ~o ~id:(Some "classif") (`Full 10) (w, w) ~b:`Assert_fit |> bias |> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ] *)

    (* |> conv2d ~o ~id:(Some "classif") (`Full 10) (1, 1) ~b:`Assert_fit |> bias |> maxpool2d ~b:`Assert_fit (w, w) |> transpose ~mapping:[`C, `C; `S0, `C; `S1, `C] *)

    (* |> conv2d ~o (`Depthwise 1) (3, 3) ~b:`Assert_fit |> bias *)

    (* Classify using a maxpool and a fully connected layer *)
    |> maxpool2d ~b:`Assert_fit (w, w) |> transpose ~mapping:[`C, `C; `S0, `C; `S1, `C] |> dense ~o:`Sgd [`C, 10] ~id:(Some "classif") |> bias

    (* Classify and flatten using flatten and fully-connected *)
    (* |> transpose ~mapping:[ (`C, `C); (`S0, `C); (`S1, `C) ] |> dense ~o:`Sgd [ (`C, 10) ] ~id:(Some "classif") |> bias *)

    |> softmax `C |> Fnn.downcast
  in
  (encoders, decoder)
