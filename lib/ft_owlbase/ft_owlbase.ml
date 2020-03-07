(* Create a signature with type constraints to make Owl_pretty work with Graph *)
module Ndarray = Owl_base_dense_ndarray_generic

module type GRAPH =
  Owl_neural_graph_sig.Sig
  with type Neuron.Optimise.Algodiff.A.arr := Owl_base_dense_ndarray.S.arr
  with type Neuron.Optimise.Algodiff.A.elt := Owl_base_dense_ndarray.S.elt

module Make_neural (Graph : GRAPH) = struct
  module Algodiff = Graph.Neuron.Optimise.Algodiff

  let to_flat_list arr =
    let l = ref [] in
    Algodiff.unpack_arr arr |> Owl_base_dense_ndarray.S.iter (fun x -> l := x :: !l);
    List.rev !l

  module Str = struct
    let shape arr =
      Algodiff.Arr.shape arr |> Array.map string_of_int |> Array.to_list |> Ft.List.string_join ", "
      |> Printf.sprintf "(%s)"

    let shape' arr =
      Algodiff.A.shape arr |> Array.map string_of_int |> Array.to_list |> Ft.List.string_join ", "
      |> Printf.sprintf "(%s)"

    let array arr =
      to_flat_list arr
      |> List.map string_of_float |> Ft.List.string_join ", "
      |> Printf.sprintf "[%s]"

                        (* Algodiff.unpack_arr arr *)
                        (* |> Owl_pretty.dsnda_to_string ~header:false ~max_row:1000 ~max_col:1000 *)
  end


  module Network_builder = struct
    (* TODO: Remove *)
    (* Base functions:
       https://ocaml.xyz/owl/owl-base/Owl_neural_graph/Make/index.html
     *)

    type 'a upstream = { filters : int; node : 'a; _ph : int }

    let conv2d kernel_size padding stride out_filters upstream =
      let padding = match padding with true -> Owl_types.SAME | false -> Owl_types.VALID in
      let kx, ky = match kernel_size with `One kx -> (kx, kx) | `Two (kx, ky) -> (kx, ky) in
      let sx, sy = match stride with `One sx -> (sx, sx) | `Two (sx, sy) -> (sx, sy) in
      let out_node =
        Graph.conv2d ~padding
                     [| kx; ky; upstream.filters; out_filters |]
                     [| sx; sy |] upstream.node
      in
      { upstream with node = out_node; filters = out_filters }

    let relu upstream =
      { upstream with node = Graph.activation Graph.Neuron.Activation.Relu upstream.node }

    let softmax2d upstream =
      { upstream with node = Graph.activation (Graph.Neuron.Activation.Softmax 3) upstream.node }

    let max_pool2d kernel_size stride upstream =
      let kx, ky = match kernel_size with `One kx -> (kx, kx) | `Two (kx, ky) -> (kx, ky) in
      let sx, sy = match stride with `One sx -> (sx, sx) | `Two (sx, sy) -> (sx, sy) in
      let node =
        Graph.max_pool2d ~padding:Owl_types.VALID [| kx; ky |]
                         [| sx; sy |] upstream.node
      in
      { upstream with node }

    let input2d x y f =
      { node = Graph.input [| x; y; f |]; filters = f; _ph = 42 }

    let get_network { node; _ } = Graph.get_network node
  end

  let float' x = Ndarray.create Bigarray.Float32 [||] x |> Algodiff.pack_arr

  let int' x = Ndarray.create Bigarray.Float32 [||] (float_of_int x) |> Algodiff.pack_arr

  let cross_entropy_of_softmaxed : int list -> 'a -> 'a =
    fun labs pred ->
    let shape = Algodiff.Arr.shape pred in
    let b, h, w, c = match shape with [| b; h; w; c |] -> (b, h, w, c) | _ -> assert false in
    assert (h == 1);
    assert (w == 1);
    assert (c == 10);

    let onehot lab =
      Ndarray.init Bigarray.Float32 [| 1; h; w; c |] (fun i -> if i == lab then 1. else 0.)
    in

    let truth =
      List.map onehot labs |> Array.of_list |> Ndarray.concatenate ~axis:0 |> Algodiff.pack_arr
    in

    Algodiff.Maths.(
      ( ( ( pred |> max2 (float' 1e-10) |> log |> neg |> ( * ) truth |> sum ~axis:3 |> fun x ->
                                                                                       assert (Array.length @@ Algodiff.Arr.shape x == 4);
                                                                                       x )
          |> fun x -> sum ~axis:0 x / int' b )
        |> fun x ->
           assert (Array.length @@ Algodiff.Arr.shape x == 4);
           x )
      |> fun x -> reshape x [| 1 |])

  module SGD = struct
    type t = unit

    let apply : unit -> float -> _ array array -> unit * _ array array =
      fun () lr grad ->
      let f grad =
        let open Algodiff.Maths in
        grad * float' lr |> neg
      in
      (), Owl_utils.aarr_map f grad
  end

  module Adam = struct
    type t = {
        beta1 : float;
        beta2 : float;
        step : int;
        rgrad : Algodiff.t array array; (* Running gradient *)
        rgrad_sq : Algodiff.t array array; (* Running gradient squared *)
      }

    let create ~beta1 ~beta2 nn =
      let aux node =
        match Graph.(node.neuron) with
        (* Initialize with scalars and let the first `apply` call broadcast to the right shape *)
        | Graph.Neuron.Conv2D _ -> [| float' 0.; float' 0. |] (* First for kernel, second for bias *)
        | _ -> [||]
      in

      let rgrad = Array.map aux Graph.(nn.topo) in
      let rgrad_sq = Array.map aux Graph.(nn.topo) in
      { beta1; beta2; step = 0; rgrad; rgrad_sq }

    let apply : t -> float -> _ array array -> t * _ array array =
      fun ({ beta1; beta2; step; rgrad; rgrad_sq } as r) lr grad ->
      let step = step + 1 in
      let correction1 = float' (1. -. (beta1 ** float_of_int step)) in
      let correction2 = float' (1. -. (beta2 ** float_of_int step)) in

      let f scale a b =
        let scale, scale' = (float' scale, float' (1. -. scale)) in
        Algodiff.Maths.((a * scale) + (b * scale'))
      in
      let rgrad = Owl_utils.aarr_map2 (f beta1) rgrad grad in
      let rgrad_sq =
        Owl_utils.aarr_map2 (f beta2) rgrad_sq (Owl_utils.aarr_map Algodiff.Maths.sqr grad)
      in

      let f rgrad rgrad_sq =
        let open Algodiff.Maths in
        let rgrad = rgrad / correction1 in
        let rgrad_sq = sqrt rgrad_sq / sqrt correction2 in
        rgrad / (rgrad_sq + float' 1e-10) * float' lr |> neg
      in
      let updates = Owl_utils.aarr_map2 f rgrad rgrad_sq in

      { r with step; rgrad; rgrad_sq }, updates
  end

end

module Init = struct
  (* Shamelessly copied from owl-base *)

  let calc_fans s =
    let _prod x = Array.fold_left (fun p q -> p * q) 1 x in
    let l = Array.length s in
    let fan_in, fan_out =
      (* for matrices *)
      if l = 2 then (float_of_int s.(0), float_of_int s.(1))
        (* for convolution kernels 1d, 2d, 3d *)
      else if l > 2 && l < 6 then
        let s' = Array.sub s 0 (l - 2) in
        let receptive = _prod s' in
        let i = s.(l - 2) * receptive |> float_of_int in
        let o = s.(l - 1) * receptive |> float_of_int in
        (i, o) (* for no specific assumptions *)
      else
        let i_o = _prod s |> float_of_int |> Stdlib.sqrt in
        (i_o, i_o)
    in
    (fan_in, fan_out)

  let run ?(kind = Bigarray.Float32) t s =
    let fan_in, fan_out = calc_fans s in
    let r0 = sqrt (1. /. fan_in) in
    let r1 = sqrt (6. /. (fan_in +. fan_out)) in
    let r2 = sqrt (2. /. (fan_in +. fan_out)) in
    let uniform, gaussian, float_to_elt = Ndarray.(uniform, gaussian, float_to_elt) in
    match t with
    | `Uniform (a, b) -> uniform kind ~a:(float_to_elt a) ~b:(float_to_elt b) s
    | `Gaussian (mu, sigma) -> gaussian kind ~mu:(float_to_elt mu) ~sigma:(float_to_elt sigma) s
    | `Standard -> uniform kind ~a:(float_to_elt (-.r0)) ~b:(float_to_elt r0) s
    | `Tanh -> uniform kind ~a:(float_to_elt (-.r1)) ~b:(float_to_elt r1) s
    | `GlorotUniform -> uniform kind ~a:(float_to_elt (-.r1)) ~b:(float_to_elt r1) s
    | `GlorotNormal -> gaussian kind ~sigma:(float_to_elt r2) s
    | `LecunNormal -> gaussian kind ~sigma:(float_to_elt r0) s
end
