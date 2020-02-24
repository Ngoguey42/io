(* Create a signature with type constraints to make Owl_pretty work with Graph *)
module type GRAPH =
  Owl_neural_graph_sig.Sig
    with type Neuron.Optimise.Algodiff.A.arr := Owl_base_dense_ndarray.D.arr
    with type Neuron.Optimise.Algodiff.A.elt := Owl_base_dense_ndarray.D.elt

module Make_neural (Graph : GRAPH) = struct
  module Algodiff = Graph.Neuron.Optimise.Algodiff

  module Str = struct
    let shape arr =
      Algodiff.Arr.shape arr |> Array.map string_of_int |> Array.to_list |> Ft.List.string_join ", "
      |> Printf.sprintf "(%s)"

    let shape' arr =
      Owl_base_dense_ndarray.D.shape arr |> Array.map string_of_int |> Array.to_list |> Ft.List.string_join ", "
      |> Printf.sprintf "(%s)"

    let array arr =
      Algodiff.unpack_arr arr
      |> Owl_pretty.dsnda_to_string ~header:false ~max_row:1000 ~max_col:1000
  end

  let to_flat_list arr =
    (* val iter : (elt -> unit) -> arr -> unit *)
    let l = ref [] in
    Algodiff.unpack_arr arr |> Owl_base_dense_ndarray.D.iter (fun x -> l := x :: !l);
    List.rev !l

  module Network_builder = struct
    (* Base functions:
       https://ocaml.xyz/owl/owl-base/Owl_neural_graph/Make/index.html
    *)

    type 'a upstream = { filters : int; node : 'a; _ph : int }

    let conv2d kernel_size padding stride out_filters upstream =
      let padding = match padding with true -> Owl_types.SAME | false -> Owl_types.VALID in
      let kx, ky = match kernel_size with `One kx -> (kx, kx) | `Two (kx, ky) -> (kx, ky) in
      let sx, sy = match stride with `One sx -> (sx, sx) | `Two (sx, sy) -> (sx, sy) in
      let out_node =
        Graph.conv2d ~padding ~act_typ:Graph.Neuron.Activation.None
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
        Graph.max_pool2d ~padding:Owl_types.VALID ~act_typ:Graph.Neuron.Activation.None [| kx; ky |]
          [| sx; sy |] upstream.node
      in
      { upstream with node }

    let input2d x y f = { node = Graph.input [| x; y; f |]; filters = f; _ph = 42 }

    let get_network { node; _ } = Graph.get_network node
  end
end
