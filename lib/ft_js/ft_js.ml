(* Tools based on owl-base to be able to run in the browser *)

(* Create a signature with type constraints to make Owl_pretty work with Graph *)
module type GRAPH = Owl_neural_graph_sig.Sig
  with type Neuron.Optimise.Algodiff.A.arr := Owl_base_dense_ndarray.D.arr
  with type Neuron.Optimise.Algodiff.A.elt := Owl_base_dense_ndarray.D.elt

module Make_neural (Graph : GRAPH) = struct
  module Algodiff = Graph.Neuron.Optimise.Algodiff

  module Str = struct
    let shape arr =
      Algodiff.Arr.shape arr
      |> Array.map string_of_int
      |> Array.to_list
      |> Ft.List.string_join ", "
      |> Printf.sprintf "(%s)"

    let array arr =
      Algodiff.unpack_arr arr
      |> Owl_pretty.dsnda_to_string
           ~header:false
           ~max_row:1000
           ~max_col:1000
  end
end
