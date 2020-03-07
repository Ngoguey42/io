(* Neural network definition
 * Supported features:
 * [X] layer11: Layer that map 1 input to 1 output
 * [ ] layern1: Layer that map n input to 1 output, with n > 0 determined at network creation
 * [ ] one layer used several time on several nodes

 * A network is a variant over node types
 * A node is an object (In order to use `Stdlib.Oo.id` for traversal)
 * A layer is a polymorphic variant (optionally containing a record)

 *)

module Firebug = Js_of_ocaml.Firebug
module Ndarray = Owl_base_dense_ndarray_generic

type float32_ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type int32_ba = (int, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type adam_content = {
  beta1 : float;
  beta2 : float;
  epsilon : float;
  (* Number of updates performed *)
  step : int;
  (* Running gradient *)
  rgrad : float32_ba;
  (* Running gradient squared *)
  rgrad_sq : float32_ba;
}

type optimizer = [ `Adam of adam_content | `Sgd ]

type conv_content = {
  kernel_size : int * int;
  stride : int * int;
  padding : bool;
  out_filters : int;
  kernel_weights : float32_ba;
  bias_weights : float32_ba;
  kernel_optimizer : optimizer;
  bias_optimizer : optimizer;
}

type maxpool2d_content = { kernel_size : int * int; stride : int * int }

type softmax_content = { axis : int }

type concat_content = { axis : int }

type add_content = { axis : int }

(* `type layer01` could be:
 * a trainable tensor
 * a non-trainable tensor
 * a noise generator
 * ? a placeholder for an input ?
*)

type layer11 =
  [ `Conv2d of conv_content | `Maxpool2d of maxpool2d_content | `Relu | `Softmax of softmax_content ]

type layern1 = [ `Concatenate of concat_content | `Add of add_content ]

type layer = [
  | layer11
  (* | layern1 *)
  ]

class type node01 = object
  method out_filters : int
  method dtype : [ `Float32 ]
end

class type ['network] node11 = object
  method upstream : 'network
  method layer : layer11
end

type t = Node01 of node01
       | Node11 of t node11

let node01 out_filters dtype =
  Node01 (object
      method out_filters = out_filters
      method dtype = dtype
    end)

let node11 upstream layer =
  Node11 (object
      method upstream = upstream
      method layer = layer
    end)

let rec fold_bottom_up f x net =
  match net with
  | Node11 content -> fold_bottom_up f (f x net) content#upstream
  | Node01 _ -> f x net

let rec fold_top_down f x net =
  match net with
  | Node11 content -> f (fold_top_down f x content#upstream) net
  | Node01 _ -> f x net

let create_optimizer optimizer shape =
  match optimizer with
  | `Sgd -> `Sgd
  | `Adam (beta1, beta2, epsilon) ->
      let rgrad = Ndarray.zeros Bigarray.Float32 shape in
      let rgrad_sq = Ndarray.zeros Bigarray.Float32 shape in
      `Adam { beta1; beta2; epsilon; step = 0; rgrad; rgrad_sq }

module Builder = struct
  type upstream = { filters : int; network : t }

  let input2d ?(dtype = `Float32) f = { filters = f; network = node01 f dtype }

  let conv2d kernel_size padding stride out_filters initialization optimizer up =
    let ((ky, kx) as kernel_size) =
      match kernel_size with `One k -> (k, k) | `Two (ky, kx) -> (ky, kx)
    in
    let kshape = [| ky; kx; up.filters; out_filters |] in
    let bshape = [| out_filters |] in
    let stride = match stride with `One s -> (s, s) | `Two (sy, sx) -> (sy, sx) in
    let kernel_weights = Ft_owlbase.Init.run initialization kshape in
    (* TODO: Init bias with zeros *)
    let bias_weights = Ft_owlbase.Init.run initialization bshape in
    let kernel_optimizer = create_optimizer optimizer kshape in
    let bias_optimizer = create_optimizer optimizer bshape in
    let layer =
      `Conv2d
        {
          kernel_size;
          stride;
          padding;
          out_filters;
          kernel_weights;
          bias_weights;
          kernel_optimizer;
          bias_optimizer;
        }
    in
    { filters = out_filters; network = node11 up.network layer }

  let relu up = { up with network = node11 up.network `Relu }

  let softmax ?(axis = 3) up = { up with network = node11 up.network (`Softmax { axis }) }

  let maxpool2d kernel_size stride up =
    let kernel_size = match kernel_size with `One k -> (k, k) | `Two (ky, kx) -> (ky, kx) in
    let stride = match stride with `One s -> (s, s) | `Two (sy, sx) -> (sy, sx) in
    { up with network = node11 up.network (`Maxpool2d { kernel_size; stride }) }

  (* let concatenate ?(axis = 3) up_list = *)
  (*   let count, filters = match List.length up_list, axis with *)
  (*     | 0, _ -> failwith "Can't concatenate nothing" *)
  (*     | count, 3 -> *)
  (*        let filters = List.fold_left (fun acc up -> acc + up.filters) 0 up_list in *)
  (*        count, filters *)
  (*     | count, _ -> *)
  (*        let filters = (List.hd up_list).filters in *)
  (*        List.iter (fun up -> assert (up.filters = filters)) up_list; *)
  (*        count, filters *)
  (*   in *)
  (*   let up_networks = List.map (fun up -> up.network) up_list in *)
  (*   { filters; network = Inner_n1 (up_networks, count, `Concatenate { axis }) } *)

  (* let add ?(axis = 3) up_list = *)
  (*   let count, filters = match List.length up_list, axis with *)
  (*     | 0, _ -> failwith "Can't add nothing" *)
  (*     | count, 3 -> *)
  (*        let filters = List.fold_left (fun acc up -> acc + up.filters) 0 up_list in *)
  (*        count, filters *)
  (*     | count, _ -> *)
  (*        let filters = (List.hd up_list).filters in *)
  (*        List.iter (fun up -> assert (up.filters = filters)) up_list; *)
  (*        count, filters *)
  (*   in *)
  (*   let up_networks = List.map (fun up -> up.network) up_list in *)
  (*   { filters; network = Inner_n1 (up_networks, count, `Add { axis }) } *)

  let finalize { network; _ } = network
end

module String = struct
  let of_dtype = function `Float32 -> "float32" | `Uint8 -> "uint8" | `Int32 -> "int32"

  let of_optimizer = function
    | `Sgd -> "Sgd"
    | `Adam c ->
        let mean_val arr beta =
          if c.step == 0 then 0.
          else
            let correction = 1. -. (beta ** float_of_int c.step) in
            let size = float_of_int (Ndarray.numel arr) in
            Ndarray.sum' arr /. size /. correction
        in
        Printf.sprintf "Adam@%d %+.3e/%+.3e" c.step (mean_val c.rgrad c.beta1)
          (mean_val c.rgrad_sq c.beta2)

  let of_layer : layer -> string = function
    | `Conv2d c ->
        let mean_val arr = Ndarray.sum' arr /. float_of_int (Ndarray.numel arr) in
        let ky, kx = c.kernel_size in
        let sy, sx = c.stride in
        Printf.sprintf "Conv2d k:%dx%d s:%dx%d <%d> k:%+.3e b:%+.3e (%s)/(%s)\n" ky kx sy sx
          c.out_filters (mean_val c.kernel_weights) (mean_val c.bias_weights)
          (of_optimizer c.kernel_optimizer) (of_optimizer c.bias_optimizer)
    | `Maxpool2d _ -> "Maxpool2d\n"
    | `Relu -> "Relu\n"
    | `Softmax _ -> "Softmax\n"
    (* | `Concatenate _ -> "Concatenate\n" *)
    (* | `Add _ -> "Add\n" *)

  let rec of_network = function
    | Node01 content ->
        Printf.sprintf "| Node01 <%d> %s\n" content#out_filters (of_dtype content#dtype)
    | Node11 content ->
       Printf.sprintf "%s> %s" (of_network content#upstream) (of_layer content#layer)
    (* | Inner_n1 (upstreams, count, layer) -> *)
    (*    Printf.sprintf "%s> %s (%d hidden branches)" (of_network (List.hd upstream)) *)
    (*                   (of_layer layer) (count - 1) *)

end
