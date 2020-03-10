(* Neural network definition
 * Supported features:
 * [X] Layers that map 1 input to 1 output
 * [.] Layers that map n input to 1 output (with n > 0 determined at network creation)
 * [X] Builder keeps track of the channel dimension and fails as soon as two layers are being
 *       combined without compatibility.
 * [ ] One convolution can be reused several times on several nodes
 * [ ] Builder keeps track of all the dimension sizes and fails as soon as two layers are being
 *        combined with an incompatible shape.
 *
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

type content0 = { axis : int }

(* `type layer01` could be:
 * a trainable tensor
 * a non-trainable tensor
 * a noise generator
 * ? a placeholder for an input ?
*)

type layer11 =
  [ `Conv2d of conv_content | `Maxpool2d of maxpool2d_content | `Relu | `Softmax of content0 ]

type layern1 = [ `Concatenate of content0 | `Add of content0 ]

type layer = [
  | layer11
  | layern1
  ]

class type node01 = object
  method out_filters : int
  method dtype : [ `Float32 ]
end

class type ['network] node11 = object
  method upstream : 'network
  method layer : layer11
end

class type ['network] noden1 = object
  method upstreams : 'network list
  method count : int
  method layer : layern1
end

type t = Node01 of node01
       | Node11 of t node11
       | Noden1 of t noden1

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

let noden1 upstreams layer =
  let count = List.length upstreams in
  if count == 0 then failwith "a node of type n->1 requires at least one input";
  Noden1 (object
      method upstreams = upstreams
      method count = count
      method layer = layer
    end)

(* let rec fold_bottom_up f x net = *)
(*   match net with *)
(*   | Node11 content -> fold_bottom_up f (f x net) content#upstream *)
(*   | Node01 _ -> f x net *)

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

  let of_layer : [< layer] -> string = function
    | `Conv2d (c : conv_content) ->
        let mean_val arr = Ndarray.sum' arr /. float_of_int (Ndarray.numel arr) in
        let ky, kx = c.kernel_size in
        let sy, sx = c.stride in
        Printf.sprintf "Conv2d k:%dx%d s:%dx%d <%d> k:%+.3e b:%+.3e (%s)/(%s)" ky kx sy sx
          c.out_filters (mean_val c.kernel_weights) (mean_val c.bias_weights)
          (of_optimizer c.kernel_optimizer) (of_optimizer c.bias_optimizer)
    | `Maxpool2d _ -> "Maxpool2d"
    | `Relu -> "Relu"
    | `Softmax _ -> "Softmax"
    | `Concatenate _ -> "Concatenate"
    | `Add _ -> "Add"

  let of_last_node = function
    | Node01 node ->
        Printf.sprintf "Input2d <%d> %s" node#out_filters (of_dtype node#dtype)
    | Node11 node -> of_layer node#layer
    | Noden1 node -> of_layer node#layer

  let rec of_network network =
    match network with
    | Node01 _ ->
        Printf.sprintf "| %s\n" (of_last_node network)
    | Node11 node ->
       Printf.sprintf "%s> %s\n" (of_network node#upstream) (of_last_node network)
    | Noden1 node ->
       Printf.sprintf "%s> %s (%d hidden branches)\n" (of_network (List.hd node#upstreams))
                      (of_layer node#layer) (node#count - 1)

end

let fold_top_down f net =
  let table = Hashtbl.create 100 in
  let f ~id acc_list net =
       Printf.printf "Computing node:%d (%s)\n%!" id (String.of_last_node net);
       let acc_list = f acc_list net in
       Hashtbl.add table id acc_list;
       acc_list
  (*   match Hashtbl.find_opt table id with *)
  (*   | None -> *)
  (*   | Some acc_list -> *)
  (*      Printf.printf "  Reusing node:%d (%s)\n%!" id (String.of_last_node net); *)
  (*      acc_list *)
  in
  let rec aux net =
    let id = match net with
      | Node01 node -> Oo.id node
      | Node11 node -> Oo.id node
      | Noden1 node -> Oo.id node
    in
    let acc_list = match net, Hashtbl.find_opt table id with
      | _, Some acc_list ->
         Printf.printf "  Reusing node:%d (%s)\n%!" id (String.of_last_node net);
         acc_list
      | Node01 _, None -> f ~id [] net
      | Node11 node, None -> f ~id [aux node#upstream] net
      | Noden1 node, None -> f ~id (List.map aux node#upstreams) net
    in
    acc_list
  in
  Printf.eprintf "Fold start...\n%!";
  let acc = aux net in
  Printf.eprintf "...Fold done\n%!";
  acc

(* let fold_top_down f net = *)
(*   let table = Hashtbl.create 100 in *)
(*   let f ~id acc_list net = *)
(*     match Hashtbl.find_opt table id with *)
(*     | None -> *)
(*        Printf.printf "Computing node:%d (%s)\n%!" id (String.of_last_node net); *)
(*        let acc_list = f acc_list net in *)
(*        Hashtbl.add table id acc_list; *)
(*        acc_list *)
(*     | Some acc_list -> *)
(*        Printf.printf "  Reusing node:%d (%s)\n%!" id (String.of_last_node net); *)
(*        acc_list *)
(*   in *)
(*   let rec aux net = *)
(*     let id, acc_list = match net with *)
(*       | Node01 node -> Oo.id node, [] *)
(*       | Node11 node -> Oo.id node, [aux node#upstream] *)
(*       | Noden1 node -> Oo.id node, List.map aux node#upstreams *)
(*     in *)
(*     f ~id acc_list net *)
(*   in *)
(*   Printf.eprintf "Fold start...\n%!"; *)
(*   let acc = aux net in *)
(*   Printf.eprintf "...Fold done\n%!"; *)
(*   acc *)

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

  let concatenate ?(axis = 3) up_list =
    let filters = match List.length up_list, axis with
      | 0, _ -> failwith "Can't concatenate nothing"
      | _, 3 -> List.fold_left (fun acc up -> acc + up.filters) 0 up_list
      | _, _ -> let filters = (List.hd up_list).filters in
                List.iter (fun up -> assert (up.filters = filters)) up_list;
                filters
    in
    let up_networks = List.map (fun up -> up.network) up_list in
    { filters; network = noden1 up_networks (`Concatenate { axis }) }

  let fork : (upstream -> upstream list) -> upstream -> upstream list = fun create_downs up ->
    create_downs up
    (* List.map (fun down -> down up) downs *)

  let add ?(axis = 3) up_list =
    let filters = match List.length up_list, axis with
      | 0, _ -> failwith "Can't add nothing"
      | _, 3 -> List.fold_left (fun acc up -> acc + up.filters) 0 up_list
      | _, _ -> let filters = (List.hd up_list).filters in
                List.iter (fun up -> assert (up.filters = filters)) up_list;
                filters
    in
    let up_networks = List.map (fun up -> up.network) up_list in
    { filters; network = noden1 up_networks (`Add { axis }) }

  let finalize { network; _ } = network
end
