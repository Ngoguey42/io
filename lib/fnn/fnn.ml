(* Functional Neural Network (fnn)


# Network objects
A network object is both a neural network layer and a neural network node. As a layer it contains
the definition (and the frozen mutable states, if any) of that layer. As a node it contains pointers
to the upstream node(s) (if any).

A network is immutable.

A network is a purely symbolic graph, it is totally agnostic to any deep learning training or
inference procedure.

The network object type is contained in a network module parameterized by a {| Tensor |} and an
{| Id |} module. {| ('a, 'b) Tensor.t |} is the type of a frozen tensor stored in layers such as
normalisation or parameter32. In the default module
{| Tensor.t = ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t |}. {| Id.t |} is the type of the
identifier of a node. In the default module {| Id.t = string option |}.

A node has a node_type that defines the number of input nodes:
- A `node01` has no upstream parent.
- A `node11` has 1 upstream parent.
- A `node21` has 2 upstream parent.
- A `noden1` has n upstream parents, determined when building the network.

In order to improve separation of concerns and allow complex network definitions, the network
parameters are inputs to the nodes that use them. In a classic use case a {| conv2d |}
node accesses its kernel weights through a {| parameter32 |} input node. In some meta-learning use
cases you want those kernel weights to be the result of a computation. In some other use-cases you
may want to reuse a kernel weight several times throughout a network.

---

# Network building

To build a network you have to use a {| Builder |} module that is parameterized by a {| State |}
module that provides a global random number generator state. All the functions available there
are meant to be user friendly.

Example: A classical residual network
{|
let open Builder in
let open Pshape.Size in

let residual_pool up =
  let K in_filters = Pshape.get up#out_shape `C |> to_known in
  let out_filters = in_filters + 20 in
  [ up
    |> conv2d (`Full out_filters) (1, 1) ~s:(2, 2) |> bias |> batch_norm
    |> downcast
  ; up
    |> relu |> conv2d (`Full out_filters) (3, 3) ~s:(2, 2) |> bias |> batch_norm
    |> downcast ]
  |> sum
in
let residual_conv up =
  let K out_filters = Pshape.get up#out_shape `C |> to_known in
  [ up
    |> downcast
  ; up
    |> relu |> conv2d (`Full out_filters) (3, 3) |> bias |> batch_norm
    |> downcast ]
  |> sum
in

let net =
  input (Pshape.sym4d_partial ~n:U ~c:(K 3) ~s0:U ~s1:U) `Uint8
  |> astype `Float32
  |> conv2d (`Full 20) (3, 3) ~s:(2, 2) |> bias |> batch_norm |> relu
  |> residual_pool
  |> residual_pool
  |> residual_pool
  |> residual_pool
  |> residual_pool
  |> residual_pool
  |> residual_conv
  |> residual_conv
  |> residual_conv
in
Printf.printf "First layer: %s\n" (find_layer Input [net])#to_string;
Printf.printf " Last layer: %s\n" net#to_string;
let param_count =
  find_layers Parameter32 [net]
  |> List.map (fun (p: parameter32) -> p#numel)
  |> List.fold_left ( + ) 0
in
Printf.printf "%d trainable parameters\n" (param_count);

let net =
  copy ~sub:[
         find_layer Input [net],
         input (Pshape.sym4d_partial ~n:U ~c:(K 3) ~s0:(K 1024) ~s1:(K 1024)) `Uint8
       ] [net]
  |> List.hd
in
Printf.printf "First layer: %s\n" (find_layer Input [net])#to_string;
Printf.printf " Last layer: %s\n" net#to_string;
|}

Prints:
{|
First layer: <input {n=_ ; c=3 ; s0=_ ; s1=_}>
 Last layer: <sum {n=_ ; c=140 ; s0=_ ; s1=_} [sum:624; sum:661]>
982300 trainable parameters
First layer: <input {n=_ ; c=3 ; s0=1024 ; s1=1024}>
 Last layer: <sum {n=_ ; c=140 ; s0=8 ; s1=8} [sum:1258; sum:1295]>
|}

---

# Shapes

This library avoids making dimension ordering assumptions when unnecessary (such as
`channel-last`). Although the axes still requires to be designated. Depending on the context
 you can either use an absolute or a symbolic designation.

For exemple:
- Absolute designation is not reliable to locate the `channel` dimension:
  - the axes of a 4d channel-first tensor would be {| [`Idx 0; `Idx 1; `Idx 2; `Idx 3] |},
  - the axes of a 4d channel-last tensor would be  {| [`Idx 0; `Idx 1; `Idx 2; `Idx 3] |}.
- Symbolic designation is reliable to locate the `channel` dimension:
  - the axes of a 4d channel-first tensor would be: {| [`N; `C; `S1; `S0] |},
  - the axes of a 4d channel-last tensor would be:  {| [`N; `S1; `S0; `C] |}.

Moreover, the definition of a neural network is usually agnostic to the actual size of certain
dimensions.

For example:
- The `N (batch) dimension is only known in the forward phase in all deep-learning libraries.
- The `S* (spatial) dimensions are only known at model instantation for tensorflow, and in the
  forward phase in pytorch.

The Pshape (Polymorphic shape) library provides that abstraction.

---

# Possible improvements:
- parameter32 takes a shape (and init too)
- change relu and softmax layer to activation with algorithm = [ `Relu; `Softmax of Pshape.Axis.t ]
- reshape (and test reshape/dense at end of network) idea: ([axis list] * Size.t) list. k->k, (u+k)->u, u->u, (k->u auto infer to ->k), NO(u->k)
- add `Ridx of int alongside `Idx of int? Only as inputs?
- slice
- node01 noise layer
- non-trainable node01 layers (bilinear upsampling kernel) (`buffer_float32` trainable=False, `constant_float32` ?)
- custom layers: Don't care because user can create its own network objects?
    But what about the variants like classified_layer and layer_type? Make those open / extensible?

### Module name
fnn (functional neural network)
fsnn (functional symbolic neural network)

 *)

(** {| memoized_walk f root |} is the result of {| follow root |} with {| follow x |}
    being the result of {| f follow x |} memoized. {| follow |} can be called from inside {| f |}
    in order to traverse data in a depth-first fashion. Memoization avoids visiting the same data
    twice. It only works with objects and it doesn't check for loop.
 *)
let memoized_walk : (((< .. > as 'a) -> 'b) -> 'a -> 'b) -> 'a -> 'b =
 fun f ->
  let table = Hashtbl.create 100 in
  let rec follow node =
    let id = Oo.id node in
    match Hashtbl.find_opt table id with
    | Some acc -> acc
    | None ->
        let acc = f follow node in
        Hashtbl.add table id acc;
        acc
  in
  follow

let memoized_walk_map : (((< .. > as 'a) -> 'b) -> 'a -> 'b) -> 'a list -> 'b list =
 fun f l -> List.map (memoized_walk f) l

module type TENSOR = sig
  type ('a, 'b) t

  val of_ba : ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> ('a, 'b) t

  val dimensions : ('a, 'b) t -> int array
end

module Bigarray_tensor :
  TENSOR with type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t = struct
  type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t

  let of_ba x = x

  let dimensions = Bigarray.Genarray.dims
end

module type ID = sig
  type t

  val create_default : unit -> t

  val compare : t -> t -> int
end

module String_option_id : ID with type t = string option = struct
  type t = string option

  let create_default () = None

  let compare = compare
end

module Make (Tensor : TENSOR) (Id : ID) = struct
  type 'b float_tensor = (float, 'b) Tensor.t

  type float32_tensor = Bigarray.float32_elt float_tensor

  type float64_tensor = Bigarray.float64_elt float_tensor

  type int64_tensor = (int64, Bigarray.int64_elt) Tensor.t

  type optimizer = [ `Sgd | `Adam of float * float * float ]

  type boundary_mode = [ `Same | `Valid | `Pad_fit | `Assert_fit ]

  type float_dtype = [ `Float32 | `Float64 ]

  type int_dtype = [ `Int64 | `Int32 | `Uint8 ]

  type dtype = [ float_dtype | int_dtype ]

  let string_of_dtype = function
    | `Float32 -> "float32"
    | `Float64 -> "float64"
    | `Int64 -> "int64"
    | `Int32 -> "int32"
    | `Uint8 -> "uint8"

  type network =
    < upstreams : network list
    ; classify_node : classified_node
    ; classify_layer : classified_layer
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> network
    ; id : Id.t
    ; layer_name : string
    ; to_string : string >

  and node01 =
    < upstreams : network list
    ; classify_node : [ `Node01 of node01 ]
    ; classify_layer : classified_layer01
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> node01
    ; id : Id.t
    ; layer_name : string
    ; to_string : string >
  (** node that maps 0 inputs to 1 output *)

  and node11 =
    < upstreams : network list
    ; classify_node : [ `Node11 of node11 ]
    ; classify_layer : classified_layer11
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; id : Id.t
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> node11
    ; layer_name : string
    ; to_string : string
    ; upstream : network >
  (** node that maps 1 input to 1 output *)

  and node21 =
    < upstreams : network list
    ; classify_node : [ `Node21 of node21 ]
    ; classify_layer : classified_layer21
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; id : Id.t
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> node21
    ; layer_name : string
    ; to_string : string
    ; upstream0 : network
    ; upstream1 : network >
  (** node that maps 2 inputs to 1 output *)

  and noden1 =
    < upstreams : network list
    ; classify_node : [ `Noden1 of noden1 ]
    ; classify_layer : classified_layern1
    ; out_shape : Pshape.any
    ; id : Id.t
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> noden1
    ; layer_name : string
    ; to_string : string >
  (** node that maps n input to 1 output *)

  and classified_node =
    [ `Node01 of node01 | `Node11 of node11 | `Noden1 of noden1 | `Node21 of node21 ]

  and classified_layer =
    [ `Input of input
    | `Sum of sum
    | `Prod of prod
    | `Concatenate of concatenate
    | `Softmax of softmax
    | `Relu of relu
    | `Astype of astype
    | `Normalisation of normalisation
    | `Tensordot of tensordot
    | `Maxpool2d of maxpool2d
    | `Conv2d of conv2d
    | `Padding2d of padding2d
    | `Reorder_axes of reorder_axes
    | `Parameter32 of parameter32 ]

  and classified_layer01 = [ `Input of input | `Parameter32 of parameter32 ]

  and classified_layer11 =
    [ `Softmax of softmax
    | `Relu of relu
    | `Astype of astype
    | `Normalisation of normalisation
    | `Maxpool2d of maxpool2d
    | `Padding2d of padding2d
    | `Reorder_axes of reorder_axes ]

  and classified_layern1 = [ `Sum of sum | `Prod of prod | `Concatenate of concatenate ]

  and classified_layer21 = [ `Conv2d of conv2d | `Tensordot of tensordot ]

  and input =
    < upstreams : network list
    ; classify_node : [ `Node01 of input ]
    ; classify_layer : [ `Input of input ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> input
    ; id : Id.t
    ; layer_name : string
    ; to_string : string >

  and parameter32 =
    < upstreams : network list
    ; classify_node : [ `Node01 of parameter32 ]
    ; classify_layer : [ `Parameter32 of parameter32 ]
    ; out_shape :
        'b 'c. ( Pshape.Length.tag, ([< Pshape.Size.tag > `K ] as 'b),
          ([< Pshape.Axis.t > `Idx ] as 'c) ) Pshape.t
    ; out_dtype : [ `Float32 ]
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> parameter32
    ; replicate :
        ?id:Id.t ->
        float32_tensor ->
        [ `Sgd | `Adam of float * float * float * int * float32_tensor * float32_tensor ] ->
        parameter32
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; tensor : float32_tensor
    ; numel : int
    ; initialization : Init.float32_init
    ; optimizer : [ `Sgd | `Adam of float * float * float * int * float32_tensor * float32_tensor ] >

  and sum =
    < upstreams : network list
    ; classify_node : [ `Noden1 of sum ]
    ; classify_layer : [ `Sum of sum ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> sum
    ; id : Id.t
    ; layer_name : string
    ; to_string : string >

  and prod =
    < upstreams : network list
    ; classify_node : [ `Noden1 of prod ]
    ; classify_layer : [ `Prod of prod ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> prod
    ; id : Id.t
    ; layer_name : string
    ; to_string : string >

  and concatenate =
    < upstreams : network list
    ; classify_node : [ `Noden1 of concatenate ]
    ; classify_layer : [ `Concatenate of concatenate ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> concatenate
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; axis : Pshape.Axis.t >

  and softmax =
    < upstreams : network list
    ; classify_node : [ `Node11 of softmax ]
    ; classify_layer : [ `Softmax of softmax ]
    ; out_shape : Pshape.any
    ; out_dtype : float_dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> softmax
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network
    ; axis : Pshape.Axis.t >

  and relu =
    < upstreams : network list
    ; classify_node : [ `Node11 of relu ]
    ; classify_layer : [ `Relu of relu ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> relu
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network >

  and astype =
    < upstreams : network list
    ; classify_node : [ `Node11 of astype ]
    ; classify_layer : [ `Astype of astype ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> astype
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network >

  and normalisation =
    < upstreams : network list
    ; classify_node : [ `Node11 of normalisation ]
    ; classify_layer : [ `Normalisation of normalisation ]
    ; out_shape : Pshape.any
    ; out_dtype : float_dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> normalisation
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network
    ; axes : Pshape.Axis.t list
    ; group_sizes : int list
    ; group_counts : int list
    ; algorithm :
        [ `Batch of float
        | `Moving32 of float * float * int * float32_tensor * float32_tensor
        | `Moving_exp32 of float * float * float32_tensor * float32_tensor ]
    ; is_batch_norm : bool
    ; is_layer_norm : bool
    ; is_instance_norm : bool
    ; is_group_norm : bool >

  and reorder_axes =
    < upstreams : network list
    ; classify_node : [ `Node11 of reorder_axes ]
    ; classify_layer : [ `Reorder_axes of reorder_axes ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> reorder_axes
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network
    ; mapping : (Pshape.Axis.t * Pshape.Axis.t) list >

  and maxpool2d =
    < upstreams : network list
    ; classify_node : [ `Node11 of maxpool2d ]
    ; classify_layer : [ `Maxpool2d of maxpool2d ]
    ; out_shape :
        'len 'ax. ( ([< Pshape.Length.tag > `L4 ] as 'len), Pshape.Size.tag,
          ([< Pshape.Axis.t > `N `C `S0 `S1 ] as 'ax) ) Pshape.t
    ; out_dtype : float_dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> maxpool2d
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network
    ; kernel_size : int * int
    ; stride : int * int
    ; boundary_mode : boundary_mode
    ; boundary_overflow : Pshape.Size.any * Pshape.Size.any
    ; boundary_underflow : Pshape.Size.any * Pshape.Size.any
    ; boundary_padding : (Pshape.Size.any * Pshape.Size.any) * (Pshape.Size.any * Pshape.Size.any)
    ; boundary_lost : Pshape.Size.any * Pshape.Size.any >

  and padding2d =
    < upstreams : network list
    ; classify_node : [ `Node11 of padding2d ]
    ; classify_layer : [ `Padding2d of padding2d ]
    ; out_shape :
        'len 'ax. ( ([< Pshape.Length.tag > `L4 ] as 'len), Pshape.Size.tag,
          ([< Pshape.Axis.t > `N `C `S0 `S1 ] as 'ax) ) Pshape.t
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> padding2d
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network
    ; value : [ `Constant of float | `Reflection | `Replication ]
    ; width : (int * int) * (int * int)
    ; top : int
    ; bottom : int
    ; left : int
    ; right : int
    ; is_symmetric : bool
    ; none_negative : bool
    ; none_positive : bool >

  and tensordot =
    < upstreams : network list
    ; classify_node : [ `Node21 of tensordot ]
    ; classify_layer : [ `Tensordot of tensordot ]
    ; out_shape : Pshape.any
    ; out_dtype : float_dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> tensordot
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream0 : network
    ; upstream1 : network
    ; mapping0 : (Pshape.Axis.t * Pshape.Axis.t option) list
    ; mapping1 : (Pshape.Axis.t * Pshape.Axis.t option) list
    ; contracted_axes0 : Pshape.Axis.t list
    ; contracted_axes1 : Pshape.Axis.t list >

  and conv2d =
    < upstreams : network list
    ; classify_node : [ `Node21 of conv2d ]
    ; classify_layer : [ `Conv2d of conv2d ]
    ; out_shape :
        'len 'ax. ( ([< Pshape.Length.tag > `L4 ] as 'len), Pshape.Size.tag,
          ([< Pshape.Axis.t > `N `C `S0 `S1 ] as 'ax) ) Pshape.t
    ; out_dtype : float_dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> conv2d
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream0 : network
    ; upstream1 : network
    ; kernel_size : int * int
    ; stride : int * int
    ; dilation : int * int
    ; in_filters : int
    ; out_filters : int
    ; group_count : int
    ; in_group_filters : int
    ; out_group_filters : int
    ; is_grouped : bool
    ; is_depthwise : bool
    ; is_pointwise : bool
    ; is_dilated : bool
    ; is_strided : bool
    ; kernel_shape : ([ `L4 ], [ `K ], [ `Idx of int ]) Pshape.t
    ; boundary_mode : boundary_mode
    ; boundary_overflow : Pshape.Size.any * Pshape.Size.any
    ; boundary_underflow : Pshape.Size.any * Pshape.Size.any
    ; boundary_padding : (Pshape.Size.any * Pshape.Size.any) * (Pshape.Size.any * Pshape.Size.any)
    ; boundary_lost : Pshape.Size.any * Pshape.Size.any >

  type 'a any = < classify_layer : [< classified_layer ] ; .. > as 'a

  type _ layer_type =
    | Input : input layer_type
    | Sum : sum layer_type
    | Prod : prod layer_type
    | Concatenate : concatenate layer_type
    | Softmax : softmax layer_type
    | Relu : relu layer_type
    | Astype : astype layer_type
    | Normalisation : normalisation layer_type
    | Tensordot : tensordot layer_type
    | Reorder_axes : reorder_axes layer_type
    | Maxpool2d : maxpool2d layer_type
    | Conv2d : conv2d layer_type
    | Padding2d : padding2d layer_type
    | Parameter32 : parameter32 layer_type

  type _ node_type =
    | Node01 : node01 node_type
    | Node11 : node11 node_type
    | Noden1 : noden1 node_type
    | Node21 : node21 node_type

  module InputSet = Set.Make (struct
    type t = input

    let compare = compare
  end)

  module Set = Set.Make (struct
    type t = network

    let compare = compare
  end)

  module Map = Map.Make (struct
    type t = network

    let compare = compare
  end)

  (* ********************************************************************************************** *)
  let unclassify : [< classified_layer ] -> network =
   fun v ->
    match (v :> classified_layer) with
    | `Input node -> (node :> network)
    | `Sum node -> (node :> network)
    | `Prod node -> (node :> network)
    | `Concatenate node -> (node :> network)
    | `Softmax node -> (node :> network)
    | `Relu node -> (node :> network)
    | `Astype node -> (node :> network)
    | `Normalisation node -> (node :> network)
    | `Tensordot node -> (node :> network)
    | `Reorder_axes node -> (node :> network)
    | `Maxpool2d node -> (node :> network)
    | `Conv2d node -> (node :> network)
    | `Padding2d node -> (node :> network)
    | `Parameter32 node -> (node :> network)

  let downcast : _ any -> network =
   fun net ->
    match net#classify_layer with
    | `Input node -> (node : input :> network)
    | `Sum node -> (node : sum :> network)
    | `Prod node -> (node : prod :> network)
    | `Concatenate node -> (node : concatenate :> network)
    | `Softmax node -> (node : softmax :> network)
    | `Relu node -> (node : relu :> network)
    | `Astype node -> (node : astype :> network)
    | `Normalisation node -> (node : normalisation :> network)
    | `Tensordot node -> (node : tensordot :> network)
    | `Reorder_axes node -> (node : reorder_axes :> network)
    | `Maxpool2d node -> (node : maxpool2d :> network)
    | `Conv2d node -> (node : conv2d :> network)
    | `Padding2d node -> (node : padding2d :> network)
    | `Parameter32 node -> (node : parameter32 :> network)

  let find_ids : ?max_size:int -> Id.t -> _ any list -> network list =
   fun ?(max_size = max_int) id network_ends ->
    let network_ends = List.map downcast network_ends in
    let tbl = Hashtbl.create 20 in
    let f follow (node : network) =
      if Hashtbl.length tbl < max_size && Id.compare node#id id = 0 then Hashtbl.add tbl node true;
      if Hashtbl.length tbl < max_size then List.iter follow node#upstreams
    in
    memoized_walk_map f network_ends |> ignore;
    assert (Hashtbl.length tbl <= max_size);
    tbl |> Hashtbl.to_seq_keys |> List.of_seq

  let find_id : Id.t -> _ any list -> network =
   fun id network_ends -> find_ids ~max_size:1 id network_ends |> List.hd

  let find_id_opt : Id.t -> _ any list -> network option =
   fun id network_ends ->
    match find_ids ~max_size:1 id network_ends with
    | [] -> None
    | [ node ] -> Some node
    | _ -> failwith "unreachable"

  let find_nodes : type a. ?max_size:int -> a node_type -> _ any list -> a list =
   fun ?(max_size = max_int) node_type network_ends ->
    let network_ends = List.map downcast network_ends in
    let tbl : (a, bool) Hashtbl.t = Hashtbl.create 20 in
    let f follow (node : network) =
      ( if Hashtbl.length tbl < max_size then
        match (node_type, node#classify_node) with
        | Node01, `Node01 node -> Hashtbl.add tbl node true
        | Node11, `Node11 node -> Hashtbl.add tbl node true
        | Noden1, `Noden1 node -> Hashtbl.add tbl node true
        | Node21, `Node21 node -> Hashtbl.add tbl node true
        | _, _ -> () );
      if Hashtbl.length tbl < max_size then List.iter follow node#upstreams
    in
    memoized_walk_map f network_ends |> ignore;
    assert (Hashtbl.length tbl <= max_size);
    tbl |> Hashtbl.to_seq_keys |> List.of_seq

  let find_layers : type a. ?max_size:int -> a layer_type -> _ any list -> a list =
   fun ?(max_size = max_int) layer_type network_ends ->
    let network_ends = List.map downcast network_ends in
    let tbl : (a, bool) Hashtbl.t = Hashtbl.create 20 in
    let f follow (node : network) =
      ( if Hashtbl.length tbl < max_size then
        match (layer_type, node#classify_layer) with
        | Input, `Input node -> Hashtbl.add tbl node true
        | Sum, `Sum node -> Hashtbl.add tbl node true
        | Prod, `Prod node -> Hashtbl.add tbl node true
        | Concatenate, `Concatenate node -> Hashtbl.add tbl node true
        | Softmax, `Softmax node -> Hashtbl.add tbl node true
        | Relu, `Relu node -> Hashtbl.add tbl node true
        | Astype, `Astype node -> Hashtbl.add tbl node true
        | Normalisation, `Normalisation node -> Hashtbl.add tbl node true
        | Tensordot, `Tensordot node -> Hashtbl.add tbl node true
        | Reorder_axes, `Reorder_axes node -> Hashtbl.add tbl node true
        | Maxpool2d, `Maxpool2d node -> Hashtbl.add tbl node true
        | Conv2d, `Conv2d node -> Hashtbl.add tbl node true
        | Padding2d, `Padding2d node -> Hashtbl.add tbl node true
        | Parameter32, `Parameter32 node -> Hashtbl.add tbl node true
        | _, _ -> () );
      if Hashtbl.length tbl < max_size then List.iter follow node#upstreams
    in
    memoized_walk_map f network_ends |> ignore;
    assert (Hashtbl.length tbl <= max_size);
    tbl |> Hashtbl.to_seq_keys |> List.of_seq

  let find_layer : 'a layer_type -> _ any list -> 'a =
   fun layer_type network_ends -> find_layers ~max_size:1 layer_type network_ends |> List.hd

  let find_layer_opt : 'a layer_type -> _ any list -> 'a option =
   fun layer_type network_ends ->
    match find_layers ~max_size:1 layer_type network_ends with
    | [] -> None
    | [ node ] -> Some node
    | _ -> failwith "unreachable"

  let inputs : _ any list -> input list = fun network_ends -> find_layers Input network_ends

  let parameters : _ any list -> network list =
   fun network_ends -> List.concat [ (find_layers Parameter32 network_ends :> network list) ]

  let map :
      (network -> [ `Bound of network | `Unbound of network list -> network | `Remove | `Skip ]) ->
      _ any list ->
      network list =
   fun f network_ends ->
    let network_ends = List.map downcast network_ends in
    let g follow (node : network) =
      match f node with
      | `Remove -> None
      | `Skip -> (
          match node#upstreams with
          | [ upstream ] -> follow upstream
          | _ -> invalid_arg "In map: Can't remove a node that takes more than 1 upstream" )
      | `Bound node -> Some node
      | `Unbound node_of_upstreams ->
          Some (List.map follow node#upstreams |> List.filter_map (fun x -> x) |> node_of_upstreams)
    in
    memoized_walk_map g network_ends |> List.filter_map (fun x -> x)

  (** Create a copy of a network.
        The nodes will be computed one by one using those rules:
        - if a node is listed in {| keep |}, the node will be kept (so are its parents).
        - if a node is listed in {| skip |}, the node will be replaced by its parent
          (the node should have exactly one upstream parent).
        - if a node is listed in {| remove |}, the branch starting at this node is removed. (this
          node must be an output or only be used by {| noden1 |} nodes.
        - if a node is listed in {| sub |}, it will be substituted by the provided node.
        - if a node is listed in {| bind |}, the provided function will be called with the new
          parents to create the new node.
        - otherwise a node is copied using the {| copy ~reinit ~rng |} method

        Use cases:
        - The default params can be used to get a structural copy of a network that share the same
          tensor pointers in the stateful layers.
        - {| ~reinit:true ~rng |} can be used to perform two trainings that uses different initial
          random seeds.
        - {| ~reinit:true ~rng |} can be used to reuse pieces of architecture several times in a
          larger network.
        - {| ~keep |} can be used to share the parameter nodes between the input and the output
          networks.
        - {| ~keep |} can be used to stop the traversal of {| copy |} in order to to preserve part
          of a network.
        - {| ~skip |} can be used to remove specific one-to-one layers.
        - {| ~remove |} can be used to get rid of an input branch in a concatenate or a sum.
        - {| ~sub |} can be used to update the parameter layers after training.
        - {| ~sub |} can be used to append a network to another.
        - {| ~sub |} can be used to change the dtypes and the shapes of a network. It may fail under
          certain conditions.
        - {| ~bind |} can be used to update the stateful layers (like batch_norm) after training.
        - {| ~bind |} can be used to change the configuration of a node.
        - If you need a more complex use-case you should implement your own network traversal
          using {| memoized_walk_map |}.

        When a call to {| copy |} changes a dtype or a shape, it may fail if the layer doesn't
        support that change. E.g. when changing the number of channels but keeping the same weights
        before a {| conv |} or a {| dense |}.
     *)
  let copy :
      ?keep:_ any list ->
      ?skip:_ any list ->
      ?remove:_ any list ->
      ?sub:(_ any * _ any) list ->
      ?bind:(_ any * (network list -> network)) list ->
      ?reinit:bool ->
      ?rng:Random.State.t ->
      _ any list ->
      network list =
   fun ?(keep = []) ?(skip = []) ?(remove = []) ?(sub = []) ?(bind = []) ?(reinit = false) ?rng
       network_ends ->
    let keep = List.map downcast keep in
    let skip = List.map downcast skip in
    let remove = List.map downcast remove in
    let sub = List.map (fun (a, b) -> (downcast a, downcast b)) sub in
    let bind = List.map (fun (a, f) -> (downcast a, f)) bind in
    let network_ends = List.map downcast network_ends in
    let keep =
      let t = Hashtbl.create (List.length keep) in
      List.map (fun x -> (x, true)) keep |> List.to_seq |> Hashtbl.add_seq t;
      t
    in
    let skip =
      let t = Hashtbl.create (List.length skip) in
      List.map (fun x -> (x, true)) skip |> List.to_seq |> Hashtbl.add_seq t;
      t
    in
    let remove =
      let t = Hashtbl.create (List.length remove) in
      List.map (fun x -> (x, true)) remove |> List.to_seq |> Hashtbl.add_seq t;
      t
    in
    let sub =
      let t = Hashtbl.create (List.length sub) in
      List.to_seq sub |> Hashtbl.add_seq t;
      t
    in
    let bind =
      let t = Hashtbl.create (List.length bind) in
      List.to_seq bind |> Hashtbl.add_seq t;
      t
    in
    let f (node : network) =
      match Hashtbl.find_opt keep node with
      | Some _ -> `Bound node
      | None -> (
          match Hashtbl.find_opt skip node with
          | Some _ -> `Skip
          | None -> (
              match Hashtbl.find_opt remove node with
              | Some _ -> `Remove
              | None -> (
                  match Hashtbl.find_opt sub node with
                  | Some node -> `Bound node
                  | None -> (
                      match Hashtbl.find_opt bind node with
                      | Some f -> `Unbound f
                      | None ->
                          `Unbound
                            ( match rng with
                            | Some rng -> node#copy ~id:node#id ~reinit ~rng
                            | None -> (node#copy ~id:node#id ~reinit : network list -> network) ) )
                  ) ) )
    in
    map f network_ends

  (* ********************************************************************************************** *)
  module type STATE = sig
    val get_state : unit -> Random.State.t
  end

  module type BUILDER = sig
    (** 1. Tools *)

    val append : ?head:_ any -> ?tail:_ any -> _ any -> network

    (** 2. Node builders *)

    val input :
      ?id:Id.t ->
      ([< Pshape.Length.tag ], [< Pshape.Size.tag ], [< Pshape.Axis.t ]) Pshape.t ->
      dtype ->
      input

    val parameter32 :
      ?id:Id.t ->
      ?rng:Random.State.t ->
      int array ->
      [< Init.float32_init ] ->
      [< optimizer ] ->
      parameter32

    val sum : ?id:Id.t -> _ any list -> sum

    val prod : ?id:Id.t -> _ any list -> prod

    val concatenate : [< Pshape.Axis.t ] -> ?id:Id.t -> _ any list -> concatenate

    val softmax : Pshape.Axis.t -> ?id:Id.t -> _ any -> softmax

    val relu : ?id:Id.t -> _ any -> relu

    val astype : dtype -> ?id:Id.t -> _ any -> astype

    val normalisation :
      ([< Pshape.Axis.t ] * int) list ->
      ?stats:[< `Batch of float | `Moving of float * float | `Moving_exp of float * float ] ->
      ?id:Id.t ->
      ?rng:Random.State.t ->
      _ any ->
      normalisation

    val reorder_axes :
      ([< Pshape.Axis.t ] * [< Pshape.Axis.t ]) list -> ?id:Id.t -> _ any -> reorder_axes

    val maxpool2d :
      ?b:[< boundary_mode ] -> ?s:int * int -> int * int -> ?id:Id.t -> _ any -> maxpool2d

    val padding2d :
      ?v:[< `Constant of float | `Reflection | `Replication ] ->
      int list ->
      ?id:Id.t ->
      _ any ->
      padding2d
    (** width can be negative *)

    val conv2d2 :
      ?g:int ->
      ?s:int * int ->
      ?d:int * int ->
      ?b:[< boundary_mode ] ->
      ?id:Id.t ->
      _ any ->
      _ any ->
      conv2d

    val tensordot :
      ([< Pshape.Axis.t ] * [< Pshape.Axis.t ] option) list ->
      ([< Pshape.Axis.t ] * [< Pshape.Axis.t ] option) list ->
      ?id:Id.t ->
      _ any ->
      _ any ->
      tensordot

    (** 3. Syntaxic sugars *)

    val cropping2d : ?id:Id.t -> int list -> _ any -> padding2d

    val dense :
      ?id:Id.t ->
      ([< Pshape.Axis.t ] * int) list ->
      ?i:[< Init.float_init ] ->
      ?o:[< optimizer ] ->
      ?rng:Random.State.t ->
      _ any ->
      tensordot
    (**
      Syntaxic sugar for:
      {[
      Builder.tensordot
        mapping0
        mapping1
        ( Builder.parameter(32|64) [| axis0_old_size; axis0_new_size; ... |] init optimizer )
        upstream
      ]} *)

    val conv2d :
      ?id:Id.t ->
      [< `Full of int | `Depthwise of int | `Grouped of int * int ] ->
      int * int ->
      ?s:int * int ->
      ?d:int * int ->
      ?b:[< boundary_mode ] ->
      ?i:[< Init.float_init ] ->
      ?o:[< optimizer ] ->
      ?rng:Random.State.t ->
      _ any ->
      conv2d
    (** Syntaxic sugar for
      {[
      Builder.conv2d2
        ~g:group_count
        ~s:stride
        ~d:dilation
        (Builder.parameter(32|64) [| ky, kx, in_filters; out_filters |] init optimizer)
        upstream
      ]} *)

    val bias :
      ?id:Id.t ->
      ?axes:[< Pshape.Axis.t ] list ->
      ?i:[< Init.float_init ] ->
      ?o:[< optimizer ] ->
      ?rng:Random.State.t ->
      _ any ->
      sum
    (**
      Syntaxic sugar for:
      {[
      Builder.sum
        [ Builder.parameter(32|64) [| axis0_size; ... |] init optimizer
          |> Builder.reorder [ axis0_mapping; ... ]
        ; upstream ]
      ]} *)

    val scale :
      ?id:Id.t ->
      ?axes:[< Pshape.Axis.t ] list ->
      ?i:[< Init.float_init ] ->
      ?o:[< optimizer ] ->
      ?rng:Random.State.t ->
      _ any ->
      prod
    (**
      Syntaxic sugar for:
      {[
      Builder.prod
        [ Builder.parameter(32|64) [| axis0_size; ... |] init optimizer
          |> Builder.reorder [ axis0_mapping; ... ]
        ; upstream ]
      ]} *)

    val batch_norm :
      ?id:Id.t ->
      ?rng:Random.State.t ->
      ?affine:bool ->
      ?stats:[< `Batch of float | `Moving of float * float | `Moving_exp of float * float ] ->
      _ any ->
      network
  end

  module Make_builder (State : STATE) : BUILDER = struct
    (* 1. Tools ********************************************************************************* *)

    let _expand_boundary_mode boundary_mode size kernel_size dilation stride =
      let open Pshape.Size in
      let open Pshape.Size.Infix in
      let kernel_size = K kernel_size in
      let stride = K stride in
      let dilation = K dilation in
      let span = kernel_size * dilation in

      let overflow, underflow =
        match (size, span) with
        | K 0, _ -> invalid_arg "size 0 input is too small to apply a kernel"
        | K size, K span when size < span -> (
            match boundary_mode with
            | `Same | `Pad_fit -> (K 0, K span - K size)
            | `Valid | `Assert_fit ->
                invalid_arg "input size is too small to apply a kernel without padding" )
        | _, _ ->
            let overflow = (size - span) mod stride in
            let underflow = (stride - overflow) mod stride in
            (overflow, underflow)
      in
      let padding_total, lost =
        match boundary_mode with
        | `Valid -> (K 0, overflow)
        | `Pad_fit -> (underflow, K 0)
        | `Same ->
            let out_size = (size + stride - K 1) / stride in
            (((out_size - K 1) * stride) - size + span, K 0)
        | `Assert_fit ->
            (match overflow with K 0 | U -> () | _ -> invalid_arg "boundary_mode=`Fit failed");
            (K 0, K 0)
      in
      let padding_start = div padding_total (K 2) in
      let padding_end = sub padding_total padding_start in
      let out_size = K 1 + ((size + padding_start + padding_end - span) / stride) in

      (overflow, underflow, (padding_start, padding_end), lost, out_size)

    let () =
      let open Pshape.Size in
      assert (_expand_boundary_mode `Valid (K 16) 10 1 8 = (K 6, K 2, (K 0, K 0), K 6, K 1));
      assert (_expand_boundary_mode `Pad_fit (K 16) 10 1 8 = (K 6, K 2, (K 1, K 1), K 0, K 2));
      assert (_expand_boundary_mode `Same (K 16) 10 1 8 = (K 6, K 2, (K 1, K 1), K 0, K 2));
      assert (_expand_boundary_mode `Pad_fit (K 1) 2 1 1 = (K 0, K 1, (K 0, K 1), K 0, K 1));
      assert (_expand_boundary_mode `Same (K 1) 2 1 1 = (K 0, K 1, (K 0, K 1), K 0, K 1));
      assert (_expand_boundary_mode `Pad_fit (K 1) 2 1 2 = (K 0, K 1, (K 0, K 1), K 0, K 1));
      assert (_expand_boundary_mode `Same (K 1) 2 1 2 = (K 0, K 1, (K 0, K 1), K 0, K 1));
      ()

    let append ?head ?tail other =
      let head_end, tail_end =
        match (head, tail) with
        | None, None | Some _, Some _ -> failwith "Provide only head or only tail"
        | Some node, _ -> (downcast node, downcast other)
        | _, Some node -> (downcast other, downcast node)
      in
      let tail_head =
        match inputs [ tail_end ] with
        | [ n ] -> downcast n
        | _ -> invalid_arg "In append: The tail network should have exactly one input node"
      in
      copy ~sub:[ (tail_head, head_end) ] [ tail_end ] |> List.hd

    (* 2. Node builders ************************************************************************* *)

    let input ?id shape dtype =
      let shape = Pshape.to_any shape in
      let rec instanciate id =
        let id = match id with None -> Id.create_default () | Some id -> id in
        object (self : input)
          method upstreams = []

          method classify_node = `Node01 self

          method classify_layer = `Input self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            if List.length upstreams <> 0 then invalid_arg "input#copy takes 0 upstreams";
            instanciate (Some id)

          method id = id

          method layer_name = "input"

          method to_string = Printf.sprintf "<input %s>" (Pshape.to_string shape)
        end
      in
      instanciate id

    let parameter32 ?id ?rng dimensions init optimizer =
      let init = (init :> Init.float32_init) in
      ( match optimizer with
      | `Sgd -> ()
      | `Adam (epsilon, beta1, beta2) ->
          if epsilon <= 0. then failwith "In parameter: epsilon should be > 0.";
          if beta1 <= 0. || beta1 >= 1. then
            failwith "In parameter: beta1 should be between 0 and 1 (excluded)";
          if beta2 <= 0. || beta2 >= 1. then
            failwith "In parameter: beta2 should be between 0 and 1 (excluded)";
          () );
      let rec instanciate tensor_optim id rng =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dimensions, tensor, optim =
          match tensor_optim with
          | None ->
              (* Take dimensions from closure, create tensor from dimensions *)
              let rng = match rng with Some v -> v | None -> State.get_state () in
              let tensor = Init.run ~rng init Bigarray.Float32 dimensions |> Tensor.of_ba in
              let optim =
                match optimizer with
                | `Sgd -> `Sgd
                | `Adam (epsilon, beta1, beta2) ->
                    let rgrad = Init.run ~rng (`FloatConstant 0.) Bigarray.Float32 dimensions in
                    let rgrad = Tensor.of_ba rgrad in
                    let rgrad_sq = Init.run ~rng (`FloatConstant 0.) Bigarray.Float32 dimensions in
                    let rgrad_sq = Tensor.of_ba rgrad_sq in
                    `Adam (epsilon, beta1, beta2, 0, rgrad, rgrad_sq)
              in
              (dimensions, tensor, optim)
          | Some (tensor, optim) ->
              (* Take tensor from inputs, create dimensions from tensor *)
              let dimensions = Tensor.dimensions tensor in
              (dimensions, tensor, optim)
        in
        let shape = Pshape.from_int_array dimensions in
        object (self : parameter32)
          method upstreams = []

          method classify_node = `Node01 self

          method classify_layer = `Parameter32 self

          method out_shape = shape |> Pshape.Open.to_total |> Pshape.Open.to_absolute

          method out_dtype = `Float32

          method stateful = true

          method copy ?(id = self#id) ?(reinit = false) ?rng upstreams =
            if List.length upstreams <> 0 then invalid_arg "parameter32#copy takes 0 upstreams";
            if reinit then instanciate None (Some id) rng
            else instanciate (Some (tensor, optim)) (Some id) rng

          method replicate ?(id = self#id) tensor optim =
            instanciate (Some (tensor, optim)) (Some id) None

          method id = id

          method layer_name = "parameter32"

          method to_string = Printf.sprintf "<parameter32 %s>" (Pshape.to_string shape)

          method tensor = tensor

          method numel =
            let (K n) = Pshape.numel shape in
            n

          method initialization = init

          method optimizer = optim
        end
      in
      instanciate None id rng

    let sum =
      let rec instanciate id upstreams =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype =
          match List.sort_uniq compare (List.map (fun up -> up#out_dtype) upstreams) with
          | [] -> invalid_arg "In sum: Can't sum nothing"
          | [ hd ] -> hd
          | _ -> invalid_arg "In sum: Input nodes have different dtypes"
        in
        let shape = List.map (fun x -> x#out_shape) upstreams |> Pshape.broadcast_all in

        object (self : sum)
          method upstreams = upstreams

          method classify_node = `Noden1 self

          method classify_layer = `Sum self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams = instanciate (Some id) upstreams

          method id = id

          method layer_name = "sum"

          method to_string =
            Printf.sprintf "<sum %s [%s]>" (Pshape.to_string shape)
              (String.concat "; "
                 (List.map (fun up -> Printf.sprintf "%s:%d" up#layer_name (Oo.id up)) upstreams))
        end
      in
      fun ?id upstreams -> instanciate id (List.map downcast upstreams)

    let prod =
      let rec instanciate id upstreams =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype =
          match List.sort_uniq compare (List.map (fun up -> up#out_dtype) upstreams) with
          | [] -> invalid_arg "In prod: Can't prod nothing"
          | [ hd ] -> hd
          | _ -> invalid_arg "In prod: Input nodes have different dtypes"
        in
        let shape = List.map (fun x -> x#out_shape) upstreams |> Pshape.broadcast_all in

        object (self : prod)
          method upstreams = upstreams

          method classify_node = `Noden1 self

          method classify_layer = `Prod self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams = instanciate (Some id) upstreams

          method id = id

          method layer_name = "prod"

          method to_string =
            Printf.sprintf "<prod %s [%s]>" (Pshape.to_string shape)
              (String.concat "; "
                 (List.map (fun up -> Printf.sprintf "%s:%d" up#layer_name (Oo.id up)) upstreams))
        end
      in
      fun ?id upstreams -> instanciate id (List.map downcast upstreams)

    let concatenate axis =
      let axis = (axis :> Pshape.Axis.t) in
      let rec instanciate id upstreams =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype =
          match List.sort_uniq compare (List.map (fun up -> up#out_dtype) upstreams) with
          | [] -> invalid_arg "In concatenate: Can't concatenate nothing"
          | [ hd ] -> hd
          | _ -> invalid_arg "In concatenate: Input nodes have different dtypes"
        in
        let shape = Pshape.concatenate_all (List.map (fun up -> up#out_shape) upstreams) axis in

        object (self : concatenate)
          method upstreams = upstreams

          method classify_node = `Noden1 self

          method classify_layer = `Concatenate self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams = instanciate (Some id) upstreams

          method id = id

          method layer_name = "concatenate"

          method to_string =
            Printf.sprintf "<concatenate %s axis:%s [%s]>" (Pshape.Axis.to_string axis)
              (Pshape.to_string shape)
              (String.concat "; "
                 (List.map (fun up -> Printf.sprintf "%s:%d" up#layer_name (Oo.id up)) upstreams))

          method axis = axis
        end
      in
      fun ?id upstreams -> instanciate id (List.map downcast upstreams)

    let softmax axis =
      let rec instanciate id upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype =
          match upstream#out_dtype with
          | #float_dtype as dtype -> dtype
          | _ -> invalid_arg "In softmax: Can only be applied on floating point dtype"
        in
        let shape = upstream#out_shape in
        if not (List.exists (fun ax -> ax = axis) (Pshape.axes shape)) then
          Printf.sprintf "In softmax: axis %s is not contained in shape %s"
            (Pshape.Axis.to_string axis) (Pshape.to_string shape)
          |> invalid_arg;
        object (self : softmax)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Softmax self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "softmax#copy takes 1 upstream"

          method id = id

          method layer_name = "softmax"

          method to_string =
            Printf.sprintf "<softmax %s axis:%s>" (Pshape.to_string shape)
              (Pshape.Axis.to_string axis)

          method upstream = upstream

          method axis = axis
        end
      in
      fun ?id upstream -> instanciate id (downcast upstream)

    let relu =
      let rec instanciate id upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype = upstream#out_dtype in
        let shape = upstream#out_shape in
        object (self : relu)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Relu self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "relu#copy takes 1 upstream"

          method id = id

          method layer_name = "relu"

          method to_string = Printf.sprintf "<relu %s>" (Pshape.to_string shape)

          method upstream = upstream
        end
      in
      fun ?id upstream -> instanciate id (downcast upstream)

    let normalisation group_size_per_axis ?stats =
      let stats =
        Option.value
          ~default:(`Moving_exp (1e-5, 0.1))
          ( stats
            :> [ `Batch of float | `Moving of float * float | `Moving_exp of float * float ] option
            )
      in
      let group_size_per_axis = (group_size_per_axis :> (Pshape.Axis.t * int) list) in

      (* Check: axes *)
      let norm_axes, group_sizes = List.split group_size_per_axis in
      if List.length norm_axes = 0 then
        invalid_arg "In normalisation: group_size_per_axis should not be an empty list";
      if List.length norm_axes <> List.length (List.sort_uniq compare norm_axes) then
        invalid_arg "In normalisation: Axis provided twice";

      (* Check: input sizes *)
      List.iter
        (fun group_size ->
          if group_size <= 0 then invalid_arg "In normalisation: group_size can't be negative")
        group_sizes;

      (* Check: algorithm *)
      ( match stats with
      | `Batch epsilon ->
          if epsilon <= 0. then invalid_arg "In normalisation: epsilon should be > 0"
      | `Moving (epsilon, momentum) ->
          if epsilon <= 0. then invalid_arg "In normalisation: epsilon should be > 0";
          if momentum <= 0. || momentum >= 1. then
            invalid_arg "In normalisation: momentum should be > 0 and < 1"
      | `Moving_exp (epsilon, momentum) ->
          if epsilon <= 0. then invalid_arg "In normalisation: epsilon should be > 0";
          if momentum <= 0. || momentum >= 1. then
            invalid_arg "In normalisation: momentum should be > 0 and < 1" );

      let rec instanciate algorithm id rng upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype =
          match upstream#out_dtype with
          | #float_dtype as dtype -> dtype
          | #int_dtype -> invalid_arg "In normalisation: Can't be applied on an integer tensor"
        in

        (* Extraction: axes *)
        let shape = upstream#out_shape in
        let all_axes = Pshape.axes shape in
        if List.exists (fun ax -> not (List.mem ax all_axes)) norm_axes then
          invalid_arg "In normalisation: bad axis in group_size_per_axis";

        (* Extraction: input sizes *)
        let group_counts =
          List.map
            (fun (ax, group_size) ->
              let size =
                match Pshape.get shape ax with
                | Pshape.Size.K i -> i
                | Pshape.Size.U ->
                    invalid_arg "In normalisation: can't normalize along an unknown axis"
              in
              if size mod group_size <> 0 then
                invalid_arg "In normalisation: group_size doesn't divide axis size";
              size / group_size)
            group_size_per_axis
        in
        let dimensions = Array.of_list group_counts in

        (* Allocation: algorithm *)
        let algorithm =
          let rng = match rng with None -> State.get_state () | Some rng -> rng in
          match (dtype, algorithm, stats) with
          | #float_dtype, Some (`Batch _ as algorithm), _ -> algorithm
          | `Float32, Some (`Moving32 (_, _, _, avg, var) as algorithm), _ ->
              if Tensor.dimensions avg <> dimensions then
                invalid_arg "In normalisation: Invalid copy, shapes are not compatible";
              if Tensor.dimensions var <> dimensions then
                invalid_arg "In normalisation: Invalid copy, shapes are not compatible";
              algorithm
          | `Float32, Some (`Moving_exp32 (_, _, avg, var) as algorithm), _ ->
              if Tensor.dimensions avg <> dimensions then
                invalid_arg "In normalisation: Invalid copy, shapes are not compatible";
              if Tensor.dimensions var <> dimensions then
                invalid_arg "In normalisation: Invalid copy, shapes are not compatible";
              algorithm
          | #float_dtype, None, `Batch epsilon -> `Batch epsilon
          | `Float32, None, `Moving (epsilon, momentum) ->
              let avg = Init.run ~rng (`FloatConstant 0.) Bigarray.Float32 dimensions in
              let avg = Tensor.of_ba avg in
              let var = Init.run ~rng (`FloatConstant 1.) Bigarray.Float32 dimensions in
              let var = Tensor.of_ba var in
              `Moving32 (epsilon, momentum, 0, avg, var)
          | `Float32, None, `Moving_exp (epsilon, momentum) ->
              let avg = Init.run ~rng (`FloatConstant 0.) Bigarray.Float32 dimensions in
              let avg = Tensor.of_ba avg in
              let var = Init.run ~rng (`FloatConstant 1.) Bigarray.Float32 dimensions in
              let var = Tensor.of_ba var in
              `Moving_exp32 (epsilon, momentum, avg, var)
          | `Float64, _, _ -> failwith "In normalisation: `Float64 not implemented"
        in

        object (self : normalisation)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Normalisation self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = true

          method copy ?(id = self#id) ?(reinit = false) ?rng upstreams =
            match upstreams with
            | [ up ] -> instanciate (if reinit then None else Some algorithm) (Some id) rng up
            | _ -> invalid_arg "normalisation#copy takes 1 upstream"

          method id = id

          method layer_name = "normalisation"

          method to_string = Printf.sprintf "<normalisation %s>" (Pshape.to_string shape)

          method upstream = upstream

          method axes = norm_axes

          method group_sizes = group_sizes

          method group_counts = group_counts

          method algorithm = algorithm

          method is_batch_norm = norm_axes = [ `C ] && group_sizes = [ 1 ]

          method is_layer_norm = norm_axes = [ `N ] && group_sizes = [ 1 ]

          method is_instance_norm =
            match (norm_axes, group_sizes) with
            | [ `N; `C ], [ 1; 1 ] | [ `C; `N ], [ 1; 1 ] -> true
            | _, _ -> false

          method is_group_norm =
            match (norm_axes, group_sizes) with
            | ([ `N; `C ], [ 1; chan_group_size ] | [ `C; `N ], [ chan_group_size; 1 ])
              when chan_group_size <> 1 ->
                true
            | _, _ -> false
        end
      in
      fun ?id ?rng upstream -> instanciate None id rng (downcast upstream)

    let reorder_axes mapping =
      let mapping = List.map (fun (a, b) -> ((a :> Pshape.Axis.t), (b :> Pshape.Axis.t))) mapping in
      let rec instanciate id upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype = upstream#out_dtype in
        let shape = Pshape.reorder mapping upstream#out_shape in
        object (self : reorder_axes)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Reorder_axes self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "reorder_axes#copy takes 1 upstream"

          method id = id

          method layer_name = "reorder_axes"

          method to_string =
            Printf.sprintf "<reorder_axes %s -> %s>"
              (Pshape.to_string upstream#out_shape)
              (Pshape.to_string shape)

          method upstream = upstream

          method mapping = mapping
        end
      in
      fun ?id upstream -> instanciate id (downcast upstream)

    let maxpool2d ?b:boundary_mode ?s:stride kernel_size =
      let boundary_mode = Option.value ~default:`Same (boundary_mode :> boundary_mode option) in
      let stride = match stride with None -> kernel_size | Some s -> s in

      if fst kernel_size <= 0 || snd kernel_size <= 0 then
        failwith "In maxpool2d: kernel_size should be >= 1";
      if fst stride <= 0 || snd stride <= 0 then failwith "In maxpool2d: stride should be >= 1";

      let rec instanciate id upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype =
          match upstream#out_dtype with
          | #float_dtype as dtype -> dtype
          | _ -> invalid_arg "In maxpool2d: Can only be applied on floating point dtype"
        in

        let out_shape = upstream#out_shape in
        if Pshape.ndim out_shape <> 4 then
          invalid_arg "In maxpool2d: upstream should have 4 dimensions";
        let overflow_s0, underflow_s0, padding_s0, lost_s0, out_size_s0 =
          _expand_boundary_mode boundary_mode
            (Pshape.get out_shape `S0)
            (snd kernel_size) 1 (snd stride)
        in
        let overflow_s1, underflow_s1, padding_s1, lost_s1, out_size_s1 =
          _expand_boundary_mode boundary_mode
            (Pshape.get out_shape `S1)
            (fst kernel_size) 1 (fst stride)
        in
        let out_shape = Pshape.set out_shape `S0 out_size_s0 in
        let out_shape = Pshape.set out_shape `S1 out_size_s1 in

        object (self : maxpool2d)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Maxpool2d self

          method out_shape = out_shape |> Pshape.Open.to_layout Pshape.Layout.Sym4d

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "maxpool2d#copy takes 1 upstream"

          method id = id

          method layer_name = "maxpool2d"

          method to_string =
            let pair_to_string tag (y, x) =
              if y = 1 && x = 1 then ""
              else if x <> y then Printf.sprintf " ~%s:%dx%d" tag y x
              else Printf.sprintf " ~%s:%d" tag y
            in
            Printf.sprintf "<maxpool2d %s%s%s>" (Pshape.to_string out_shape)
              (pair_to_string "k" kernel_size) (pair_to_string "s" stride)

          method upstream = upstream

          method kernel_size = kernel_size

          method stride = stride

          method boundary_mode = boundary_mode

          method boundary_overflow = (overflow_s1, overflow_s0)

          method boundary_underflow = (underflow_s1, underflow_s0)

          method boundary_padding = (padding_s1, padding_s0)

          method boundary_lost = (lost_s1, lost_s0)
        end
      in
      fun ?id upstream -> instanciate id (downcast upstream)

    let padding2d ?v:value width =
      let value = (value :> [ `Constant of float | `Reflection | `Replication ] option) in
      let value = match value with None -> `Constant 0. | Some v -> v in

      let width =
        match width with
        | [ w ] -> ((w, w), (w, w))
        | [ w; w' ] -> ((w, w), (w', w'))
        | [ t; b; l; r ] -> ((t, b), (l, r))
        | _ -> failwith "width should contain 1, 2 or 4 elements"
      in
      let rec instanciate id upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype = upstream#out_dtype in
        let out_shape = upstream#out_shape in
        if Pshape.ndim out_shape <> 4 then
          invalid_arg "In padding2d: upstream should have 4 dimensions";

        let out_shape =
          let open Pshape.Size in
          let open Pshape.Size.Infix in
          let (t, b), (l, r) = width in
          let out_shape = Pshape.set out_shape `S0 (Pshape.get out_shape `S0 + K l + K r) in
          let out_shape = Pshape.set out_shape `S1 (Pshape.get out_shape `S1 + K t + K b) in
          out_shape
        in

        object (self : padding2d)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Padding2d self

          method out_shape = out_shape |> Pshape.Open.to_layout Pshape.Layout.Sym4d

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "padding2d#copy takes 1 upstream"

          method id = id

          method layer_name = "padding2d"

          method to_string =
            if self#is_symmetric then
              Printf.sprintf "<padding2d %s %d>" (Pshape.to_string out_shape) self#left
            else
              Printf.sprintf "<padding2d %s (%d, %d), (%d %d)>" (Pshape.to_string out_shape)
                self#top self#bottom self#left self#right

          method upstream = upstream

          method value = value

          method width = width

          method top = width |> fst |> fst

          method bottom = width |> fst |> snd

          method left = width |> snd |> fst

          method right = width |> snd |> snd

          method is_symmetric = self#right = self#left && self#top = self#bottom

          method none_negative = self#right > 0 && self#left > 0 && self#top > 0 && self#bottom > 0

          method none_positive = self#right < 0 && self#left < 0 && self#top < 0 && self#bottom < 0
        end
      in
      fun ?id upstream -> instanciate id (downcast upstream)

    let astype dtype =
      let rec instanciate id upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let shape = upstream#out_shape in
        object (self : astype)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Astype self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "astype#copy takes 1 upstream"

          method id = id

          method layer_name = "astype"

          method to_string = Printf.sprintf "<astype %s>" (Pshape.to_string shape)

          method upstream = upstream
        end
      in
      fun ?id upstream -> instanciate id (downcast upstream)

    let conv2d2 ?g:(group_count = 1) ?s:(stride = (1, 1)) ?d:(dilation = (1, 1)) ?b:boundary_mode =
      let boundary_mode = Option.value ~default:`Same (boundary_mode :> boundary_mode option) in

      if group_count <= 0 then failwith "In conv2d2: group_count should be >= 1";
      if fst stride <= 0 || snd stride <= 0 then failwith "In conv2d2: stride should be >= 1";
      if fst dilation <= 0 || snd dilation <= 0 then failwith "In conv2d2: dilation should be >= 1";
      let rec instanciate id weights x =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let wshape =
          weights#out_shape |> Pshape.Open.to_layout Pshape.Layout.Abs4d |> Pshape.to_total
        in
        let xshape = x#out_shape |> Pshape.Open.to_layout Pshape.Layout.Sym4d in
        let kernel_size =
          let (Pshape.Size.K ky) = Pshape.get wshape (`Idx 0) in
          let (Pshape.Size.K kx) = Pshape.get wshape (`Idx 1) in
          (ky, kx)
        in
        let (Pshape.Size.K in_filters) = Pshape.get wshape (`Idx 2) in
        let (Pshape.Size.K out_group_filters) = Pshape.get wshape (`Idx 3) in
        let dtype =
          match weights#out_dtype with
          | #float_dtype as dtype -> dtype
          | _ -> invalid_arg "In conv2d2: Can only be applied on floating point dtype"
        in

        if x#out_dtype <> dtype then failwith "In conv2d2: The inputs disagree on dtype";
        if Pshape.get xshape `C <> Pshape.Size.K in_filters then
          failwith "In conv2d2: The inputs disagree on the number of channels";
        let out_filters = out_group_filters * group_count in
        let overflow_s0, underflow_s0, padding_s0, lost_s0, out_size_s0 =
          _expand_boundary_mode boundary_mode
            (Pshape.get xshape `S0)
            (snd kernel_size) (snd dilation) (snd stride)
        in
        let overflow_s1, underflow_s1, padding_s1, lost_s1, out_size_s1 =
          _expand_boundary_mode boundary_mode
            (Pshape.get xshape `S1)
            (fst kernel_size) (fst dilation) (fst stride)
        in
        let out_shape = xshape in
        let out_shape = Pshape.set out_shape `C (Pshape.Size.K out_filters) in
        let out_shape = Pshape.set out_shape `S0 out_size_s0 in
        let out_shape = Pshape.set out_shape `S1 out_size_s1 in

        if fst kernel_size <= 0 || snd kernel_size <= 0 then
          failwith "In conv2d2: kernel_size should be >= 1";
        if in_filters mod group_count <> 0 then
          failwith "In conv2d2: group_count should divide in_filters";

        object (self : conv2d)
          method upstreams = [ weights; x ]

          method classify_node = `Node21 self

          method classify_layer = `Conv2d self

          method out_shape = out_shape |> Pshape.Open.to_layout Pshape.Layout.Sym4d

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ weights; x ] -> instanciate (Some id) weights x
            | _ -> invalid_arg "conv2d#copy takes 2 upstreams"

          method id = id

          method layer_name = "conv2d"

          method to_string =
            let pair_to_string ?(tilde = true) ?(show_one = false) tag (y, x) =
              let header = if tilde then " ~" else " " in
              if (not show_one) && y = 1 && x = 1 then ""
              else if x <> y then Printf.sprintf "%s%s:%dx%d" header tag y x
              else Printf.sprintf "%s%s:%d" header tag y
            in
            let filters_str =
              match (group_count, in_filters = group_count) with
              | 1, _ -> Printf.sprintf " filters:%d->%d" in_filters out_filters
              | _, true -> Printf.sprintf " depthwise filters:%d->%d" in_filters out_filters
              | _, _ -> Printf.sprintf " ~g:%d filters:%d->%d" group_count in_filters out_filters
            in
            let padding_str =
              match boundary_mode with
              | `Same -> " ~b:same"
              | `Valid -> " ~b:valid"
              | `Assert_fit -> " ~b:assert_fit"
              | `Pad_fit -> " ~b:padfit"
            in
            Printf.sprintf "<conv2d %s%s%s%s%s%s>" filters_str (Pshape.to_string out_shape)
              (pair_to_string ~tilde:false ~show_one:true "k" kernel_size)
              (pair_to_string "s" stride) (pair_to_string "d" dilation) padding_str

          method upstream0 = weights

          method upstream1 = x

          method kernel_size = kernel_size

          method stride = stride

          method dilation = dilation

          method in_filters = in_filters

          method out_filters = out_filters

          method group_count = group_count

          method in_group_filters = in_filters / group_count

          method out_group_filters = out_filters / group_count

          method is_grouped = group_count > 1

          method is_depthwise = group_count = in_filters

          method is_pointwise = kernel_size = (1, 1)

          method is_dilated =
            let y, x = dilation in
            y > 1 || x > 1

          method is_strided =
            let y, x = stride in
            y > 1 || x > 1

          method kernel_shape = wshape

          method boundary_mode = boundary_mode

          method boundary_overflow = (overflow_s1, overflow_s0)

          method boundary_underflow = (underflow_s1, underflow_s0)

          method boundary_padding = (padding_s1, padding_s0)

          method boundary_lost = (lost_s1, lost_s0)
        end
      in
      fun ?id weights x -> instanciate id (downcast weights) (downcast x)

    let tensordot mapping0 mapping1 =
      let mapping0 = (mapping0 :> (Pshape.Axis.t * Pshape.Axis.t option) list) in
      let mapping1 = (mapping1 :> (Pshape.Axis.t * Pshape.Axis.t option) list) in

      let contracted_axes0 =
        List.filter_map
          (fun (ax, opt) -> match opt with None -> Some ax | Some _ -> None)
          mapping0
      in
      let contracted_axes1 =
        List.filter_map
          (fun (ax, opt) -> match opt with None -> Some ax | Some _ -> None)
          mapping1
      in
      let rec instanciate id x0 x1 =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let shape0, shape1 = (x0#out_shape, x1#out_shape) in
        let shape = Pshape.contract mapping0 mapping1 shape0 shape1 in
        let dtype =
          match x0#out_dtype with
          | #float_dtype as dtype -> dtype
          | _ -> invalid_arg "In tensordot: Can only be applied on floating point dtype"
        in

        if x1#out_dtype <> dtype then failwith "In tensordot: The inputs disagree on dtype";

        object (self : tensordot)
          method upstreams = [ x0; x1 ]

          method classify_node = `Node21 self

          method classify_layer = `Tensordot self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ x0; x1 ] -> instanciate (Some id) x0 x1
            | _ -> invalid_arg "tensordot#copy takes 1 upstream"

          method id = id

          method layer_name = "tensordot"

          method to_string =
            let contracted =
              List.map2
                (fun ax0 ax1 ->
                  Printf.sprintf "%s/%s" (Pshape.Axis.to_string ax0) (Pshape.Axis.to_string ax1))
                contracted_axes0 contracted_axes1
              |> String.concat ", "
            in
            Printf.sprintf "<tensordot %s from %s.%s on [%s]" (Pshape.to_string shape)
              (Pshape.to_string shape0) (Pshape.to_string shape1) contracted

          method upstream0 = x0

          method upstream1 = x1

          method mapping0 = mapping0

          method mapping1 = mapping1

          method contracted_axes0 = contracted_axes0

          method contracted_axes1 = contracted_axes1
        end
      in
      fun ?id x0 x1 -> instanciate id (downcast x0) (downcast x1)

    (* 3. Syntaxic sugars ************************************************************************ *)

    let cropping2d ?id width =
      let id = match id with None -> Id.create_default () | Some id -> id in
      List.map
        (fun x ->
          if x < 0 then invalid_arg "In cropping2d: width must be positive";
          -x)
        width
      |> padding2d ~id

    let dense ?id new_sizes ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let new_sizes = (new_sizes :> (Pshape.Axis.t * int) list) in
      let init = Option.value (init :> Init.float_init option) ~default:`Tanh in
      let optimizer = Option.value (optimizer :> optimizer option) ~default:`Sgd in
      let upstream = downcast upstream in

      let id = match id with None -> Id.create_default () | Some id -> id in
      let shape = upstream#out_shape in
      let contracted_axes = List.map fst new_sizes in
      let kept_axes =
        List.filter_map
          (fun ax -> if List.mem ax contracted_axes then None else Some ax)
          (Pshape.axes shape)
      in
      if List.length contracted_axes = 0 then invalid_arg "In dense: At least one axis required";
      if List.length contracted_axes <> (List.sort_uniq compare contracted_axes |> List.length) then
        invalid_arg "In dense: Axis provided twice";
      let dimensions =
        List.map
          (fun (axis, size1) ->
            match Pshape.get shape axis with
            | Pshape.Size.K size0 -> [ size0; size1 ]
            | Pshape.Size.U -> invalid_arg "In dense: Can't contract an unknown axis")
          new_sizes
        |> List.concat |> Array.of_list
      in
      let mapping_up =
        List.map (fun ax -> (ax, None)) contracted_axes
        @ List.map (fun ax -> (ax, Some ax)) kept_axes
      in
      let mapping_w =
        List.mapi
          (fun i axis -> [ (`Idx (i * 2), None); (`Idx ((i * 2) + 1), Some axis) ])
          contracted_axes
        |> List.concat
      in
      let dtype =
        match upstream#out_dtype with
        | #float_dtype as dtype -> dtype
        | _ -> invalid_arg "In dense: Can only be applied on floating point dtype"
      in
      let weights =
        match (dtype, init) with
        | `Float32, (#Init.float32_init as init) -> parameter32 ~rng dimensions init optimizer
        | `Float64, #Init.float64_init -> failwith "not implemented"
        | _, _ -> invalid_arg "In dense: init is incompatible with upstream's dtype"
      in
      tensordot ~id mapping_up mapping_w upstream (weights :> network)

    let conv2d ?id filters kernel_size ?s:(stride = (1, 1)) ?d:(dilation = (1, 1)) ?b:boundary_mode
        ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let filters = (filters :> [ `Full of int | `Depthwise of int | `Grouped of int * int ]) in
      let init = Option.value (init :> Init.float_init option) ~default:`Tanh in
      let optimizer = Option.value (optimizer :> optimizer option) ~default:`Sgd in
      let upstream = downcast upstream in
      let boundary_mode = Option.value ~default:`Same (boundary_mode :> boundary_mode option) in

      let id = match id with None -> Id.create_default () | Some id -> id in
      let dtype =
        match upstream#out_dtype with
        | #float_dtype as dtype -> dtype
        | _ -> invalid_arg "In conv2d: Can only be applied on floating point dtype"
      in
      let shape = upstream#out_shape |> Pshape.Open.to_layout Pshape.Layout.Sym4d in
      let in_filters =
        match Pshape.get shape `C with
        | Pshape.Size.U -> failwith "In conv2d: input network should have a known channel size"
        | Pshape.Size.K v -> v
      in
      let group_count, out_filters =
        match filters with
        | `Full v ->
            if v < 0 then failwith "In conv2d: out_filters should not be negative";
            (1, v)
        | `Depthwise v ->
            if v < 1 then failwith "In conv2d: expansion rate >= 1";
            (in_filters, in_filters * v)
        | `Grouped (g, f) ->
            if f < 0 then failwith "In conv2d: out_filters should not be negative";
            (g, f)
      in
      if out_filters mod group_count <> 0 then
        failwith "In conv2d: group_count should divide out_filters";
      let out_group_filters = out_filters / group_count in
      let dimensions = [| fst kernel_size; snd kernel_size; in_filters; out_group_filters |] in

      let weights =
        match (dtype, init) with
        | `Float32, (#Init.float32_init as init) -> parameter32 ~rng dimensions init optimizer
        | `Float64, #Init.float64_init -> failwith "not implemented"
        | _, _ -> invalid_arg "In conv2d: init is incompatible with upstream's dtype"
      in
      conv2d2 ~id ~g:group_count ~s:stride ~d:dilation ~b:boundary_mode
        (weights :> network)
        upstream

    let bias ?id ?axes ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let axes = Option.value (axes :> Pshape.Axis.t list option) ~default:[ `C ] in
      let init = Option.value (init :> Init.float_init option) ~default:(`FloatConstant 0.) in
      let optimizer = Option.value (optimizer :> optimizer option) ~default:`Sgd in
      let upstream = downcast upstream in

      let id = match id with None -> Id.create_default () | Some id -> id in
      let shape = upstream#out_shape in
      if List.length axes = 0 then invalid_arg "In bias: At least one axis required";
      if List.length axes <> List.length (List.sort_uniq compare axes) then
        invalid_arg "In bias: Axis provided twice";
      let dimensions =
        List.map
          (fun axis ->
            match Pshape.get shape axis with
            | Pshape.Size.K i -> i
            | Pshape.Size.U -> invalid_arg "In bias: Can't apply bias on unknown axis")
          axes
        |> Array.of_list
      in
      let mapping = List.mapi (fun i axis -> (`Idx i, axis)) axes in

      let dtype =
        match upstream#out_dtype with
        | #float_dtype as dtype -> dtype
        | _ -> invalid_arg "In bias: Can only be applied on floating point input dtype"
      in
      ( match (dtype, init) with
      | `Float32, (#Init.float32_init as init) -> parameter32 ~rng dimensions init optimizer
      | `Float64, #Init.float64_init -> failwith "not implemented"
      | _, _ -> invalid_arg "In bias: init is incompatible with upstream's dtype" )
      |> reorder_axes mapping |> downcast
      |> fun w -> sum ~id [ upstream; w ]

    let scale ?id ?axes ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let axes = Option.value (axes :> Pshape.Axis.t list option) ~default:[ `C ] in
      let init = Option.value (init :> Init.float_init option) ~default:(`FloatConstant 1.) in
      let optimizer = Option.value (optimizer :> optimizer option) ~default:`Sgd in
      let upstream = downcast upstream in

      let id = match id with None -> Id.create_default () | Some id -> id in
      let shape = upstream#out_shape in
      if List.length axes = 0 then invalid_arg "In scale: At least one axis required";
      if List.length axes <> List.length (List.sort_uniq compare axes) then
        invalid_arg "In scale: Axis provided twice";
      let dimensions =
        List.map
          (fun axis ->
            match Pshape.get shape axis with
            | Pshape.Size.K i -> i
            | Pshape.Size.U -> invalid_arg "In scale: Can't apply scale on unknown axis")
          axes
        |> Array.of_list
      in
      let mapping = List.mapi (fun i axis -> (`Idx i, axis)) axes in

      let dtype =
        match upstream#out_dtype with
        | #float_dtype as dtype -> dtype
        | _ -> invalid_arg "In scale: Can only be applied on floating point input dtype"
      in
      ( match (dtype, init) with
      | `Float32, (#Init.float32_init as init) -> parameter32 ~rng dimensions init optimizer
      | `Float64, #Init.float64_init -> failwith "not implemented"
      | _, _ -> invalid_arg "In scale: init is incompatible with upstream's dtype" )
      |> reorder_axes mapping |> downcast
      |> fun w -> prod ~id [ upstream; w ]

    let batch_norm ?id ?(rng = State.get_state ()) ?(affine = true) ?stats upstream =
      let stats =
        Option.value
          ~default:(`Moving_exp (1e-5, 0.1))
          ( stats
            :> [ `Batch of float | `Moving of float * float | `Moving_exp of float * float ] option
            )
      in

      let upstream = downcast upstream in
      let id = match id with None -> Id.create_default () | Some id -> id in
      if affine then normalisation ~id ~rng ~stats [ (`C, 1) ] upstream |> scale |> bias |> downcast
      else normalisation ~id ~rng ~stats [ (`C, 1) ] upstream |> downcast
  end

  module Builder = Make_builder (struct
    let get_state = Random.get_state
  end)

  let create_builder ?rng () =
    let rng = match rng with Some rng -> rng | None -> Random.State.make_self_init () in
    ( module Make_builder (struct
      let get_state () = rng
    end) : BUILDER )
end

include Make (Bigarray_tensor) (String_option_id)
