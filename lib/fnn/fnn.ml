(* Functional Neural Network (fnn)


# Network objects
A network object is both a neural network layer and a neural network node. As a layer it contains
the definition (and the frozen mutable states, if any) of that layer. As a node it contains pointers
to the upstream node(s) (if any).

A network is immutable.

A network is a purely symbolic graph, it is totally agnostic to any deep learning training or
inference procedure.

A network is a directed acyclic graph, it is impossible to induce loops when building one.

The network object type is contained in a {| Network(Tensor, Id) |} module. {| ('a, 'b) Tensor.t |}
is the type of a frozen tensor stored in layers such as normalisation or parameter32, {| Id.t |} is the
type of the identifier of a node. In the default module
{| Tensor.t = ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t |} and {| Id.t = string option |}.

A node has a node_type that defines the number of input nodes:
- A `node01` has no upstream parent.
- A `node11` has 1 upstream parent.
- A `node21` has 2 upstream parent.
- A `noden1` has n upstream parents, determined when building the network.

In order to improve separation of concerns and allow complex network definitions, the network
parameters are standalone layers that are inputs to the nodes that use them. For example:
- In the classic convolution use case, a {| conv2d |} node accesses its kernel weights through a
  {| parameter32 |} input node.
- In some meta-learning use convolution use cases the kernel weights are the result of a
  computation.
- In some other use-cases you may want to reuse a kernel weight several times throughout a network.

---

# Network building

To build a network you have to use a {| Builder(State) |} module. The State provides a global random
number generator state. All the functions available in {| Builder |} are meant to be user friendly.

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

# Tensors stored inside layers

When building a stateful layer, the tensors are not directly allocated, only a recipe to build
them deterministically is kept. A tensor is stored inside a layer only after calling a `#replicate`
method.

For example, when constructing a `parameter32` layer, the `init` parameter of type `Init.t`
will be internally converted to a `Init.Deterministic.t` by sampling an initialization seed using
`Builder.state`. When calling `#tensor` on that layer, the tensor will be deterministically computed
and returned (but not stored, remember that the layers are immutable). After calling `#replicate` on
that layer, `#tensor` will return the tensor passed to `#replicate` instead of initializing a new
tensor.

This features makes the newly created networks very light.

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
- rename all `replicate` methods to `update/amend/upgrade/something else`, rename all `copy` methods/function to `replicate`
- parameter32 takes a shape (and init too)
- change relu and softmax layer to activation with algorithm = [ `Relu; `Softmax of Pshape.Axis.t ]
- reshape (and test reshape/dense at end of network) idea: ([axis list] * Size.t) list. k->k, (u+k)->u, u->u, (k->u auto infer to ->k), NO(u->k)
- add `Ridx of int alongside `Idx of int? Only as inputs?
- slice
- node01 noise layer
- non-trainable node01 layers (bilinear upsampling kernel) (`buffer_float32` trainable=False, `constant_float32` ?)
- custom layers:
  - Don't care because user can create its own network objects?
    But what about the variants like classified_layer and layer_type? Make those open / extensible?
  - Create an Fnn.custom_layer class and a `Builder.custom custom_defn ...` function?
  Extensible GADT?
- Transform `parameter32` to `parameter`?
- Rewrite the library without the use of ocaml objects because:
  - Those are impossible to serialize in js
  - Polymorphic variants and top-level functions could provide roughly the same properties:
    - e.g. `val kernel_size : [< `Maxpool2d | `Conv2d ] network -> int * int`
    Does this mean that `val upstreams : [< any_network ] network -> any_network list` requires
      a massive pattern matching?
        - Is this pattern matching a problem?
        - I picked ocaml object to avoid those pattern matching
        - Maybe this pattern matching can be avoided by giving a proper definition to 'a network

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
  module Tensor = Tensor
  module Id = Id

  type 'b float_tensor = (float, 'b) Tensor.t

  type float32_tensor = Bigarray.float32_elt float_tensor

  type float64_tensor = Bigarray.float64_elt float_tensor

  type int64_tensor = (int64, Bigarray.int64_elt) Tensor.t

  type optimizer32 =
    [ `Sgd | `Adam of float * float * float * int * float32_tensor * float32_tensor ]

  type optimizer_conf = [ `Sgd | `Adam of float * float * float ]

  type normalization_algo =
    [ `Local of float
    | `Global32 of float * int * float32_tensor * float32_tensor
    | `Exp_moving32 of float * float * float32_tensor * float32_tensor
    | `Global64 of float * int * float64_tensor * float64_tensor
    | `Exp_moving64 of float * float * float64_tensor * float64_tensor ]

  type normalization_algo_conf =
    [ `Local of float | `Global of float | `Exp_moving of float * float ]

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
    | `Padding of padding
    | `Transpose of transpose
    | `Parameter32 of parameter32 ]

  and classified_layer01 = [ `Input of input | `Parameter32 of parameter32 ]

  and classified_layer11 =
    [ `Softmax of softmax
    | `Relu of relu
    | `Astype of astype
    | `Normalisation of normalisation
    | `Maxpool2d of maxpool2d
    | `Padding of padding
    | `Transpose of transpose ]

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
    ; replicate : ?id:Id.t -> float32_tensor -> optimizer32 -> parameter32
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; init : Init.float32
    ; init_deterministic : Init.Deterministic.float32
    ; tensor : float32_tensor
    ; tensor_opt : float32_tensor option
    ; optimizer_conf : optimizer_conf
    ; optimizer : optimizer32
    ; optimizer_opt : optimizer32 option
    ; numel : int >

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
    ; upstream : network
    ; dtype : dtype >

  and normalisation =
    < upstreams : network list
    ; classify_node : [ `Node11 of normalisation ]
    ; classify_layer : [ `Normalisation of normalisation ]
    ; out_shape : Pshape.any
    ; out_dtype : float_dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> normalisation
    ; replicate : ?id:Id.t -> normalization_algo -> network -> normalisation
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network
    ; axes : Pshape.Axis.t list
    ; algorithm_conf : normalization_algo_conf
    ; algorithm : normalization_algo
    ; algorithm_opt : normalization_algo option
    ; is_batch_norm : bool
    ; is_layer_norm : bool
    ; is_instance_norm : bool
    ; is_group_norm : bool >

  and transpose =
    < upstreams : network list
    ; classify_node : [ `Node11 of transpose ]
    ; classify_layer : [ `Transpose of transpose ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> transpose
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

  and padding =
    < upstreams : network list
    ; classify_node : [ `Node11 of padding ]
    ; classify_layer : [ `Padding of padding ]
    ; out_shape : Pshape.any
    ; out_dtype : dtype
    ; stateful : bool
    ; copy : ?id:Id.t -> ?reinit:bool -> ?rng:Random.State.t -> network list -> padding
    ; id : Id.t
    ; layer_name : string
    ; to_string : string
    ; upstream : network
    ; value : [ `Constant of float | `Reflection | `Replication ]
    ; paddings_of_axis : 'a. ([< Pshape.Axis.t ] as 'a) -> int * int
    ; axes : Pshape.Axis.t list
    ; is_uniform : bool
    ; is_symmetric : bool
    ; is_padding : bool
    ; is_cropping : bool
    ; is_mixed : bool
    ; is_noop : bool >

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
    ; contracted_axes1 : Pshape.Axis.t list
    ; input_axis_of_output_axis :
        Pshape.Axis.t -> [ `Left of Pshape.Axis.t | `Right of Pshape.Axis.t ] >

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
    | Transpose : transpose layer_type
    | Maxpool2d : maxpool2d layer_type
    | Conv2d : conv2d layer_type
    | Padding : padding layer_type
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
    | `Transpose node -> (node :> network)
    | `Maxpool2d node -> (node :> network)
    | `Conv2d node -> (node :> network)
    | `Padding node -> (node :> network)
    | `Parameter32 node -> (node :> network)

  (* The network objects cannot be implicitly downcasted to a `network` type, an explicit cast is
     required, this functions provides a way to downcast a layer.
  *)
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
    | `Transpose node -> (node : transpose :> network)
    | `Maxpool2d node -> (node : maxpool2d :> network)
    | `Conv2d node -> (node : conv2d :> network)
    | `Padding node -> (node : padding :> network)
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
        | Transpose, `Transpose node -> Hashtbl.add tbl node true
        | Maxpool2d, `Maxpool2d node -> Hashtbl.add tbl node true
        | Conv2d, `Conv2d node -> Hashtbl.add tbl node true
        | Padding, `Padding node -> Hashtbl.add tbl node true
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

  let parameters : _ any list -> < network ; numel : int > list =
   fun network_ends ->
    List.concat [ (find_layers Parameter32 network_ends :> < network ; numel : int > list) ]

  let iter_top_down : (network -> unit) -> _ any list -> unit =
   fun f network_ends ->
    let g follow (node : network) =
      List.iter follow node#upstreams;
      f node
    in
    memoized_walk_map g network_ends |> ignore

  let iter_bottom_up : (network -> unit) -> _ any list -> unit =
   fun f network_ends ->
    let g follow (node : network) =
      f node;
      List.iter follow node#upstreams
    in
    memoized_walk_map g network_ends |> ignore

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
          certain conditions. (see below)
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
      [< Init.float32 ] ->
      [< optimizer_conf ] ->
      parameter32

    val sum : ?id:Id.t -> _ any list -> sum

    val prod : ?id:Id.t -> _ any list -> prod

    val concatenate : [< Pshape.Axis.t ] -> ?id:Id.t -> _ any list -> concatenate

    val softmax : Pshape.Axis.t -> ?id:Id.t -> _ any -> softmax

    val relu : ?id:Id.t -> _ any -> relu

    val astype : dtype -> ?id:Id.t -> _ any -> astype

    val normalisation :
      [< Pshape.Axis.t ] list ->
      ?algo_conf:[< normalization_algo_conf ] ->
      ?id:Id.t ->
      _ any ->
      normalisation

    val transpose :
      ?ndim:int ->
      ?mapping:([< Pshape.Axis.t ] * [< Pshape.Axis.t ]) list ->
      ?id:Id.t ->
      _ any ->
      transpose

    val maxpool2d :
      ?b:[< boundary_mode ] -> ?s:int * int -> int * int -> ?id:Id.t -> _ any -> maxpool2d

    val padding :
      ?v:[< `Constant of float | `Reflection | `Replication ] ->
      ([< Pshape.Axis.t ] * int list) list ->
      ?id:Id.t ->
      _ any ->
      padding
    (** width can be negative to perform cropping *)

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

    val padding2d :
      ?v:[< `Constant of float | `Reflection | `Replication ] ->
      int list ->
      ?id:Id.t ->
      _ any ->
      padding

    val cropping2d : ?id:Id.t -> int list -> _ any -> padding

    val dense :
      ?id:Id.t ->
      ([< Pshape.Axis.t ] * int) list ->
      ?i:[< Init.float ] ->
      ?o:[< optimizer_conf ] ->
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
      ?i:[< Init.float ] ->
      ?o:[< optimizer_conf ] ->
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
      ?i:[< Init.float ] ->
      ?o:[< optimizer_conf ] ->
      ?rng:Random.State.t ->
      _ any ->
      sum
    (**
      Syntaxic sugar for:
      {[
      Builder.sum
        [ Builder.parameter(32|64) [| axis0_size; ... |] init optimizer
          |> Builder.transpose [ axis0_mapping; ... ]
        ; upstream ]
      ]} *)

    val scale :
      ?id:Id.t ->
      ?axes:[< Pshape.Axis.t ] list ->
      ?i:[< Init.float ] ->
      ?o:[< optimizer_conf ] ->
      ?rng:Random.State.t ->
      _ any ->
      prod
    (**
      Syntaxic sugar for:
      {[
      Builder.prod
        [ Builder.parameter(32|64) [| axis0_size; ... |] init optimizer
          |> Builder.transpose [ axis0_mapping; ... ]
        ; upstream ]
      ]} *)

    val batch_norm :
      ?id:Id.t -> ?affine:bool -> ?algo_conf:[< normalization_algo_conf ] -> _ any -> network
  end

  module Make_builder (State : STATE) : BUILDER = struct
    (* 1. Tools ********************************************************************************* *)

    let _expand_boundary_mode boundary_mode size kernel_size dilation stride =
      let open Pshape.Size in
      let open Pshape.Size.Infix in
      let kernel_size = K kernel_size in
      let stride = K stride in
      let dilation = K dilation in
      let span = K 1 + ((kernel_size - K 1) * dilation) in

      let fail why =
        let bm =
          match boundary_mode with
          | `Valid -> "`Valid"
          | `Pad_fit -> "`Pad_fit"
          | `Assert_fit -> "`Assert_fit"
          | `Same -> "`Same"
        in
        Printf.sprintf
          ( "boundary_mode=%s, kernel_size=%s, dilation=%s, stride=%s "
          ^^ "is incompatible with dimension-size=%s because %s" )
          bm (to_string kernel_size) (to_string dilation) (to_string stride) (to_string size) why
        |> invalid_arg
      in

      let overflow, underflow =
        match (size, span) with
        | K 0, _ -> fail "size 0 input is too small to apply any kernel"
        | K size, K span when size < span -> (
            match boundary_mode with
            | `Same | `Pad_fit -> (K 0, K span - K size)
            | `Valid | `Assert_fit ->
                fail "input size is too small to apply this kernel without padding" )
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
            (match overflow with K 0 | U -> () | _ -> fail "kernel doesn't fit");
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
        | None, None | Some _, Some _ -> invalid_arg "Provide only head or only tail"
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

    let parameter32 ?id ?rng dimensions init optimizer_conf =
      let init = (init :> Init.float32) in
      let optimizer_conf = (optimizer_conf :> optimizer_conf) in
      ( match optimizer_conf with
      | `Sgd -> ()
      | `Adam (epsilon, beta1, beta2) ->
          if epsilon <= 0. then invalid_arg "In parameter32: epsilon should be > 0.";
          if beta1 <= 0. || beta1 >= 1. then
            invalid_arg "In parameter32: beta1 should be between 0 and 1 (excluded)";
          if beta2 <= 0. || beta2 >= 1. then
            invalid_arg "In parameter32: beta2 should be between 0 and 1 (excluded)";
          () );
      let rec instanciate ?id ?rng dimensions optimizer_conf tensor_opt optim_opt =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let init_deter = Init.float32_to_deterministic ?rng init in

        let get_tensor () =
          match tensor_opt with
          | Some v -> v
          | None -> Init.Deterministic.run init_deter Bigarray.Float32 dimensions |> Tensor.of_ba
        in
        let get_optim () =
          match optim_opt with
          | Some v -> v
          | None -> (
              match optimizer_conf with
              | `Sgd -> `Sgd
              | `Adam (epsilon, beta1, beta2) ->
                  let rgrad =
                    Init.run (`Float_constant 0.) Bigarray.Float32 dimensions |> Tensor.of_ba
                  in
                  let rgrad_sq =
                    Init.run (`Float_constant 0.) Bigarray.Float32 dimensions |> Tensor.of_ba
                  in
                  `Adam (epsilon, beta1, beta2, 0, rgrad, rgrad_sq) )
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
            if reinit then instanciate ~id ?rng dimensions optimizer_conf None None
            else instanciate ~id dimensions optimizer_conf tensor_opt optim_opt

          method replicate ?(id = self#id) tensor optim =
            let newdims = Tensor.dimensions tensor in
            ( match optim with
            | `Sgd -> ()
            | `Adam (_, _, _, _, rgrad, rgrad_sq) ->
                if Tensor.dimensions rgrad <> newdims then
                  invalid_arg "In parameter32#replicate: bad rgrad shape";
                if Tensor.dimensions rgrad_sq <> newdims then
                  invalid_arg "In parameter32#replicate: bad rgrad_sq shape" );
            let optimizer_conf =
              match optim with
              | `Sgd -> `Sgd
              | `Adam (epsilon, beta1, beta2, _, _, _) -> `Adam (epsilon, beta1, beta2)
            in
            instanciate ~id newdims optimizer_conf (Some tensor) (Some optim)

          method id = id

          method layer_name = "parameter32"

          method to_string = Printf.sprintf "<parameter32 %s>" (Pshape.to_string shape)

          method init = init

          method init_deterministic = init_deter

          method tensor = get_tensor ()

          method tensor_opt = tensor_opt

          method optimizer_conf = optimizer_conf

          method optimizer = get_optim ()

          method optimizer_opt = optim_opt

          method numel =
            let (K n) = Pshape.numel shape in
            n
        end
      in
      instanciate ?id ?rng dimensions optimizer_conf None None

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

    let normalisation axes ?algo_conf =
      let algo_conf =
        Option.value
          ~default:(`Exp_moving (1e-5, 0.99))
          (algo_conf :> normalization_algo_conf option)
      in
      let axes = (axes :> Pshape.Axis.t list) in

      (* Check: axes *)
      if List.length axes = 0 then invalid_arg "In normalisation: axes should not be an empty list";
      if List.length axes <> List.length (List.sort_uniq compare axes) then
        invalid_arg "In normalisation: Axis provided twice";

      (* Check: algorithm *)
      ( match algo_conf with
      | `Local epsilon ->
          if epsilon <= 0. then invalid_arg "In normalisation: epsilon should be > 0"
      | `Global epsilon ->
          if epsilon <= 0. then invalid_arg "In normalisation: epsilon should be > 0"
      | `Exp_moving (epsilon, momentum) ->
          if epsilon <= 0. then invalid_arg "In normalisation: epsilon should be > 0";
          if momentum <= 0. || momentum >= 1. then
            invalid_arg "In normalisation: momentum should be > 0 and < 1" );

      let rec instanciate ?id algo_conf algo_opt upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype =
          match upstream#out_dtype with
          | #float_dtype as dtype -> dtype
          | #int_dtype -> invalid_arg "In normalisation: Can't be applied on an integer tensor"
        in

        (* Extraction: input sizes *)
        let shape = upstream#out_shape in
        let dimensions =
          List.map
            (fun ax ->
              match Pshape.get_opt shape ax with
              | Some (Pshape.Size.K i) -> i
              | Some Pshape.Size.U ->
                  invalid_arg "In normalisation: can't normalize along an unknown axis"
              | None -> invalid_arg "In normalisation: bad axis in axes")
            axes
          |> Array.of_list
        in

        let get_algo () =
          match algo_opt with
          | Some v -> v
          | None -> (
              match (dtype, algo_conf) with
              | `Float32, `Local epsilon | `Float64, `Local epsilon -> `Local epsilon
              | `Float32, `Global epsilon ->
                  let avg =
                    Init.run (`Float_constant 0.) Bigarray.Float32 dimensions |> Tensor.of_ba
                  in
                  let var =
                    Init.run (`Float_constant 1.) Bigarray.Float32 dimensions |> Tensor.of_ba
                  in
                  `Global32 (epsilon, 0, avg, var)
              | `Float32, `Exp_moving (epsilon, momentum) ->
                  let avg =
                    Init.run (`Float_constant 0.) Bigarray.Float32 dimensions |> Tensor.of_ba
                  in
                  let var =
                    Init.run (`Float_constant 1.) Bigarray.Float32 dimensions |> Tensor.of_ba
                  in
                  `Exp_moving32 (epsilon, momentum, avg, var)
              | `Float64, `Global epsilon ->
                  let avg =
                    Init.run (`Float_constant 0.) Bigarray.Float64 dimensions |> Tensor.of_ba
                  in
                  let var =
                    Init.run (`Float_constant 1.) Bigarray.Float64 dimensions |> Tensor.of_ba
                  in
                  `Global64 (epsilon, 0, avg, var)
              | `Float64, `Exp_moving (epsilon, momentum) ->
                  let avg =
                    Init.run (`Float_constant 0.) Bigarray.Float64 dimensions |> Tensor.of_ba
                  in
                  let var =
                    Init.run (`Float_constant 1.) Bigarray.Float64 dimensions |> Tensor.of_ba
                  in
                  `Exp_moving64 (epsilon, momentum, avg, var) )
        in
        ( match (dtype, algo_opt) with
        | `Float64, Some (`Global32 _) -> failwith "In normalisation: incompatible dtypes"
        | `Float64, Some (`Exp_moving32 _) -> failwith "In normalisation: incompatible dtypes"
        | `Float32, Some (`Global64 _) -> failwith "In normalisation: incompatible dtypes"
        | `Float32, Some (`Exp_moving64 _) -> failwith "In normalisation: incompatible dtypes"
        | `Float32, Some (`Global32 (_, _, avg, var))
        | `Float32, Some (`Exp_moving32 (_, _, avg, var)) ->
            if Tensor.dimensions avg <> dimensions then
              invalid_arg "In normalisation: Invalid copy, shapes are not compatible";
            if Tensor.dimensions var <> dimensions then
              invalid_arg "In normalisation: Invalid copy, shapes are not compatible"
        | `Float64, Some (`Global64 (_, _, avg, var))
        | `Float64, Some (`Exp_moving64 (_, _, avg, var)) ->
            if Tensor.dimensions avg <> dimensions then
              invalid_arg "In normalisation: Invalid copy, shapes are not compatible";
            if Tensor.dimensions var <> dimensions then
              invalid_arg "In normalisation: Invalid copy, shapes are not compatible"
        | _, None -> ()
        | _, Some (`Local _) -> () );

        object (self : normalisation)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Normalisation self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = true

          method copy ?(id = self#id) ?(reinit = false) ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate ~id algo_conf (if reinit then None else algo_opt) up
            | _ -> invalid_arg "normalisation#copy takes 1 upstream"

          method replicate ?(id = self#id) algorithm upstream =
            let algo_conf =
              match algorithm with
              | `Local epsilon -> `Local epsilon
              | `Global64 (epsilon, _, _, _) | `Global32 (epsilon, _, _, _) -> `Global epsilon
              | `Exp_moving32 (epsilon, momentum, _, _) | `Exp_moving64 (epsilon, momentum, _, _) ->
                  `Exp_moving (epsilon, momentum)
            in
            instanciate ~id algo_conf (Some algorithm) upstream

          method id = id

          method layer_name = "normalisation"

          method to_string = Printf.sprintf "<normalisation %s>" (Pshape.to_string shape)

          method upstream = upstream

          method axes = axes

          method algorithm_conf = algo_conf

          method algorithm = get_algo ()

          method algorithm_opt = algo_opt

          method is_batch_norm = axes = [ `C ]

          method is_layer_norm = axes = [ `N ]

          method is_instance_norm = match axes with [ `N; `C ] | [ `C; `N ] -> true | _ -> false

          method is_group_norm = match axes with [ `N; `C ] | [ `C; `N ] -> true | _ -> false
        end
      in
      fun ?id upstream -> instanciate ?id algo_conf None (downcast upstream)

    let transpose ?ndim ?mapping =
      let rec instanciate id upstream =
        let mapping =
          match mapping with
          | Some mapping ->
              List.map (fun (a, b) -> ((a :> Pshape.Axis.t), (b :> Pshape.Axis.t))) mapping
          | None ->
              let axes = Pshape.axes upstream#out_shape in
              List.combine axes (List.rev axes)
        in
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype = upstream#out_dtype in
        let shape =
          match ndim with
          | Some ndim -> Pshape.transpose ~ndim ~mapping upstream#out_shape
          | None -> Pshape.transpose ~mapping upstream#out_shape
        in
        object (self : transpose)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Transpose self

          method out_shape = shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "transpose#copy takes 1 upstream"

          method id = id

          method layer_name = "transpose"

          method to_string =
            Printf.sprintf "<transpose %s -> %s>"
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
        invalid_arg "In maxpool2d: kernel_size should be >= 1";
      if fst stride <= 0 || snd stride <= 0 then invalid_arg "In maxpool2d: stride should be >= 1";

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

    let padding ?v:value paddings_per_axis =
      let paddings_per_axis = (paddings_per_axis :> (Pshape.Axis.t * int list) list) in
      let value = (value :> [ `Constant of float | `Reflection | `Replication ] option) in
      let value = match value with None -> `Constant 0. | Some v -> v in
      let axes = List.map fst paddings_per_axis in
      let paddings =
        List.map snd paddings_per_axis
        |> List.map (function
             | [ p ] -> (p, p)
             | [ bef; aft ] -> (bef, aft)
             | _ -> invalid_arg "In padding: padding list should contain 1 or 2 integers")
      in
      if List.length axes <> List.length (List.sort_uniq compare axes) then
        invalid_arg "In padding: Duplicate axis";
      let axes, paddings =
        List.combine axes paddings
        |> List.filter_map (fun ((_, (bef, aft)) as pair) ->
               if bef = 0 && aft = 0 then None else Some pair)
        |> List.split
      in

      let rec instanciate id upstream =
        let id = match id with None -> Id.create_default () | Some id -> id in
        let dtype = upstream#out_dtype in
        let out_shape = upstream#out_shape in
        let shape_axes = Pshape.axes out_shape in
        List.iter
          (fun ax ->
            if not (List.mem ax shape_axes) then
              invalid_arg "In padding: Axis missing from input shape")
          axes;

        let out_shape =
          List.combine axes paddings
          |> List.fold_left
               (fun s (ax, (bef, aft)) ->
                 let open Pshape.Size in
                 let open Pshape.Size.Infix in
                 Pshape.set s ax (Pshape.get s ax + K bef + K aft))
               out_shape
        in

        object (self : padding)
          method upstreams = [ upstream ]

          method classify_node = `Node11 self

          method classify_layer = `Padding self

          method out_shape = out_shape

          method out_dtype = dtype

          method stateful = false

          method copy ?(id = self#id) ?reinit:_ ?rng:_ upstreams =
            match upstreams with
            | [ up ] -> instanciate (Some id) up
            | _ -> invalid_arg "padding#copy takes 1 upstream"

          method id = id

          method layer_name = "padding"

          method to_string =
            match (paddings, self#is_uniform) with
            | [], _ -> Printf.sprintf "<padding %s>" (Pshape.to_string out_shape)
            | (bef, aft) :: _, true ->
                Printf.sprintf "<padding %s (%d, %d)>" (Pshape.to_string out_shape) bef aft
            | _, false ->
                List.map (fun (bef, aft) -> Printf.sprintf "%d, %d" bef aft) paddings
                |> String.concat "; "
                |> Printf.sprintf "<padding %s [%s]>" (Pshape.to_string out_shape)

          method upstream = upstream

          method value = value

          method paddings_of_axis axis =
            let axis = (axis :> Pshape.Axis.t) in
            match List.combine axes paddings |> List.find_opt (fun (ax, _) -> ax = axis) with
            | Some (_, paddings) -> paddings
            | None ->
                if List.mem axis shape_axes then (0, 0)
                else invalid_arg "In padding#paddings_of_axis: Unknown axis"

          method axes = axes

          method is_uniform =
            match paddings with
            | [] -> true
            | (bef, aft) :: tl -> List.for_all (fun (bef', aft') -> bef = bef' && aft = aft') tl

          method is_symmetric = List.for_all (fun (bef, aft) -> bef = aft) paddings

          method is_padding = List.for_all (fun (bef, aft) -> bef >= 0 && aft >= 0) paddings

          method is_cropping = List.for_all (fun (bef, aft) -> bef <= 0 && aft <= 0) paddings

          method is_mixed = self#is_padding = false && self#is_cropping = false

          method is_noop = self#is_padding = true && self#is_cropping = true
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

          method dtype = dtype
        end
      in
      fun ?id upstream -> instanciate id (downcast upstream)

    let conv2d2 ?g:(group_count = 1) ?s:(stride = (1, 1)) ?d:(dilation = (1, 1)) ?b:boundary_mode =
      let boundary_mode = Option.value ~default:`Same (boundary_mode :> boundary_mode option) in

      if group_count <= 0 then invalid_arg "In conv2d2: group_count should be >= 1";
      if fst stride <= 0 || snd stride <= 0 then invalid_arg "In conv2d2: stride should be >= 1";
      if fst dilation <= 0 || snd dilation <= 0 then
        invalid_arg "In conv2d2: dilation should be >= 1";
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

        if x#out_dtype <> dtype then invalid_arg "In conv2d2: The inputs disagree on dtype";
        if Pshape.get xshape `C <> Pshape.Size.K in_filters then
          invalid_arg "In conv2d2: The inputs disagree on the number of channels";
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
          invalid_arg "In conv2d2: kernel_size should be >= 1";
        if in_filters mod group_count <> 0 then
          invalid_arg "In conv2d2: group_count should divide in_filters";

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
            Printf.sprintf "<conv2d %s%s%s%s%s%s>" (Pshape.to_string out_shape) filters_str
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

          method is_grouped = group_count > 1 && in_filters > 1

          method is_depthwise = group_count = in_filters && in_filters > 1

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

        if x1#out_dtype <> dtype then invalid_arg "In tensordot: The inputs disagree on dtype";

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

          method input_axis_of_output_axis ax =
            let find_opt_in_mapping mapping =
              let f = function _, Some ax1 when ax1 = ax -> true | _, _ -> false in
              match List.find_opt f mapping with Some (ax, _) -> Some ax | None -> None
            in

            match find_opt_in_mapping mapping0 with
            | Some ax -> `Left ax
            | None -> (
                match find_opt_in_mapping mapping1 with
                | Some ax -> `Right ax
                | None ->
                    "In tensordot#input_axis_of_output_axis: axis doesnt belon to output shape"
                    |> invalid_arg )
        end
      in
      fun ?id x0 x1 -> instanciate id (downcast x0) (downcast x1)

    (* 3. Syntaxic sugars ************************************************************************ *)

    let padding2d ?v p =
      let p =
        match p with
        | [ p ] -> [ (`S1, [ p ]); (`S0, [ p ]) ]
        | [ p; p' ] -> [ (`S1, [ p ]); (`S0, [ p' ]) ]
        | [ t; b; l; r ] -> [ (`S1, [ t; b ]); (`S0, [ l; r ]) ]
        | _ -> invalid_arg "padding should contain 1, 2 or 4 elements"
      in
      match v with None -> padding p | Some v -> padding ~v p

    let cropping2d ?id p =
      let id = match id with None -> Id.create_default () | Some id -> id in
      List.map
        (fun x ->
          if x < 0 then invalid_arg "In cropping2d: padding must be positive";
          -x)
        p
      |> padding2d ~id

    let dense ?id new_sizes ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let new_sizes = (new_sizes :> (Pshape.Axis.t * int) list) in
      let init = Option.value (init :> Init.float option) ~default:`Tanh in
      let optimizer = Option.value (optimizer :> optimizer_conf option) ~default:`Sgd in
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
        | `Float32, (#Init.float32 as init) -> parameter32 ~rng dimensions init optimizer
        | `Float64, #Init.float64 -> failwith "not implemented"
        | _, _ -> invalid_arg "In dense: init is incompatible with upstream's dtype"
      in
      tensordot ~id mapping_up mapping_w upstream (weights :> network)

    let conv2d ?id filters kernel_size ?s:(stride = (1, 1)) ?d:(dilation = (1, 1)) ?b:boundary_mode
        ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let filters = (filters :> [ `Full of int | `Depthwise of int | `Grouped of int * int ]) in
      let init = Option.value (init :> Init.float option) ~default:`Tanh in
      let optimizer = Option.value (optimizer :> optimizer_conf option) ~default:`Sgd in
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
        | Pshape.Size.U -> invalid_arg "In conv2d: input network should have a known channel size"
        | Pshape.Size.K v -> v
      in
      let group_count, out_filters =
        match filters with
        | `Full v ->
            if v < 0 then invalid_arg "In conv2d: out_filters should not be negative";
            (1, v)
        | `Depthwise v ->
            if v < 1 then invalid_arg "In conv2d: expansion rate >= 1";
            (in_filters, in_filters * v)
        | `Grouped (g, f) ->
            if f < 0 then invalid_arg "In conv2d: out_filters should not be negative";
            (g, f)
      in
      if out_filters mod group_count <> 0 then
        invalid_arg "In conv2d: group_count should divide out_filters";
      let out_group_filters = out_filters / group_count in
      let dimensions = [| fst kernel_size; snd kernel_size; in_filters; out_group_filters |] in

      let weights =
        match (dtype, init) with
        | `Float32, (#Init.float32 as init) -> parameter32 ~rng dimensions init optimizer
        | `Float64, #Init.float64 -> failwith "not implemented"
        | _, _ -> invalid_arg "In conv2d: init is incompatible with upstream's dtype"
      in
      conv2d2 ~id ~g:group_count ~s:stride ~d:dilation ~b:boundary_mode
        (weights :> network)
        upstream

    let bias ?id ?axes ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let axes = Option.value (axes :> Pshape.Axis.t list option) ~default:[ `C ] in
      let init = Option.value (init :> Init.float option) ~default:(`Float_constant 0.) in
      let optimizer = Option.value (optimizer :> optimizer_conf option) ~default:`Sgd in
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
      | `Float32, (#Init.float32 as init) -> parameter32 ~rng dimensions init optimizer
      | `Float64, #Init.float64 -> failwith "not implemented"
      | _, _ -> invalid_arg "In bias: init is incompatible with upstream's dtype" )
      |> transpose ~mapping |> downcast
      |> fun w -> sum ~id [ upstream; w ]

    let scale ?id ?axes ?i:init ?o:optimizer ?(rng = State.get_state ()) upstream =
      (* Cast *)
      let axes = Option.value (axes :> Pshape.Axis.t list option) ~default:[ `C ] in
      let init = Option.value (init :> Init.float option) ~default:(`Float_constant 1.) in
      let optimizer = Option.value (optimizer :> optimizer_conf option) ~default:`Sgd in
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
      | `Float32, (#Init.float32 as init) -> parameter32 ~rng dimensions init optimizer
      | `Float64, #Init.float64 -> failwith "not implemented"
      | _, _ -> invalid_arg "In scale: init is incompatible with upstream's dtype" )
      |> transpose ~mapping |> downcast
      |> fun w -> prod ~id [ upstream; w ]

    let batch_norm ?id ?(affine = true) ?algo_conf upstream =
      let algo_conf =
        Option.value
          ~default:(`Exp_moving (1e-5, 0.99))
          (algo_conf :> normalization_algo_conf option)
      in

      let upstream = downcast upstream in
      let id = match id with None -> Id.create_default () | Some id -> id in
      if affine then normalisation ~id ~algo_conf [ `C ] upstream |> scale |> bias |> downcast
      else normalisation ~id ~algo_conf [ `C ] upstream |> downcast
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
