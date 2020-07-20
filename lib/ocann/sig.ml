(* Mostly automatically generated from make_ocann.ml *)
module type NETWORK = sig
  module Tensor : Misc.TENSOR

  module Id : Misc.ID

  type 'b float_tensor = (float, 'b) Tensor.t

  type float32_tensor = Bigarray.float32_elt float_tensor

  type float64_tensor = Bigarray.float64_elt float_tensor

  type int64_tensor = (int64, Bigarray.int64_elt) Tensor.t

  type optimizer32 =
    [ `Adam of float * float * float * int * float32_tensor * float32_tensor | `Sgd ]

  type optimizer_conf = [ `Adam of float * float * float | `Sgd ]

  type normalization_algo =
    [ `Exp_moving32 of float * float * float32_tensor * float32_tensor
    | `Exp_moving64 of float * float * float64_tensor * float64_tensor
    | `Global32 of float * int * float32_tensor * float32_tensor
    | `Global64 of float * int * float64_tensor * float64_tensor
    | `Local of float ]

  type normalization_algo_conf =
    [ `Exp_moving of float * float | `Global of float | `Local of float ]

  type boundary_mode = [ `Assert_fit | `Pad_fit | `Same | `Valid ]

  type float_dtype = [ `Float32 | `Float64 ]

  type int_dtype = [ `Int32 | `Int64 | `Uint8 ]

  type dtype = [ `Float32 | `Float64 | `Int32 | `Int64 | `Uint8 ]

  type state_copy_strategy = [ `Keep | `Reinit | `Scratch ]

  val string_of_dtype : [< `Float32 | `Float64 | `Int32 | `Int64 | `Uint8 ] -> string

  type network =
    < classify_layer : classified_layer
    ; classify_node : classified_node
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> network
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >

  and node01 =
    < classify_layer : classified_layer01
    ; classify_node : [ `Node01 of node01 ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> node01
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >

  and node11 =
    < classify_layer : classified_layer11
    ; classify_node : [ `Node11 of node11 ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> node11
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstream : network
    ; upstreams : network list >

  and node21 =
    < classify_layer : classified_layer21
    ; classify_node : [ `Node21 of node21 ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> node21
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstream0 : network
    ; upstream1 : network
    ; upstreams : network list >

  and noden1 =
    < classify_layer : classified_layern1
    ; classify_node : [ `Noden1 of noden1 ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> noden1
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >

  and classified_node =
    [ `Node01 of node01 | `Node11 of node11 | `Node21 of node21 | `Noden1 of noden1 ]

  and classified_layer =
    [ `Astype of astype
    | `Concatenate of concatenate
    | `Conv2d of conv2d
    | `Input of input
    | `Maxpool2d of maxpool2d
    | `Normalisation of normalisation
    | `Padding of padding
    | `Parameter32 of parameter32
    | `Prod of prod
    | `Relu of relu
    | `Softmax of softmax
    | `Sum of sum
    | `Tensordot of tensordot
    | `Transpose of transpose ]

  and classified_layer01 = [ `Input of input | `Parameter32 of parameter32 ]

  and classified_layer11 =
    [ `Astype of astype
    | `Maxpool2d of maxpool2d
    | `Normalisation of normalisation
    | `Padding of padding
    | `Relu of relu
    | `Softmax of softmax
    | `Transpose of transpose ]

  and classified_layern1 = [ `Concatenate of concatenate | `Prod of prod | `Sum of sum ]

  and classified_layer21 = [ `Conv2d of conv2d | `Tensordot of tensordot ]

  and input =
    < classify_layer : [ `Input of input ]
    ; classify_node : [ `Node01 of input ]
    ; copy : ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> input
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >

  and parameter32 =
    < classify_layer : [ `Parameter32 of parameter32 ]
    ; classify_node : [ `Node01 of parameter32 ]
    ; copy :
        ?id:Id.t ->
        ?states:state_copy_strategy ->
        ?rng:Random.State.t ->
        network list ->
        parameter32
    ; id : Id.t
    ; init : Init.Deterministic.float32
    ; layer_name : string
    ; numel : int
    ; optimizer : optimizer32
    ; optimizer_conf : optimizer_conf
    ; optimizer_opt : optimizer32 option
    ; out_dtype : [ `Float32 ]
    ; out_shape :
        'a 'b. ( Pshape.Length.tag, ([< Pshape.Size.tag > `K ] as 'a),
          ([< Pshape.Axis.t > `Idx ] as 'b) ) Pshape.t
    ; replicate : ?id:Id.t -> float32_tensor -> optimizer32 -> parameter32
    ; stateful : bool
    ; tensor : float32_tensor
    ; tensor_opt : float32_tensor option
    ; to_string : string
    ; upstreams : network list >

  and sum =
    < classify_layer : [ `Sum of sum ]
    ; classify_node : [ `Noden1 of sum ]
    ; copy : ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> sum
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >

  and prod =
    < classify_layer : [ `Prod of prod ]
    ; classify_node : [ `Noden1 of prod ]
    ; copy : ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> prod
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >

  and concatenate =
    < axis : Pshape.Axis.t
    ; classify_layer : [ `Concatenate of concatenate ]
    ; classify_node : [ `Noden1 of concatenate ]
    ; copy :
        ?id:Id.t ->
        ?states:state_copy_strategy ->
        ?rng:Random.State.t ->
        network list ->
        concatenate
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >

  and softmax =
    < axis : Pshape.Axis.t
    ; classify_layer : [ `Softmax of softmax ]
    ; classify_node : [ `Node11 of softmax ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> softmax
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : float_dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstream : network
    ; upstreams : network list >

  and relu =
    < classify_layer : [ `Relu of relu ]
    ; classify_node : [ `Node11 of relu ]
    ; copy : ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> relu
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstream : network
    ; upstreams : network list >

  and astype =
    < classify_layer : [ `Astype of astype ]
    ; classify_node : [ `Node11 of astype ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> astype
    ; dtype : dtype
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstream : network
    ; upstreams : network list >

  and normalisation =
    < algorithm : normalization_algo
    ; algorithm_conf : normalization_algo_conf
    ; algorithm_opt : normalization_algo option
    ; axes : Pshape.Axis.t list
    ; classify_layer : [ `Normalisation of normalisation ]
    ; classify_node : [ `Node11 of normalisation ]
    ; copy :
        ?id:Id.t ->
        ?states:state_copy_strategy ->
        ?rng:Random.State.t ->
        network list ->
        normalisation
    ; id : Id.t
    ; is_batch_norm : bool
    ; is_group_norm : bool
    ; is_instance_norm : bool
    ; is_layer_norm : bool
    ; layer_name : string
    ; out_dtype : float_dtype
    ; out_shape : Pshape.any
    ; replicate : ?id:Id.t -> normalization_algo -> network -> normalisation
    ; stateful : bool
    ; to_string : string
    ; upstream : network
    ; upstreams : network list >

  and transpose =
    < classify_layer : [ `Transpose of transpose ]
    ; classify_node : [ `Node11 of transpose ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> transpose
    ; id : Id.t
    ; layer_name : string
    ; mapping : (Pshape.Axis.t * Pshape.Axis.t) list
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstream : network
    ; upstreams : network list >

  and maxpool2d =
    < boundary_lost : Pshape.Size.any * Pshape.Size.any
    ; boundary_mode : boundary_mode
    ; boundary_overflow : Pshape.Size.any * Pshape.Size.any
    ; boundary_padding : (Pshape.Size.any * Pshape.Size.any) * (Pshape.Size.any * Pshape.Size.any)
    ; boundary_underflow : Pshape.Size.any * Pshape.Size.any
    ; classify_layer : [ `Maxpool2d of maxpool2d ]
    ; classify_node : [ `Node11 of maxpool2d ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> maxpool2d
    ; id : Id.t
    ; kernel_size : int * int
    ; layer_name : string
    ; out_dtype : float_dtype
    ; out_shape :
        'a 'b. ( ([< Pshape.Length.tag > `L4 ] as 'a), Pshape.Size.tag,
          ([< Pshape.Axis.t > `C `N `S0 `S1 ] as 'b) ) Pshape.t
    ; stateful : bool
    ; stride : int * int
    ; to_string : string
    ; upstream : network
    ; upstreams : network list >

  and padding =
    < axes : Pshape.Axis.t list
    ; classify_layer : [ `Padding of padding ]
    ; classify_node : [ `Node11 of padding ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> padding
    ; id : Id.t
    ; is_cropping : bool
    ; is_mixed : bool
    ; is_noop : bool
    ; is_padding : bool
    ; is_symmetric : bool
    ; is_uniform : bool
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; paddings_of_axis : 'a. ([< Pshape.Axis.t ] as 'a) -> int * int
    ; stateful : bool
    ; to_string : string
    ; upstream : network
    ; upstreams : network list
    ; value : [ `Constant of float | `Reflection | `Replication ] >

  and tensordot =
    < classify_layer : [ `Tensordot of tensordot ]
    ; classify_node : [ `Node21 of tensordot ]
    ; contracted_axes0 : Pshape.Axis.t list
    ; contracted_axes1 : Pshape.Axis.t list
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> tensordot
    ; id : Id.t
    ; input_axis_of_output_axis :
        Pshape.Axis.t -> [ `Left of Pshape.Axis.t | `Right of Pshape.Axis.t ]
    ; layer_name : string
    ; mapping0 : (Pshape.Axis.t * Pshape.Axis.t option) list
    ; mapping1 : (Pshape.Axis.t * Pshape.Axis.t option) list
    ; out_dtype : float_dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstream0 : network
    ; upstream1 : network
    ; upstreams : network list >

  and conv2d =
    < boundary_lost : Pshape.Size.any * Pshape.Size.any
    ; boundary_mode : boundary_mode
    ; boundary_overflow : Pshape.Size.any * Pshape.Size.any
    ; boundary_padding : (Pshape.Size.any * Pshape.Size.any) * (Pshape.Size.any * Pshape.Size.any)
    ; boundary_underflow : Pshape.Size.any * Pshape.Size.any
    ; classify_layer : [ `Conv2d of conv2d ]
    ; classify_node : [ `Node21 of conv2d ]
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> conv2d
    ; dilation : int * int
    ; group_count : int
    ; id : Id.t
    ; in_filters : int
    ; in_group_filters : int
    ; is_depthwise : bool
    ; is_dilated : bool
    ; is_grouped : bool
    ; is_pointwise : bool
    ; is_strided : bool
    ; kernel_shape : ([ `L4 ], [ `K ], [ `Idx of int ]) Pshape.t
    ; kernel_size : int * int
    ; layer_name : string
    ; out_dtype : float_dtype
    ; out_filters : int
    ; out_group_filters : int
    ; out_shape :
        'a 'b. ( ([< Pshape.Length.tag > `L4 ] as 'a), Pshape.Size.tag,
          ([< Pshape.Axis.t > `C `N `S0 `S1 ] as 'b) ) Pshape.t
    ; stateful : bool
    ; stride : int * int
    ; to_string : string
    ; upstream0 : network
    ; upstream1 : network
    ; upstreams : network list >

  type 'a any = 'a constraint 'a = < classify_layer : [< classified_layer ] ; .. >

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

  module InputSet : sig
    include Stdlib.Set.S with type elt = input
  end

  module Set : sig
    include Stdlib.Set.S with type elt = network
  end

  module Map : sig
    include Stdlib.Map.S with type key = network
  end

  val unclassify : [< classified_layer ] -> network

  val downcast : _ any -> network

  val find_ids : ?max_size:int -> Id.t -> _ any list -> network list

  val find_id : Id.t -> _ any list -> network

  val find_id_opt : Id.t -> _ any list -> network option

  val find_nodes : ?max_size:int -> 'a node_type -> _ any list -> 'a list

  val find_layers : ?max_size:int -> 'a layer_type -> _ any list -> 'a list

  val find_layer : 'a layer_type -> _ any list -> 'a

  val find_layer_opt : 'a layer_type -> _ any list -> 'a option

  val inputs : _ any list -> input list

  val parameters :
    _ any list ->
    < classify_layer : classified_layer
    ; classify_node : classified_node
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> network
    ; id : Id.t
    ; layer_name : string
    ; numel : int
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >
    list

  val iter_top_down :
    (network -> unit) ->
    < classify_layer : classified_layer
    ; classify_node : classified_node
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> network
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >
    list ->
    unit

  val iter_bottom_up :
    (network -> unit) ->
    < classify_layer : classified_layer
    ; classify_node : classified_node
    ; copy :
        ?id:Id.t -> ?states:state_copy_strategy -> ?rng:Random.State.t -> network list -> network
    ; id : Id.t
    ; layer_name : string
    ; out_dtype : dtype
    ; out_shape : Pshape.any
    ; stateful : bool
    ; to_string : string
    ; upstreams : network list >
    list ->
    unit

  val map :
    (network -> [ `Bound of network | `Remove | `Skip | `Unbound of network list -> network ]) ->
    _ any list ->
    network list

  val copy :
    ?keep:_ any list ->
    ?skip:_ any list ->
    ?remove:_ any list ->
    ?sub:(_ any * _ any) list ->
    ?bind:(_ any * (network list -> network)) list ->
    ?states:state_copy_strategy ->
    ?rng:Random.State.t ->
    _ any list ->
    network list

  module type STATE = Misc.STATE

  module type BUILDER = sig
    val append : ?head:_ any -> ?tail:_ any -> _ any -> network

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

    val conv2d :
      ?id:Id.t ->
      [< `Depthwise of int | `Full of int | `Grouped of int * int ] ->
      int * int ->
      ?s:int * int ->
      ?d:int * int ->
      ?b:[< boundary_mode ] ->
      ?i:[< Init.float ] ->
      ?o:[< optimizer_conf ] ->
      ?rng:Random.State.t ->
      _ any ->
      conv2d

    val bias :
      ?id:Id.t ->
      ?axes:[< Pshape.Axis.t ] list ->
      ?i:[< Init.float ] ->
      ?o:[< optimizer_conf ] ->
      ?rng:Random.State.t ->
      _ any ->
      sum

    val scale :
      ?id:Id.t ->
      ?axes:[< Pshape.Axis.t ] list ->
      ?i:[< Init.float ] ->
      ?o:[< optimizer_conf ] ->
      ?rng:Random.State.t ->
      _ any ->
      prod

    val batch_norm :
      ?id:Id.t -> ?affine:bool -> ?algo_conf:[< normalization_algo_conf ] -> _ any -> network
  end

  module Make_builder : functor (State : STATE) -> BUILDER

  module Builder : BUILDER

  val create_builder : ?rng:Random.State.t -> unit -> (module BUILDER)
end
