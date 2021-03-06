(* Functional Neural Network (ocann)

Quick and dirty pieces of documentation.


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
will be internally converted to a `Init.Deterministic.t` by sampling an initialisation seed using
`Builder.state`. When calling `#tensor` on that layer, the tensor will be deterministically computed
and returned (but not stored, remember that the layers are immutable). After calling `#replicate` on
that layer, `#tensor` will return the tensor passed to `#replicate` instead of initialising a new
tensor.

This features makes the newly created networks very light.

---

# Shapes

This library avoids making dimension ordering assumptions when unnecessary (such as
`channel-last`). Although the axes still requires to be designated. Depending on the context
you can either use an absolute or a symbolic designation.

For example:
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

# Determinism and Init.t
When creating a new network some randomness is required to initialise specific components like
convolution kernels, fully-connected weights or dropout layers. The Builder module
can be instantiated using an `rng` parameter that will be used to sample a unique seed for each
individual component that depends on randomness. This way, an instantiated network, is always fully
determined.

More specifically, when the user passes a `Init.t` value to build a layer, the layer will
convert it right away to a `Init.Deterministic.t` by sampling a seed if necessary.

In a newly created network, the statful tensors are not instantiated right away in order to keep
the network as light as possible for as long as possible. The first times a stateful tensor is
fetched, it is deterministically computed using it's associated `Init.Deterministic.t` value.
Using the `replicate` methods, the user can manually store a new version of a stateful tensor inside
a layer (e.g. after training). Only then a layer stores some tensor.

---

# Possible improvements:
- rename network to node? we could say that `network` == `list of node`
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
  - Create an Ocann.custom_layer class and a `Builder.custom custom_defn ...` function?
  Extensible GADT?
- Transform `parameter32` to `parameter`?
- What about 1 operation that outputs more than 1 tensor? A cumbersome solution is to define the operation with 2 layers `(X0, ..., Xn)->Yph, Yph->Y0, Yph->Y1` and erase that detail in the binding.
- Find a less rigid way to implement the `pshape` lib. Modular implicits (lol)?
- Rewrite the library without the use of ocaml objects because:
  - Those are impossible to serialize in js
  - Polymorphic variants and top-level functions could provide roughly the same properties:
    - e.g. `val kernel_size : [< `Maxpool2d | `Conv2d ] network -> int * int`
    Does this mean that `val upstreams : [< any_network ] network -> any_network list` requires
      a massive pattern matching?
        - Is this pattern matching a problem?
        - I picked ocaml object to avoid those pattern matching
        - Maybe this pattern matching can be avoided by giving a proper definition to 'a network
  - It is impossible to create a network ex-nihilo without requiring to a builder function or a
    method. It brings rigidity to some low level use cases.
- How to reuse a normalisation layer in several spots? Higher order layers?
- How to streamline "optimizer"? Higher order layers?
- Use poly-var on network type to encode shape/dtype? That way a constructor can restrict its input from the type
- Remove the `unit` in create_builder?
- Use int list instead of int array everywhere?
- In bindings, assert that unkown dimensions are right when revealed
- Should we restrict even more the shape combinations by disallowing the wildcard dimensions of size "1" ? This would:
   - Allow to infer the output shape of sum [(5); (U)] to be (5).
   - simplify the `sympy-like unknown dimensions` semantic.

# Possible improvement: Sympy-like unknown dimensions
- An unknown dimension can be constrained from below
  - a = <~s0:U, ...>, _ = conv2d (5, 5) ~s:2 ~b:`Assert_fit a => A#s0 % 5 == 0
  - a = (U), _ = sum [A; (5)] => A = (5) || (1)
- Two networks with a common unkown ancestor can have incompatible interpretations of that ancestor
  - a = (U), _ = sum [U; (5)], _ = sum [U; (6)]
- Beware of the `reshape` operation, it might not be implementable in this scheme

 *)

module Init = Init
include Misc
module Make = Make_ocann.Make

module type NETWORK = Sig.NETWORK

module Default = struct
  include (
    Make_ocann.Default :
      NETWORK
        with type ('a, 'b) Tensor.t = ('a, 'b) Bigarray_tensor.t
         and type Id.t = String_option_id.t )

  module Patch = Patch
end
