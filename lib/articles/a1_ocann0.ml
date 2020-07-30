open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
  module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
  module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
  module Ndarray = Owl_base_dense_ndarray_generic
  module Typed_array = Js_of_ocaml.Typed_array
  module Reactjs = Ft_js.Reactjs
  module Lwt_js = Js_of_ocaml_lwt.Lwt_js
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

(*

<h2>Binding a Framework</h2> <!-- -------------------------------------------------------------- -->
<p>
   In order to expose a framework in a functional fashion, the binding should be
   connected at the lowest level - to the tensor computation engine - and ignore
   everything else in the framework.
</p>
<p>
   The tensor computation engine can be seen as a push-based black box that can
   dynamically receive computation graph nodes and output tensor promises on certain nodes.
   A node can either be an input tensor (e.g., image, sound, text, network parameters) or
   an operation between nodes (e.g., forward-conv, backward-conv-x, backward-conv-w).
   An output tensor is the output of any node
   (e.g., predictions, gradients, feature maps, updated network parameters).
</p>
<p>
   Binding a framework in such a way offers full flexibility to write an OCaml-friendly
   binding, but some problems may arise with certain frameworks:
   <ul>

   <li>Not all DL frameworks expose their engine in a low level fashion. In that case
   a binding has to use a higher-level API, often effectful, that will cause some rigidity
   (e.g., the TensorFlow.js framework).</li>

   <li>It is often quicker to write a binding that reuses the higher level abstractions
   of a framework (e.g., the Owl binding uses Algodiff).</li>

   <li>Some engines will not natively support certain operations (i.e., certain NN layers).
   In that case the binding can either raise an exception or provide an ad-hoc
   implementation using the framework's constructs. Examples:
   <ul>
   <li>In the current TensorFlow.js binding tensordot is implemented using transpose/reshape/dot.</li>
   <li>In the Owl binding, tensordot raises an exception unless it can be substituted
   with a simple dot operation.</li>
   <li>Some frameworks only allow inference and not traning.</li>
   </ul>
   </li>

   </ul>

</p>


 *)

let t0 =
  {|
<h3>WORK IN PROGRESS!</h3>
<p>
   OCaNN's ambition is to bring to OCaml a functional neural networks (NN) semantic
   without actualy writing a tensor processing engine nor depending on one specific external
   deep-learning (DL) framework.
</p>
<p>
   To achieve that goal OCaNN provides on one hand a common NN abstraction that is both
   framework-agnostic and ocaml-friendly and on the other hand separate individual DL
   framework bindings that makes the necessary trade-offs to expose their library in an efficient
   and as functional as possible way.
</p>
<p>
   A first implementation is available
   <a href="https://github.com/Ngoguey42/io/tree/master/lib/ocann">here</a> (and
   <a href="https://github.com/Ngoguey42/io/tree/master/lib/pshape">here</a> for the shape abstraction)
   and two bindings already exist,
   <a href="https://github.com/Ngoguey42/io/tree/master/lib/ocann_owl">ocann-owl</a> and
   <a href="https://github.com/Ngoguey42/io/tree/master/lib/ocann_tfjs">ocann-tfjs</a>.
   All have been used to create the <a href="mnist-jsoo.html">mnist-jsoo</a> page.
</p>

<h2>Example: Two-Layer Perceptron</h2> <!-- ---------------------------------------------------- -->
<h4>First we create the network using the main module</h4>
<pre><code class="language-ocaml">let nn =
  let open Ocann.Default.Builder in                 (* Exposes the layer constructors *)
  let open Ocann.Pshape.Size in               (* Exposes the `U` and `K` constructors *)
  input (Pshape.abs2d_partial U (K (28 * 28))) `Float32      (* shape: (unknown, 784) *)
  |> dense [ (`Idx 1, 512) ] |> bias |> relu
  |> dense [ (`Idx 1, 10) ] |> bias
  |> softmax (`Idx 1)                                         (* shape: (unknown, 10) *)
</code></pre>

<h4>We can then train this NN using the existing Owl Algodiff (effectful) binding</h4>
<pre><code class="language-ocaml">module Algodiff = struct
  type ba = Owl_base_algodiff_primal_ops.S.arr
  type ba_elt = Bigarray.float32_elt
  include Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end
module Binding = Ocann_owl.Make (Algodiff) (Ocann.Default)

let categorical_crossentropy epsilon softmaxed_pred truth =
  let open Algodiff.Maths in
  softmaxed_pred |> max2 (epsilon |> Algodiff.pack_flt)
  |> log |> mul truth |> neg |> sum ~axis:(-1) |> mean

let learning_rate = 1e-3
let new_nn =
  let flattened_images, truths = (* Algodiff tensors *) in
  let forward, optimizations, pack = Binding.unpack_for_training nn in

  (* Forward pass *)
  let predictions = forward flattened_images in
  let loss = categorical_crossentropy 1e-10 predictions truths in

  (* Backward pass *)
  Algodiff.reverse_prop (Algodiff.pack_flt 1.) loss;

  (* Optimization *)
  Ocann_owl.OptiMap.iter (fun _ opti -> opti learning_rate) optimizations;

  pack ()
</code></pre>

<h4>Or use the exising TensorFlow.js (effectful too) binding</h4>
<pre><code class="language-ocaml">module Binding = Ocann_tfjs.Make (Ocann.Default)

let learning_rate = 1e-3
let new_nn =
  let flattened_images, truths = (* Tfjs tensors *) in
  let forward, optimizations, pack = Binding.unpack_for_training nn in

  (* Forward and backward pass *)
  let f () =
    let predictions = forward flattened_images in
    let loss = Tfjs.categorical_crossentropy 1e-10 predictions truths in
    loss
  in
  let loss, gradients = Tfjs.variable_grads f in

  (* Optimization *)
  Ocann_tfjs.OptiMap.iter
    (fun name opti -> opti learning_rate (Tfjs.Named_tensor_map.find name gradients))
    optimizations;

  pack ()
</code></pre>

<h2>Benefits</h2> <!-- ------------------------------------------------------------------------- -->
<p>
   There are multiple benefits to OCaNN's approach:
   <ul>
   <li>Get rid of the awkward feeling of being writing Python in OCaml by tailoring the
   APIs for OCaml.</li>
   <li>No need to write yet another tensor processing engine — which is the most
   expensive part of a DL framework.</li>
   <li>Clear separation of concerns between the features that require a computation engine
   (i.e., number crunching for inference or training) and the rest
   (e.g., network construction, initialization, modification, storage, graph analysis).</li>
   <li>It is quicker to write a new binding for a framework</li>
   <li>By sharing pieces of code between bindings, it is easier for the user to adapt the
   framework used to his needs. (e.g., he may want to use a specialized engine in production while
   reusing pieces of code from the training phase).</li>
   <li>Since a binding is an independent library, very few constrains apply on the design of it
   (e.g., a framework can possess several bindings)
   (e.g., a binding can be specialized for one specific task).</li>
   <li>Compared to a library that gives a single interface to multiple DL frameworks, having
   bindings specialized for their own library allows for more flexibility.</li>
   </ul>
</p>

<h2>Motivation for Functional Neural Networks</h2> <!-- ---------------------------------------- -->
<p>
   Anyone who as trained a NN at least once has faced the situation where
   the training program is running without errors but the network doesn't seem
   to be learning anything. Either:
   <ul>
   <li>There is a bug in your code that remained undetected by the static checks,
   the runtime checks and yourself.</li>
   <li>Your code is fine but some hyperparameter is wrong, such as the network architecture or the
   learning rate.</li>
   <li>Your code and your configurations are fine, your network is currently in a local minima,
   you just need to wait.</li>
   </ul>
   This time consuming situation is symtomatic of the lack safety in the programming
   languages and the DL frameworks widely used today.
</p>
<p>
   In order to spend less time on debugging, a DL library should allow for more static
   and runtime checks.
</p>
<p>
   Bringing safety to DL programs is already a concern for many people out there:
   <ul><li>
   <a href="https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html">Practical Dependent Types in Haskell: Type-Safe Neural Networks</a>.
   </li><li>
   <a href="http://hackage.haskell.org/package/tensor-safe">TensorSafe</a>, one of many Haskell NN library.
   </li><li>
   <a href="https://blog.janestreet.com/playing-atari-games-with-ocaml-and-deep-rl/#improving-type-safety">An interesting attempt</a> at typing tensor dimensions in OCaml.
   </li></ul>
</p>

<h2>Safeties Already Offered by OCaNN</h2> <!-- ------------------------------------------------- -->



<h3>1. Immutability of Networks</h3>
<p>
In OCaNN a network is an immutable DAG that is built one layer at a time from forward inputs
to forward outputs (i.e. from upstream to downstream). This implies that a layer only possess
pointers to the upstream graph and that to be modified, a layer has to be reinstanciated
along with all its downstream graph.
</p>
<p>
Even if this functional definition seems very restrictive, it in no way limits the flexibility
of NNs compared to an imperative definition. It even simplifies the garbage collection in schemes
involving reusing a layer multiple times  throughout a network.
</p>
<p>
The <code>copy</code> method available in a <code>Network</code> module can be used for all
kind of reinstanciation operations, e.g., updating the trainable parameters, appending a network
to another.
</p>



<h3>2. Structural Polymorphism of Layers</h3>
<p>
The layers in OCaNN are OCaml objects implementing a common interface that defines:
<ul><li>
A list of upstream layers.
</li><li>
Informations on the output tensor of that layer (i.e. shape and dtype).
</li><li>
A generic copy method to change the upstream parents or reinitialize the states.
</li><li>
A conversion of the layer to a variant that can be pattern matched to retreive the actual
subtype of that layer.
</li><li>
Several identification methods: <code>to_string</code>, <code>layer_name</code>, <code>id</code>.
</li></ul>
</p>



<h3>3. Smaller Layers</h3>
<p>
Certain NN layers that are usually monolithic have been split in OCaNN to improve the
<i>separation of concerns</i>.
</p>
<p>
For example the <code>conv2d</code> layer is split into 6 layers:
The kernel application (<code>conv2d</code>),
the kernel (<code>parameter32</code>),
the bias application (<code>sum</code>),
the bias values (<code>parameter32</code>),
the conversion of the bias values from an absolute 1d shape to a symbolic 4d shape
(<code>transpose</code>) and
the activation (e.g., <code>relu</code>).
</p>
<p>
To avoid boilerplate when defining a network, the <code>Builder</code> module exposes the
<code>conv2d</code> and <code>bias</code> syntaxic sugars:
</p>

<pre><code class="language-ocaml">upstream |> conv2d (`Full 32) (3, 3) |> bias |> relu</code></pre>
<p>
roughly desugars to:
</p>
<pre><code class="language-ocaml">let Pshape.Size.K in_channels = Pshape.get upstream#out_shape `C in
let p = parameter32 [| 3, 3, in_channels; 32 |] `Tanh `Sgd) in
let p' =
  parameter32 [| 32 |] (`Float_constant 0.) `Sgd
  |> transpose ~ndim:4 [ (`Idx 0, `C) ]
in
upstream |> conv2d2 p |> sum p' |> relu
</code></pre>



<h3>4. Polymorphic Shapes</h3>
<p>
In OCaNN the shape type is polymorphic on the number of dimensions
(0, 1, 2, 3, ... or a any combination), on the type of dimensions size
(known, unknown or any) and on the way dimensions are denominated (absolute, symbolic or any).
</p>
<h4>Length</h4>
<p>
The number of dimensions is encoded using a polymorphic variant with this upper bound:
<code>Length.t = [ `L0 | `L1 | `L2 | `L3 | `L4 | `L5 ]</code>.
</p>
<h4>Size</h4>
<p>
If one knows at compile time that a shape has no unknown dimensions, he can use the
<code>[ `K ]</code> type parameter, otherwise he can use the
<code>Size.tag = [ `U | `K ]</code> upper bound.
</p>
<h4>Denomination</h4>
<p>
To avoid uncecessary dimension ordering and references to well-known dimensions using
ad-hoc indices, OCaNN offers a <i>symbolic</i> shape type where each dimension is
identified by a predefined name.
</p>
<p>
The symbolic dimension names are chosen by the library and not by the user:
<ul><li>
<code>[ `N ]</code> is the type parameter for a 1d symbolic shape,
</li><li>
<code>[ `N | `C ]</code> for 2d,
</li><li>
<code>[ `N | `C | `S0 ]</code> for 3d,
</li><li>
<code>[ `N | `C | `S0 | `S1 ]</code> for 4d
</li><li>
etc...
</li></ul>
</p>
<p>
A conventional shape in which the dimensions are identified by indices is called <i>absolute</i>
and its parameter type is <code>[ `Idx of int ]</code>. The most generic parameter type for a shape
that is either <i>symbolic</i> or <i>absolute</i> is
<code>Axis.t = [ `N | `C | `S0 | `S1 | `S2 | `Idx of int ]</code>.
</p>
<p>
<h4>Examples</h4>
<ul><li>
Any shape can be the output of a <code>sqrt</code> layer because it is a simple element-wise
operation. The most generic shape type is <code>(Length.tag, Size.tag, Axis.t) t</code>.
</li><li>
The output of a <code>conv2d</code> layer is always a 4d tensor with a clearly identified
<i>channel</i> dimension: <code>([> `L4 ], Size.tag, [> `N `C `S0 `S1 ]) t</code>.
</li><li>
Since the <code>parameter32</code> layer explicitly stores a concrete tensor, its output shape is
<code>(Length.tag, [> `K ], [> `Idx of int]) t</code>.
</li><li>
In the Pshape library the type of the <code>nth</code> function is
<code>(_, 'sz, [ `Idx of int ]) t -> int -> 'sz Size.t</code>.
</li></ul>
</p>



<h3>5. Restrictions of Broadcasts</h3>
<p>
Broadcasting is a convenient but error prone technique that makes possible the use of input
tensors with heterogeneous shapes in operations like sum or product (refer to
<a href="https://numpy.org/doc/stable/user/basics.broadcasting.html">numpy's documentation</a>
for a detailed explanation).
</p>
<p>
This technique can be divided into 2 steps.
</p>
<h4>Alignment step</h4>
<p>
Firstly the shapes are padded with "1" on their lefts to match the size of the longest shape.
This step doesn't require to change the memory representation of the tensor and it can be
replaced by a <code>reshape</code>, a <code>view</code> or a <code>transpose</code> operation.
</p>
<h4>Stretching step</h4>
<p>
Secondly the dimensions with size "1" are stretched to match the size of their longest
homologous dimension. This step require to change the memory representation but in practice
the stretchings are optimized away by loops inside the arithmetic operations. It can be
replaced by a <code>repeat</code> or a <code>tile</code> operation with performance loss.
</p>
<h4>Examples</h4>
<p>
To add an image of shape <code>(32, 32)</code> to all the images of a tensor
of shape <code>(5, 32, 32)</code>, the broadcast steps are:
</p>
<pre><code class="language-txt">  before step 1  before step 2  output shape
a     (32, 32) -> (1, 32, 32) -> (5, 32, 32)
b  (5, 32, 32) -> (5, 32, 32) -> (5, 32, 32)
</code></pre>
<p>
Or to add a line of shape <code>(5)</code> to all the lines of an image of shape
<code>(5, 5)</code>:
</p>
<pre><code class="language-txt">  before step 1  before step 2  output shape
a       (5, 5) ->      (5, 5) ->      (5, 5)
b          (5) ->      (1, 5) ->      (5, 5)
</code></pre>
<p>
This exemple is error prone because it can be easily mixed up with the operation that adds
a column.
</p>
<pre><code class="language-txt"> before reshape  before step 2  output shape
a       (5, 5) ->      (5, 5) ->      (5, 5)
b       (5)    ->      (5, 1) ->      (5, 5)
</code></pre>
<h4>In OCaNN</h4>
<p>
OCaNN disables the first broadcast step to force the user to reshape manually
using the <code>transpose</code> layer. By doing so the user has to
aknowledge the alignment of the dimensions of the operands.
</p>
<p>
The second broadcast step is less error prone and cannot be removed without affecting
the performances.
It currently isn't disabled in OCaNN but it would be possible to make it explicit by adding a
<code>stretch</code> parameter to the constructors where it takes
place to force the user to aknowledge the stretchings.
</p>



<h3>6. Runtime Checks on Shapes Compatibility</h3>
<p>
When constructing a layer, its output shape is infered from the parameters and especially
from the output shape of the upstream layers. By doing so, all the shape incompatibilities
are prevented except those involving unknown dimensions.
</p>
<h4>Examples</h4>
<p>
In the <code>sum</code> constructor, all the input shapes should have the same length
(i.e. number of dimensions), the same denomination (i.e. symbolic or absolute) and have
homologous dimensions either equal, or equal to one, or unknown
(e.g., <code>(U, 5, 1, 6) + (U, 5, 5, U) -> (U, 5, 5, 6)</code>).
</p>
<p>
In the <code>conv2d</code> constructor, the input shape must be 4d, symbolic and the <code>`C</code>
dimension must be known. In addition to the usual <i>same</i> and <i>valid</i>
boundary modes, a new <i>assert_fit</i> option can be used to make sure that the known spatial
dimensions do not require padding.
</p>



<h3>7. Network Parameters Initialization</h3>
<!--
<p>
When initilializing a parameter layer in OCaNN, two things are required:
<ul><li>
a method of type <code>Init.t</code>, e.g, <code>`Tanh</code>, <code>`Float_constant 0.</code> and
</li><li>
a Random Number Generator (RNG) of type <code>Stdlib.Random.State.t</code>.
</li></ul>

The construction of a parameter layer in OCaNN requires a value <code>init</code> of
type <code>Init.t</code> that defines how to initilialize the tensor
(e.g, <code>`Gaussian (mu, sigma)</code>, <code>`Float_constant 0.</code>).

The constructor internally coerce <code>init</code> to a
<code>Init.Deterministic.t</code> possibly by sampling a seed using a provided
<code>Stdlib.Random.State.t</code> value.

The construction process will then coerce the <code>Init.t</code> value to a <code>Init.Deterministic.t</code> value by sampling a seed

The RNG can either be passed to the layer when constructing it, or defined inside the <code>Network.Builder</code> module when constructing it with <code>Network.create_builder</code>



https://github.com/Ngoguey42/io/blob/master/lib/ocann/init.ml


<code></code>
<code></code>
<code></code>
When creating a network, the parameters must be initialized. Two things are required, a  and a

When initilializing a parameter layer in OCaNN you may choose
</p>
<p>

</p>
<p>

</p>
<p>

</p>
<p>

</p>
-->



<h3>8. No General Purpose Bidirectional NN Conversion In Bindings</h3><p>lorem ipsum</p>



<h3>9. Avoid Black Boxes</h3>
<p>
DL frameworks tend to forget the
<i><a href="https://en.wikipedia.org/wiki/Worse_is_better">worse is better</a></i>
philosophy by including a plethora of small algorithms implemented using smaller
bricks also exposed by the framework.
Those small algorithms often have subtle input/output domains because there are several ways
of implementing them and no way to reflect the implementation choices on the types.
</p>
<p>
For example the <code>categorical_crossentropy</code> used in the introduction (and reproduced below)
is 7 operations long, but it makes 6 assumptions (at runtime) on the input data:

<ul><li>
<code>epsilon</code> should be comprised in the interval <code>]0; 1[</code>.
</li><li>
Both input tensors should have the same shape and dtype.
</li><li>
The <i>channel</i> dimension must be last.
</li><li>
An element in the <code>truth</code> tensor should be 0 or 1.
</li><li>
The <code>softmaxed_pred</code> tensor should be the output of a <code>softmax</code> layer.
</li><li>
A <code>NAN</code> in <code>softmaxed_pred</code> will contaminate the result.
</li></ul>

And makes 2 choices for the output tensor:
<ul><li>
The dimensions have all been squeezed (i.e. the number of dimensions is 0 instead of being same as input).
</li><li>
All the dimensions have been meaned together at the end.
</li></ul>
</p>
<pre><code class="language-ocaml">let categorical_crossentropy epsilon softmaxed_pred truth =
  let open Algodiff.Maths in
  softmaxed_pred |> max2 (epsilon |> Algodiff.pack_flt)
  |> log |> mul truth |> neg |> sum ~axis:(-1) |> mean
</code></pre>

<p>
Other examples of such algorithms are the optimizers, the statistics computations,
the LSTM layer or the one-liner train function. OCaNN encourage the user to implement
his own algorithms.
</p>

<h2>Future</h2> <!-- --------------------------------------------------------------------------- -->
<p>
   After writing and using OCaNN v0 it feel that some parts should be redesigned:
   <ul><li>
   The APIs should be based on an existing abstraction (e.g., ONNX, NNEF).
   </li><li>
   Polymorphic variants should be used instead of objects (e.g., <code>val kernel_size_2d : [< `Conv2d | `Maxpool2d ] network -> int * int</code>).
   </li><li>
   Some features should be streamline to feel less ad-hoc (e.g., optimizer).
   </li><li>
   The Pshape library relies too much on unsafe operations.
   </li><li>
   A true functional binding for Owl is possible by bypassing Algodiff.
   </li></ul>
</p>
<p>
   Some features are yet to be implemented:
   <ul><li>
   Many basic layers are missing (e.g., transpose convolution, sqrt).
   </li><li>
   Some backends are obviously missing (e.g., torch, tensorflow, owl's OpenCL backend).
   </li><li>
   Conversions to/from storage formats such as ONNX and NNEF.
   </li><li>
   Many smaller improvements listed in
   <a href="https://github.com/Ngoguey42/io/blob/master/lib/ocann/ocann.ml">ocann.ml</a>.
   </li></ul>
</p>
<p>
   And some problems inherent to all DL frameworks are yet to be solved:
   <ul><li>
   In a network, the <b>unknown dimensions should be mathematical symbolic variables</b>
   to allow the detection of inconsistent combinations of unknown dimensions, and to be able
   to infer the domain of the unknown dimensions from the operations they are used in.
   </li><li>
   It is vain to try to include in the library all the NN layers that exists out there.
   Instead, the user should be able to define and implement his own custom layers
   (in the common module, and in the binding he uses).
   To make that process painless, the <b>custom layers should be first class citizens</b>
   in a new design.
   </li><li>
   Powerful DL use cases involve transforming the gradient tensors from the backward phase
   using trainable forward operations, but since the definition of a network is rooted on the
   forward phase, such use cases are impossible to define in a network.
   It might be possible to overhaul the network definition abstraction in such a way that
   makes the <b>backward phase a first class citizen</b>.
   </li><li>
   Some problems might be solved by allowing <b>higher order layers</b>. More precisely,
   the edge between two layers currently represents a <code>tensor</code> (i.e. a shape/dtype pair),
   an edge could then also represent a <code>tensor -> tensor</code>.
   </li></ul>
</p>
<p>
   If you wish to chat about OCaNN, e-mail me!
</p>

<h2>One Last Example</h2> <!-- ----------------------------------------------------------------- -->

Definition of a residual convolutional NN with symbolic shapes.
<pre><code class="language-ocaml">let nn =
  let open Ocann.Default.Builder in
  let open Ocann.Pshape.Size in

  (* Input shape is <x:24; y:24; c:3; n:unknown> *)
  input (Pshape.sym4d_partial ~n:U ~c:(K 3) ~s0:(K 24) ~s1:(K 24)) `Int32
  |> astype `Float32
  |> conv2d (`Full 32) (3, 3) ~s:(2, 2) ~b:`Same
  |> bias
  |> (fun up ->
       [
         up |> conv2d (`Full 64) (1, 1) ~s:(2, 2) ~b:`Same |> Ocann.Default.downcast
       ; up |> relu |> conv2d (`Full 64) (3, 3) ~s:(2, 2) ~b:`Same
         |> bias |> Ocann.Default.downcast
       ])
  |> sum
  |> (fun up ->
       [
         up |> conv2d (`Full 128) (1, 1) ~s:(2, 2) ~b:`Same |> Ocann.Default.downcast
       ; up |> relu |> conv2d (`Full 128) (3, 3) ~s:(2, 2) ~b:`Same
         |> bias |> Ocann.Default.downcast
       ])
  |> sum
  |> conv2d (`Full 10) (3, 3) ~b:`Assert_fit
  |> bias
  |> softmax `C
  (* Output shape is <x:1; y:1; c:10; n:unknown> *)
  </code></pre>

|}

let construct_reactjs_article () =
  let ref = Reactjs.create_ref () in
  let render () =
    let open Reactjs.Jsx in
    let box =
      [ of_tag "h1" [ of_string "OCaNN library" ]; of_tag "div" ~inner_html:t0 [] ]
      |> of_bootstrap ~ref "Col" >> of_bootstrap "Row"
      >> of_bootstrap "Container" ~fluid:true ~classes:[ "bigbox1" ]
    in
    of_react "Fragment" [ box ]
  in
  let mount () =
    (let ( >|= ) opt f = Js.Opt.iter opt f in
     let tolist all =
       List.init all##.length (fun i -> all##item i |> Js.Opt.to_option) |> List.filter_map Fun.id
     in
     ref##.current >|= fun elt ->
     Firebug.console##log elt;
     elt##querySelectorAll (Js.string "pre > code")
     |> tolist |> List.map Misc.highlight_element |> ignore);
    fun () -> ()
  in
  Reactjs.construct ~mount render

let main () =
  let open Lwt.Infix in
  let body = Dom_html.window##.document##.body in

  let div =
    [%html "<div><div style='text-align: center;'>loading</div></div>"]
    |> Tyxml_js.To_dom.of_element
  in
  Dom.appendChild body div;
  Ft_js.Scripts.import `Reactjs >>= fun () ->
  Ft_js.Scripts.import `Reactjsbootstrap >>= fun () ->
  Ft_js.Scripts.import `Highlightjs >>= fun () ->
  Ft_js.import_css "styles.css" >>= fun () ->
  Reactjs.render (Reactjs.Jsx.of_constructor construct_reactjs_article ()) div;
  Lwt.return ()
