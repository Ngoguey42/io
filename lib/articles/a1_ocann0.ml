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
   <a href="https://github.com/Ngoguey42/ngoguey42.github.io/tree/master/lib/ocann">here</a> (and
   <a href="https://github.com/Ngoguey42/ngoguey42.github.io/tree/master/lib/pshape">here</a> for the shape abstraction)
   and two bindings already exist,
   <a href="https://github.com/Ngoguey42/ngoguey42.github.io/tree/master/lib/ocann_owl">ocann-owl</a> and
   <a href="https://github.com/Ngoguey42/ngoguey42.github.io/tree/master/lib/ocann_tfjs">ocann-tfjs</a>.
   All have been used to create the <a href="mnist-jsoo.html">mnist-jsoo</a> page.
</p>

<h2>Example: Two-Layer Perceptron</h2> <!-- ---------------------------------------------------- -->
First we create the network using the main module.
<pre><code>let nn =
  let open Ocann.Default.Builder in                 (* Exposes the layer constructors *)
  let open Ocann.Pshape.Size in               (* Exposes the `U` and `K` constructors *)
  input (Pshape.abs2d_partial U (K (28 * 28))) `Float32      (* shape: (unknown, 784) *)
  |> dense [ (`Idx 1, 512) ] |> bias |> relu
  |> dense [ (`Idx 1, 10) ] |> bias
  |> softmax (`Idx 1)                                         (* shape: (unknown, 10) *)
</code></pre>

We can then train this NN using the existing Owl Algodiff (effectful) binding,
<pre><code>module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
(* TODO: Adapt for new functor design of owl binding *)

let categorical_crossentropy epsilon softmaxed_pred truth =
  let open Algodiff.Maths in
  softmaxed_pred |> max2 (epsilon |> Algodiff.pack_flt)
  |> log |> mul truth |> neg |> sum ~axis:(-1) |> mean

let learning_rate = 1e-3
let new_nn =
  let flattened_images, truths = ... in (* Algodiff tensors *)
  let forward, optimizations, pack = Ocann_owl.unpack_for_training nn in

  (* Forward pass *)
  let predictions = forward flattened_images in
  let loss = categorical_crossentropy 1e-10 predictions truths in

  (* Backward pass *)
  Algodiff.reverse_prop (Algodiff.pack_flt 1.) loss;

  (* Optimization *)
  Ocann_owl.OptiMap.iter (fun _ opti -> opti learning_rate) optimizations;

  pack ()
</code></pre>

or the exising TensorFlow.js (effectful too) binding.
<pre><code>let learning_rate = 1e-3
let new_nn =
  let flattened_images, truths = ... in (* Tfjs tensors *)
  let forward, optimizations, pack = Ocann_tfjs.unpack_for_training nn in

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
   <li>No need to write yet another tensor processing engine - which is the most
   expensive part of a DL framework.</li>
   <li>Clear separation of concerns between the features that require a computation engine
   (i.e., number crunching for inference or training) and the rest
   (e.g., network construction, initialization, modification, storage, graph analysis).</li>
   <li>It is quicker to write a new binding for a framework</li>
   <li>By sharing pieces of code between bindings, it is easier for the user to adapt the
   framework used to his needs. (e.g., he may want to use a specialized engine in production while
   reusing pieces of code from the training phase)</li>
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
   <li>Your code is fine but some hyperparameter is wrong, such as the network architecture.</li>
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
   Brinding safety to DL programs is already a concern for many people out there:
   <ul><li>
   <a href="https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html">Type-Safe Neural Networks</a> in Haskell.
   </li><li>
   <a href="http://hackage.haskell.org/package/tensor-safe">TensorSafe</a>, one of many Haskell NN library.
   </li><li>
   <a href="https://blog.janestreet.com/playing-atari-games-with-ocaml-and-deep-rl/#improving-type-safety">An interesting attempt</a> at typing tensor dimensions in OCaml.
   </li></ul>
</p>

<h2>Safeties Alrady Offered by OCaNN</h2> <!-- ------------------------------------------------- -->
<p>
<h3>Immutability of Networks</h3><p>lorem ipsum</p>
<h3>Smaller Layers</h3><p>lorem ipsum</p>
<h3>Polymorphic Shapes</h3><p>lorem ipsum</p>
<h3>Runtime Checks on Dimensions Compatibility</h3><p>lorem ipsum</p>
<h3>A More Robust Way of Handling RNG at NN Initialization</h3><p>lorem ipsum</p>
<h3>No General Purpose Bidirectional NN Conversion In Bindings</h3><p>lorem ipsum</p>
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
   </li><li>
   It would be vain to try to include in the library all the NN layers that exists out there.
   Instead, the user should be able to define his own layers in both the common module and the
   binding he uses. To make that process painless, the custom layers should be first class
   citizens in a new design.
   </li><li>
   All the layers that have to reference tensors currently store a pointer to them
   (e.g., parameter32, normalisation). Another design using a global weak dictionnary might be an
   interesting alternative.
   </li><li>
   As in all deep learning frameworks, the network definition in OCaNN treats the backward phase
   as a second class citizen. Some powerful DL use cases involve transforming the gradient tensors
   from the backward phase with regular forward operations. Such use cases are hard
   - if not impossible - to define within the network's architecture.
   </li></ul>
</p>
<p>
   Some features are yet to be implemented:
   <ul><li>
   Many basic layers are missing (e.g., ConvTranspose, sqrt).
   </li><li>
   Some backends are obviously missing (e.g., torch, tensorflow, owl's opencl backend).
   </li><li>
   Conversions to/from storage formats such as ONNX and NNEF.
   </li><li>
   Many smaller improvements listed in <a href="">ocann.ml</a>.
   </li><li>
   New static and runtime checks (e.g., typing shape and data-type of tensors, analysis of the unkonwn dimensions throughout a network).
   </li></ul>
</p>
<p>
   If you wish to contribute or discuss about it, e-mail me!
</p>

<h2>One Last Example</h2> <!-- ----------------------------------------------------------------- -->

Definition of a residual convolutional NN that uses symbolic shapes.
<pre><code>let open Ocann.Default.Builder in
let open Ocann.Pshape.Size in

(* Input shape is {x:24; y:24; c:3; n:unknown} *)
input (Pshape.sym4d_partial ~n:U ~c:(K 3) ~s0:(K 24) ~s1:(K 24)) `Int32
|> astype `Float32
|> conv2d ~o (`Full 32) (3, 3) ~s:(2, 2) ~b:`Same
|> bias
|> (fun up ->
     [
       up |> conv2d ~o (`Full 64) (1, 1) ~s:(2, 2) ~b:`Same |> Ocann.downcast
     ; up |> relu |> conv2d ~o (`Full 64) (3, 3) ~s:(2, 2) ~b:`Same
       |> bias |> Ocann.downcast
     ])
|> sum
|> (fun up ->
     [
       up |> conv2d ~o (`Full 128) (1, 1) ~s:(2, 2) ~b:`Same |> Ocann.downcast
     ; up |> relu |> conv2d ~o (`Full 128) (3, 3) ~s:(2, 2) ~b:`Same
       |> bias |> Ocann.downcast
     ])
|> sum
|> conv2d ~o (`Full 10) (3, 3) ~b:`Assert_fit
|> bias
|> softmax `C
(* Output shape is {x:1; y:1; c:10; n:unknown} *)
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
    begin
      let ( >|= ) opt f = Js.Opt.iter opt f in
      let tolist all =
        List.init all##.length (fun i -> all##item i |> Js.Opt.to_option)
        |> List.filter_map Fun.id
      in
      ref##.current >|= fun elt ->
      Firebug.console##log elt;
      elt##querySelectorAll (Js.string "pre > code")
      |> tolist
      |> List.map Misc.highlight_element
      |> ignore;
    end;
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
