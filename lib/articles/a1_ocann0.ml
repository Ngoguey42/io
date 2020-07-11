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

let t0 =
  {|
<h3>WORK IN PROGRESS!</h3>
<p>
   OCaNN's ambition is to bring to OCaml a functional neural networks (NN) semantic
   without actualy writing a tensor processing engine nor depending on one specific external
   deep-learning (DL) framework.
</p>
<p>
   To achieve that goal OCaNN provides on one hand a NN abstraction that is both
   framework-agnostic and ocaml-friendly and on the other hand separate individual DL
   framework bindings that makes the necessary tradeoffs to expose their library in an efficient
   and as functional as possible way.
</p>
<p>
   A first implementation is available
   <a href="">here</a> (and
   <a href="">here</a> for the shape abstraction)
   and two bindings already exist,
   <a href="">ocann-owl</a> and
   <a href="">ocann-tfjs</a>.
   All have been used to create the <a href="mnist-jsoo.html">mnist-jsoo</a> page.
</p>

<h2>Example: Two-Layer Perceptron</h2>

First we create the network using the main module.
<pre><code>
let nn =
  let open Ocann.Default.Builder in
  let open Ocann.Pshape.Size in
  (* Input shape is (unknown, 784) *)
  input (Pshape.abs2d_partial U (K (28 * 28))) `Float32
  |> dense [ (`Idx 1, 512) ] |> bias |> relu
  |> dense [ (`Idx 1, 10) ] |> bias
  |> softmax (`Idx 1)
  (* Output shape is (unknown, 10) *)
</code></pre>

We can train this NN using the existing Owl Algodiff (effectful) binding
<pre><code>
let categorical_crossentropy epsilon softmaxed_pred truth =
  let open Algodiff.Maths in
  softmaxed_pred |> max2 (epsilon |> Algodiff.pack_flt)
  |> log |> mul truth |> neg |> sum ~axis:(-1) |> mean

let learning_rate = 1e-3
let new_nn =
  let flattened_images, truths = ... in (* Algodiff tensors *)
  let forward, optimizations, pack = Ocann_owl.unpack_for_training nn in
  let predictions = forward flattened_images in
  let loss = categorical_crossentropy 1e-10 predictions truths in
  Algodiff.reverse_prop (Algodiff.pack_flt 1.) loss;
  Ocann_owl.OptiMap.iter (fun _ optimization -> optimization learning_rate) optimizations;
  pack ()
</code></pre>

or the exising TensorFlow.js (effectful too) binding.
<pre><code>
let learning_rate = 1e-3
let new_nn =
  let flattened_images, truths = ... in (* Tfjs tensors *)
  let forward, optimizations, pack = Ocann_tfjs.unpack_for_training nn in
  let f () =
    let predictions = forward flattened_images in
    let loss = Tfjs.categorical_crossentropy 1e-10 predictions truths in
    loss
  in
  let loss, grads = Tfjs.variable_grads f in
  Ocann_tfjs.OptiMap.iter
    (fun name optimization -> optimization learning_rate (Tfjs.Named_tensor_map.find name grads))
    optimizations;
  pack ()
</code></pre>

<h2>Benefits</h2>
<p>
   There are multiple benefits to OCaNN's approach:
   <ul><li>
   Get rid of the awkward feeling of being writing Python in OCaml by tailoring the APIs for OCaml.
   </li><li>
   No need to write yet another tensor processing engine which is the most expensive part of a DL
   framework.
   </li><li>
   Clear separation of concerns between the operations that require a computation engine
   (e.g. number crunching, inference, training) and the rest
   (e.g. network construction, initialization, modification, storage and analysis).
   </li><li>
   Sharing a common basis between multiple DL framework bindings simplifies:
   - Writing a new binding for a new fashionable DL framework.
   - Supporting several frameworks at once (e.g. you may want to use a specialized engine in production while reusing pieces of code from the training phase).
   - Porting a program from one framework to another.

   </li></ul>
</p>

<h2>Frameworks Bindings</h2>
<p>
   The ideal binding to a DL framework with OCaNN bypasses everything but the tensor computation engine. Such an engine can be seen as a push-based black-box that can dynamically receive nodes of a computation graph and output tensor promises on certain nodes. A node can either be an input tensor (e.g. image, sound, text, network parameters) or an operation between nodes (e.g. forward-conv, backward-conv-x, backward-conv-w). An output tensor is the output of any node (e.g. predictions, gradients, feature maps, updated network parameters). Binding a framework in such a way offers full flexibility to write an OCaml-friendly binding, but some problems may arise:
   <ul><li>
   Not all DL frameworks expose their engine in a low level fashion. In that case a binding has to use a higher-level API (often effectful) (e.g. the TensorFlow.js framework). TODO: What about a specialized engine that does it on purpose for performance.
   </li><li>
   It is often quicker to write a binding that reuses the higher level abstractions of a framework (e.g. the Owl binding uses Algodiff).
   </li><li>
   Some engines will not natively support certain operations. In that case the binding can either raise an exception or provide an ad-hoc implementation using the framework's constructs. Examples:
   - In the TensorFlow.js binding tensordot is implemented using transpose/reshape/dot.
   - In the Owl binding tensordot raises an exception unless it can be substituted with a dot.
   - Some engines only allow inference and not traning.
   </li></ul>
</p>

<h2>Paradigms</h2>
<p>
   Lorem ipsum
</p>


<h2>Future</h2>
<p>
   After writing and using this v0 it feel that some parts should be rewritten:
   <ul><li>
   The API should be based on an existing abstraction (i.e. ONNX, NNEF).
   </li><li>
   Polymorphic variants should be used instead of objects (e.g. [ `Conv2d ] network).
   </li><li>
   Custom layers should be first class citizens to encourage the user to implement his own abstrations.
   </li><li>
   Some features should be streamline to feel less ad-hoc (i.e. optimizer).
   </li><li>
   The Pshape library rely too much on unsafe operations.
   </li><li>
   The Owl binding should bypass Algodiff to avoid the mutable paradigm.
   </li><li>
   Some layers store tensors (e.g. parameter32, normalisation), but a design with a global weak dictionnary might be a good alternative.
   </li><li>
   As in all deep learning frameworks the network definition in OCaNN treats the backward phase as a second class citizen. Some powerful use cases involve transforming the gradient tensors from the backward phase with regular forward operations. Those use cases are hard - if not impossible - to define at the network creation time.
   </li></ul>
</p>
<p>
   Some features are yet to be implemented:
   - Many basic layers are missing (i.e. ConvTranspose, sqrt).
   - Some backends are obviously missing (i.e. torch).
   - Conversions to/from storage formats such as ONNX and NNEF.
   - Small improvements listed in <a href="">ocann.ml</a>.
</p>
<p>
   If you wish to contribute or discuss about it, e-mail me!
</p>

<h2>Example: Symbolic Shapes and Residual CNN</h2>

<pre><code>
let open Ocann.Default.Builder in
let open Ocann.Pshape.Size in

(* Input shape is {x:24; y:24; c:3; n:unknown} *)
input (Pshape.sym4d_partial ~n:U ~c:(K 3) ~s0:(K 24) ~s1:(K 24)) `Int32
|> astype `Float32
|> conv2d ~o (`Full 32) (3, 3) ~s:(2, 2) ~b:`Same
|> bias
|> (fun up ->
     [
       up |> conv2d ~o (`Full 64) (1, 1) ~s:(2, 2) ~b:`Same |> Fnn.downcast
     ; up |> relu |> conv2d ~o (`Full 64) (3, 3) ~s:(2, 2) ~b:`Same
       |> bias |> Fnn.downcast
     ])
|> sum
|> (fun up ->
     [
       up |> conv2d ~o (`Full 128) (1, 1) ~s:(2, 2) ~b:`Same |> Fnn.downcast
     ; up |> relu |> conv2d ~o (`Full 128) (3, 3) ~s:(2, 2) ~b:`Same
       |> bias |> Fnn.downcast
     ])
|> sum
|> conv2d ~o (`Full 10) (3, 3) ~b:`Assert_fit
|> bias
|> softmax `C
(* Output shape is {x:1; y:1; c:10; n:unknown} *)
</code></pre>

<h2>Brouillon</h2>
<pre>

- 1 framework for 1 situation
  - Les frameworks sont tres nombreux avec chacun leur interet, certains sont rapides, certains sont en JS, certains sont leges, certains ne gerent que l'inference
  - De nouveaux arrivent tous les ans
  - Ca fait egalement echo a la mouvance qui est d'avoir des formats de NN interchangeables avec des runtimes specialises independants des languages.
- Apports d'OCaml et des paradigmes fonctionnels pour faire du DL.
  - Un programme de training python est error-prone sur des details qui pourraient etre verifies par des types et moins de mutabilite
    - problemes de dimensions par exemple

# Paradigms
- This is not keras
  - framework-agnostic-global-module / single-framework-aware-module
  - On a rarement besoin de devoir gerer plusieurs engines avec 1 seul code
    - comme ce que propose Keras
    - j'avais besoin dans mnist-jsoo, j'ai du dupliquer des bouts de code
  - L'approche Keras fait trop de compromis, les "advanced use cases" sont infaisable
- The main module ne transige pas avec les aspects fonctionnels, un module specialise transige comme il faut pour que ce soit efficient+fonctionnel
- The library should be as generic as possible.
  - Ex: No asumptions on shape ordering.
- Favor low-level flexible APIs versus the high-level black-box ones
- encourage user to write his own implementations rather than providing ad-hoc code (No LSTM layer in lib)
  - Ex: No `train` function in the spec modules, everyone that does deep learning know in which order the general operations should take place
  - interoperability with user's ad-hoc layers
- Favor user algorithms implementations in bridge modules
- User layers first class (implement all pre-made layers
- Support advanced use cases
  - Weight sharing
  - kernel weight is result of operation (meta learning)
  - Layers that only affect backward
  - Gradient tempering, Backward phase first-class
- Preserve the network structure at the end of a training, at all cost.
- Why weight/bias have been split
- Talk about tensor initialization and RNG (just one sentense for now)
- Why symbolic shapes (link those libs/articles)
- What about modular implicits, what will those bring?
- OCaNN shouldn't be low level enough to avoid feeling like a rigid toybox
- OCaNN shouldn't be a barrier when the user want to implement a new way of doing deep learning.
- OCaNN is not just for inference
- A network is a list of leaf nodes and everything that is reachable from them (might have disconnected components)



# Chunks
OCaml doesn't possess an industrial strength deep-learning framework like python does. The only
way to bring industrial strength deep-learning to OCaml is through bindings to the latests fashionable
frameworks out there.

Unfortunatly for OCaml the API of those libraries most of the time rely on imperative paradigms.
But what makes a deep-learning framework to be of industrial strength today is only its capacity
to crunch numbers as quickly as possible given a computation graph and a list of available hardware devices.

Everything else (i.e. its user API) is designed for the host programming language and
is unfit to be direcly used from a functional programming language like OCaml.

The <code>Ocann</code> module provides all aspects of a deep-learning frameworks that do not
require heavy computations (e.g. neural network definition, manipulation, storage)
with an OCaml friendly API. Other standalone modules (e.g. <code>Ocann_tfjs</code>,
<code>Ocann_owl</code>) provide bindings to individual foreign deep learning frameworks
built on top of the <code>Ocann</code> module.

</pre>
|}

let construct_reactjs_article () =
  let render () =
    let open Reactjs.Jsx in
    let box =
      [ of_tag "h1" [ of_string "OCaNN library" ]; of_tag "div" ~inner_html:t0 [] ]
      |> of_bootstrap "Col" >> of_bootstrap "Row"
      >> of_bootstrap "Container" ~fluid:true ~classes:[ "bigbox1" ]
    in
    of_react "Fragment" [ box ]
  in
  Reactjs.construct render

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
