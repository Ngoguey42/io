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
<p>
WIP
</p>
<p>
   OCaNN's ambition is to bring to OCaml a functional neural networks (NN) semantic
   without actually writing a tensor processing engine nor depending on one specific external
   deep-learning (DL) framework.
</p>
<p>
   To achieve that goal OCaNN provides on one hand a NN abstraction that is both
   framework-agnostic and ocaml-friendly and on the other hand separate individual DL
   framework bindings that makes the necessary tradeoffs to expose their library in an efficient
   and as functional as possible way.
</p>
<p>
   etc...
</p>
<p>
   To conclude the presentation here are two network construction examples using OCaNN. The first
   one builds a simple two-layer perceptron, the second one builds a small residual convolutional
   NN.
</p>
<pre><code>
let open Ocann.Default.Builder in
let open Ocann.Pshape.Size in

(* Input shape is (unknown, 784) *)
input (Pshape.abs2d_partial U (K (28 * 28))) `Float32
|> dense [ (`Idx 1, 512) ] |> bias |> relu
|> dense [ (`Idx 1, 10) ] |> bias
|> softmax (`Idx 1)
(* Output shape is (unknown, 10) *)
</code></pre>

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


<pre>Brouillon

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


# TODO
- take a look at ocamltorch ocamltf

# Intro
[X] On veut faire du DL fonctionnel en OCaml
[X] On depend pas d'un framework en particulier
[X] 1 main module.
  [] Ne transige pas avec les aspects fonctionnels
  [] Facilite le binding des libs spe
[X] N spec modules
  [X] Transige comme il faut pour que ce soit efficient+fonctionnel

# Les benefices specifiques a cette aproche framework-agnostic-global-module / single-framework-aware-module
- Premisses
  - Dans un framework ce qui coute cher c'est l'engine, on reuse plutot que de recoder
  - Les frameworks sont tres nombreux avec chacun leur interet, certains sont rapides, certains sont en JS, certains sont leges
  - De nouveaux arrivent tous les ans
  - On a rarement besoin de devoir gerer plusieurs engines avec 1 seul code
    - comme ce que propose Keras
    - j'avais besoin dans mnist-jsoo, j'ai du dupliquer des bouts de code
  - L'approche Keras fait trop de compromis, les "advanced use cases" sont infaisable
- Resultat
  - pas de charge de travail de codage d'engine
  - moins de charge de travail pour creer des bindings de nouvelles libs
  - moins de charge de travail pour changer de framework dans un code vu qu'une partie est commune

# Motivations
- Dans une lib de deep learning la partie engine est d'une maniere ou d'une autre standalone. il recoit un computation graph dynamiquement, et il se debrouille. e.g. pas de reelle notion de backward si ce n'est:
  - forward-conv (x, w) -> z
  - backward-conv-a (x, w, z, z') -> x'
  - backward-conv-a (x, w, z, z') -> w'
- On peut se plug directement sur les engines et donner une ocaml-flavor a tout le reste
- Apports d'OCaml et des paradigmes fonctionnels pour faire du DL.
  - Un programme de training python est error-prone sur des details qui pourraient etre verifies par des types et moins de mutabilite
    - problemes de dimensions par exemple

# Details
- Un bout de code qui utilise seulement le module `Ocann` est:
  - compatible avec tous les frameworks
  - peut etre execute sans framework
- Les modules spe doivent se connecter a un framework au niveau le plus bas possible pour proposer une bonne api (c'est pas toujours tres bas => ce n'est pas toujours tres bon)
- Exemples d'use case avances
  - Weight sharing
  - kernel weight is result of operation (meta learning)
  - Layers that only affect backward
  - Gradient tempering, Backward phase first-class

# NN tasks that do not require computations
- construction
- initialization
- modifications
- storage
- network analysis
- symbolic analysis of unknown dimensions (can help warn in network is un-instanciable)

# Benefits
- No need to rewrite yet another tensor processing
- A single abstraction for unix, JS and other environments
- No need to reinvent everything for every new fashionable DL framework
- Bring type-safety and immutability to NNs manipulations
- APIs tailored for OCaml (doesn't give the ackward feeling of writing python in OCaml)
- The library should be as generic as possible.
  - Ex: No asumptions on shape ordering.
- encourage user to write his own implementations rather than providing ad-hoc code (No LSTM layer in lib)
  - Ex: No `train` function in the spec modules, everyone that does deep learning know in which order the general operations should take place
  - interoperability with user's ad-hoc layers
- Ca fait egalement echo a la mouvance qui est d'avoir des formats de NN interchangeables avec des runtimes specialises independants des languages.
- Autre avantage: pas besoin d'importer tout tensorflow dans un programme qui ne va faire que manipuler le reseau

# Problems
- It might not be possible to bind some backends on a level low enough to create nice abstractions
- Some operations available in the main module might not be implementable with some frameworks.

# Future
- No more OCaml objects
- A revised Pshape library
- More exhaustiveness on classic layers (i.e. ConvTranspose, sqrt)
- Streamline some features that currently seem a bit ad-hoc (i.e. optimizer)
- Custom Layers (to be deeply anchored inside the library)
- An owl backend that bypasses Algodiff (say why)
- More backends

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
