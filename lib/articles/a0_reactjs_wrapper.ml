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
   <a href="https://github.com/Ngoguey42/io/blob/master/lib/ft_js/reactjs.mli">
   This small unnamed wrapper</a>
   enhances the <a href="https://reactjs.org/">Reactjs</a> experience in OCaml by
   hiding most of the boilerplate and offering a <cite>functional reactive programming</cite>
   interface through <a href="https://erratique.ch/software/react">React(ml)</a>.
</p>

<p>
   You may copy/modify/distribute this wrapper at will (ISC License).
</p>

<h2>Reactjs Components</h2>

<p>
   In Reactjs, a component is a JavaScript object (or closure) with bits of logit
   attached to it
   in order to govern the behavior of a DOM element.
   There are two ways for a user to define a component, either by creating a class inheriting from
   <code>React.Component</code> or using the Hooks API.
</p>

<p>
   When using the inheritance pattern, your component interacts with the Reactjs
   engine through the methods and properties of your object (e.g. the <code>render</code> method
   is called when a render has to take place, you mutate the <code>state</code>
   variable to trigger renders). On the other hand, when using hooks you only
   define a <code>render</code> function and access the other features of Reactjs by performing
   side effects through the <code>React.use*</code> functions.
</p>

<p>
   None of those two syntaxes fit nicely with OCaml (IMO):
   <ul>
   <li>
   <a href="https://reactjs.org/docs/hooks-intro.html#motivation">The argument</a>
   made by Reactjs developers in favor of hooks and against the class syntax also
   holds for OCaml (i.e. classes require a higher cognitive load that functions).
   </li><li>
   The Hooks API is full of subtle side effects. Reactjs developers
   even recommend using a <a href="https://reactjs.org/docs/hooks-rules.html">static analyzer</a>
   to verify the control flow of a user defined component.
   </li>
   </ul>
</p>

<p>
   Consequently, this OCaml wrapper is based on the <code>React.Component</code> class, exposed
   through functions, but using a less esoteric <i>constructor first</i> paradigm instead of the
   <i>render first</i> paradigm adopted by Hooks.
   You define a constructor function in which
   you define at least a <code>render</code> function to be returned.
   To access the other features of Reactjs you may return more than
   a <code>render</code> function at the end of the constructor. For example:
   <ul>
   <li>one or more React(ml) signals to trigger renders
   (i.e. binding of <code>React.Component.state</code>),</li>
   <li>a <code>mount</code> function
   (i.e. binding of <code>React.Component.componentDidMount</code>),</li>
   <li>an <code>update</code> function
   (i.e. binding of <code>React.Component.componentDidUpdate</code>),</li>
   </ul>

   Thanks to the use of React(ml), all the explicit side effects on the <code>.state</code>
   variable (needed to trigger renders) are hidden away from the user.
</p>

<h2>JSX</h2>
<p>
   Reactjs defines a very convenient syntax extension called JSX to
   help make <cite>render</cite> functions look like HTML. This syntax extension hides
   calls to <code>React.createElement</code>, a function that can be called from
   OCaml.
   The <code>Reactjs.Jsx</code> module of this wrapper provides several high level functions above
   <code>React.createElement</code>.
</p>
<p>
   This table lists some correspondences between JSX and the OCaml wrapper.
</p>
<table class="fttable">
<thead><tr>
   <th>JSX</th>
   <th>OCaml</th>
</tr></thead>
<tbody>
<tr><th><code>
   &lt;br/&gt;
   </code></th><th><code>
   of_tag "br" []
</code></th></tr>
<tr><th><code>
   &lt;h1&gt;Hello&lt;div/&gt;
   </code></th><th><code>
   of_string "Hello" >> of_tag "h1"
</code></th></tr>
<tr><th><code>
   &lt;img src=&quot;icon.png&quot;/&gt;
   </code></th><th><code>
   of_tag "img" ~src:"icon.png" []
</code></th></tr>
<tr><th><code>
   &lt;Fragment&gt;{ array_of_components }&lt;/Fragment&gt;
   </code></th><th><code>
   of_react "Fragment" list_of_components
</code></th></tr>
<tr><th><code>
   &lt;Spinner size=&quot;sm&quot;/&gt;
   </code></th><th><code>
   of_bootstrap "Spinner" ~size:"sm" []
</code></th></tr>
<tr><th><code>
   &lt;MyComponent set_state={ set_state } /&gt;
   </code></th><th><code>
   of_constructor my_constructor set_state
</code></th></tr>
</tbody>
</table>

<h2>Props</h2>
<p>
   <cite>Props</cite> is the name given to the data travelling from a parent component to a child
   component in Reactjs. Both the constructor and the render function receive the
   props because Reactjs remembers the hierarchy of the components from
   one render to another and calls the constructor only before the first render.
</p>
<p>
   In order to ensure the proper typing of props and preserve Reactjs's
   behavior regarding component hierarchy, some <cite>magic</cite> is required under the hood.
</p>

<h2>Simple Example</h2>
<p>
  Two components are defined in this example.
</p>

<p>
  The one above is a stateless component that only defines a <code>render</code> function. Its
  input prop is a tuple of 3 values.
</p>

<p>
  The one below is a stateful component that defines one signal and a
  <code>render</code> function. Reactjs will call the <code>render</code> function
  after construction and every time the signal's value changes
  (i.e. after a call to <code>set_signal</code>).
</p>

<p>
  Both can be instantiated into element objects using <code>Reactjs.of_constructor</code>.
</p>

<p>
   In this example a <code>React.event</code> is created and transformed into a
   <code>React.signal</code> using <code>React.S.accum</code>.
   Many other operations are available in React(ml)
   to transform and create primitives (i.e. signal / event). Take a look at the documentation
   <a href="https://erratique.ch/software/react">https://erratique.ch/software/react</a>.
</p>

<p>
   As Reactjs developers recommend
   <a href="https://reactjs.org/docs/lifting-state-up.html">here</a>,
   the state (<code>value_signal</code>) is stored in the topmost component
   and a callback to update the state (<code>fire_operation</code>) is passed to the child components
   through the props.
<p>
|}

let t1 =
  {|
<h2>Automatic Garbage Collection of React(ml) Primitives</h2>

<p>
   In the previous example the <code>operation_events</code> value is explicitly collected inside
   the <code>unmount</code> callback.
   In general, when using React(ml) with Js_of_ocaml, all primitives must
   be explicitly collected.
   The reason is because React(ml) internally relies on
   OCaml's <cite>weak hash tables</cite> to ensure proper garbage collection, which is currently
   implemented with regular <cite>hash tables</cite> in Js_of_ocaml.
</p>
<p>
   To help in this tedious process the <code>of_constructor_*</code> functions
   offer a way to automatically collect the input primitives to a component when
   <cite>unmount</cite> is fired by Reactjs.
   This will trigger the garbage collection of most of the primitives created in a component
   while preserving the original input primitives integrity if they are still in use elsewhere.
</p>
<p>
   More formally, if the component <code>a</code> creates two components <code>b</code> and
   <code>c</code> and sends them the same primitive <code>p</code>, the wrapper will not
   directly pass <code>p</code> to <code>b</code> but a copy <code>p'</code> of
   <code>p</code>. When <code>b</code> unmounts, <code>p'</code> will be automatically stopped
   which will trigger the collection of
   all the other primitives created by <code>b</code> that only depends on <code>p'</code>.
   <code>p</code> will survive this collection, which is important because it is still in use by
   <code>c</code> through its own copy of <code>p</code>.
</p>
<p>
   The other primitives created by calls to <code>React.S.create</code> and
   <code>React.E.create</code> still have to be manually collected.
</p>

<h2>GC Example</h2>
<p>
   This example is a bit more complicated. Its only purpose is to highlight the leak occurring
   inside the
   <code>leaking_head (queries, answer)</code> constructor versus the leak-free
   <code>safe_head ~e0:queries answer</code> constructor.
</p>
<p>
   For typing concerns, <code>safe_head</code> has to be instantiated using the
   <code>of_constructor_e</code> function. Several other permutations are available such as
   <code>of_constructor_ssee</code> for a constructor with the following type:
</p>
<pre><code id="type0" class="language-ocaml">s0:'s0 React.signal ->
s1:'s1 React.signal ->
e0:'e0 React.event ->
e1:'e1 React.event ->
'main_props ->
'main_props construction</code></pre>
|}

let construct_reactjs_article () =
  let ref0 = Reactjs.create_ref () in
  let render () =
    let open Reactjs.Jsx in
    let to_table title content =
      let head = of_string title >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
      let body = content >> of_tag "th" >> of_tag "tr" >> of_tag "tbody" in
      [ head; body ] |> of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm"
    in

    let snip_jsx0 = of_constructor Reactjs_ex0.construct_component () in
    let snip_code0 = of_constructor Misc.construct_article_code "reactjs_ex0.ml" in
    let snip_jsx1 = of_constructor Reactjs_ex1.construct_component () in
    let snip_code1 = of_constructor Misc.construct_article_code "reactjs_ex1.ml" in

    let box =
      [
        of_tag "h1" [ of_string "Reactjs OCaml wrapper" ];
        of_tag "div" ~inner_html:t0 [];
        to_table "Simple Example: Code" snip_code0;
        to_table "Simple Example: Result" snip_jsx0;
        of_tag "div" ~inner_html:t1 ~ref:ref0 [];
        to_table "GC Example: Code" snip_code1;
        to_table "GC Example: Result" snip_jsx1;
      ]
      |> of_bootstrap "Col" >> of_bootstrap "Row"
      >> of_bootstrap "Container" ~fluid:true ~classes:[ "bigbox1" ]
    in
    of_react "Fragment" [ box ]
  in
  let mount () =
    (let ( >|= ) opt f = Js.Opt.iter opt f in
     ref0##.current >|= fun elt ->
     elt##querySelector (Js.string "#type0") >|= fun elt -> Misc.highlight_element elt);
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
