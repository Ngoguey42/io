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
   <a href="https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib/ft_js/reactjs.ml">
   This small wrapper</a>
   enhances the <a href="https://reactjs.org/"><cite>Reactjs</cite></a> experience in OCaml by
   hiding most of the boilerplate and offering a <cite>functionnal reactive programming</cite>
   interface through <a href="https://erratique.ch/software/react"><cite>React(ml)</cite></a>.
</p>

<p>
   You may copy/modify/distribute this wrapper at will.
</p>

<h2>React Components</h2>

<p>
   To define a component with <cite>Reactjs</cite> you can either define a class that inherits from
   <code>React.Component</code> or use the <cite>Hook</cite> API.
</p>

<p>
   When using the inheritance pattern your component interacts with <cite>Reactjs</cite>
   through the methods and properties of your class (e.g. the <cite>render</cite> method
   is called when a render has to take place) (e.g. you modify the <cite>state</cite>
   variable to trigger renders). On the other hand when using <cite>Hooks</cite> you only
   define a render function and access the other features of <cite>Reactjs</cite> by performing
   side effects through the <code>React.use*</code> functions.
</p>

<p>
   This OCaml wrapper is based on the <code>React.Component</code> class, but when using it
   it feel a lot like the <cite>Reacjs Hooks</cite>. You define a constructor function
   (much like the <code>constructor</code> method of a <code>React.Component</code>) in which
   you define at least a <code>render</code> function to be returned.
   To access the other features of <cite>Reactjs</cite> you may return more than
   a <code>render</code> function at the end of the constructor. For example:
   <ul>
   <li>one or more <cite>React(ml)</cite> signals to trigger renders (binding of <code>.state</code>),</li>
   <li>a <code>mount</code> function (binding of <code>.componentDidMount</code>),</li>
   <li>an <code>update</code> function (binding of <code>.componentDidUpdate</code>),</li>
   <li>etc...</li>
   </ul>

   With this approch, the only OCaml-side side effects are hidden behind the <cite>React(ml)</cite>
   primitives.
</p>

<h2>JSX</h2>
<p>
   <cite>Reactjs</cite> defines a very convenient syntax extension called <cite>JSX</cite> to
   help make <code>render</code> functions look like HTML. This syntax hides
   calls to <cite>React.createElement</cite> that can be called from <cite>Js_of_ocaml</cite>.
   The <code>Reactjs.Jsx</code> module of this wrapper provides several high level functions above
   <cite>React.createElement</cite>.
</p>
<p>
   This table lists some correspondences between <cite>JSX</cite> and the OCaml wrapper.
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
   component in <cite>Reactjs</cite>. Both the constructor and the render function receive the
   <cite>props</cite> because <cite>Reactjs</cite> remembers the hierarchy of the components from
   one render to another and calls the constructor only before the first render.
</p>
<p>
   In order to ensure the proper typing of <code>props</code> and preserve <cite>Reactjs</cite>'s
   behavior regarding components' hierarchy, some <cite>magic</cite> is required under the hood.
</p>

<h2>Simple Example</h2>
<p>
  Two components are defined in this example.
</p>

<p>
  The one above is a stateless component that only defines a render function.
</p>

<p>
  The one below is a stateful component that defines one signal and a
  render function. <cite>Reactjs</cite> will call the <cite>render</cite> function
  after construction and everytime the signal's value changes
  (i.e. after a call to <code>set_signal</code>).
</p>

<p>
  Both can be instanciated into <cite>JSX</cite> objects using <code>Reactjs.of_constructor</code>.
</p>

<p>
   In this exemple a <code>React.event</code> is created and transformed to a
   <code>React.signal</code> using <code>React.S.accum</code>.
   Many other operations are available in <cite>React(ml)</cite>
   to transform and create primitives (i.e. signal / event). Take a look at the documentation
   <a href="https://erratique.ch/software/react">https://erratique.ch/software/react</a>.
</p>

<p>
   As <cite>Reactjs</cite> recommends
   <a href="https://reactjs.org/docs/lifting-state-up.html">here</a>,
   the state (<code>value_signal</code>) is stored in the topmost component
   and a callback to update the state (<code>fire_operation</code>) is passed to the child components
   through the <cite>props</cite>.
<p>
|}

let t1 =
  {|
<h2><cite>React(ml)</cite> Garbage Collection</h2>

<p>
   In the previous example the <code>operation_events</code> value is explicitly collected inside
   the <code>unmount</code> callback.
   In general when using <cite>React(ml)</cite> with <cite>Js_of_ocaml</cite>, all primitives must
   be explicitly collected.
   The reason is because <cite>React(ml)</cite> internally relies on
   OCaml's <cite>weak hash tables</cite> to ensure proper garbage collection which is currently
   implemented with regular <cite>hash tables</cite> in <cite>Js_of_ocaml</cite>.
</p>
<p>
   To help in this tedious process the <code>of_constructor_*</code> functions
   offer a way to automatically collect the input primitives to a component when
   <cite>unmount</cite> is fired by <cite>Reactjs</cite>.
   This will trigger the garbage collection of most of the primitives created in a component
   while preserving the original input primitives integrity if they are still in use elsewhere.
</p>
<p>
   More formally, if the component <code>a</code> creates two components <code>b</code> and
   <code>c</code> and sends them the same primitive <code>pa</code>, the wrapper will not
   directly pass <code>pa</code> to <code>b</code> but a copy <code>pa'</code> of
   <code>pa</code>. When <code>b</code> unmounts, <code>pa'</code> will be automatically collected
   which will trigger the collection of
   all the other primitives created by <code>b</code> that only depends on <code>pa'</code>.
   <code>pa</code> will survive this collection since it is still in use by <code>c</code>.
</p>
<p>
   The other primitives created by calls to <code>React.S.create</code> and
   <code>React.E.create</code> still have to be manually collected.
</p>

<h2>GC Example</h2>
<p>
   This example highlights a leak occuring inside the
   <code>leaking_head (queries, answer)</code> constructor versus the leak-free
   <code>safe_head ~e0:queries answer</code> constructor.
</p>
<p>
   For typing concerns, <code>safe_head</code> has to be instanciated using the
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
  Printf.printf "> Component - reactjs_article | construct\n%!";
  let ref0 = Reactjs.create_ref () in
  let render () =
    let open Reactjs.Jsx in
    Printf.printf "> Component - reactjs_article | render\n%!";
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
        of_tag "h1" [ of_string "Reactjs wrapper" ];
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
    let ( >|= ) opt f = Js.Opt.iter opt f in
    ref0##.current >|= fun elt ->
    elt##querySelector (Js.string "#type0") >|= fun elt -> Misc.highlight_element elt
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
