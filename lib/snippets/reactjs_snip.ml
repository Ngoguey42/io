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
   enhances the <cite>Reactjs</cite> experience in OCaml by hiding most of the
   boilerplate and offering a <cite>functionnal reactive programming</cite> experience through
   <cite>Reactjs(ml)</cite>.
</p>

<p>
   You may copy/modify/distribute this wrapper at will (without warranty, etc...).
</p>

<h2>React Components</h2>

<p>
   To define a component in <cite>Reactjs</cite> you can either define a class that inherits from
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
   This OCaml wrapper is in-between the two native approaches. You define a constructor function
   (much like the <code>constructor</code> method of a <code>React.Component</code>) in which
   you define at least a <code>render</code> function to be returned.
   To access the other features of <cite>Reactjs</cite> you may return more than
   a <code>render</code> function at the end of the constructor. For example:
   <ul>
   <li>one or more <code>React(ml)</code> signals to trigger renders (binding of <code>.state</code>)</li>
   <li>a <code>mount</code> function (binding of <code>.componentDidMount</code>)</li>
   <li>an <code>update</code> function (binding of <code>.componentDidUpdate</code>)</li>
   <li>etc...</li>
   </ul>

   With this approch, the only OCaml-side side effects are hidden behind the <cite>React(ml)</cite>
   primitives.
</p>

<h2>JSX</h2>
<p>
   <cite>Reactjs</cite> defines a very convenient syntax extension called <cite>JSX</cite> to
   help <code>render</code> functions look like HTML. This syntax hides
   calls to <cite>React.createElement</cite> that can be called with <cite>Js_of_ocaml</cite>.
   The <code>Reactjs.Jsx</code> module provides several high level functions above
   <cite>React.createElement</cite>.
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
   component in <cite>Reactjs</cite>. Both the constructor and the render function receive the
   <cite>props</cite> because <cite>Reactjs</cite> remembers the hierarchy of the components from
   one render to another and calls the constructor only before the first render.
   The same goes with this OCaml wrapper.
</p>

<h2>Example</h2>
<p>
  Two components are defined in this example.
</p>

<p>
  The one above is a stateless component that only defines a render function. It looks a lot like
  <cite>Reactjs</cite>'s <cite>hooks</cite>. It can be turned to <cite>Jsx</cite> using
  <code>Reactjs.of_render</code>.
</p>

<p>
  The one below is a stateful component that defines one signal and a
  render function. <cite>Reactjs</cite> will call the <cite>render</cite> function
  after construction and everytime the signal's value changes
  (i.e. after a call to <code>set_signal</code>). It can be turned to <cite>Jsx</cite> using
  <code>Reactjs.of_constructor</code>.
</p>

<p>
   A <code>React.event</code> is created and transformed to a signal using
   <code>React.S.accum</code>. Many other operations are available in <cite>React(ml)</cite>
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

let construct_reactjs_snippet () =
  Printf.printf "> Component - reactjs_snippet | construct\n%!";
  let render () =
    let open Reactjs.Jsx in
    Printf.printf "> Component - reactjs_snippet | render\n%!";
    let to_table title content =
      let head = of_string title >> of_tag "th" >> of_tag "tr" >> of_tag "thead" in
      let body = content >> of_tag "th" >> of_tag "tr" >> of_tag "tbody" in
      [ head; body ] |> of_bootstrap "Table" ~classes:[ "smallbox0" ] ~bordered:true ~size:"sm"
    in

    let snip_jsx = of_constructor Reactjs_ex0.construct_component () in
    let snip_code = of_constructor Misc.construct_snippet_code "reactjs_ex0.ml" in

    let box =
      [
        of_tag "h1" [ of_string "Reactjs wrapper" ];
        of_tag "div" ~inner_html:t0 [];
        to_table "Example: Code" snip_code;
        to_table "Example: Result" snip_jsx;
      ]
      |> of_bootstrap "Col" >> of_bootstrap "Row"
      >> of_bootstrap "Container" ~fluid:true ~classes:[ "bigbox1" ]
    in
    of_react "Fragment" [ box ]
  in

  Reactjs.construct render
