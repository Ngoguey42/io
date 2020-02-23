module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

let create_content () =
  [%html
    {|
    <div style="
       width: 800px;
       margin: 0 auto;
       border-radius: 10px;
       background: #EBEBEB;
       padding: 10px;
       line-height: 1.4;
       border-color: #CBCBCB;
       border-style: solid;
       border-width: medium;
    ">
      <h1>Making-of</h1>


      <h2>This website</h2>
      This website is hosted by <cite>github</cite> as a
      <a href="https://pages.github.com/"><cite>GitHub Pages</cite></a>,
      the source code is publicly available on
      <a href="https://github.com/ngoguey42/ngoguey42.github.io"><cite>github</cite></a>.

      Everything here is static and written in
      <a href="https://ocaml.org/"><cite>OCaml</cite></a>,
      built using
      <a href="https://github.com/ocaml/dune"><cite>dune</cite></a>
      and transpiled from <cite>OCaml</cite> to <cite>JavaScript</cite> using
      <a href="https://ocsigen.org/js_of_ocaml"><cite>Js_of_ocaml</cite></a>.

      The asynchronous <cite>JavaScript</cite> events are handled using the
      <a href="https://ocsigen.org/lwt"><cite>Lwt</cite></a>
      <cite>OCaml</cite> library.

      The <cite>html</cite> is typed according to the <cite>W3C</cite> recommendations using the
      <a href="https://ocsigen.org/tyxml"><cite>TyXML</cite></a>
      <cite>OCaml</cite> library before being generated.

      The
      <a href="https://ocsigen.org/tyxml/4.3.0/manual/ppx"><cite>TyXML-ppx</cite></a>
      and
      <a href="https://ocsigen.org/js_of_ocaml/3.1.0/manual/ppx"><cite>Js_of_ocaml-ppx</cite></a>
      <cite>OCaml</cite> syntax extensions are used throughout the project.


      <h3>Code organization</h3>
      All the pages share a single entry point:
      <a href="https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/bin/page_builder.ml">bin/page_builder.ml</a>,
      the rest of the code is scattered between the
      <a href="https://github.com/Ngoguey42/ngoguey42.github.io/tree/master/bin">bin</a>
      directory and a collection of libraries living in the
      <a href="https://github.com/Ngoguey42/ngoguey42.github.io/tree/master/lib">lib</a>
      directory. The entry point and all its <cite>OCaml</cite> dependencies are transpiled to
      a single <cite>.js</cite> file living in the
      <cite>build</cite> directory (the usual <cite>_build</cite> directory name could not be used
      because <cite>Github Pages</cite> seem to ignore directories starting with an <cite>_</cite>).
      The generated <cite>.js</cite> file is then commited to the repository and
      each page of the website owns a small <cite>html</cite> file that loads the generated
      file.


      <h3>The <cite>Learning Rate Craft</cite> game</h3>
      The
      <a href="https://ocaml.xyz/"><cite>Owl</cite></a>
      scientific computating library is used for all the <cite>numpy-like</cite> and
      <cite>pytorch-like</cite> jobs. Only <cite>owl-base</cite>, a subset of <cite>owl</cite>,
      can be compiled to <cite>JavaScript</cite>.

      It implies that all deep-learning computations
      of this project happend inside a single-threaded javascript
      <a href="https://ocaml.xyz/book/backend.html">backend</a>.

      ONNX? owl-symbolic? webgl? a js deeplearning lib? totally drop owl to use a js lib ?

      <br/>
      MNIST downloaded from / cached to IndexedDb using


      <h2>Author</h2>
      Made by ngoguey,
      <br/>
      <a href="https://github.com/ngoguey42">www.github.com/ngoguey42</a>,
      <br/>
      <cite style="
         border-radius: 5px;
         background: #CBCBCB;
         padding: 1px 10px;
         border-color: #BBBBBB;
         border-style: solid;
         border-width: 1px;
      ">echo "rf.24.tneduts@yeugogn" | rev</cite>.
    </div>
  |}]
