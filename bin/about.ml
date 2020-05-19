module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

let url_repo = "https://github.com/ngoguey42/ngoguey42.github.io"

let url_bin = "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/bin"

let url_lib = "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib"

let url_ftreact =
  "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib/ft_js/reactjs.ml"

let url_entrypoint =
  "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/bin/page_builder.ml"

let url_jsooppx = "https://ocsigen.org/js_of_ocaml/3.1.0/manual/ppx"

let create_content () =
  [%html
    {|
    <div class="textdiv">
      <h1>Making-of</h1>
      <h2>This website</h2>

      This website is hosted by <cite>github</cite> as a
      <a href="https://pages.github.com/"><cite>GitHub Pages</cite></a>,
      the source code is publicly available on
      <a href="|}
      url_repo
      {|"><cite>github</cite></a>.

      <br/><br/>

      Everything here is static and written in
      <a href="https://ocaml.org/"><cite>OCaml</cite></a>,
      built using
      <a href="https://github.com/ocaml/dune"><cite>dune</cite></a>
      and transpiled from <cite>OCaml</cite> to <cite>JavaScript</cite> using
      <a href="https://ocsigen.org/js_of_ocaml"><cite>Js_of_ocaml</cite></a>.

      <br/><br/>

      The asynchronous <cite>JavaScript</cite> events are handled using the
      <a href="https://ocsigen.org/lwt"><cite>Lwt</cite></a>
      <cite>OCaml</cite> library and the
      <a href="https://erratique.ch/software/react"><cite>React</cite></a>
      <cite>OCaml</cite> library (Not to be confused with <cite>React (js)</cite>).

      <br/><br/>

      The <cite>html</cite> is generated using either the
      <a href="https://ocsigen.org/tyxml"><cite>TyXML</cite></a>
      <cite>OCaml</cite> library or using the
      <a href="https://reactjs.org/"><cite>React</cite></a> <cite>JavaScript</cite> library
      when complex interactions are required.
      <a href="|}
      url_ftreact
      {|">This small wrapper</a>
      enhances the <cite>React (js)</cite> experience in <cite>OCaml</cite> by hidding most of the
      boilerplate and hiding the <cite>states</cite> behind the functionnal reactive primitives of
      <cite>React (ml)</cite>.

      <br/><br/>

      The
      <a href="https://ocsigen.org/tyxml/4.3.0/manual/ppx"><cite>TyXML-ppx</cite></a>
      and
      <a href="|}
      url_jsooppx
      {|"><cite>Js_of_ocaml-ppx</cite></a>
      <cite>OCaml</cite> syntax extensions are used throughout the project.

      <h3>Code organization</h3>

      All the pages share a single entry point:
      <a href="|}
      url_entrypoint
      {|">bin/page_builder.ml</a>,
      the rest of the code is scattered between the
      <a href="|}
      url_bin
      {|">bin</a>
      directory and a collection of libraries living in the
      <a href="|}
      url_lib
      {|">lib</a>
      directory. The entry point and all its <cite>OCaml</cite> dependencies are transpiled to
      a single <cite>.js</cite> file living in the
      <cite>build</cite> directory (the usual <cite>_build</cite> directory name could not be used
      because <cite>Github Pages</cite> seem to ignore directories starting with an <cite>_</cite>).
      The generated <cite>.js</cite> file is then commited to the repository and
      each page of the website owns a small <cite>html</cite> file that loads the generated
      file. The external libraries are loaded on the fly.

      <br/><br/>

      <h3>The <cite>Learning Rate Craft</cite> game</h3>

      <h4>TODO</h4>
     <br/> - Fnn
     <br/> - Training from a webworker to avoid blocking
     <br/> - Tensorflowjs backend with fnn
     <br/> - Owl backend with fnn
     <br/> - MNIST dataset
     <br/> - Irmin

      <h4>OLD</h4>
      The
      <a href="https://ocaml.xyz/"><cite>Owl</cite></a>
      scientific computating library is used for all the <cite>numpy-like</cite> and
      <cite>pytorch-like</cite> jobs. Only <cite>owl-base</cite>, a subset of <cite>owl</cite>,
      can be compiled to <cite>JavaScript</cite>.

      It implies that all deep-learning computations
      of this project happend inside a single-threaded javascript
      <a href="https://ocaml.xyz/book/backend.html">backend</a>.

      ONNX? owl-symbolic? webgl? a js deeplearning lib? totally drop owl to use a js lib ?
      <br/><br/>

      The <cite>MNIST</cite> dataset is downloaded from
      <a href="http://yann.lecun.com/exdb/mnist/">http://yann.lecun.com/exdb/mnist</a>,
      unzipped using the
      <a href="https://github.com/nodeca/pako"><cite>pako</cite></a>
      <cite>JavaScript</cite> library, and cached inside the indexed-db of the browser
      (private navigation may fail because of this).
      <br/><br/>

      with the
      <a href="https://github.com/mirage/irmin/"><cite>irmin</cite></a>
      OCaml library (modded with
      <a href="https://github.com/talex5/irmin-indexeddb"><cite>irmin-indexeddb</cite></a>
      and
      <a href="https://cryptojs.gitbook.io/docs/"><cite>CryptoJS</cite></a>,
      an <cite>OCaml</cite> and a <cite>JavaScript</cite> library).

      <h2>Author</h2>
      Made by ngoguey,
      <br/>
      <a href="https://github.com/ngoguey42">www.github.com/ngoguey42</a>,
      <br/>
      <code>echo "rf.24.tneduts@yeugogn" | rev</code>.
    </div>
  |}]
