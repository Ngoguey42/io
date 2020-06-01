module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

let url_repo = "https://github.com/ngoguey42/ngoguey42.github.io"

let url_bin = "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/bin"

let url_lib = "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib"

let url_ftreact =
  "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib/ft_js/reactjs.ml"

let url_ftww = "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib/ft_js/webworker.ml"

let url_entrypoint =
  "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/bin/page_builder.ml"

let url_jsooppx = "https://ocsigen.org/js_of_ocaml/3.1.0/manual/ppx"

let[@ocamlformat "disable"] create_content () = [%html {|
<div class="bigbox0">
  <h1>Making-of</h1>
  <h2>This website</h2>

  <p>
    This website is hosted and statically served by <cite>github</cite>,
    written with OCaml, built using
    <a href="https://github.com/ocaml/dune"><cite>dune</cite></a>
    and transpiled from OCaml to JavaScript using
    <a href="https://ocsigen.org/js_of_ocaml"><cite>Js_of_ocaml</cite></a>.
    The source code is publicly available <a href="|} url_repo {|">here</a>.
  </p>

  <p>
    The asynchronous JavaScript events are handled using the
    <a href="https://ocsigen.org/lwt"><cite>Lwt</cite></a>
    OCaml library and the
    <a href="https://erratique.ch/software/react"><cite>React</cite></a>
    OCaml library (Not to be confused with <cite>Reactjs</cite>).
  </p>

  <p>
    The <cite>html</cite> is generated using either the
    <a href="https://ocsigen.org/tyxml"><cite>TyXML</cite></a>
    OCaml library or using the
    <a href="https://reactjs.org/"><cite>React</cite></a> and
    <a href="https://react-bootstrap.github.io/"><cite>React Bootstrap</cite></a>
    JavaScript libraries when complex interactions are required.
    <a href="|} url_ftreact {|">This small wrapper</a>
    enhances the <cite>Reactjs</cite> experience in OCaml by hiding most of the
    boilerplate (e.g. <cite>Reactjs states</cite> are made available through the functionnal
    reactive primitives of <cite>React(ml)</cite>).
  </p>

  <p>
    The <a href="https://ocsigen.org/tyxml/4.3.0/manual/ppx"><cite>TyXML-ppx</cite></a>,
    <a href="https://github.com/ocaml-ppx/ppx_deriving"><cite>ppx_deriving</cite></a> and
    <a href="|} url_jsooppx {|"><cite>Js_of_ocaml-ppx</cite></a>
    OCaml syntax extensions are used throughout the project.
  </p>

  <p>
    In the <cite>mnist-jsoo</cite> page, the <cite>MNIST</cite> dataset is downloaded from
    <a href="http://yann.lecun.com/exdb/mnist/">http://yann.lecun.com/exdb/mnist</a>
    and cached inside the browser (private navigation may fail because of this). Those additional
     JavaScript libraries are loaded on-the-fly:
    <a href="https://www.tensorflow.org/js/"><cite>Tensorflow.js</cite></a> for efficient tensor
    computations,
    <a href="https://github.com/nodeca/pako"><cite>pako</cite></a> to unzip the MNIST dataset,
    <a href="https://plotly.com/javascript/"><cite>plotly</cite></a> for plots. The heavy
    computations are offloaded to a <cite>Web Worker</cite> using
    <a href="|} url_ftww {|">this small wrapper</a>
    that helps preserving types between threads.
  </p>

  <p>
    TODO: Owl and Fnn (and Tfjs again)
  </p>

  <p>
    The <cite>Cinquante</cite> game is fully written with JavaScript using those libraries:
    <a href="http://piqnt.com/planck.js/"><cite>Planck.js</cite></a> for the physical engine,
    <a href="https://mourner.github.io/simplify-js/"><cite>Simplify.js</cite></a>
    for polygon simplification and
    <a href="https://github.com/mapbox/earcut"><cite>Earcut</cite></a> for polygon tessellation. The
    10 digits used in the game are pre-computed with a python script. This game is a proof of concept
    preceding an end-to-end implementation with <cite>Js_of_ocaml</cite> where the game could either
    be played by a human or by a <cite>Reinforcement Learning</cite> bot trained by the human in the
    same webpage (possibly using <cite>DeepMind</cite>'s
    <a href="https://arxiv.org/abs/1911.08265"><cite>MuZero</cite></a>). This POC revealed that the
    current physical engine would most likely be too slow to simulate a sufficient amount of game,
    this
    <a href="https://github.com/kripken/box2d.js"><cite>Wasm</cite> port of <cite>Box2d</cite></a>
    might offer better performances.
  </p>

  <h3>Code organization</h3>

  <p>
    All the pages share a single entry point:
    <a href="|} url_entrypoint {|">bin/page_builder.ml</a>.
    The rest of the code is scattered between the <a href="|} url_bin {|">bin</a>
    directory and a collection of libraries living in the <a href="|} url_lib {|">lib</a>
    directory. The entry point and all its OCaml dependencies are transpiled to
    a single <cite>.js</cite> file living in the <cite>build</cite> directory
    The generated <cite>.js</cite> file is then commited to the repository and
    each page of the website owns a small <cite>.html</cite> file that loads the generated
    file. (TODO: Update for future workflow)
  </p>

  <h2>Author</h2>
  Made by ngoguey,
  <br/>
  <a href="https://github.com/ngoguey42">www.github.com/ngoguey42</a>,
  <br/>
  <code>echo "rf.24.tneduts@yeugogn" | rev</code>.
</div>
    |}]
