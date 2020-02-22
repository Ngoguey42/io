module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

let create_content () =
  [%html{|
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
      This website is hosted by github as a
      <a href="https://pages.github.com/">GitHub Pages</a>,
      the source code is publicly available on
      <a href="https://github.com/ngoguey42/ngoguey42.github.io">github</a>.
      Everything here is static and written in
      <a href="https://ocaml.org/">OCaml</a>,
      built using
      <a href="https://github.com/ocaml/dune">dune</a>
      and tranpsiled from OCaml to JavaScript using
      <a href="https://ocsigen.org/js_of_ocaml">Js_of_ocaml</a>.
      The html is typed according to the W3C recommendations using the
      <a href="https://ocsigen.org/tyxml">TyXML</a>
      OCaml library before being generated.
      The
      <a href="https://ocsigen.org/tyxml/4.3.0/manual/ppx">TyXML-ppx</a>
      and
      <a href="https://ocsigen.org/js_of_ocaml/3.1.0/manual/ppx">Js_of_ocaml-ppx</a>
      OCaml syntax extensions are used throughout the project.

      <h2>The <cite>Learning Rate Craft</cite> game</h2>
      the asynchronous JavaScript events are handled using the
      <a href="https://ocsigen.org/lwt">Lwt</a>
      OCaml library.

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
