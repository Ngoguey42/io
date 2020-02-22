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
      Welcome to my <img style="vertical-align:middle" src="images/ocaml.png" alt="OCaml"/> website!
    </div>
  |}]
