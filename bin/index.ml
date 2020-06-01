module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

let create_content () =
  [%html
    {|
    <div class="bigbox0">
      Welcome to my <img style="vertical-align:middle" src="images/ocaml.png" alt="OCaml"/> website!
    </div>
  |}]
