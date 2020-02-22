module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

let _outline = "text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;"

let create_softmax_div probas =
  let open Html in
  let rec aux l i =
    match l with
    | x :: tail ->
        let c = Ft.Color.Jet.get x in
        let c = Ft.Color.to_hex_string c in
        ignore c;
        let t = Printf.sprintf "%.0f%%" (x *. 100.) in
        let t = [ i |> string_of_int |> Html.txt; br (); Html.txt t ] in
        let style =
          "width: 34px; height: 34px; text-align: center; color: white;" ^ _outline
          ^ "font-size: small;"
        in
        let style = Printf.sprintf "%s; background: %s" style c in
        let elt = td ~a:[ a_style style ] t in
        elt :: aux tail (i + 1)
    | [] -> []
  in
  table @@ [ tr @@ aux probas 0 ]
