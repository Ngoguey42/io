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

let construct_snippet_code filename =
  let signal, set_signal = React.S.create `Loading in
  let code_ref = Reactjs.create_ref () in
  let on_error _exn =
    set_signal `Error;
    Lwt.return ()
  in
  let raw_gh_url, gh_url =
    let ( / ) = Filename.concat in
    let origin = Ft_js.origin_of_url (Dom_html.window##.location##.href |> Js.to_string) in
    ( origin / "lib/snippets" / filename,
      "https://github.com/Ngoguey42/ngoguey42.github.io/blob/master/lib/snippets" / filename )
  in
  let load_code () =
    let open Lwt.Infix in
    Ft_js.blob_of_url raw_gh_url >>= fun blob ->
    Ft_js.js_string_of_blob blob >>= fun txt ->
    let txt = Js.to_string txt in
    let pat =
      Js_of_ocaml.Regexp.regexp_with_flag "snip-before[^\\n]*\\n(.+)\\n[^\\n]*snip-after" "s"
    in
    let res =
      let ( >>> ) opt f = Option.bind opt f in
      Js_of_ocaml.Regexp.string_match pat txt 0 >>> fun res ->
      Js_of_ocaml.Regexp.matched_group res 1
    in
    (match res with None -> set_signal `Error | Some s -> `Loaded s |> set_signal);

    Lwt.return ()
  in

  let render _ =
    let open Reactjs.Jsx in
    match React.S.value signal with
    | `Loading ->
        [
          of_bootstrap "Spinner" ~animation_string:"border" ~variant:"primary" ~size:"sm" [];
          of_string " loading code";
        ]
        |> of_react "Fragment"
    | `Error ->
        [ of_string "failed to load code: "; of_tag ~href:gh_url "a" [ of_string gh_url ] ]
        |> of_react "Fragment"
    | `Loaded txt ->
        txt |> of_string
        >> of_tag ~ref:code_ref "code" ~classes:[ "language-ocaml" ]
        >> of_tag "pre"
  in
  let mount () = Lwt.catch load_code on_error |> ignore in
  let update () =
    match code_ref##.current |> Js.Opt.to_option with
    | None -> ()
    | Some elt -> Js.Unsafe.global##.hljs##highlightBlock elt
  in
  Reactjs.construct ~signal ~mount ~update render
