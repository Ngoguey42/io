module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug



class type event =
  object

  end

module Element =
  struct

    class type t =
      object

      end

    let of_tag :
          ?on_click:(event -> unit) ->
          ?disabled:bool ->
          ?c:(t Js.t list) ->
          string -> t Js.t =
      fun ?on_click ?disabled ?c:(children=[]) name ->
      let open Js.Unsafe in
      let props = object end in
      Option.iter (fun fn -> set props (Js.string "onClick") (Js.wrap_callback fn)) on_click;
      Option.iter (fun v -> set props (Js.string "disabled") v) disabled;

      let args = [| name |> Js.string |> inject; inject props |] in
      let children = List.map inject children |> Array.of_list in
      let args = Array.concat [ args; children ] in
      fun_call global##._React##.createElement args

    let of_make : ('props -> t Js.t) -> 'props -> t Js.t =
      fun make props ->
      let open Js.Unsafe in
      fun_call global##._React##.createElement [| inject make; inject props |]

    let of_string s : t Js.t = s |> Js.string |> Obj.magic

  end

let use_reducer : ('state -> 'action -> 'state) -> (unit -> 'state) -> 'state * ('action -> unit) =
  fun reduce init ->
  let open Js.Unsafe in
  let args = [|
      reduce |> Js.wrap_callback |> inject;
      inject Js.null;
      init |> Js.wrap_callback |> inject
    |] in
  let arr =
    fun_call global##._React##.useReducer args
    |> Js.to_array
  in
  match arr with
  | [| s; f |] -> s, (fun s -> fun_call f [| inject s |])
  | _ -> failwith "unreachable, bad useReducer return"

let use_state : (unit -> 'state) -> 'state * (('state -> 'state) -> unit) =
  fun init ->
  let open Js.Unsafe in
  let arr =
    fun_call global##._React##.useState [| init |> Js.wrap_callback |> inject |]
    |> Js.to_array
  in
  match arr with
  | [| s; f |] -> s, (fun s -> fun_call f [| inject s |])
  | _ -> failwith "unreachable, bad useState return"

let render : Element.t Js.t -> Dom_html.element Js.t -> unit =
  fun elt container ->
  let open Js.Unsafe in
  fun_call global##._ReactDOM##.render [| inject elt; inject container |]

type state = [
    `Init
  | `Downloading
  | `Downloaded
  ]

type action = [
   `Click
  ]

let make = fun _ ->
  let to_string = function
    | `Init -> "init"
    | `Downloading -> "dl"
    | `Downloaded -> "done"
  in
  let reduce s a =
    Printf.eprintf "> act on s=%s\n%!" (to_string s);
    match s, a with
    | `Init, `Click -> `Downloading
    | `Downloading, `Click -> `Downloaded
    | `Downloaded, `Click -> s
  in
  let state, act = use_reducer reduce (fun () -> `Init) in
  Printf.eprintf "> render with s=%s\n%!" (to_string state);
  let onclick _ =
    Printf.eprintf "> onclick\n%!";
    act `Click;
  in
  Element.of_tag "div" ~c:[
    Element.of_tag "button" ~disabled:false ~on_click:onclick ~c:[
      Element.of_string "click me"
    ];
    Element.of_string " coucou ";
    Element.of_string (to_string state);
  ]

let lol () =
  let body = Dom_html.window##.document##.body in
  let elt = Element.of_make make 42 in
  render elt body;

  Firebug.console##log (
      Html.a_disabled ()
    );
  ()
