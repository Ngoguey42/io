module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

class type event = object end

module Jsx = struct
  class type t = object end

  let of_tag :
      string ->
      ?on_click:(event -> unit) ->
      ?disabled:bool ->
      ?colspan:string ->
      ?class_:string ->
      t Js.t list ->
      t Js.t =
   fun name ?on_click ?disabled ?colspan ?class_ children ->
    let open Js.Unsafe in
    let props = object end in
    Option.iter (fun fn -> set props (Js.string "onClick") (Js.wrap_callback fn)) on_click;
    Option.iter (fun v -> set props (Js.string "disabled") v) disabled;
    Option.iter (fun v -> set props (Js.string "colSpan") v) colspan;
    Option.iter (fun v -> set props (Js.string "class") v) class_;

    let args = [| name |> Js.string |> inject; inject props |] in
    let children = List.map inject children |> Array.of_list in
    let args = Array.concat [ args; children ] in
    fun_call global##._React##.createElement args

  let of_make : ('props -> t Js.t) -> 'props -> t Js.t =
   fun make props ->
    let open Js.Unsafe in
    let props =
      object%js
        val data = props
      end
    in
    let make props = make props##.data in

    fun_call global##._React##.createElement [| inject make; inject props |]

  let of_string s : t Js.t = s |> Js.string |> Obj.magic
end

let use_reducer : ('state -> 'action -> 'state) -> (unit -> 'state) -> 'state * ('action -> unit) =
 fun reduce init ->
  let open Js.Unsafe in
  let args =
    [| reduce |> Js.wrap_callback |> inject; inject Js.null; init |> Js.wrap_callback |> inject |]
  in
  let arr = fun_call global##._React##.useReducer args |> Js.to_array in
  match arr with
  | [| s; f |] -> (s, fun s -> fun_call f [| inject s |])
  | _ -> failwith "unreachable, bad useReducer return"

let use_state : (unit -> 'state) -> 'state * (('state -> 'state) -> unit) =
 fun init ->
  let open Js.Unsafe in
  let arr =
    fun_call global##._React##.useState [| init |> Js.wrap_callback |> inject |] |> Js.to_array
  in
  match arr with
  | [| s; f |] -> (s, fun s -> fun_call f [| inject s |])
  | _ -> failwith "unreachable, bad useState return"

let use_effect : ?deps:'a array -> (unit -> unit -> unit) -> unit =
 fun ?deps f ->
  let open Js.Unsafe in
  let deps = match deps with None -> Js.null | Some a -> Js.Opt.return (Js.array a) in
  let f = Js.wrap_callback (fun () -> Js.wrap_callback (f ())) in
  fun_call global##._React##.useEffect [| inject f; inject deps |]

let render : Jsx.t Js.t -> Dom_html.element Js.t -> unit =
 fun elt container ->
  let open Js.Unsafe in
  fun_call global##._ReactDOM##.render [| inject elt; inject container |]

module Bind = struct
  let constructor f props =
    let status = ref `First in
    let make props =
      match !status with
      | `First ->
          let render, hooks = f props in
          status := `Subsequent (render, hooks);
          render ()
      | `Subsequent (render, hooks) ->
          List.iter (fun f -> f ()) hooks;
          render ()
    in
    Jsx.of_make make props

  let return ?mount ?(signals = []) render =
    let hook_of_mount f =
      let hook () = use_effect ~deps:[||] f in
      hook ();
      hook
    in
    let hook_of_signal s =
      let init () = React.S.value s in
      let _, set_state = use_state init in
      let hook () = use_state init |> ignore in
      (* TODO: retain? *)
      React.S.changes s |> React.E.map (fun s -> set_state (fun _ -> s)) |> ignore;
      hook
    in
    let hooks =
      (mount |> Option.to_list |> List.map hook_of_mount) @ List.map hook_of_signal signals
    in
    (render, hooks)
end
