module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

class type event = object end

module Jsx = struct
  class type t = object end

  let of_tag : string -> ?on_click:(event -> unit) -> ?disabled:bool -> t Js.t list -> t Js.t =
   fun name ?on_click ?disabled children ->
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
    let props =
      object%js
        method data = props
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
  type builder = { render : (unit -> Jsx.t Js.t) option ref; hooks : (unit -> unit) list ref }

  type status = Building of builder | Built of (unit -> Jsx.t Js.t) * (unit -> unit) list

  let constructor f props =
    let status = ref (Building { render = ref None; hooks = ref [] }) in
    let make props =
      match !status with
      | Building builder ->
          f builder props;
          let render =
            match !(builder.render) with
            | Some render -> render
            | None ->
                failwith
                  "Reacjs.Bind.render should be called exactly once per Reactjs.Bind.constructor"
          in
          let hooks = !(builder.hooks) in
          status := Built (render, hooks);

          List.iter (fun f -> f ()) hooks;
          render ()
      | Built (render, hooks) ->
          List.iter (fun f -> f ()) hooks;
          render ()
    in
    Jsx.of_make make props

  let render builder f =
    match !(builder.render) with
    | None -> builder.render := Some f
    | Some _ ->
        failwith "Reacjs.Bind.render should be called exactly once per Reactjs.Bind.constructor"

  let mount builder f =
    let fetch () = use_effect ~deps:[||] f in
    builder.hooks := !(builder.hooks) @ [ fetch ]

  let state : builder -> (unit -> 's) -> (unit -> 's) * (('s -> 's) -> unit) =
   fun builder init ->
    let ref_s, ref_set = (ref None, ref None) in
    let fetch () =
      let s, set = use_state init in
      ref_s := Some s;
      ref_set := Some set
    in
    let get () =
      match !ref_s with
      | None ->
          failwith "Can't call returned functions of Reactjs.Bind.state from within constructor"
      | Some s -> s
    in
    let set s =
      match !ref_set with
      | None ->
          failwith "Can't call returned functions of Reactjs.Bind.state from within constructor"
      | Some set -> set s
    in
    builder.hooks := !(builder.hooks) @ [ fetch ];
    (get, set)

  let reducer : _ -> ('s -> 'a -> 's) -> (unit -> 's) -> (unit -> 's) * ('a -> unit) =
   fun builder reduce init ->
    let ref_s, ref_set = (ref None, ref None) in
    let fetch () =
      let s, set = use_reducer reduce init in
      ref_s := Some s;
      ref_set := Some set
    in
    let get () =
      match !ref_s with
      | None ->
          failwith "Can't call returned functions of Reactjs.Bind.reducer from within constructor"
      | Some s -> s
    in
    let set a =
      match !ref_set with
      | None ->
          failwith "Can't call returned functions of Reactjs.Bind.state from within constructor"
      | Some set -> set a
    in
    builder.hooks := !(builder.hooks) @ [ fetch ];
    (get, set)

  let signal : _ -> 'a React.signal -> unit =
   fun builder s ->
    let _, set_state = state builder (fun () -> React.S.value s) in

    (* TODO: Really necessary to retain? *)
    React.S.changes s
    |> React.E.map (fun s -> set_state (fun _ -> s))
    |> (fun _ () -> ())
    |> React.S.retain s |> ignore
end
