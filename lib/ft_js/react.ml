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

    (* let props = props##.data in *)
    let props = object%js method data = props end in
    let make = fun props -> make props##.data in

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

let use_effect : ?deps:('a array) -> (unit -> (unit -> unit)) -> unit =
 fun ?deps f ->
  let open Js.Unsafe in
  let deps = match deps with
    | None -> Js.null
    | Some a -> Js.Opt.return (Js.array a)
  in
  let f = Js.wrap_callback (fun () -> Js.wrap_callback (f ())) in
  fun_call global##._React##.useEffect [| inject f ; inject deps |]

let render : Jsx.t Js.t -> Dom_html.element Js.t -> unit =
 fun elt container ->
  let open Js.Unsafe in
  fun_call global##._ReactDOM##.render [| inject elt; inject container |]

type state = [ `Init | `Downloading | `Downloaded ]

type action = [ `Click ]

module Bind = struct
  type builder = {
      render : (unit -> Jsx.t Js.t) option ref;
      hooks : (unit -> unit) list ref;
    }
  type status =
    | Building of builder
    | Built of (unit -> Jsx.t Js.t) * (unit -> unit) list

  let constructor f =
    let status = ref (Building { render = ref None ; hooks = ref [] }) in
    let make props =
      match !status with
      | Building builder ->
         Printf.eprintf "!make initial\n%!";
         f builder props;

         let render = match !(builder.render) with
           | Some render -> render
           | None -> failwith "Missing render"
         in
         let hooks = !(builder.hooks) in

         status := Built (render, hooks);

         List.iter (fun f -> f ()) hooks;
         render ()
      | Built (render, hooks) ->
         Printf.eprintf "!make subsequent, %d hooks\n%!" (List.length hooks);
         List.iter (fun f -> f ()) hooks;
         render ()
    in
    Jsx.of_make make

  let render builder f =
    match !(builder.render) with
    | None -> builder.render := Some f
    | Some _ -> failwith "Multiple render"

  let mount builder f =
    let fetch () = use_effect ~deps:[| |] f in
    builder.hooks := !(builder.hooks) @ [fetch]

  let state : builder -> (unit -> 's) -> (unit -> 's) * (('s -> 's) -> unit) = fun builder init ->
    let ref_s, ref_set = ref None, ref None in
    let fetch () =
      Printf.eprintf "!fetch state\n%!";
      let s, set = use_state init in
      ref_s := Some s;
      ref_set := Some set;
    in
    let get () = match !ref_s with
      | None -> failwith "Calling get too early"
      | Some s -> s
    in
    let set s = match !ref_set with
      | None -> failwith "Calling set too early"
      | Some set -> set s
    in
    builder.hooks := !(builder.hooks) @ [fetch];
    get, set

  let rstate : _ -> ('s -> 'a -> 's) -> (unit -> 's) -> (unit -> 's) * ('a -> unit) =
   fun builder reduce init ->
    let ref_s, ref_set = ref None, ref None in
    let fetch () =
      Printf.eprintf "!fetch rstate\n%!";
      let s, set = use_reducer reduce init in
      ref_s := Some s;
      ref_set := Some set;
    in
    let get () = match !ref_s with
      | None -> failwith "Calling get too early"
      | Some s -> s
    in
    let set a =
      Printf.eprintf "!set rstate\n%!";
      match !ref_set with
      | None -> failwith "Calling set too early"
      | Some set -> set a
    in
    builder.hooks := !(builder.hooks) @ [fetch];
    get, set

end

let make' =
  (fun builder on_finished ->
    Printf.eprintf ">>> Construct\n%!";
    Firebug.console##log on_finished;
    let to_string = function `Init -> "init" | `Downloading -> "dl" | `Downloaded -> "downloaded" in
    let reduce_state1 s a =
      match (s, a) with
      | `Init, `Click ->
         Printf.eprintf ">>> Reduce from state %s to `Downloading\n%!" (s |> to_string);
         `Downloading
      | `Downloading, `Click ->
         Printf.eprintf ">>> Reduce from state %s to `Downloaded\n%!" (s |> to_string);
          `Downloaded
      | `Downloaded, `Click ->
         Printf.eprintf ">>> Reduce from state %s to same\n%!" (s |> to_string);
         on_finished ();
         s
    in

    (* let _get_state0, _set_state0 = Bind.state builder (fun () -> 0) in *)
    let get_state1, set_state1 = Bind.rstate builder reduce_state1 (fun () -> `Init) in
    let onclick _ = set_state1 `Click in

    let render () =
      Printf.eprintf ">>> Render %s\n%!" (get_state1 () |> to_string);
      Jsx.of_tag "div"
        [
          Jsx.of_tag "button" ~disabled:false ~on_click:onclick [ Jsx.of_string "click me" ];
          Jsx.of_string " coucou ";
          Jsx.of_string (get_state1 () |> to_string);
        ]
    in
    let unmount () =
      Printf.eprintf ">>> Unmount\n%!";
    in
    let mount () =
      Printf.eprintf ">>> Mount\n%!";
      unmount
    in

    Bind.render builder render;
    Bind.mount builder mount;
  )
  |> Bind.constructor

let make =
  (fun builder (_props : int) ->
    Printf.eprintf "> Construct\n%!";

    let get_state0, set_state0 = Bind.state builder (fun () -> true) in
    let on_finished () =
      Printf.eprintf "> On finished begin\n%!";
      set_state0 (fun _ -> false);
      Printf.eprintf "> On finished end\n%!";
    in

    let render () =
      Printf.eprintf "> Render %b\n%!" (get_state0 ());
      if get_state0 () then
        Jsx.of_make make' on_finished
      else
        Jsx.of_string "bye bye"
    in

    Bind.render builder render;
  )
  |> Bind.constructor

(* let make _ = *)
(*   (\* let open React in *\) *)
(*   let to_string = function `Init -> "init" | `Downloading -> "dl" | `Downloaded -> "done" in *)
(*   let reduce s a = *)
(*     match (s, a) with *)
(*     | `Init, `Click -> `Downloading *)
(*     | `Downloading, `Click -> `Downloaded *)
(*     | `Downloaded, `Click -> s *)
(*   in *)
(*   let state, act = use_reducer reduce (fun () -> `Init) in *)
(*   let onclick _ = act `Click in *)

(*   Jsx.of_tag "div" *)
(*     [ *)
(*       Jsx.of_tag "button" ~disabled:false ~on_click:onclick [ Jsx.of_string "click me" ]; *)
(*       Jsx.of_string " coucou "; *)
(*       Jsx.of_string (to_string state); *)
(*     ] *)

let lol () =
  let body = Dom_html.window##.document##.body in
  let elt = Jsx.of_make make 42 in
  render elt body;

  Firebug.console##log (Html.a_disabled ());
  ()
