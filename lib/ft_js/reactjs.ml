module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module Js = Js_of_ocaml.Js
module Firebug = Js_of_ocaml.Firebug

module WeakJsObjDict = Ephemeron.K1.Make (struct
  type t = Js.Unsafe.any

  let equal a b = a == b

  let hash = Hashtbl.hash
end)

class type event = object end

class type ['props] component_class = object end

class type component = object end

class type jsx = object end

type 'props construction =
  ('props -> jsx Js.t) * (unit -> unit) * (unit -> unit) * (component Js.t -> unit) list

let construct ?mount ?unmount ?signal:s0 ?signal:s1 ?signal:s2 ?signal:s3 ?signal:s4 render :
    'props construction =
  let mount = match mount with None -> fun () -> () | Some mount -> mount in
  let unmount = match unmount with None -> fun () -> () | Some unmount -> unmount in

  let setup_signal signal idx self =
    Js.Unsafe.set (Js.Unsafe.get self (Js.string "state")) idx (React.S.value signal);
    let update_state value =
      let o = object%js end in
      Js.Unsafe.set o idx value;
      Js.Unsafe.meth_call self "setState" [| Js.Unsafe.inject o |]
    in
    React.S.changes signal |> React.E.map update_state |> ignore
  in

  let setup_signals =
    List.concat
      [
        s0 |> Option.to_list |> List.map (fun s -> setup_signal s 0);
        s1 |> Option.to_list |> List.map (fun s -> setup_signal s 1);
        s2 |> Option.to_list |> List.map (fun s -> setup_signal s 2);
        s3 |> Option.to_list |> List.map (fun s -> setup_signal s 3);
        s4 |> Option.to_list |> List.map (fun s -> setup_signal s 4);
      ]
  in
  (render, mount, unmount, setup_signals)

module Jsx = struct
  external _ft_js_create_component_type :
    (component Js.t -> < data : 'props Js.readonly_prop > Js.t -> unit) ->
    'props component_class Js.t = "ft_js_create_component_type"

  let _class_of_constructor : Js.Unsafe.any WeakJsObjDict.t = WeakJsObjDict.create 25

  let _class_of_render : Js.Unsafe.any WeakJsObjDict.t = WeakJsObjDict.create 25

  let of_string s : jsx Js.t = s |> Js.string |> Obj.magic

  let of_tag :
      string ->
      ?on_click:(event -> unit) ->
      ?disabled:bool ->
      ?colspan:string ->
      ?class_:string ->
      ?style:(string * string) list ->
      jsx Js.t list ->
      jsx Js.t =
   fun name ?on_click ?disabled ?colspan ?class_ ?style children ->
    let open Js.Unsafe in
    let props = object%js end in
    Option.iter (fun fn -> set props (Js.string "onClick") (Js.wrap_callback fn)) on_click;
    Option.iter (fun v -> set props (Js.string "disabled") v) disabled;
    Option.iter (fun v -> set props (Js.string "colSpan") (Js.string v)) colspan;
    Option.iter (fun v -> set props (Js.string "className") (Js.string v)) class_;
    Option.iter
      (fun l ->
        let style = object%js end in
        List.iter (fun (k, v) -> set style (Js.string k) (Js.string v)) l;
        set props (Js.string "style") style)
      style;

    let args = [| name |> Js.string |> inject; inject props |] in
    let children = List.map inject children |> Array.of_list in
    let args = Array.concat [ args; children ] in
    fun_call global##._React##.createElement args

  (* Constructor is called with `props`. `render` is also called with `props` because React doesn't
     re-instanciate the component when `props` changes. Two design patterns can be adopted:
     - Ignore the props passed to `render`, and use a `key` in `of_constructor` to trigger
       re-instanciations (a.k.a. Fully uncontrolled component with a key).
     - Ignore the mutable props passed to construct and read the props passed to `render`.
  *)
  let of_constructor : ?key:'a -> ('props -> 'props construction) -> 'props -> jsx Js.t =
   fun ?key constructor props ->
    let open Js.Unsafe in
    let props =
      object%js
        val data = props
      end
    in
    (match key with None -> () | Some key -> set props "key" key);

    let cls : 'props component_class Js.t =
      match WeakJsObjDict.find_opt _class_of_constructor (inject constructor) with
      | Some cls -> Obj.magic cls
      | None ->
          let cls =
            let g self props =
              let render, mount, unmount, setup_signals = constructor props##.data in
              (Js.Unsafe.coerce self)##.ftJsRender := Js.wrap_callback render;
              (Js.Unsafe.coerce self)##.ftJsMount := Js.wrap_callback mount;
              (Js.Unsafe.coerce self)##.ftJsUnmount := Js.wrap_callback unmount;
              List.iter (fun fn -> fn self) setup_signals
            in
            _ft_js_create_component_type g
          in
          WeakJsObjDict.add _class_of_constructor (inject constructor) (inject cls);
          Obj.magic cls
    in

    fun_call global##._React##.createElement [| inject cls; inject props |]

  let of_render : ?key:'a -> ('props -> jsx Js.t) -> 'props -> jsx Js.t =
   fun ?key render props ->
    let open Js.Unsafe in
    let props =
      object%js
        val data = props
      end
    in
    (match key with None -> () | Some key -> set props "key" key);

    let cls : 'props component_class Js.t =
      match WeakJsObjDict.find_opt _class_of_render (inject render) with
      | Some cls -> Obj.magic cls
      | None ->
          let cls =
            let g self _ =
              let render, mount, unmount, setup_signals = construct render in
              (Js.Unsafe.coerce self)##.ftJsRender := Js.wrap_callback render;
              (Js.Unsafe.coerce self)##.ftJsMount := Js.wrap_callback mount;
              (Js.Unsafe.coerce self)##.ftJsUnmount := Js.wrap_callback unmount;
              List.iter (fun fn -> fn self) setup_signals
            in
            _ft_js_create_component_type g
          in
          WeakJsObjDict.add _class_of_render (inject render) (inject cls);
          Obj.magic cls
    in

    fun_call global##._React##.createElement [| inject cls; inject props |]
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

let render : jsx Js.t -> Dom_html.element Js.t -> unit =
 fun elt container ->
  let open Js.Unsafe in
  fun_call global##._ReactDOM##.render [| inject elt; inject container |]
