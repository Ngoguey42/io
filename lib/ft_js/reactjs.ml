open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Dom = Js_of_ocaml.Dom
  module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
  module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
  module Js = Js_of_ocaml.Js
  module Firebug = Js_of_ocaml.Firebug
end

module WeakJsObjDict = Ephemeron.K1.Make (struct
  type t = Js.Unsafe.any

  let equal a b = a == b

  let hash = Hashtbl.hash
end)

class type ['target] event =
  object
    method target : 'target Js.readonly_prop

    method preventDefault : unit Js.meth
  end

class type ['props] component_class = object end

class type component = object end

class type jsx = object end

type 'props construction =
  ('props -> jsx Js.t)
  * (unit -> unit)
  * (unit -> unit)
  * (unit -> unit)
  * (component Js.t -> unit) list

type 'props constructor = 'props -> 'props construction

class type ref_ =
  object
    method current : Dom_html.element Js.t Js.Opt.t Js.readonly_prop
  end

let create_ref () : ref_ Js.t =
  let open Js.Unsafe in
  fun_call global##._React##.createRef [||]

let construct ?mount ?update ?unmount ?signal:s0 ?signal:s1 ?signal:s2 ?signal:s3 ?signal:s4 render
    : 'props construction =
  let mount = match mount with None -> fun () -> () | Some mount -> mount in
  let update = match update with None -> fun () -> () | Some update -> update in
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
  (render, mount, update, unmount, setup_signals)

module Jsx = struct
  external _ft_js_create_component_type :
    (component Js.t -> < data : 'props Js.readonly_prop > Js.t -> unit) ->
    'props component_class Js.t = "ft_js_create_component_type"

  let _class_of_constructor : Js.Unsafe.any WeakJsObjDict.t = WeakJsObjDict.create 25

  let _class_of_render : Js.Unsafe.any WeakJsObjDict.t = WeakJsObjDict.create 25

  let of_string s : jsx Js.t = s |> Js.string |> Obj.magic

  let _create_element :
      'a ->
      ?ref:ref_ Js.t ->
      ?key:string ->
      ?on_click:(Dom_html.buttonElement Js.t event Js.t -> unit) ->
      ?disabled:bool ->
      ?colspan:string ->
      ?href:string ->
      ?placement:string ->
      ?overlay:jsx Js.t ->
      ?bordered:bool ->
      ?sm:int ->
      ?placeholder:string ->
      ?type_:string ->
      ?min:float ->
      ?step:float ->
      ?default_value:string ->
      ?value:string ->
      ?on_change:(Dom_html.inputElement Js.t event Js.t -> unit) ->
      ?as_:string ->
      ?size:string ->
      ?title:string ->
      ?id:string ->
      ?class_:string list ->
      ?style:(string * string) list ->
      jsx Js.t list ->
      jsx Js.t =
   fun ctor ?ref ?key ?on_click ?disabled ?colspan ?href ?placement ?overlay ?bordered ?sm
       ?placeholder ?type_ ?min ?step ?default_value ?value ?on_change ?as_ ?size ?title ?id ?class_
       ?style children ->
    let open Js.Unsafe in
    let props = object%js end in
    Option.iter (fun v -> set props (Js.string "ref") v) ref;
    Option.iter (fun v -> set props (Js.string "key") v) key;
    Option.iter (fun fn -> set props (Js.string "onClick") (Js.wrap_callback fn)) on_click;
    Option.iter (fun v -> set props (Js.string "disabled") v) disabled;
    Option.iter (fun v -> set props (Js.string "colSpan") (Js.string v)) colspan;
    Option.iter (fun v -> set props (Js.string "href") (Js.string v)) href;
    Option.iter (fun v -> set props (Js.string "title") (Js.string v)) title;
    Option.iter (fun v -> set props (Js.string "placement") (Js.string v)) placement;
    Option.iter (fun v -> set props (Js.string "overlay") v) overlay;
    Option.iter (fun v -> set props (Js.string "bordered") v) bordered;
    Option.iter (fun v -> set props (Js.string "sm") v) sm;
    Option.iter (fun v -> set props (Js.string "placeholder") (Js.string v)) placeholder;
    Option.iter (fun v -> set props (Js.string "type") (Js.string v)) type_;
    Option.iter (fun v -> set props (Js.string "min") v) min;
    Option.iter (fun v -> set props (Js.string "step") v) step;
    Option.iter (fun v -> set props (Js.string "defaultValue") (Js.string v)) default_value;
    Option.iter (fun v -> set props (Js.string "value") (Js.string v)) value;
    Option.iter (fun fn -> set props (Js.string "onChange") (Js.wrap_callback fn)) on_change;
    Option.iter (fun v -> set props (Js.string "as") (Js.string v)) as_;

    Option.iter (fun v -> set props (Js.string "size") (Js.string v)) size;
    Option.iter (fun v -> set props (Js.string "id") v) id;
    Option.iter
      (fun l -> set props (Js.string "className") (String.concat " " l |> Js.string))
      class_;
    Option.iter
      (fun l ->
        let style = object%js end in
        List.iter (fun (k, v) -> set style (Js.string k) (Js.string v)) l;
        set props (Js.string "style") style)
      style;

    let args =
      Array.concat [ [| ctor; inject props |]; List.map inject children |> Array.of_list ]
    in
    fun_call global##._React##.createElement args

  let of_tag name =
    let open Js.Unsafe in
    _create_element (name |> Js.string |> inject)

  let of_bootstrap path =
    let open Js.Unsafe in
    let fail name = Printf.sprintf "Could not find %s in ReactBootstrap.%s" name path |> failwith in
    let o =
      match global##._ReactBootstrap |> Js.Optdef.to_option with
      | None -> fail "ReactBootstrap"
      | Some o -> o
    in
    let o =
      List.fold_left
        (fun o name ->
          match get o (Js.string name) |> Js.Optdef.to_option with None -> fail name | Some o -> o)
        o (String.split_on_char '.' path)
    in
    _create_element o

  (* Constructor is called with `props`. `render` is also called with `props` because React doesn't
     re-instanciate the component when `props` changes. Two design patterns can be adopted:
     - Ignore the props passed to `render`, and use a `key` in `of_constructor` to trigger
       re-instanciations (a.k.a. Fully uncontrolled component with a key).
     - Ignore the mutable props passed to construct and read the props passed to `render`.
  *)
  let of_constructor : ?key:'a -> 'props constructor -> 'props -> jsx Js.t =
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
          let name =
            Js.Unsafe.get constructor (Js.string "name")
            |> Fun.flip Js.Optdef.get (fun () -> "no_name")
            |> Js.string
          in
          let cls =
            let g self props =
              let render, mount, update, unmount, setup_signals = constructor props##.data in
              (Js.Unsafe.coerce self)##.ftJsRender := Js.wrap_callback render;
              (Js.Unsafe.coerce self)##.ftJsMount := Js.wrap_callback mount;
              (Js.Unsafe.coerce self)##.ftJsUpdate := Js.wrap_callback update;
              (Js.Unsafe.coerce self)##.ftJsUnmount := Js.wrap_callback unmount;
              List.iter (fun fn -> fn self) setup_signals
            in
            _ft_js_create_component_type g
          in
          Js.Unsafe.set cls (Js.string "displayName") name;
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
              let render, mount, update, unmount, setup_signals = construct render in
              (Js.Unsafe.coerce self)##.ftJsRender := Js.wrap_callback render;
              (Js.Unsafe.coerce self)##.ftJsMount := Js.wrap_callback mount;
              (Js.Unsafe.coerce self)##.ftJsUpdate := Js.wrap_callback update;
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
