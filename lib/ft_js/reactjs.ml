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

let construct ?mount ?update ?unmount ?signal:s0 ?signal:s1 ?signal:s2 ?signal:s3 ?signal:s4
    ?events:e0 render : 'props construction =
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

  let setup_events events idx self =
    let count = ref 1 in
    Js.Unsafe.set (Js.Unsafe.get self (Js.string "state")) idx 0;
    let update_state _ =
      let o = object%js end in
      Js.Unsafe.set o idx !count;
      incr count;
      Js.Unsafe.meth_call self "setState" [| Js.Unsafe.inject o |]
    in
    events |> React.E.map update_state |> ignore
  in

  let setup_signals =
    List.concat
      [
        s0 |> Option.to_list |> List.map (fun s -> setup_signal s 0);
        s1 |> Option.to_list |> List.map (fun s -> setup_signal s 1);
        s2 |> Option.to_list |> List.map (fun s -> setup_signal s 2);
        s3 |> Option.to_list |> List.map (fun s -> setup_signal s 3);
        s4 |> Option.to_list |> List.map (fun s -> setup_signal s 4);
        e0 |> Option.to_list |> List.map (fun s -> setup_events s 5);
      ]
  in
  (render, mount, update, unmount, setup_signals)

module Jsx = struct
  let ( >> ) : jsx Js.t -> (jsx Js.t list -> jsx Js.t) -> jsx Js.t = fun v f -> f [ v ]

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
      ?on_select:(Js.js_string Js.t -> Dom_html.anchorElement Js.t event Js.t -> unit) ->
      ?disabled:bool ->
      ?inline:bool ->
      ?colspan:string ->
      ?href:string ->
      ?src:string ->
      ?placement:string ->
      ?overlay:jsx Js.t ->
      ?bordered:bool ->
      ?xs_span:int ->
      ?sm_span:int ->
      ?md_span:int ->
      ?lg_span:int ->
      ?xl_span:int ->
      ?xs_order:int ->
      ?sm_order:int ->
      ?md_order:int ->
      ?lg_order:int ->
      ?xl_order:int ->
      ?event_key:string ->
      ?default_active_key:string ->
      ?active_key:string ->
      ?placeholder:string ->
      ?animation:string ->
      ?variant:string ->
      ?type_:string ->
      ?min:float ->
      ?step:float ->
      ?default_value:string ->
      ?value:string ->
      ?on_change:(Dom_html.inputElement Js.t event Js.t -> unit) ->
      ?as_:string ->
      ?size:string ->
      ?title:string ->
      ?title_jsx:jsx Js.t ->
      ?transition:bool ->
      ?id:string ->
      ?class_:string list ->
      ?style:(string * string) list ->
      ?label:string ->
      ?name:string ->
      ?no_gutters:bool ->
      jsx Js.t list ->
      jsx Js.t =
   fun ctor ?ref ?key ?on_click ?on_select ?disabled ?inline ?colspan ?href ?src ?placement ?overlay
       ?bordered ?xs_span ?sm_span ?md_span ?lg_span ?xl_span ?xs_order ?sm_order ?md_order
       ?lg_order ?xl_order ?event_key ?default_active_key ?active_key ?placeholder ?animation
       ?variant ?type_ ?min ?step ?default_value ?value ?on_change ?as_ ?size ?title ?title_jsx
       ?transition ?id ?class_ ?style ?label ?name ?no_gutters children ->
    let open Js.Unsafe in
    let props = object%js end in

    let xs = lazy (object%js end) in
    Option.iter (fun v -> set (Lazy.force xs) (Js.string "span") v) xs_span;
    Option.iter (fun v -> set (Lazy.force xs) (Js.string "order") v) xs_order;
    if Lazy.is_val xs then set props (Js.string "xs") (Lazy.force xs);

    let sm = lazy (object%js end) in
    Option.iter (fun v -> set (Lazy.force sm) (Js.string "span") v) sm_span;
    Option.iter (fun v -> set (Lazy.force sm) (Js.string "order") v) sm_order;
    if Lazy.is_val sm then set props (Js.string "sm") (Lazy.force sm);

    let md = lazy (object%js end) in
    Option.iter (fun v -> set (Lazy.force md) (Js.string "span") v) md_span;
    Option.iter (fun v -> set (Lazy.force md) (Js.string "order") v) md_order;
    if Lazy.is_val md then set props (Js.string "md") (Lazy.force md);

    let lg = lazy (object%js end) in
    Option.iter (fun v -> set (Lazy.force lg) (Js.string "span") v) lg_span;
    Option.iter (fun v -> set (Lazy.force lg) (Js.string "order") v) lg_order;
    if Lazy.is_val lg then set props (Js.string "lg") (Lazy.force lg);

    let xl = lazy (object%js end) in
    Option.iter (fun v -> set (Lazy.force xl) (Js.string "span") v) xl_span;
    Option.iter (fun v -> set (Lazy.force xl) (Js.string "order") v) xl_order;
    if Lazy.is_val xl then set props (Js.string "xl") (Lazy.force xl);

    Option.iter (fun v -> set props (Js.string "ref") v) ref;
    Option.iter (fun v -> set props (Js.string "key") v) key;
    Option.iter (fun fn -> set props (Js.string "onClick") (Js.wrap_callback fn)) on_click;
    Option.iter (fun fn -> set props (Js.string "onSelect") (Js.wrap_callback fn)) on_select;
    Option.iter (fun v -> set props (Js.string "disabled") v) disabled;
    Option.iter (fun v -> set props (Js.string "inline") v) inline;
    Option.iter (fun v -> set props (Js.string "colSpan") (Js.string v)) colspan;
    Option.iter (fun v -> set props (Js.string "href") (Js.string v)) href;
    Option.iter (fun v -> set props (Js.string "src") (Js.string v)) src;
    Option.iter (fun v -> set props (Js.string "title") (Js.string v)) title;
    Option.iter (fun v -> set props (Js.string "title") v) title_jsx;
    Option.iter (fun v -> set props (Js.string "transition") v) transition;
    Option.iter (fun v -> set props (Js.string "placement") (Js.string v)) placement;
    Option.iter (fun v -> set props (Js.string "overlay") v) overlay;
    Option.iter (fun v -> set props (Js.string "bordered") v) bordered;
    Option.iter (fun v -> set props (Js.string "eventKey") (Js.string v)) event_key;
    Option.iter (fun v -> set props (Js.string "defaultActiveKey") (Js.string v)) default_active_key;
    Option.iter (fun v -> set props (Js.string "activeKey") (Js.string v)) active_key;
    Option.iter (fun v -> set props (Js.string "placeholder") (Js.string v)) placeholder;
    Option.iter (fun v -> set props (Js.string "animation") (Js.string v)) animation;
    Option.iter (fun v -> set props (Js.string "variant") (Js.string v)) variant;
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
    Option.iter (fun v -> set props (Js.string "label") (Js.string v)) label;
    Option.iter (fun v -> set props (Js.string "name") (Js.string v)) name;
    Option.iter (fun v -> set props (Js.string "noGutters") v) no_gutters;

    let args =
      Array.concat [ [| ctor; inject props |]; List.map inject children |> Array.of_list ]
    in
    fun_call global##._React##.createElement args

  let of_tag name =
    let open Js.Unsafe in
    _create_element (name |> Js.string |> inject)

  let of_react path =
    let open Js.Unsafe in
    let fail name = Printf.sprintf "Could not find %s in React.%s" name path |> failwith in
    let o =
      match global##._React |> Js.Optdef.to_option with None -> fail "React" | Some o -> o
    in
    let o =
      List.fold_left
        (fun o name ->
          match get o (Js.string name) |> Js.Optdef.to_option with None -> fail name | Some o -> o)
        o (String.split_on_char '.' path)
    in
    _create_element o

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
