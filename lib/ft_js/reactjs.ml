open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Js = Js_of_ocaml.Js
end

(* Types **************************************************************************************** *)

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

(* Functions ************************************************************************************ *)

(** https://reactjs.org/docs/react-dom.html#render *)
let render : jsx Js.t -> Dom_html.element Js.t -> unit =
 fun elt container ->
  let open Js.Unsafe in
  fun_call global##._ReactDOM##.render [| inject elt; inject container |]

(** To be called at the end of a constructor.

    OCaml piece of code | React.Component class equivalent
    ------------------------------------------------------
    construct caller    | .constructor
    ~mount              | .componentDidMount
    ~update             | .componentDidUpdate
    ~unmount            | .componentDidMount
    ~signal             | .state
    ~events             | .state
    render              | .render

    How to improve the {| ?signal -> ?signal -> ?signal |} trick?
*)
let construct ?mount ?update ?unmount ?signal:s0 ?signal:s1 ?signal:s2 ?signal:s3 ?signal:s4
    ?events:e0 render : 'props construction =
  let mount = Option.value ~default:(fun () -> ()) mount in
  let update = Option.value ~default:(fun () -> ()) update in
  let unmount = Option.value ~default:(fun () -> ()) unmount in

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

(** https://reactjs.org/docs/refs-and-the-dom.html *)
let create_ref () : ref_ Js.t =
  let open Js.Unsafe in
  fun_call global##._React##.createRef [||]

(** Reactjs.Jsx to be opened in render functions *)
module Jsx = struct
  let ( >> ) : jsx Js.t -> (jsx Js.t list -> jsx Js.t) -> jsx Js.t = fun v f -> f [ v ]

  external _ft_js_create_component_type :
    (component Js.t -> < data : 'props Js.readonly_prop > Js.t -> unit) ->
    'props component_class Js.t = "ft_js_create_component_type"

  let _class_of_constructor : Js.Unsafe.any WeakJsObjDict.t = WeakJsObjDict.create 25

  let _class_of_render : Js.Unsafe.any WeakJsObjDict.t = WeakJsObjDict.create 25

  let _create_element :
      'a ->
      ?active_key:string ->
      ?animation_bool:bool ->
      ?animation_string:string ->
      ?as_:string ->
      ?bordered:bool ->
      ?classes:string list ->
      ?colspan:string ->
      ?default_active_key:string ->
      ?default_value:string ->
      ?disabled:bool ->
      ?event_key:string ->
      ?fluid:bool ->
      ?href:string ->
      ?id:string ->
      ?inline:bool ->
      ?inner_html:string ->
      ?key:string ->
      ?label:string ->
      ?min:float ->
      ?name:string ->
      ?no_gutters:bool ->
      ?on_change:(Dom_html.inputElement Js.t event Js.t -> unit) ->
      ?on_click:(Dom_html.buttonElement Js.t event Js.t -> unit) ->
      ?on_close:(unit -> unit) ->
      ?on_select:(Js.js_string Js.t -> Dom_html.anchorElement Js.t event Js.t -> unit) ->
      ?overlay:jsx Js.t ->
      ?placeholder:string ->
      ?placement:string ->
      ?ref:ref_ Js.t ->
      ?size:string ->
      ?src:string ->
      ?step:float ->
      ?style:(string * string) list ->
      ?title:string ->
      ?title_jsx:jsx Js.t ->
      ?transition:bool ->
      ?type_:string ->
      ?value:string ->
      ?variant:string ->
      ?lg_order:int ->
      ?lg_span:int ->
      ?md_order:int ->
      ?md_span:int ->
      ?sm_order:int ->
      ?sm_span:int ->
      ?xl_order:int ->
      ?xl_span:int ->
      ?xs_order:int ->
      ?xs_span:int ->
      jsx Js.t list ->
      jsx Js.t =
   fun ctor ?active_key ?animation_bool ?animation_string ?as_ ?bordered ?classes ?colspan
       ?default_active_key ?default_value ?disabled ?event_key ?fluid ?href ?id ?inline ?inner_html
       ?key ?label ?min ?name ?no_gutters ?on_change ?on_click ?on_close ?on_select ?overlay
       ?placeholder ?placement ?ref ?size ?src ?step ?style ?title ?title_jsx ?transition ?type_
       ?value ?variant ?lg_order ?lg_span ?md_order ?md_span ?sm_order ?sm_span ?xl_order ?xl_span
       ?xs_order ?xs_span children ->
    let open Js.Unsafe in
    let props = object%js end in
    let set_prop opt bootstrap_name preprocess =
      Option.iter (fun v -> set props (Js.string bootstrap_name) (preprocess v)) opt
    in

    (* Props *)
    set_prop active_key "activeKey" Js.string;
    set_prop animation_bool "animation" Js.bool;
    set_prop animation_string "animation" Js.string;
    set_prop as_ "as" Js.string;
    set_prop bordered "bordered" Js.bool;
    set_prop classes "className" (fun l -> String.concat " " l |> Js.string);
    set_prop colspan "colSpan" Js.string;
    set_prop default_active_key "defaultActiveKey" Js.string;
    set_prop default_value "defaultValue" Js.string;
    set_prop disabled "disabled" Js.bool;
    set_prop event_key "eventKey" Js.string;
    set_prop fluid "fluid" Js.bool;
    set_prop href "href" Js.string;
    set_prop id "id" Js.string;
    set_prop inline "inline" Js.bool;
    set_prop inner_html "dangerouslySetInnerHTML" (fun v ->
        let o = object%js end in
        set o (Js.string "__html") (Js.string v);
        o);
    set_prop key "key" Js.string;
    set_prop label "label" Js.string;
    set_prop min "min" Fun.id;
    set_prop name "name" Js.string;
    set_prop no_gutters "noGutters" Js.bool;
    set_prop on_change "onChange" Js.wrap_callback;
    set_prop on_click "onClick" Js.wrap_callback;
    set_prop on_close "onClose" Js.wrap_callback;
    set_prop on_select "onSelect" Js.wrap_callback;
    set_prop overlay "overlay" Fun.id;
    set_prop overlay "overlay" Fun.id;
    set_prop placeholder "placeholder" Js.string;
    set_prop placement "placement" Js.string;
    set_prop ref "ref" Fun.id;
    set_prop size "size" Js.string;
    set_prop src "src" Js.string;
    set_prop step "step" Fun.id;
    set_prop style "style" (fun l ->
        let style = object%js end in
        List.iter (fun (k, v) -> set style (Js.string k) (Js.string v)) l;
        style);
    set_prop title "title" Js.string;
    set_prop title_jsx "title" Fun.id;
    set_prop transition "transition" Js.bool;
    set_prop type_ "type" Js.string;
    set_prop value "value" Js.string;
    set_prop variant "variant" Js.string;

    (* Props: ReactBootstrap.Col specific *)
    let set_col_prop bootstrap_name span_opt order_opt =
      let o = lazy (object%js end) in
      Option.iter (fun v -> set (Lazy.force o) (Js.string "span") v) span_opt;
      Option.iter (fun v -> set (Lazy.force o) (Js.string "order") v) order_opt;
      if Lazy.is_val o then set props (Js.string bootstrap_name) (Lazy.force o)
    in
    set_col_prop "xs" xs_span xs_order;
    set_col_prop "sm" sm_span sm_order;
    set_col_prop "md" md_span md_order;
    set_col_prop "lg" lg_span lg_order;
    set_col_prop "xl" xl_span xl_order;

    Array.concat [ [| ctor; inject props |]; List.map inject children |> Array.of_list ]
    |> fun_call global##._React##.createElement

  (** Create a Jsx object from an OCaml string *)
  let of_string : string -> jsx Js.t = fun s -> s |> Js.string |> Obj.magic

  (** Create a Jsx object from:
      - an html tag name (e.g. "img", "div"),
      - optional properties (e.g. ~title:"Hello", ~style:["width", "200px"])
      - a list of Jsx children
   *)
  let of_tag name =
    let open Js.Unsafe in
    _create_element (name |> Js.string |> inject)

  (** Create a Jsx object from:
      - a React object path, (e.g. "Fragment")
      - optional properties (e.g. ~title:"Hello", ~style:["width", "200px"])
      - a list of Jsx children
   *)
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

  (** Create a Jsx object from:
      - a ReactBootstrap object path, (e.g. "Col" or "Form.Group")
      - optional properties (e.g. ~title:"Hello", ~style:["width", "200px"])
      - a list of Jsx children
   *)
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

  (** Create a Jsx object from:
      - an OCaml function ending with a call to `Reactjs.constructor`,
      - the props taken by that constructor and its render function.

     Constructor is called with `props`. `render` is also called with `props` too because React
     doesn't re-instanciate the component when `props` changes. Three design patterns can be adopted:
     - Don't perform render conditionned on props. Use signals.
     - Perform render conditionned on constructor's props. To do so, use a new `key` in parent's
       `of_constructor` call to trigger re-instanciations
       (a.k.a. Fully uncontrolled component with a key).
     - Perform render conditionned on render's props. To do so, ignore the mutable props passed to
       construct and read them from `render`. (Caveat: I couldn't get rid of certain re-renders).
       It also might be enough to use `of_render` instead of `of_constructor` in that case.
  *)
  let of_constructor : ?key:'a -> 'props constructor -> 'props -> jsx Js.t =
   fun ?key constructor props ->
    let open Js.Unsafe in
    let props =
      object%js
        val data = props
      end
    in
    Option.iter (fun v -> set props (Js.string "key") v) key;

    let cls : 'props component_class Js.t =
      match WeakJsObjDict.find_opt _class_of_constructor (inject constructor) with
      | Some cls -> Obj.magic cls
      | None ->
          let name =
            Js.Unsafe.get constructor (Js.string "name")
            |> Fun.flip Js.Optdef.get (fun () -> "no_name")
            |> Js.string
          in
          (* Printf.printf "> Reactjs - cache miss | constructing %s's class \n%!" (Js.to_string name); *)
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

  (** Create a Jsx object from:
      - an OCaml function returning Jsx a object
      - the props taken by that render function.
   *)
  let of_render : ?key:'a -> ('props -> jsx Js.t) -> 'props -> jsx Js.t =
   fun ?key render props ->
    let open Js.Unsafe in
    let props =
      object%js
        val data = props
      end
    in
    Option.iter (fun v -> set props (Js.string "key") v) key;

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
