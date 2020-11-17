open struct
  module Dom_html = Js_of_ocaml.Dom_html
  module Js = Js_of_ocaml.Js
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
end

(* Misc Types *********************************************************************************** *)
class type ['target] event =
  object
    method target : 'target Js.readonly_prop

    method preventDefault : unit Js.meth
  end

class type ref_ =
  object
    method current : Dom_html.element Js.t Js.Opt.t Js.readonly_prop
  end

class type element = object end

class type component_class = object end

class type constructor_as_obj =
  object
    method component_class : component_class Js.t Js.optdef_prop

    method name : Js.js_string Js.t Js.optdef_prop
  end

type instanciator =
  ?active_key:string ->
  ?animation_bool:bool ->
  ?animation_string:string ->
  ?as_:string ->
  ?bordered:bool ->
  ?classes:string list ->
  ?colspan:string ->
  ?custom:bool ->
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
  ?overlay:element Js.t ->
  ?placeholder:string ->
  ?placement:string ->
  ?ref:ref_ Js.t ->
  ?size:string ->
  ?src:string ->
  ?step:float ->
  ?style:(string * string) list ->
  ?title:string ->
  ?title_jsx:element Js.t ->
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
  element Js.t list ->
  element Js.t

(* User's component types *********************************************************************** *)
type 'main_props render = 'main_props -> element Js.t

type 'main_props construction =
  'main_props render * (unit -> unit -> unit) * (unit -> unit) * (unit -> unit) * unit React.event

(* 0 primitives *)
type 'main_props constructor = 'main_props -> 'main_props construction

(* 1 primitive *)
type ('main_props, 's0_prop) constructor_s =
  s0:'s0_prop React.signal -> 'main_props -> 'main_props construction

type ('main_props, 'e0_prop) constructor_e =
  e0:'e0_prop React.event -> 'main_props -> 'main_props construction

(* 2 primitives *)
type ('main_props, 's0_prop, 's1_prop) constructor_ss =
  s0:'s0_prop React.signal -> s1:'s1_prop React.signal -> 'main_props -> 'main_props construction

type ('main_props, 's0_prop, 'e0_prop) constructor_se =
  s0:'s0_prop React.signal -> e0:'e0_prop React.event -> 'main_props -> 'main_props construction

type ('main_props, 'e0_prop, 'e1_prop) constructor_ee =
  e0:'e0_prop React.event -> e1:'e1_prop React.event -> 'main_props -> 'main_props construction

(* 3 primitives *)
type ('main_props, 's0_prop, 's1_prop, 'e0_prop) constructor_sse =
  s0:'s0_prop React.signal ->
  s1:'s1_prop React.signal ->
  e0:'e0_prop React.event ->
  'main_props ->
  'main_props construction

type ('main_props, 's0_prop, 'e0_prop, 'e1_prop) constructor_see =
  s0:'s0_prop React.signal ->
  e0:'e0_prop React.event ->
  e1:'e1_prop React.event ->
  'main_props ->
  'main_props construction

(* 4 primitives *)
type ('main_props, 's0_prop, 's1_prop, 'e0_prop, 'e1_prop) constructor_ssee =
  s0:'s0_prop React.signal ->
  s1:'s1_prop React.signal ->
  e0:'e0_prop React.event ->
  e1:'e1_prop React.event ->
  'main_props ->
  'main_props construction

type ('main, 's0, 's1, 'e0, 'e1, 'constructor) props_package =
  | Unit : 'main -> ('main, _, _, _, _, 'main constructor) props_package
  | S :
      ('main * 's0 React.signal)
      -> ('main, 's0, _, _, _, ('main, 's0) constructor_s) props_package
  | E : ('main * 'e0 React.event) -> ('main, _, _, 'e0, _, ('main, 'e0) constructor_e) props_package
  | Ss :
      ('main * 's0 React.signal * 's1 React.signal)
      -> ('main, 's0, 's1, _, _, ('main, 's0, 's1) constructor_ss) props_package
  | Sse :
      ('main * 's0 React.signal * 's1 React.signal * 'e0 React.event)
      -> ('main, 's0, 's1, 'e0, _, ('main, 's0, 's1, 'e0) constructor_sse) props_package
  | Ssee :
      ('main * 's0 React.signal * 's1 React.signal * 'e0 React.event * 'e1 React.event)
      -> ('main, 's0, 's1, 'e0, 'e1, ('main, 's0, 's1, 'e0, 'e1) constructor_ssee) props_package

class type ['main, 's0, 's1, 'e0, 'e1, 'constructor, 'key] props_holder =
  object
    method data : ('main, 's0, 's1, 'e0, 'e1, 'constructor) props_package Js.readonly_prop

    method key : 'key Js.Optdef.t Js.readonly_prop
  end

class type state =
  object
    method revision : int Js.readonly_prop
  end

class type ['main, 's0, 's1, 'e0, 'e1, 'constructor, 'key] component =
  object
    method setState : state Js.t -> unit Js.meth

    method props :
      ('main, 's0, 's1, 'e0, 'e1, 'constructor, 'key) props_holder Js.t Js.readonly_prop

    method ftJsRender : (unit -> element Js.t) Js.callback Js.writeonly_prop

    method ftJsMount : (unit -> unit) Js.callback Js.writeonly_prop

    method ftJsUpdate : (unit -> unit) Js.callback Js.writeonly_prop

    method ftJsUnmount : (unit -> unit) Js.callback Js.writeonly_prop
  end

(* Helpers ************************************************************************************** *)
let _name_of_function : 'constructor -> string option =
 fun f -> Js.Unsafe.get f (Js.string "name") |> Js.Optdef.to_option |> Option.map Js.to_string

let _component_class_of_constructor :
    'constructor -> (unit -> component_class Js.t) -> component_class Js.t =
 fun constructor build_class ->
  let constructor : constructor_as_obj Js.t = Obj.magic constructor in

  match constructor##.component_class |> Js.Optdef.to_option with
  | Some cls -> cls
  | None ->
      let cls = build_class () in
      constructor##.component_class := cls;
      cls

external _create_component_class :
  (('main, 's0, 's1, 'e0, 'e1, 'constructor, 'key) component Js.t ->
  ('main, 's0, 's1, 'e0, 'e1, 'constructor, 'key) props_holder Js.t ->
  unit)
  Js.callback ->
  Js.js_string Js.t ->
  component_class Js.t = "ft_js_create_component_class"

let _of_constructor :
    type main s0 s1 e0 e1 constructor.
    constructor -> (main, s0, s1, e0, e1, constructor, 'key) props_holder Js.t -> element Js.t =
 fun constructor ->
  let build_class_on_cache_miss () =
    let prime_react_component :
        (main, s0, s1, e0, e1, constructor, 'key) component Js.t ->
        (main, s0, s1, e0, e1, constructor, 'key) props_holder Js.t ->
        unit =
     fun self props_holder ->
      (* Call user's constructor *)
      let render, user_mount, update, user_unmount, state_changes =
        match props_holder##.data with
        | Unit main -> constructor main
        | S (main, s0) -> constructor main ~s0
        | E (main, e0) -> constructor main ~e0
        | Ss (main, s0, s1) -> constructor main ~s0 ~s1
        | Sse (main, s0, s1, e0) -> constructor main ~s0 ~s1 ~e0
        | Ssee (main, s0, s1, e0, e1) -> constructor main ~s0 ~s1 ~e0 ~e1
      in
      self##.ftJsUpdate := Js.wrap_callback update;

      (* Wrap user's render *)
      let render () =
        match self##.props##.data with
        | Unit main -> render main
        | S (main, _) -> render main
        | E (main, _) -> render main
        | Ss (main, _, _) -> render main
        | Sse (main, _, _, _) -> render main
        | Ssee (main, _, _, _, _) -> render main
      in
      self##.ftJsRender := Js.wrap_callback render;

      (* Bind react primitives to reactjs states (they were all mapped to a single unit React.event
         in `construct`.

         Each time a primitive changes we don't update the component's state right away in order to
         give time to Reactml to finish its React.step before some new rendering is triggered.
         If several update are scheduled simultaneously we discard them all but the last one.
      *)
      let count = ref 0 in
      let unmounted = ref false in
      let state_changes =
        let onchange () =
          incr count;
          let c = !count in
          Lwt_js_events.async (fun () ->
              ( if c = !count && !unmounted = false then
                let o =
                  object%js
                    val revision = !count
                  end
                in
                self##setState o );
              Lwt.return ())
        in
        React.E.map onchange state_changes
      in

      (* prepare mount and unmount *)
      let user_unmount' : (unit -> unit) option ref = ref None in
      let mount () = user_unmount' := Some (user_mount ()) in
      let unmount () =
        unmounted := true;
        React.E.stop ~strong:true state_changes;
        ( match props_holder##.data with
        | Unit _ -> ()
        | S (_, s0) -> React.S.stop ~strong:true s0
        | E (_, e0) -> React.E.stop ~strong:true e0
        | Ss (_, s0, s1) ->
            React.S.stop ~strong:true s0;
            React.S.stop ~strong:true s1
        | Sse (_, s0, s1, e0) ->
            React.S.stop ~strong:true s0;
            React.S.stop ~strong:true s1;
            React.E.stop ~strong:true e0
        | Ssee (_, s0, s1, e0, e1) ->
            React.S.stop ~strong:true s0;
            React.S.stop ~strong:true s1;
            React.E.stop ~strong:true e0;
            React.E.stop ~strong:true e1 );
        user_unmount ();
        match !user_unmount' with None -> () | Some f -> f ()
      in
      self##.ftJsMount := Js.wrap_callback mount;
      self##.ftJsUnmount := Js.wrap_callback unmount
    in

    let display_name =
      match _name_of_function constructor with
      | None -> Js.string "ocaml_constructor"
      | Some name -> "ocaml_" ^ name |> Js.string
    in
    _create_component_class (Js.wrap_callback prime_react_component) display_name
  in
  let cls = _component_class_of_constructor constructor build_class_on_cache_miss in
  fun props_holder ->
    let open Js.Unsafe in
    fun_call global##._React##.createElement [| inject cls; inject props_holder |]

(* Functions ************************************************************************************ *)

let render elt container =
  let open Js.Unsafe in
  fun_call global##._ReactDOM##.render [| inject elt; inject container |]

let construct ?mount ?update ?unmount ?signal:s0 ?signal:s1 ?signal:s2 ?signal:s3 ?signal:s4
    ?events:e0 render =
  let mount = Option.value ~default:(fun () () -> ()) mount in
  let update = Option.value ~default:(fun () -> ()) update in
  let unmount = Option.value ~default:(fun () -> ()) unmount in
  let diffs =
    [
      s0 |> Option.to_list |> List.map (fun s -> s |> React.S.changes |> React.E.map (fun _ -> ()));
      s1 |> Option.to_list |> List.map (fun s -> s |> React.S.changes |> React.E.map (fun _ -> ()));
      s2 |> Option.to_list |> List.map (fun s -> s |> React.S.changes |> React.E.map (fun _ -> ()));
      s3 |> Option.to_list |> List.map (fun s -> s |> React.S.changes |> React.E.map (fun _ -> ()));
      s4 |> Option.to_list |> List.map (fun s -> s |> React.S.changes |> React.E.map (fun _ -> ()));
      e0 |> Option.to_list |> List.map (fun s -> s |> React.E.map (fun _ -> ()));
    ]
    |> List.concat |> React.E.select
  in
  (render, mount, update, unmount, diffs)

let create_ref () =
  let open Js.Unsafe in
  fun_call global##._React##.createRef [||]

module Jsx = struct
  let ( >> ) v f = f [ v ]

  let _create_element ctor ?active_key ?animation_bool ?animation_string ?as_ ?bordered ?classes
      ?colspan ?custom ?default_active_key ?default_value ?disabled ?event_key ?fluid ?href ?id
      ?inline ?inner_html ?key ?label ?min ?name ?no_gutters ?on_change ?on_click ?on_close
      ?on_select ?overlay ?placeholder ?placement ?ref ?size ?src ?step ?style ?title ?title_jsx
      ?transition ?type_ ?value ?variant ?lg_order ?lg_span ?md_order ?md_span ?sm_order ?sm_span
      ?xl_order ?xl_span ?xs_order ?xs_span children =
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
    set_prop custom "custom" Js.bool;
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

  let of_string s = s |> Js.string |> Obj.magic

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

  let of_constructor constructor ?key main =
    let props_holder =
      object%js
        val data = Unit main

        val key = Js.Optdef.option key
      end
    in
    _of_constructor constructor props_holder

  let of_constructor_e constructor ?key main ~e0 =
    let emap s = React.E.map Fun.id s in
    let props_holder =
      object%js
        val data = E (main, emap e0)

        val key = Js.Optdef.option key
      end
    in
    _of_constructor constructor props_holder

  let of_constructor_s constructor ?key main ~s0 =
    let smap s = React.S.map ~eq:( == ) Fun.id s in
    let props_holder =
      object%js
        val data = S (main, smap s0)

        val key = Js.Optdef.option key
      end
    in
    _of_constructor constructor props_holder

  let of_constructor_ss constructor ?key main ~s0 ~s1 =
    let smap s = React.S.map ~eq:( == ) Fun.id s in
    let props_holder =
      object%js
        val data = Ss (main, smap s0, smap s1)

        val key = Js.Optdef.option key
      end
    in
    _of_constructor constructor props_holder

  let of_constructor_sse constructor ?key main ~s0 ~s1 ~e0 =
    let smap s = React.S.map ~eq:( == ) Fun.id s in
    let emap s = React.E.map Fun.id s in
    let props_holder =
      object%js
        val data = Sse (main, smap s0, smap s1, emap e0)

        val key = Js.Optdef.option key
      end
    in
    _of_constructor constructor props_holder

  let of_constructor_ssee constructor ?key main ~s0 ~s1 ~e0 ~e1 =
    let smap s = React.S.map ~eq:( == ) Fun.id s in
    let emap s = React.E.map Fun.id s in
    let props_holder =
      object%js
        val data = Ssee (main, smap s0, smap s1, emap e0, emap e1)

        val key = Js.Optdef.option key
      end
    in
    _of_constructor constructor props_holder
end
