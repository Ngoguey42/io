(** Js_of_ocaml wrapper for Reactjs using React(ml).

    See this blog post for an introduction: https://ngoguey42.github.io/reactjs-wrapper

    Alternatives or similar projects:
    - https://github.com/jchavarri/jsoo-react
    - https://github.com/reasonml/reason-react
    - https://github.com/briskml/brisk-reconciler

   I've identified at least those two design flaws:
   - The {v ?signal:s0 ?signal:s1 ?signal:s2 ... v} trick in {!construct} is very unusual. I'm
     looking for a better idea.
   - The optional constructor parameters (e.g. [~href]) must be manually declared in
     {!instanciator} and in {!_create_element} which is a tedious process. I've only included
     the ones I needed. I'm also looking for a better idea.
 *)

(** {1 Misc Types} *)

(** Type of Reactjs's event JS object. *)
class type ['target] event =
  object
    method target : 'target Js_of_ocaml.Js.readonly_prop

    method preventDefault : unit Js_of_ocaml.Js.meth
  end

(** Type of Reactjs's ref JS object. *)
class type ref_ =
  object
    method current :
      Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t Js_of_ocaml.Js.Opt.t
      Js_of_ocaml.Js.readonly_prop
  end

class type element = object end
(** Type of Reactjs's element JS object. Usually constructed using the JSX syntax. Always
    constructed a call to React.createElement (in JavaScript). *)

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
  ?on_change:(Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t event Js_of_ocaml.Js.t -> unit) ->
  ?on_click:(Js_of_ocaml.Dom_html.buttonElement Js_of_ocaml.Js.t event Js_of_ocaml.Js.t -> unit) ->
  ?on_close:(unit -> unit) ->
  ?on_select:
    (Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
    Js_of_ocaml.Dom_html.anchorElement Js_of_ocaml.Js.t event Js_of_ocaml.Js.t ->
    unit) ->
  ?overlay:element Js_of_ocaml.Js.t ->
  ?placeholder:string ->
  ?placement:string ->
  ?ref:ref_ Js_of_ocaml.Js.t ->
  ?size:string ->
  ?src:string ->
  ?step:float ->
  ?style:(string * string) list ->
  ?title:string ->
  ?title_jsx:element Js_of_ocaml.Js.t ->
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
  element Js_of_ocaml.Js.t list ->
  element Js_of_ocaml.Js.t
(** Trailing parameters of all the constructor function (except `of_string` and user
    defined ones).
 *)

(** {1 User Defined Component Types} *)

type 'main_props render = 'main_props -> element Js_of_ocaml.Js.t
(** Type of the render function inside a user defined constructor. *)

type 'main_props construction =
  'main_props render * (unit -> unit -> unit) * (unit -> unit) * (unit -> unit) * unit React.event
(** Output type of a user defined constructor. A value of such type is built by a call
    to `construct`.
 *)

type 'main_props constructor = 'main_props -> 'main_props construction
(** Type of a simple user defined constructor. *)

type ('main_props, 's0_prop) constructor_s =
  s0:'s0_prop React.signal -> 'main_props -> 'main_props construction
(** Type of a user defined constructor accepting 1 signal automatically collected. *)

type ('main_props, 'e0_prop) constructor_e =
  e0:'e0_prop React.event -> 'main_props -> 'main_props construction
(** Type of a user defined constructor accepting 1 event automatically collected. *)

type ('main_props, 's0_prop, 's1_prop) constructor_ss =
  s0:'s0_prop React.signal -> s1:'s1_prop React.signal -> 'main_props -> 'main_props construction
(** Type of a user defined constructor accepting 2 signals automatically collected. *)

type ('main_props, 's0_prop, 'e0_prop) constructor_se =
  s0:'s0_prop React.signal -> e0:'e0_prop React.event -> 'main_props -> 'main_props construction
(** Type of a user defined constructor accepting 1 signal and 1 event automatically collected. *)

type ('main_props, 'e0_prop, 'e1_prop) constructor_ee =
  e0:'e0_prop React.event -> e1:'e1_prop React.event -> 'main_props -> 'main_props construction
(** Type of a user defined constructor accepting 2 events automatically collected. *)

type ('main_props, 's0_prop, 's1_prop, 'e0_prop) constructor_sse =
  s0:'s0_prop React.signal ->
  s1:'s1_prop React.signal ->
  e0:'e0_prop React.event ->
  'main_props ->
  'main_props construction
(** Type of a user defined constructor accepting 2 signals and 1 event automatically collected. *)

type ('main_props, 's0_prop, 'e0_prop, 'e1_prop) constructor_see =
  s0:'s0_prop React.signal ->
  e0:'e0_prop React.event ->
  e1:'e1_prop React.event ->
  'main_props ->
  'main_props construction
(** Type of a user defined constructor accepting 1 signal and 2 events automatically collected. *)

type ('main_props, 's0_prop, 's1_prop, 'e0_prop, 'e1_prop) constructor_ssee =
  s0:'s0_prop React.signal ->
  s1:'s1_prop React.signal ->
  e0:'e0_prop React.event ->
  e1:'e1_prop React.event ->
  'main_props ->
  'main_props construction
(** Type of a user defined constructor accepting 2 signals and 2 events automatically collected. *)

(** {1 Functions} *)

val render : element Js_of_ocaml.Js.t -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit
(** Direct binding to Reactjs's render function.
    @see https://reactjs.org/docs/react-dom.html#render { JS documentation. } *)

val construct :
  ?mount:(unit -> unit -> unit) ->
  ?update:(unit -> unit) ->
  ?unmount:(unit -> unit) ->
  ?signal:'a React.signal ->
  ?signal:'b React.signal ->
  ?signal:'c React.signal ->
  ?signal:'d React.signal ->
  ?signal:'e React.signal ->
  ?events:'f React.event ->
  'props render ->
  'props construction
(** To be called at the end of a constructor.

    {v
    OCaml piece of code | React.Component class equivalent
    ------------------------------------------------------
    construct caller    | .constructor
    ~mount              | .componentDidMount
    ~update             | .componentDidUpdate
    ~unmount            | .componentDidMount
    ~signal             | .state
    ~events             | .state
    render              | .render
    v} *)

val create_ref : unit -> ref_ Js_of_ocaml.Js.t
(** Direct binding to Reactjs's ref function.
    @see https://reactjs.org/docs/refs-and-the-dom.html { JS documentation }. *)

(** Module to be opened in user defined render functions. *)
module Jsx : sig
  val ( >> ) :
    element Js_of_ocaml.Js.t ->
    (element Js_of_ocaml.Js.t list -> element Js_of_ocaml.Js.t) ->
    element Js_of_ocaml.Js.t
  (** [x >> y] is [x |> [y]]. *)

  val of_string : string -> element Js_of_ocaml.Js.t
  (** [of_string s] is a reactjs element representing a DOM string. *)

  val of_tag : string -> instanciator
  (** [of_tag t children] is a reactjs element representing a DOM element of tag [t] (e.g. "div")
      and containing the [children] list of reactjs elements.

      Optional parameters may be passed such as [~title:"Hello"] or [~style:["width", "200px"]].
   *)

  val of_react : string -> instanciator
  (** [of_react path children] is a reactjs element, instance of the predefined class located
      at [path] in the React JavaScript library (e.g. "Fragment").

      Optional parameters may be passed such as [~title:"Hello"] or [~style:["width", "200px"]].
   *)

  val of_bootstrap : string -> instanciator
  (** [of_bootstrap path children] is a reactjs element, instance of the predefined class located
      at [path] in the ReactBootstrap JavaScript library (e.g. "Col", "Form.Group").

      Optional parameters may be passed such as [~title:"Hello"] or [~style:["width", "200px"]].
   *)

  val of_constructor : 'main constructor -> ?key:'key -> 'main -> element Js_of_ocaml.Js.t
  (** [of_constructor c ~key props] is a reactjs element, instance of a user defined component
      written in OCaml and constructed by a call to [c props].

     Both the {e constructor} and the {e render} functions are called with the {e props}.
     The [props] content may vary from one render to another. Three design patterns can be adopted:

     - Perform render conditionned on {e render}'s props. To do so, ignore the mutable props passed
       to {e construct} and read those from {e render}.
     - Re-instanciate a new component on each render. To do so, pass a different [key] from
       parent's `of_constructor` call to trigger re-instantiations.
       (a.k.a. Fully uncontrolled component with a key).
     - Don't condition render on props (use [React.signal] and [React.event]). To do so, ignore the
       {e props} passed to `render` and use one of the constructor below. See
       {!of_constructor_ssee}.
   *)

  val of_constructor_e :
    ('main, 'e0) constructor_e ->
    ?key:'key ->
    'main ->
    e0:'e0 React.event ->
    element Js_of_ocaml.Js.t
  (** Specialization of {!of_constructor}. See {!of_constructor_ssee}. *)

  val of_constructor_s :
    ('main, 's0) constructor_s ->
    ?key:'key ->
    'main ->
    s0:'s0 React.signal ->
    element Js_of_ocaml.Js.t
  (** Specialization of {!of_constructor}. See {!of_constructor_ssee}. *)

  val of_constructor_ss :
    ('main, 's0, 's1) constructor_ss ->
    ?key:'key ->
    'main ->
    s0:'s0 React.signal ->
    s1:'s1 React.signal ->
    element Js_of_ocaml.Js.t
  (** Specialization of {!of_constructor}. See {!of_constructor_ssee}. *)

  val of_constructor_sse :
    ('main, 's0, 's1, 'e0) constructor_sse ->
    ?key:'key ->
    'main ->
    s0:'s0 React.signal ->
    s1:'s1 React.signal ->
    e0:'e0 React.event ->
    element Js_of_ocaml.Js.t
  (** Specialization of {!of_constructor}. See {!of_constructor_ssee}. *)

  val of_constructor_ssee :
    ('main, 's0, 's1, 'e0, 'e1) constructor_ssee ->
    ?key:'key ->
    'main ->
    s0:'s0 React.signal ->
    s1:'s1 React.signal ->
    e0:'e0 React.event ->
    e1:'e1 React.event ->
    element Js_of_ocaml.Js.t
  (** Specialization of {!of_constructor} that takes 4 FRP primitives props in addition to the
      usual props.

      Since Reactml doesn't use a {e weak dict} with Js_of_ocaml there is no automatic collection
      of FRP primitives, all collections must be manual. This function automates most of the process
      when using with Reactjs.

      At construction time, the 4 input FRP primitives are internally copied (with an identity
      mapping) before being passed to the user defined constructor. At unmount time, the copies
      are automatically stopped with an explicit call to [React.S.stop] and [React.E.stop].
      Consequently, all the primitives derived from those initial 4 will be collected at unmount,
      and the original 4 input primitives will not be invalidated in case they are used inside
      sibling components or inside the parent itself.

      Implementation detail: The physical equality [==] is used for identity mapping of signals.
  *)
end
