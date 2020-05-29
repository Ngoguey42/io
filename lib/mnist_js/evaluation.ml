open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

open Types

let _routine { db; encoder; decoder; config = { verbose; batch_size; backend; _ } } fire_event =
  let main () =
    let module Backend = (val Backend.create backend) in
    Backend.eval ~fire_event ~verbose ~batch_size ~db ~encoder ~decoder
  in
  let on_error exn =
    fire_event (`Outcome (`Crash exn));
    Lwt.return ()
  in
  Lwt.catch main on_error

let repair_bigarray : 'a -> 'a =
 fun a ->
  let f : _ -> _ -> _ -> _ -> 'a = Ft_js.caml_ba_create_unsafe in
  f
    (Js.Unsafe.get a (Js.string "kind"))
    (Js.Unsafe.get a (Js.string "layout"))
    (Js.Unsafe.get a (Js.string "dims"))
    (Js.Unsafe.get a (Js.string "data"))

module Webworker_routine = struct
  type parameters = {
    db : db_test;
    encoder : Fnn.storable_nn;
    decoder : Fnn.storable_nn;
    config : evaluation_config;
  }

  type _in_msg = [ `Prime of parameters ]

  type outcome = [ `End of evaluation_stats | `Crash of exn ]

  type event = [ `Init | `Batch_begin of int | `Batch_end of int | `Outcome of evaluation_outcome ]

  type _out_msg = event

  let preprocess_in_msg : _ -> _in_msg = function
    | `Prime Types.{ db; encoder; decoder; config } ->
        let f = Fnn.storable_of_fnn in
        let encoder = f encoder in
        let decoder = f decoder in
        `Prime { db; encoder; decoder; config }

  let postprocess_in_msg : _in_msg -> _ = function
    | `Prime { db = imgs, labs; encoder; decoder; config } ->
        let f : Fnn.storable_nn -> Fnn.network =
          Fnn.fnn_of_storable (module Fnn.Builder : Fnn.BUILDER)
        in
        let encoder = f encoder in
        let decoder = f decoder in
        let imgs = repair_bigarray imgs in
        let labs = repair_bigarray labs in
        Types.(`Prime { db = (imgs, labs); encoder; decoder; config })

  let preprocess_out_msg : Types.evaluation_routine_event -> _out_msg = fun v -> v

  let postprocess_out_msg : _out_msg -> Types.evaluation_routine_event = function
    | `Outcome (`End stats) ->
        let test_set_sample_probas = repair_bigarray stats.test_set_sample_probas in
        `Outcome (`End { stats with test_set_sample_probas })
    | v -> v

  module rec Ww : (Webworker.S with type in_msg = _in_msg and type out_msg = _out_msg) =
  Webworker.Make (struct
    type in_msg = _in_msg

    type out_msg = _out_msg

    let create_on_in_message () =
      let routine_events, fire_routine_event = React.E.create () in

      routine_events |> React.E.map preprocess_out_msg |> React.E.map Ww.post_out_message |> ignore;

      let on_in_message msg =
        match postprocess_in_msg msg with
        | `Prime params ->
            Ft_js.Scripts.urls_of_entry ~what:`Js `Tfjs
            |> List.iter Js_of_ocaml.Worker.import_scripts;
            let routine () = _routine params fire_routine_event in
            Js_of_ocaml_lwt.Lwt_js_events.async routine
      in
      on_in_message
  end)

  include Ww
end

let routine params fire_upstream_event =
  if not params.config.from_webworker then _routine params fire_upstream_event
  else
    let on_error ev =
      let ev =
        match
          ( (Js.Unsafe.coerce ev)##.msg |> Js.Optdef.to_option,
            (Js.Unsafe.coerce ev)##.message |> Js.Optdef.to_option )
        with
        | None, None -> "unknown"
        | Some s, _ -> Js.to_string s
        | _, Some s -> Js.to_string s
      in
      let ev = `Outcome (`Crash (Failure ev)) in
      fire_upstream_event ev
    in
    let fire_upstream_event ww ev =
      let ev = ev |> Webworker_routine.postprocess_out_msg in
      (match ev with `Outcome _ -> Webworker_routine.terminate ww | _ -> ());
      fire_upstream_event ev
    in
    let ww = Webworker_routine.create fire_upstream_event on_error in
    Webworker_routine.post_in_message ww (Webworker_routine.preprocess_in_msg (`Prime params));
    Lwt.return ()
