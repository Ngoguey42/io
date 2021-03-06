open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

open Types

let repair_string : string -> string = fun s -> String.sub s 0 (String.length s)

let repair_bigarray : 'a -> 'a =
 fun a ->
  let f : _ -> _ -> _ -> _ -> 'a = Ft_js.caml_ba_create_unsafe in
  f
    (Js.Unsafe.get a (Js.string "kind"))
    (Js.Unsafe.get a (Js.string "layout"))
    (Js.Unsafe.get a (Js.string "dims"))
    (Js.Unsafe.get a (Js.string "data"))

let repair_storable_nn : 'a -> 'a =
 fun snn ->
  let layer_ids, graph, layers = snn in
  List.iter
    (fun i ->
      match Hashtbl.find layers i with
      | `Parameter32 (id, dimensions, init, o, tensor_opt, optim_opt) ->
          let tensor_opt =
            match tensor_opt with None -> None | Some v -> Some (repair_bigarray v)
          in
          let optim_opt =
            match optim_opt with
            | Some (`Adam (a, b, c, d, t0, t1)) ->
                Some (`Adam (a, b, c, d, repair_bigarray t0, repair_bigarray t1))
            | v -> v
          in
          Hashtbl.add layers i (`Parameter32 (id, dimensions, init, o, tensor_opt, optim_opt))
      | _ -> ())
    layer_ids;

  (layer_ids, graph, layers)

let _routine { db; encoder; decoder; config = { verbose; batch_size; backend; _ } } fire_event =
  let main () =
    let module Backend = (val Backend.create backend) in
    let yield_sleep_length = if Ft_js.Webworker.is_web_worker then 0. else 0.01 in
    Backend.eval ~fire_event ~verbose ~yield_sleep_length ~batch_size ~db ~encoder ~decoder
  in
  let on_error exn =
    fire_event (`Outcome (`Crash (Printexc.to_string exn)));
    Lwt.return ()
  in
  Lwt.catch main on_error

module Webworker_routine = struct
  type parameters = {
    db : db_test;
    encoder : Ocann.Default.Patch.storable_nn;
    decoder : Ocann.Default.Patch.storable_nn;
    config : evaluation_config;
  }

  type _in_msg = [ `Prime of parameters ]

  type outcome = [ `End of evaluation_stats | `Crash of exn ]

  type event = [ `Init | `Batch_begin of int | `Batch_end of int | `Outcome of evaluation_outcome ]

  type _out_msg = event

  let preprocess_in_msg : _ -> _in_msg = function
    | `Prime Types.{ db; encoder; decoder; config } ->
        let f = Ocann.Default.Patch.storable_of_ocann in
        let encoder = f encoder in
        let decoder = f decoder in
        `Prime { db; encoder; decoder; config }

  let postprocess_in_msg : _in_msg -> _ = function
    | `Prime { db = imgs, labs; encoder; decoder; config } ->
        let f : Ocann.Default.Patch.storable_nn -> Ocann.Default.network =
         fun nn ->
          nn |> repair_storable_nn
          |> Ocann.Default.Patch.ocann_of_storable
               (module Ocann.Default.Builder : Ocann.Default.BUILDER)
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
    | `Outcome (`Crash msg) -> `Outcome (`Crash (repair_string msg))
    | v -> v

  module rec Ww : (Webworker.S with type in_msg = _in_msg and type out_msg = _out_msg) =
  Webworker.Make (struct
    type in_msg = _in_msg

    type out_msg = _out_msg

    let create_on_in_message () =
      let routine_events, fire_routine_event =
        React.E.create ()
        (* collected when WW is stopped *)
      in

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
    let on_out_msg ww ev =
      let ev = ev |> Webworker_routine.postprocess_out_msg in
      (match ev with `Outcome _ -> Webworker_routine.terminate ww | _ -> ());
      fire_upstream_event ev
    in
    let on_out_error_msg ev =
      let ev =
        match
          ( (Js.Unsafe.coerce ev)##.msg |> Js.Optdef.to_option,
            (Js.Unsafe.coerce ev)##.message |> Js.Optdef.to_option )
        with
        | None, None -> "unknown"
        | Some s, _ -> Js.to_string s
        | _, Some s -> Js.to_string s
      in
      let ev = `Outcome (`Crash ev) in
      fire_upstream_event ev
    in
    let ww = Webworker_routine.create on_out_msg on_out_error_msg in
    `Prime params |> Webworker_routine.preprocess_in_msg |> Webworker_routine.post_in_message ww;
    Lwt.return ()
