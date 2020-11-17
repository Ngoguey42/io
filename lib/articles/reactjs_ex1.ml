module Reactjs = Ft_js.Reactjs

(* snip-before *)

(** The [React.E.map] output event will never be collected. *)
let leaking_head (queries, answer) =
  let heads = [| "\u{1F622}"; "\u{1F625}"; "\u{1F62D}"; "\u{1F630}" |] in
  let head = heads.(Random.int 4) in
  queries |> React.E.map (fun () -> answer (fun s -> s ^ head)) |> ignore;
  let render _ = Reactjs.Jsx.of_string head in
  Reactjs.construct render

(** [queries] is a copy of the original. It will be automatically collected on unmount,
    triggering the collection of the [React.E.map] output event.
*)
let safe_head ~e0:queries answer =
  let heads = [| "\u{1F60D}"; "\u{1F618}"; "\u{1F63B}"; "\u{1F970}" |] in
  let head = heads.(Random.int 4) in
  queries |> React.E.map (fun () -> answer (fun s -> s ^ head)) |> ignore;
  let render _ = Reactjs.Jsx.of_string head in
  Reactjs.construct render

let construct_component () =
  (* Store a `revision` integer that will be used to force the reconstruction of children
     components (a.k.a. Fully uncontrolled component with a key in Reactjs parlance). *)
  let revision, set_revision = React.S.create (Random.int 10000) in
  let on_click0 _ = set_revision (Random.int 10000) in

  (* Build React events for the children components to connect to. *)
  let probe_queries, probe = React.E.create () in
  let probe_answers, answer_probe = React.E.create () in
  let answer_probe : (string -> string) -> unit = answer_probe in
  let probe_result = React.S.accum probe_answers "" in
  let on_click1 _ =
    answer_probe (fun _ -> "");
    probe ()
  in

  let render () =
    let open Reactjs.Jsx in
    let classes = [ "btn"; "btn-primary" ] in
    let current_heads =
      let revision = React.S.value revision in
      let h0 = of_constructor ~key:(revision, 0) leaking_head (probe_queries, answer_probe) in
      let h1 = of_constructor_e ~key:(revision, 1) safe_head ~e0:probe_queries answer_probe in
      [ of_string "The current leaking head:"; h0; of_string " | The current safe head:"; h1 ]
      |> of_tag "h3"
    in
    let connected_heads =
      [
        of_string "Probe connected heads" >> of_tag "button" ~on_click:on_click1 ~classes;
        React.S.value probe_result |> of_string;
      ]
      |> of_tag "h3"
    in
    let rebuild =
      of_string "Construct new random heads"
      >> of_tag "button" ~on_click:on_click0 ~classes
      >> of_tag "h3"
    in
    [ current_heads; connected_heads; rebuild ] |> of_tag "div" ~style:[ ("textAlign", "center") ]
  in

  let unmount () =
    React.S.stop ~strong:true revision;
    React.E.stop ~strong:true probe_queries;
    React.E.stop ~strong:true probe_answers
  in

  Reactjs.construct ~signal:probe_result ~signal:revision ~unmount render

(* snip-after *)
