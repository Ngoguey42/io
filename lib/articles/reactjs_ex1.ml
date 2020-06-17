module Reactjs = Ft_js.Reactjs

(* snip-before *)
let leaking_head (queries, answer) =
  let heads = [| "\u{1F622}"; "\u{1F625}"; "\u{1F62D}"; "\u{1F630}" |] in
  let head = heads.(Random.int 4) in
  queries |> React.E.map (fun () -> answer (fun s -> s ^ head)) |> ignore;
  let render _ = Reactjs.Jsx.of_string head in
  Reactjs.construct render

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

  (* Build React events for the children components to connect to. *)
  let probe_queries, probe = React.E.create () in
  let probe_answers, answer_probe = React.E.create () in
  let answer_probe : (string -> string) -> unit = answer_probe in
  let probe_result = React.S.accum probe_answers "" in

  let render () =
    let open Reactjs.Jsx in
    let classes = [ "btn"; "btn-primary" ] in
    let heads =
      let revision = React.S.value revision in
      let on_click _ = set_revision (Random.int 10000) in
      let h0 = of_constructor ~key:(revision, 0) leaking_head (probe_queries, answer_probe) in
      let h1 = of_constructor_e ~key:(revision, 1) safe_head ~e0:probe_queries answer_probe in
      [
        of_string "Reconstruct heads" >> of_tag "button" ~on_click ~classes;
        of_string "(";
        h0;
        of_string " will leak)(";
        h1;
        of_string " will not leak)";
      ]
      |> of_tag "h3"
    in
    let connected_heads =
      let on_click _ =
        answer_probe (fun _ -> "");
        probe ()
      in
      [
        of_string "Probe connected heads" >> of_tag "button" ~on_click ~classes;
        React.S.value probe_result |> of_string;
      ]
      |> of_tag "h3"
    in
    [ heads; connected_heads ] |> of_tag "div" ~style:[ ("textAlign", "center") ]
  in
  let unmount () =
    React.S.stop ~strong:true revision;
    React.E.stop ~strong:true probe_queries;
    React.E.stop ~strong:true probe_answers
  in
  Reactjs.construct ~signal:probe_result ~signal:revision ~unmount render

(* snip-after *)
