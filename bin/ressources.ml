open struct
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
  module Reactjs = Ft_js.Reactjs
  module Mnist = Ft_cnnjs.Mnist
  module Scripts = Ft_js.Scripts
end

type uint8_ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

module Vertex = struct
  type t = [ Ft_js.Scripts.entry | Mnist.entry ]

  let compare = Stdlib.compare

  let hash = Hashtbl.hash

  let equal a b = compare a b = 0
end

module G = Graph.Persistent.Digraph.Concrete (Vertex)
module Vset = Set.Make (Vertex)

let dependencies =
  [
    (`Pako, []);
    (`Tfjs, []);
    (`Cryptojs, []);
    (`Train_imgs, [ `Pako ]);
    (`Train_labs, [ `Pako ]);
    (`Test_imgs, [ `Pako ]);
    (`Test_labs, [ `Pako ]);
  ]

let name_of_entry : Vertex.t -> string = function
  | #Mnist.entry as entry -> Mnist.filename_of_entry entry
  | `Tfjs -> "Tensorflow JS"
  | `Pako -> "Pako JS"
  | `Cryptojs -> "Crypto JS"
  | `Reactjs -> "React JS"

let initial_graph =
  List.fold_left
    (fun g (entry, deps) ->
      let g = G.add_vertex g entry in
      List.fold_left (fun g entry' -> G.add_edge g entry entry') g deps)
    G.empty dependencies

let get_leaf_vertices g =
  let leaves = G.fold_vertex (fun v l -> if G.out_degree g v = 0 then v :: l else l) g [] in
  assert (G.is_empty g = (List.length leaves = 0));
  leaves |> List.to_seq |> Vset.of_seq

let construct_tr (entry, events) =
  let name = name_of_entry entry in
  let signal =
    events
    |> React.E.filter (fun (entry', _) -> entry = entry')
    |> React.E.map (fun (_, event) -> event)
    |> React.S.hold (`Ongoing "Pending")
  in
  let render _ =
    let open Reactjs.Jsx in
    let s = match React.S.value signal with `Ongoing s -> s | `Done -> "\u{02713}" in
    of_tag "tr" [ of_tag "th" [ of_string name ]; of_tag "th" [ of_string s ] ]
  in
  Reactjs.construct ~signal render

let construct : (uint8_ba * uint8_ba * uint8_ba * uint8_ba -> unit) -> _ =
 fun on_complete ->
  let signal, set_signal = React.S.create (initial_graph, Vset.empty) in
  let events, fire_event = React.E.create () in
  let mnist_tensors = Hashtbl.create 4 in

  let launch_mnist_fetch () =
    let fire_mnist_event (entry, status) =
      let entry = (entry :> Vertex.t) in
      match status with
      | `Unknown -> fire_event (entry, `Ongoing "?")
      | `Check -> fire_event (entry, `Ongoing "Checking...")
      | `Download (i, j) ->
          let f = float_of_int i /. float_of_int j *. 100. in
          fire_event (entry, `Ongoing (Printf.sprintf "Downloading... (%.0f%%)" f))
      | `Unzip -> fire_event (entry, `Ongoing "Unzipping...")
      | `Store -> fire_event (entry, `Ongoing "Storing...")
      | `Ready tensor ->
          Hashtbl.add mnist_tensors entry tensor;
          fire_event (entry, `Done)
    in
    Lwt_js_events.async (fun () -> Mnist.get fire_mnist_event)
  in

  let launch_script_fetch entry =
    Lwt_js_events.async (fun () ->
        let open Lwt.Infix in
        fire_event (entry, `Ongoing "Downloading...");
        Scripts.import entry >|= fun () -> fire_event (entry, `Done))
  in

  let launch_some_tasks (g, ongoing) =
    let leaves = get_leaf_vertices g in
    assert (Vset.subset ongoing leaves);
    let to_launch = Vset.diff leaves ongoing in
    if Vset.cardinal to_launch > 0 then set_signal (g, leaves);

    let mnist_tasks =
      to_launch |> Vset.to_seq |> List.of_seq
      |> List.filter_map (function #Mnist.entry as e -> Some e | #Scripts.entry -> None)
    in
    if List.length mnist_tasks > 0 then (
      assert (List.length mnist_tasks = 4);
      launch_mnist_fetch () );

    let script_tasks =
      to_launch |> Vset.to_seq |> List.of_seq
      |> List.filter_map (function #Scripts.entry as e -> Some e | #Mnist.entry -> None)
    in
    List.iter launch_script_fetch script_tasks
  in

  let on_event (entry, event) =
    match event with
    | `Ongoing _ -> ()
    | `Done ->
        let g, ongoing = React.S.value signal in
        assert (Vset.mem entry ongoing);
        let ongoing = Vset.remove entry ongoing in
        let g = G.remove_vertex g entry in
        set_signal (g, ongoing);

        if G.is_empty g then
          on_complete
            ( Hashtbl.find mnist_tensors `Train_imgs,
              Hashtbl.find mnist_tensors `Train_labs,
              Hashtbl.find mnist_tensors `Test_imgs,
              Hashtbl.find mnist_tensors `Test_labs )
        else launch_some_tasks (g, ongoing)
  in

  let render _ =
    let open Reactjs.Jsx in
    let head = of_tag "tr" [ of_tag "th" ~colspan:"2" [ of_string "Ressources" ] ] in
    let entries = List.map fst dependencies in
    let tails =
      List.map (fun entry -> of_constructor ~key:entry construct_tr (entry, events)) entries
    in
    of_tag "table" ~class_:"ressources" [ of_tag "thead" [ head ]; of_tag "tbody" tails ]
  in

  let mount () =
    React.E.map on_event events |> ignore;

    let g, ongoing = React.S.value signal in
    assert (g == initial_graph);
    assert (Vset.is_empty ongoing);
    launch_some_tasks (g, ongoing)
  in
  Reactjs.construct ~mount render
