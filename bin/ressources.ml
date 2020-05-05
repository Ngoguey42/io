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
    (`Pagebuilder, []);
    (`Reactjs, []);
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
  | `Pagebuilder -> "OCaml code"
  | `Reactjs -> "ReactJS"
  | `Tfjs -> "TensorFlow.js"
  | `Pako -> "pako"
  | `Cryptojs -> "CryptoJS"

let description_of_entry : Vertex.t -> string = function
  | `Pagebuilder -> "Webpage source code and OCaml external libraries transpiled to one JavaScript file"
  | `Reactjs -> "User interface js library"
  | `Tfjs -> "Tensor computations js library running on cpu or gpu using WebGL"
  | `Pako -> "Compression js library"
  | `Cryptojs -> "Cryptography js library"
  | `Train_imgs -> "MNIST train-set images"
  | `Train_labs -> "MNIST train-set labels"
  | `Test_imgs -> "MNIST test-set images"
  | `Test_labs -> "MNIST test-set labels"

let size_of_entry : Vertex.t -> int = function
  | `Pagebuilder -> 987654321
  | `Reactjs -> 98765432
  | `Tfjs -> 9876543
  | `Pako -> 987654
  | `Cryptojs -> 98765
  | `Train_imgs -> 9876
  | `Train_labs -> 987
  | `Test_imgs -> 98
  | `Test_labs -> 9

let string_of_byte_count count =
  assert (count >= 0);
  let suffix, count =
    if count < 1000 then ("B", float_of_int count)
    else if count < 1000 * 1000 then ("KB", (float_of_int count) /. 1000.)
    else if count < 1000 * 1000 * 1000 then ("MB", (float_of_int (count / 1000)) /. 1000.)
    else ("GB", (float_of_int (count / 1000 / 1000)) /. 1000.)
  in
  let right_digit_count =
    if suffix = "B" then `Zero
    else if count < 10. then `Two
    else if count < 100. then `One
    else `Zero
  in
  match right_digit_count with
  | `Two -> Printf.sprintf "%.2f %s" count suffix
  | `One -> Printf.sprintf "%.1f %s" count suffix
  | `Zero -> Printf.sprintf "%.0f %s" count suffix

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
  let description = description_of_entry entry in
  let size = Printf.sprintf "\u{00a0}(%s)" (size_of_entry entry |> string_of_byte_count) in
  let signal =
    events
    |> React.E.filter (fun (entry', _) -> entry = entry')
    |> React.E.map (fun (_, event) -> event)
    |> React.S.hold (`Ongoing "Pending")
  in
  let render _ =
    let open Reactjs.Jsx in
    let s = match React.S.value signal with `Ongoing s -> s | `Done -> "\u{02713}" in
    let tt = of_tag "span" ~class_:"tooltiptext" [ of_string description ] in
    ignore size;
    of_tag "tr"
      [
        of_tag "th" ~class_:"entry-info" [ of_tag "div" [
                               of_tag "div" ~class_:"tooltip" [ of_string name; tt ]
                             ; of_tag "div" ~class_:"entry-size" [ of_string size ]
                             (* ; tt *)

                    ] ];
        of_tag "th" ~class_:"entry-status" [ of_string s ];
      ]
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
    if entry = `Reactjs || entry = `Pagebuilder then fire_event (entry, `Done)
    else
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
