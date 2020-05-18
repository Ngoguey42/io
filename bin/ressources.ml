open struct
  module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
  module Reactjs = Ft_js.Reactjs
  module Mnist = Ft_cnnjs.Mnist
  module Scripts = Ft_js.Scripts
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
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
    (`Reactjsbootstrap, [ `Reactjs ]);
    (`Pako, []);
    (`Tfjs, []);
    (`Cryptojs, []);
    (`Train_imgs, [ `Pako ]);
    (`Train_labs, [ `Pako ]);
    (`Test_imgs, [ `Pako ]);
    (`Test_labs, [ `Pako ]);
  ]

let name_of_entry : [< Vertex.t ] -> string = function
  | #Mnist.entry as entry -> Mnist.filename_of_entry entry
  | `Pagebuilder -> "Website's OCaml source code"
  | `Reactjs -> "ReactJS"
  | `Reactjsbootstrap -> "ReactJS Bootstrap"
  | `Tfjs -> "TensorFlow.js"
  | `Pako -> "pako"
  | `Cryptojs -> "CryptoJS"
  | `Bootstrap -> "Bootstrap"

let description_of_entry : Vertex.t -> string = function
  | `Pagebuilder ->
      "Website source code, OCaml runtime and OCaml external libraries transpiled to one \
       JavaScript file"
  | `Reactjs -> "User interface js library"
  | `Reactjsbootstrap -> "User interface js library"
  | `Tfjs -> "Tensor computations js library running on cpu or gpu using WebGL"
  | `Pako -> "Compression js library"
  | `Cryptojs -> "Cryptography js library"
  | `Bootstrap -> "User interface js/css library"
  | `Train_imgs -> "MNIST dataset train-set images"
  | `Train_labs -> "MNIST dataset train-set labels"
  | `Test_imgs -> "MNIST dataset test-set images"
  | `Test_labs -> "MNIST dataset test-set labels"

let urls_of_entry : Vertex.t -> string list = function
  | #Ft_js.Scripts.entry as entry -> Ft_js.Scripts.urls_of_entry entry |> List.concat
  | #Mnist.entry as entry -> [ Mnist.url_of_entry entry ]

(* Some hard-coded compressed file sizes that I couldn't retrieve through XHR *)
let byte_count_of_url_opt : string -> Int64.t option = function
  | "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs@1.7.3/dist/tf.min.js" -> Some 204294L
  | "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs-backend-wasm@1.7.3/dist/tf-backend-wasm.min.js" -> Some 11545L
  | "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/core.min.js" -> Some 1463L
  | "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/sha1.min.js" -> Some 700L
  | "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/4.0.0/sha256.min.js" -> Some 830L
  | "https://cdnjs.cloudflare.com/ajax/libs/pako/1.0.10/pako_inflate.min.js" -> Some 7412L
  | "https://unpkg.com/react@16/umd/react.development.js" -> Some 30840L
  | "https://unpkg.com/react-dom@16/umd/react-dom.development.js" -> Some 245565L
  | "https://cdnjs.cloudflare.com/ajax/libs/react-bootstrap/1.0.1/react-bootstrap.min.js" -> Some 33519L
  | "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css" -> Some 22555L
  | "https://code.jquery.com/jquery-3.5.1.slim.min.js" -> Some 23311L
  | "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/js/bootstrap.bundle.min.js" -> Some 20581L
  | _ -> None

let string_of_byte_count count =
  let k = Int64.of_int 1000 in
  let m = Int64.of_int (1000 * 1000) in
  let g = Int64.of_int (1000 * 1000 * 1000) in
  let suffix, count =
    if Int64.compare count k < 0 then ("B", Int64.to_float count)
    else if Int64.compare count m < 0 then ("KB", Int64.to_float count /. 1000.)
    else if Int64.compare count g < 0 then ("MB", Int64.to_float (Int64.div count k) /. 1000.)
    else ("GB", Int64.to_float (Int64.div count m) /. 1000.)
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
  let urls = urls_of_entry entry in
  let size_fetch_events, fire_size_fetch_event = React.E.create () in

  let size_option_signal =
    size_fetch_events
    |> React.E.map (fun (_, size) -> size)
    |> React.E.fold
         (fun acc res ->
           match (acc, res) with
           | Ok (count, sum), Ok size -> Ok (count + 1, Int64.add sum size)
           | Error _, _ -> acc
           | _, (Error _ as res) -> res)
         (Ok (0, Int64.zero))
    |> React.E.fmap (function
         | Ok (count, size) when count = List.length urls -> Some (Some size)
         | _ -> None)
    |> React.S.hold None
  in

  let signal =
    events
    |> React.E.filter (fun (entry', _) -> entry = entry')
    |> React.E.map (fun (_, event) -> event)
    |> React.S.hold (`Ongoing "Pending")
  in
  let render _ =
    let open Reactjs.Jsx in
    let s = match React.S.value signal with `Ongoing s -> s | `Done -> "\u{02713}" in
    let size =
      match React.S.value size_option_signal with
      | None -> ""
      | Some size -> Printf.sprintf "\u{00a0}(%s)" (string_of_byte_count size)
    in
    let tt =
      of_bootstrap "Tooltip" ~id:"tooltip-right" [ of_tag "div" [ of_string description ] ]
    in
    of_tag "tr"
      [
        of_tag "th" ~class_:[ "entry-name" ]
          [
            of_bootstrap "OverlayTrigger" ~placement:"right" ~overlay:tt
              [ of_tag "div" [ of_string (name ^ size) ] ];
          ];
        of_tag "th" ~class_:[ "entry-status" ] [ of_string s ];
      ]
  in
  let mount () =
    List.iter
      (fun url ->
        match byte_count_of_url_opt url with
        | Some size -> fire_size_fetch_event (url, Ok size)
        | None -> Ft_js.size_of_urls [ url ] fire_size_fetch_event)
      urls
  in

  Reactjs.construct ~signal ~signal:size_option_signal ~mount render

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

  let launch_script_fetch : Ft_js.Scripts.entry -> _ =
   fun entry ->
    if entry = `Reactjs || entry = `Pagebuilder || entry = `Reactjsbootstrap then
      Lwt_js_events.async (fun () ->
          fire_event ((entry :> Vertex.t), `Done);
          Lwt.return ())
    else
      Lwt_js_events.async (fun () ->
          let open Lwt.Infix in
          fire_event ((entry :> Vertex.t), `Ongoing "Downloading...");
          Scripts.import entry >|= fun () -> fire_event ((entry :> Vertex.t), `Done))
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
    let head = of_tag "tr" [ of_tag "th" ~colspan:"3" [ of_string "Ressources" ] ] in
    let entries = List.map fst dependencies in
    let tails =
      List.map (fun entry -> of_constructor ~key:entry construct_tr (entry, events)) entries
    in
    of_bootstrap "Table" ~class_:[ "ressources" ] ~bordered:true ~size:"sm"
      [ of_tag "thead" [ head ]; of_tag "tbody" tails ]
  in

  let mount () =
    React.E.map on_event events |> ignore;

    let g, ongoing = React.S.value signal in
    assert (g == initial_graph);
    assert (Vset.is_empty ongoing);
    launch_some_tasks (g, ongoing)
  in
  Reactjs.construct ~mount render
