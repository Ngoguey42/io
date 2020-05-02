include Training_types
open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

let[@ocamlformat "disable"] create_backend : backend -> (module TRAINER) = function
  (* | `Owl_cpu -> (module Mnist_owl) *)
  | `Tfjs_webgl -> (module Mnist_tfjs.Make_backend (struct let v = `Webgl end))
  | `Tfjs_cpu -> (module Mnist_tfjs.Make_backend (struct let v = `Cpu end))
  | `Tfjs_wasm -> (module Mnist_tfjs.Make_backend (struct let v = `Wasm end))

let routine {db = (train_imgs, train_labs, _, _); networks=(encoders, decoder); config}
            fire_event instructions =
  let module Backend = (val create_backend config.backend) in
  let verbose = config.verbose in
  let rng = Random.State.make [| config.seed |] in
  let batch_count, batch_size = (config.batch_count, config.batch_size) in
  let get_lr =
    match config.lr with
    | `Down (lr0, lr1) ->
        assert (0. <= lr1);
        assert (lr1 <= lr0);
        let lr2 = lr0 -. lr1 in
        fun i ->
          let ratio = float_of_int i /. float_of_int batch_count in
          lr1 +. (lr2 *. (1. -. ratio))
  in
  let get_data _ =
    let indices = Array.init batch_size (fun _ -> Random.State.int rng 60000) in
    let imgs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_imgs i 1) indices
      |> Ndarray.concatenate ~axis:0
    in
    let labs =
      Array.map (fun i -> Bigarray.Genarray.sub_left train_labs i 1) indices
      |> Ndarray.concatenate ~axis:0
    in
    (imgs, labs)
  in
  Backend.train ~fire_event ~instructions ~verbose ~batch_count ~get_lr ~get_data ~encoders ~decoder

type msg = [ `Prime of training_parameters | user_status ]
type msg' = routine_event
module rec Webworker_routine : (Webworker.S with type in_msg := msg and type out_msg := msg') =
Webworker.Make (struct
  type in_msg = msg

  type out_msg = msg'

  let create_on_in_message () =
    (* let open Lwt.Infix in *)
    let routine_events, fire_routine_event = React.E.create () in
    let user_status, set_user_status = React.S.create `Train_to_end in

    React.E.map Webworker_routine.post_out_message routine_events |> ignore;

    let on_in_message = function
      | `Prime params ->
         Ft_js.Scripts.import_sync `Tfjs;
         let routine () = routine params fire_routine_event user_status in
         Js_of_ocaml_lwt.Lwt_js_events.async routine
      | #user_status as msg -> set_user_status msg
    in
    on_in_message



(*     let fire_event : 'a -> 'b = fire_event in *)
(*     let next_event = Lwt_react.E.next events in *)

(*     let routine () = *)

(*       Ft_js.Scripts.import `Tfjs >>= fun () -> *)
(*       Printf.eprintf "> Worker-routine : imported tfjs \n%!"; *)
(*       Ft_js.Scripts.import `Cryptojs >>= fun () -> *)
(*       Printf.eprintf "> Worker-routine : imported cryp\n%!"; *)
(*       Ft_js.Scripts.import `Pako >>= fun () -> *)
(*       Printf.eprintf "> Worker-routine : imported pako\n%!"; *)
(*       Ft_js.Scripts.import `Reactjs >>= fun () -> *)
(*       Printf.eprintf "> Worker-routine : imported react\n%!"; *)

(*       next_event >>= fun _ev -> *)
(*       Printf.eprintf "> Worker-routine : on message async\n%!"; *)
(*       Webworker_routine.post_out_message "coucou"; *)

(*       Lwt.return () *)
(*     in *)
(*     Lwt_js_events.async routine; *)

(*     (\* let on_in_msg msg = *\) *)
(*     (\*   Printf.eprintf "> Worker-routine : on message \n%!"; *\) *)
(*     (\*   fire_event msg *\) *)
(*     (\* in *\) *)
(*     (\* on_in_msg *\) *)
(*     fire_event *)

end)

let render_instructions props =
  let user_status, routine_status, fire_user_event = props in
  let open Reactjs.Jsx in
  let button ?event ?(style = []) txt =
    match event with
    | None -> of_tag "button" ~style ~disabled:true [ of_string txt ]
    | Some event ->
        of_tag "button" ~style ~on_click:(fun _ -> fire_user_event event) [ of_string txt ]
  in
  let green =
    [
      ("color", "green");
      ("borderColor", "transparent");
      ("backgroundColor", "transparent");
      ("fontWeight", "bold");
    ]
  in
  let gray = [ ("borderColor", "transparent"); ("backgroundColor", "transparent") ] in
  match (user_status, routine_status) with
  | `Train_to_end, `Running ->
      of_tag "div"
        [
          button ~style:green "Train to end";
          of_string " | ";
          button ~event:`Early_stop "Early stop";
          of_string " | ";
          button ~event:`Abort "Abort";
        ]
  | `Train_to_end, _ ->
      of_tag "div"
        [
          button ~style:green "Train to end";
          of_string " | ";
          button ~style:gray "Early stop";
          of_string " | ";
          button ~style:gray "Abort";
        ]
  | `Early_stop, _ ->
      of_tag "div"
        [
          button ~style:gray "Train to end";
          of_string " | ";
          button ~style:green "Early stop";
          of_string " | ";
          button ~style:gray "Abort";
        ]
  | `Abort, _ ->
      of_tag "div"
        [
          button ~style:gray "Train to end";
          of_string " | ";
          button ~style:gray "Early stop";
          of_string " | ";
          button ~style:green "Abort";
        ]

type props = training_parameters * (outcome -> unit)

let construct (props : props) =
  let params, on_completion = props in

  let routine_events, fire_routine_event = React.E.create () in
  let routine_progress =
    React.S.fold (fun i a -> match a with `Batch_end (i, _) -> i + 1 | _ -> i) 0 routine_events
  in
  let routine_status =
    let reduce s ev =
      match (s, ev) with
      | `Running, `End _ ->
          on_completion `End;
          `Ended
      | `Running, `Abort ->
          on_completion `Abort;
          `Aborted
      | `Running, `Crash exn ->
          on_completion (`Crash exn);
          `Crashed
      | _, _ -> s
    in
    React.S.fold reduce `Running routine_events
  in

  let user_events, fire_user_event = React.E.create () in
  let fire_user_event : user_event -> unit = fire_user_event in
  let user_status =
    let reduce s ev =
      match (s, ev) with
      | `Train_to_end, `Early_stop -> `Early_stop
      | `Train_to_end, `Abort -> `Abort
      | _, `Early_stop | _, `Abort -> s
    in
    React.S.fold reduce `Train_to_end user_events
  in

  let render _ =
    let open Reactjs.Jsx in
    let routine_status = React.S.value routine_status in
    let routine_progress = React.S.value routine_progress in
    let user_status = React.S.value user_status in

    of_tag "div"
      [
        of_render render_instructions (user_status, routine_status, fire_user_event);
        of_tag "div"
          [
            of_string "Routine: ";
            ( match routine_status with
            | `Running -> "Running"
            | `Ended -> "Ended"
            | `Aborted -> "Aborted"
            | `Crashed -> "Crashed" )
            |> of_string;
          ];
        of_tag "div"
          [
            of_string "Progress: ";
            (routine_progress |> float_of_int) /. (params.config.batch_count |> float_of_int) *. 100.
            |> Printf.sprintf "%.0f%%" |> of_string;
          ];
      ]
  in
  let mount () =
    let ww = Webworker_routine.create fire_routine_event (fun _err ->
                                       Printf.eprintf "Webworker err\n%!"
                                      ) in
    React.S.changes user_status
    |> React.E.map (fun s -> Webworker_routine.post_in_message ww (s :> msg))
    |> ignore;
    Webworker_routine.post_in_message ww (React.S.value user_status :> msg);
    Webworker_routine.post_in_message ww (`Prime params);

    (* let test : 'a -> 'a  = fun v -> *)
    (* (\* let test : type a. a -> a  = fun v -> *\) *)
    (*   Printf.printf "test on\n%!"; *)
    (*   Firebug.console##log (snd v); *)
    (*   let v = Js_of_ocaml.Json.output v in *)
    (*   Js_of_ocaml.Json.unsafe_input v *)

    (*   (\* { v with *\) *)
    (*   (\*   config = ( *\) *)
    (*   (\*   Printf.eprintf "aa\n%!"; *\) *)
    (*   (\*   let v = Js_of_ocaml.Json.output v.config in *\) *)
    (*   (\*   Firebug.console##log v; *\) *)
    (*   (\*   (\\* let v = Marshal.to_string v.config [] in *\\) *\) *)
    (*   (\*   Printf.eprintf "bb\n%!"; *\) *)
    (*   (\*   (\\* Marshal.from_string v 0 *\\) *\) *)
    (*   (\*   Js_of_ocaml.Json.unsafe_input v *\) *)
    (*   (\*   ) *\) *)
    (*   (\* } *\) *)

    (* in *)
    (* Printf.eprintf "a\n%!"; *)
    (* let params = { *)
    (*     params with *)
    (*     (\* db = test params.db; *\) *)
    (*     (\* config = test params.config; *\) *)
    (*     networks = test params.networks; *)
    (*   } in *)
    (* Printf.eprintf "b\n%!"; *)
    (* Firebug.console##log (snd params.networks); *)

    (* let routine () = routine params fire_routine_event user_status in *)
    (* Js_of_ocaml_lwt.Lwt_js_events.async routine *)

  in
  Reactjs.construct ~mount ~signal:routine_progress ~signal:user_status ~signal:routine_status
    render

(* class lol = *)
(* object *)
(*   method tamere = 42 *)
(* end [@@deriving yojson] *)

(* type lol = int [@@deriving yojson] *)


(* (\* let test : 'a -> 'a  = fun v -> *\) *)
(* (\*   (\\* let test : type a. a -> a  = fun v -> *\\) *\) *)
(* (\*   let v = Js_of_ocaml.Json.output v in *\) *)
(* (\*   Js_of_ocaml.Json.unsafe_input v *\) *)

(* let test' : 'a -> 'a = fun v -> *)
(*   Printf.eprintf "\n%!"; *)
(*   Firebug.console##log v; *)
(*   let v = Marshal.to_string v [Marshal.Closures] in *)
(*   Firebug.console##log v; *)
(*   let v = Marshal.from_string v 0 in *)
(*   Firebug.console##log v; *)
(*   v *)

(* let () = *)
(*   print_endline lol_of_yojson; *)
(*   ignore (test' 42); *)
(*   ignore (test' 10.); *)
(*   ignore (test' "salut"); *)
(*   ignore (test' `Test); *)
(*   (\* ignore (test' (new lol)); *\) *)
