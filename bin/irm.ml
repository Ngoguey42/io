(* module Append_only : Irmin.APPEND_ONLY_STORE_MAKER *)
(* type t = Irmin.config *)
(* module T = Irmin *)
(* module Content = struct *)
(* end *)

module S = Ft.Irmin_mem.KV (Irmin.Contents.String)

(* module Truc: Irmin.S = Irmin_maime.KV(Irmin.Contents.String) *)
(* module Store = Irmin_maime.Make () *)

let info (* Fmt.kstrf *) msg () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = "No one" in
  Irmin.Info.v ~date ~author msg

(* fmt *)

let a =
  let open Lwt.Infix in
  (* let info () = Irmin.Info.v "commit" in *)
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= fun _ ->
  Printf.printf "Get repo and branch\n";
  S.Repo.v (Ft.Irmin_mem.config ()) >>= S.master >>= fun t ->
  Printf.printf "Set commit\n";
  S.set_exn t [ "a" ] "Coucou" ~info:(info "salut") >>= fun () ->
  Printf.printf "Get commit\n";
  S.get t [ "a" ] >|= Printf.printf "%s\n" >>= fun () ->
  Printf.printf "Get repo and branch\n";
  S.Repo.v (Ft.Irmin_mem.config ()) >>= S.master >>= fun t ->
  (* Printf.printf "Set commit\n"; *)
  (* S.set_exn t ["a"] "Coucou" ~info:(info "salut") >>= fun () -> *)
  Printf.printf "Get commit\n";
  S.get t [ "a" ] >|= Printf.printf "%s\n"

(* let () = Lwt_main.run a *)
