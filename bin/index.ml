
open Tyxml

let () =
  let to_ocaml = Html.(a ~a:[a_href "ocaml.org"] [txt "monlien"]) in
  let s = Format.asprintf "%a" (Html.pp_elt ()) to_ocaml in
  print_endline s;
  print_endline "Hello, World!"


  (* Lwt_main.run (Lwt_io.printf "Hello, again!\n") *)
