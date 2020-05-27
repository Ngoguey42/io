open struct
  module Reactjs = Ft_js.Reactjs
  module Firebug = Js_of_ocaml.Firebug
  module Js = Js_of_ocaml.Js
  module Ndarray = Owl_base_dense_ndarray_generic
  module Webworker = Ft_js.Webworker
end

open Types

let routine params fire_event =
  ignore params;
  ignore fire_event;
  Printf.eprintf "Firing dummy Evaluation `End event\n%!";
  `End
    {
      confusion_matrix = Bigarray.Genarray.create Bigarray.Int32 Bigarray.c_layout [| 10; 10 |];
      marked_digits_probas =
        Bigarray.Genarray.create Bigarray.Float32 Bigarray.c_layout [| 10; 10 |];
    }
  |> fire_event;
  Lwt.return ()
