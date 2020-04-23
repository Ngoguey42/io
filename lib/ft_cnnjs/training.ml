include Training_types

let[@ocamlformat "disable"] get_backend : backend -> (module TRAINER) = function
  | `Tfjs_webgl -> (module Mnist_tfjs.Make_backend (struct let v = `Webgl end))
  | `Tfjs_cpu -> (module Mnist_tfjs.Make_backend (struct let v = `Cpu end))

(* | `Owl_cpu -> (module Mnist_owl) *)
