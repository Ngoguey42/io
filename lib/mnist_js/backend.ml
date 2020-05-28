open Types

let[@ocamlformat "disable"] create : backend -> (module BACKEND) = function
  | `Tfjs_webgl ->
     (module struct
        include Training_tfjs.Make_backend (struct let v = `Webgl end)
        include Evaluation_tfjs.Make_backend (struct let v = `Webgl end)
      end)
  | `Tfjs_cpu ->
     (module struct
        include Training_tfjs.Make_backend (struct let v = `Cpu end)
        include Evaluation_tfjs.Make_backend (struct let v = `Cpu end)
      end)
  | `Tfjs_wasm ->
     (module struct
        include Training_tfjs.Make_backend (struct let v = `Wasm end)
        include Evaluation_tfjs.Make_backend (struct let v = `Wasm end)
      end)
