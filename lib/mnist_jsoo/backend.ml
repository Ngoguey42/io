open Types

let[@ocamlformat "disable"] create : backend -> (module BACKEND) = function
  | `Tfjs_webgl ->
     if not (Ocann_tfjs.Tfjs.has_webgl ()) then (
       if Ft_js.Webworker.is_web_worker then
         failwith "Tensorflow failed to initialize WebGL from webworker"
       else
         failwith "Tensorflow failed to initialize WebGL"
     )
     else
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
  | `Owl_algodiff_cpu ->
     (module struct
        include Training_owl_cpu
        include Evaluation_owl_cpu
      end)
