# TODO
- mnist-jsoo soon
  1. Chart with plotly (seed TODO in dedicated file)
  1. fix plotly version
  1. Description of individual stats meaning below chart
  1. in training configuration show
     - how many images and epochs it corresponds to
     - how many images and epochs we will reach


- mnist-jsoo soon (bugs)
   1. Handle the case of corrupted cache/indexeddb.
   1. Aborting training still produce extraneous events (and silent exceptions)


- mnist-jsoo soon (repo)
   1. Split the repo. Keep only the Github Pages resources inside this one (through copy and dune build). Move the source code to another one where the build directory has been removed from all commits.
   1. Update libraries and constrain dependencies (with >=)
      - Update js_of_ocaml for new bigarray conversions
   1. Fix dune build (dev/prod profiles, optimise js_of_ocaml and ocamlc flags even of intermediate files)


- mnist-jsoo display maybe
   - Show time left on training/testing


- mnist-jsoo maybe
   - Find a better pattern than `?signal ?signal ?signal` in `Reactjs`
   - PR or issue `js_of_ocaml ErrorError##.msg -> ErrorError##.message`
   - PR or issue `owl get_slice [first; start] -> [start; stop[`
      - (60000) [[0; 499]] -> (500)
      - (1, 9, 9, 1) [[0;0];[1;6];[1;6];[0;0]] -> (1, 6, 6, 1) // yes, for real
      - (1, 9, 9, 1) [[]   ;[1;6];[1;6];[]   ] -> (1, 7, 7, 1)
   - PR or issue `why not WeakMap in js_of_ocaml`
   - PR or issue `owl Algodiff.Arr.shape should use prime' (Same in Algodiff.?.Softmax, theres an unpack_arr)`
   - PR or issue `owl why is Algodiff's softmax centered??`
   - PR or issue `owl need an unpack_flt'`
   - Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
   - Ensure no mem leaks with Reactml? Why warning in console from Reactjs about a leak
   - Find out why cpu+ww is sometime slow on chrome
   - Use js's WeakMap instead of OCaml's one
