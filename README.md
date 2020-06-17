# TODO
- blog
  - Make first post about fnn

- mnist-jsoo soon
  1. collect frp primitives in chart
  1. Chart
     - Face improvements
       - Show somewhere the current amount of smoothing and max training points (and total number of training samples)
       - Description of individual stats and axes meaning below chart
     - Clean code / Improve separation of concerns
     - wontfix
       - Show both epoch count and image count in x-axis hover
  1. fix plotly version


- mnist-jsoo soon (bugs)
   1. Handle the case of corrupted cache/indexeddb.


- mnist-jsoo soon (repo)
   1. Split the repo. Keep only the Github Pages resources inside this one (through copy and dune build --release). Move the source code to another one where the build directory has been removed from all commits (but keep historic!!).
   1. Update OCaml libraries and constrain dependencies (with >=)
      - Update js_of_ocaml for new bigarray conversions
   1. Fix dune build (dev/prod profiles, optimise js_of_ocaml and ocamlc flags even of intermediate files)


- mnist-jsoo display maybe
   - Colorize network code
   - Mouseover the probability in the test-set sample
   - Show time left on training/testing
   - FLOP per forward image
     - in network creation be able to choose flops alongside parameter count (one changes the other)
     - In chart, be able to plot by FLOP too
     - Show current FLOPS alongside time left (need forward+backward FLOP on each layer)
   - Change color in firegrass to use the right red (what about the green?)
   - in training configuration show
     - how many images and epochs it corresponds to
     - how many images and epochs we will reach

- mnist-jsoo maybe
   - PR or issue `js_of_ocaml ErrorError##.msg -> ErrorError##.message`
   - PR or issue `owl get_slice [first; start] -> [start; stop[`
      - (60000) [[0; 499]] -> (500)
      - (1, 9, 9, 1) [[0;0];[1;6];[1;6];[0;0]] -> (1, 6, 6, 1) // yes, for real
      - (1, 9, 9, 1) [[]   ;[1;6];[1;6];[]   ] -> (1, 7, 7, 1)
   - issue `why not WeakMap in js_of_ocaml`
   - PR or issue `owl Algodiff.Arr.shape should use prime' (Same in Algodiff.?.Softmax, theres an unpack_arr)`
   - PR or issue `owl need an unpack_flt'`
   - Find a better pattern than `?signal ?signal ?signal` in `Reactjs`
   - Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
   - Find out why cpu+ww is sometime slow on chrome
   - Use js's WeakMap instead of OCaml's one
   - Add `probabilistic` stats alongside the `top1` ones
