# TODO
- mnist-jsoo soon
   1. Stats chart with plotly
      - Live update
         - Debounce chart update given idleness of window
         - Store stats in `Js.js_array` directly for good complexity
      - Share the plot axes and traces visibility between tabs
         - Choose between X=images-seen / X=batch-idx
      - Stats
         - IOU Recall Precision (train/test)
         - Loss (train)
         - LR
         - Gradient Norm Sum in decoder? (what about bias, what about LR?, only use loss?)
   1. Handle the case of corrupted cache/indexeddb.
   1. Update libraries and constraint dependencies (with >=)
      - Update js_of_ocaml for new bigarray conversions
   1. Fix dune build (dev/prod profiles, optimise js_of_ocaml and ocamlc flags even of intermediate files)
   1. OWL backend
   1. Split the repo. Keep only the Github Pages resources inside this one (through copy and dune build). Move the source code to another one where the build directory has been removed from all commits.
   1. Fix the `Uncaught TypeError: Cannot read property '__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED' of undefined` `TypeError: Js_of_ocaml_Js[50][1].ReactDOM.render is not a function` error

- mnist-jsoo maybe
   - Find a better pattern than `?signal ?signal ?signal` in `Reactjs`
   - PR or issue `js_of_ocaml ErrorError##.msg -> ErrorError##.message`
   - PR or issue `owl get_slice [first; start] -> [start; stop[`
   - PR or issue `why not WeakMap in js_of_ocaml`
   - Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
   - Ensure no mem leaks with Reactml? Why warning in console from Reactjs about leak
   - Find out why cpu+ww is sometime slow on chrome
   - Use js's WeakMap instead of OCaml's one

- mnist-jsoo display maybe
   - Show time left on training
   - Foldable panels
   - Auto scroll at bottom of page
   - Replicate `Nav` at bottom of the page
   - Some tips at the very bottom (and theoritical results to reproduce)
      - Markdown with render engine
      - Cyclical LR yields good results (todo: Check)
      - Batch-size changes the result
         - BS=60000 yields poor results (GD vs SGD)
         - BS=1 yields poor results
         - What about the BS/2 BC*2 iterated strategy
      - Maxpool decoder yields less overfitted results, but requires a larger and deeper network. (todo: Check)
      - Larger and deeper networks are easier to train with adam. smaller are easier with SGD (todo: Check)
      - Different initial seeds rougly yield the same results (todo: Check)
      - Adam hates varying learning rates (todo: Check)
      - Hint on how to reach good performances
