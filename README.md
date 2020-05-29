# TODO
- Fix dune build (dev/prod profiles, optimise js_of_ocaml and ocamlc flags even of intermediate files)
- Update js_of_ocaml for new bigarray conversions
- Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
- Handle the case of corrupted cache/indexeddb
- constrain lib dependencies (with >=)
- Finish `Making-of` as soon as MNIST-js is finished
- MNIST js
   1. Stats chart with plotly
      - Live update
         - Debounce display every (max 1. (render length * 10)) seconds
         - Store stats in a data structure with a good append complexity
      - Share the plot axes and traces visibility
         - Choose between X=images-seen / X=batch-idx
      - Stats
         - IOU (train/test)
         - Recall (train/test)
         - Loss
         - Gradient Norm Sum in decoder (what about bias, what about LR?, only use loss?)
   3. Display popup on crash
   4. Fix determinism WW vs no-WW
   4. Find out why cpu+ww is sometime slow on chrome
   5. OWL backend
   6. Shut down the webworker on abort
   - time left on training?
   - Foldable panels?
   - Auto scroll at bottom of page?
   - Replicate `Nav` at bottom of the page?
   - Some tips at the very bottom? (and theoritical results to reproduce)
      - Markdown with render engine?
      - Cyclical LR yields good results
      - Batch-size changes the result
         - BS=60000 yields poor results (GD vs SGD)
         - BS=1 yields poor results
         - What about the BS/2 BC*2 iterated strategy?
      - Larger networks overfit more but are harder to train (really?). Using max-pool in decoder yields less overfited results
      - Different initial seeds rougly yield the same results
      - Hint on how to reach good performances?
- Find a better pattern than `?signal ?signal ?signal` in `Reactjs`?
- PR or issue `js_of_ocaml ErrorError##.msg -> ErrorError##.message`
- PR or issue `owl get_slice [first; start] -> [start; stop[`
