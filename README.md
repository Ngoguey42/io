# TODO
- Fix dune build (dev/prod profiles, optimise js_of_ocaml and ocamlc flags even of intermediate files)
- Update js_of_ocaml for new bigarray conversions
- Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
- Handle the case of corrupted cache/indexeddb
- constrain lib dependencies (with >=)
- Finish `Making-of` as soon as MNIST-js is finished
- MNIST js
   1. Eval before first training and after each successful training
      - Get stats and 10 ref test digits
   2. Stats chart with plotly
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
   3. Use `images_seen` in conjunction with `seed` to sample the images
   4. Make sure errors (like memory) don't silently crash the page (either recover or alert about crashed page)
   5. Repair on firefox
   - Foldable panels?
   - Auto scroll at bottom of page?
   - OWL backend?
   - Replicate `Nav` at bottom of the page?
   - Shut down the webworker on abort?
   - Some tips at the very bottom? (and theoritical results to reproduce)
      - Cyclical LR yields good results
      - Batch-size changes the result
         - BS=60000 yields poor results (GD vs SGD)
         - BS=1 yields poor results
         - What about the BS/2 BC*2 iterated strategy?
      - Larger networks overfit more but are harder to train (really?). Using max-pool in decoder yields less overfited results
      - Different initial seeds rougly yield the same results
      - Hint on how to reach good performances?
- Find a better pattern than `?signal ?signal ?signal` in `Reactjs`?

```
MNIST js - display
# 1 - loading
nada

# 2 - loading resources
- Resources

# 3 - creating network
- Resources
- tabN
   - Network Creation(on)

# 4 - picking backend
- Resources
- tabN
   - Network Creation(off)
   - Backend Selection(on)
   - Results

# 5 - evaluating #0
- Resources
- tabN
   - Network Creation(off)
   - Evaluation Backend(off)
   - Results

# 6 - configuring training
- Resources
- tabN
   - Network Creation(off)
   - Backend Selection(on)
   - Results
      - Charts
      - Digits
   - Training Conf(on)

# 7 - training/eval #1
- Resources
- tabN
   - Network Creation(off)
   - Evaluation Backend(off)
   - Results
      - Charts
      - Digits
   - Training Conf(off)
   - Routine control buttons

# 8 - back to #6

```
