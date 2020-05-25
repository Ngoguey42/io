# TODO
- Fix dune build (dev/prod profiles, optimise js_of_ocaml and ocamlc flags even of intermediate files, ocamlformat)
- Update js_of_ocaml for new bigarray conversions
- Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
- Handle the case of corrupted cache/indexeddb
- constrain lib dependencies (with >=)
- Find a better pattern than `?signal ?signal ?signal` in `Reactjs`
- MNIST js
   1. Eval before first training and after each successful training
      - Get stats and 10 ref test digits
   2. Stats chart with plotly
      - Live update
      - Share the plot axes and traces visibility
      - Choose between X=images-seen / X=batch-idx
   - Foldable panels?
   - Auto scroll at bottom of page?
   - OWL backend?
   - Shut down the webworker on abort?
   - Normalize CSS of training-control buttons

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
