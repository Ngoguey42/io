# TODO
- Fix dune build (dev/prod profiles, optimise js_of_ocaml and ocamlc flags even of intermediate files, ocamlformat)
- Update js_of_ocaml for new bigarray conversions
- Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
- Handle the case of corrupted cache/indexeddb
- constrain lib dependencies (with >=)
- Find a better pattern than `?signal ?signal ?signal` in `Reactjs`
- MNIST js
   1. Training configuration panel
   2. Eval at the beginning and after each successful training
      - Get stats and 10 ref test digits
   3. Stats chart with plotly
   4. Multiple network bootstrap-tabs that share the plot axes and traces visibility
      - Gif that tells if a tab is computing something
   - Foldable panels?

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

# 4 - picking evaluation backend
- Resources
- tabN
   - Network Creation(off)
   - Results
      - Evaluation Backend(on)

# 5 - evaluating #0
- Resources
- tabN
   - Network Creation(off)
   - Results
      - Evaluation Backend(off)

# 6 - configuring training
- Resources
- tabN
   - Network Creation(off)
   - Results
      - Evaluation Backend(on)
      - Charts
      - Digits
   - Training Conf(on)

# 7 - training/eval #1
- Resources
- tabN
   - Network Creation(off)
   - Results
      - Evaluation Backend(off)
      - Charts
      - Digits
   - Training Conf(off)
   - Routine control buttons

# 8 - back to #6

```
