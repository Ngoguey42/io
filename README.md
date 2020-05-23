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
   - Display
      1. Nada (loading)
      2. Resources
      3. Resources :: tabN :: Network Creation(enabled)
      4. Resources :: tabN :: Network Creation(disabled) :: Chart :: Digits :: Training conf
      5. Resources :: tabN :: Network Creation(disabled) :: Chart :: Digits :: Training conf(disabled) :: Training control buttons
