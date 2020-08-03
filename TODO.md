
--------------------------------------------------------------------------------

- maybe
   - PR or issue `owl get_slice [first; start] -> [start; stop[`
      - (60000) [[0; 499]] -> (500)
      - (1, 9, 9, 1) [[0;0];[1;6];[1;6];[0;0]] -> (1, 6, 6, 1) // yes, for real
      - (1, 9, 9, 1) [[]   ;[1;6];[1;6];[]   ] -> (1, 7, 7, 1)
   - PR or issue `owl Algodiff.Arr.shape should use prime' (Same in Algodiff.?.Softmax, theres an unpack_arr)`
   - Find a better pattern than `?signal ?signal ?signal` in `Reactjs`
   - Download all js/css in parallel and reorder when inserting in <head>
   - Find a better syntax coloring lib (Use compiler libs? Might be too unstable)
   - Make articles standalone using module interfaces
   - RSS flux (part of the deployment process when 2 repos?)
   - Switch to release versions of js libs (like react) on profile release
   - Need to statically distribute some(all) pages to allow anchors
   - How to get referenced? (dynamic website)
--------------------------------------------------------------------------------

- mnist-jsoo display maybe
   - Colorize network code
   - Mouseover the probability in the test-set sample
   - Show time left on training/testing
   - FLOP per forward image
     - in network creation be able to choose flops alongside parameter count (one changes the other)
     - In chart, be able to plot by FLOP too
     - Show current FLOPS alongside time left (need forward+backward FLOP on each layer)
   - Change color in firegrass to use the website's red (what about the green?)
   - in training configuration show
     - how many images and epochs it corresponds to
     - how many images and epochs we will reach
   - Show both epoch count and image count in x-axis hover

--------------------------------------------------------------------------------

- mnist-jsoo maybe
   - Ensure good propagation of exceptions throughout reactml/reactjs/webworker/lwt
   - Find out why cpu+ww is sometime slow on chrome
   - Add `probabilistic` stats alongside the `top1` ones (less nans in those)
   - Handle the case of corrupted cache/indexeddb.
