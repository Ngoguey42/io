(*
  `Misc.ALGODIFF` constrains the type of `Owl_algodiff_generic_sig.Sig`:

  **********

  Thanks to this constaint:
  ```
  Owl_algodiff_generic_sig.Sig.A.elt is abstact
                 Misc.ALGODIFF.A.elt == float
  ```
  we can explicitly manipulate float inside the binding. Complex numbers are not handled.

  **********

  Thanks to this constaint:
  ```
  Owl_algodiff_generic_sig.Sig.A.arr is abstact
                 Misc.ALGODIFF.A.arr == Misc.ALGODIFF.ba
                    Misc.ALGODIFF.ba == (_, Misc.ALGODIFF.ba_elt, _) Bigarray.Genarray.t
                Misc.ALGODIFF.ba_elt is abstract
  ```
  we can perform this conversion:
  `(_, _) Nn.Tensor.t => (_, _, _) Bigarray.Genarray.t => Algodiff.A.arr`
  when unpacking, and this one:
  `Algodiff.A.arr => (_, _, _) Bigarray.Genarray.t => (_, _) Nn.Tensor.t`
  when packing. In case of mismatch between element type in conversion, an error is raised

 *)
module Make (Algodiff : Misc.ALGODIFF) (Nn : Ocann.NETWORK) = struct
  open Bridge_eval.Make (Algodiff) (Nn)

  let unpack_for_evaluation = unpack_for_evaluation

  open Bridge_train.Make (Algodiff) (Nn)

  let unpack_for_training = unpack_for_training

  open Misc.Make (Algodiff) (Nn)

  module OptiMap = OptiMap
end
