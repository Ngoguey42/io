module Make (Algodiff : Misc.ALGODIFF) (Nn : Ocann.NETWORK) = struct
  open Bridge_eval.Make (Algodiff) (Nn)

  let unpack_for_evaluation = unpack_for_evaluation

  open Bridge_train.Make (Algodiff) (Nn)

  let unpack_for_training = unpack_for_training

  open Misc.Make (Algodiff) (Nn)

  module OptiMap = OptiMap
end
