module Tfjs = Tfjs_api

module Make (Nn : Ocann.NETWORK) = struct
  open Bridge_eval.Make (Nn)

  let unpack_for_evaluation = unpack_for_evaluation

  open Bridge_train.Make (Nn)

  let unpack_for_training = unpack_for_training

  open Misc.Make (Nn)

  module OptiMap = OptiMap
end
