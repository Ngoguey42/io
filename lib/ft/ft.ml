
module Irmin_mem = Irmin_mem

module List = struct
  let rec string_join sep = function
    | a::(_::_ as tail) -> a ^ sep ^ string_join sep tail
    | [a] -> a
    | [] -> ""
end
