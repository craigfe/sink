include Num_intf

module Make_infix (N : S) = struct
  let ( + ) = N.add
  let ( - ) = N.subtract
  let ( * ) = N.multiply
end
