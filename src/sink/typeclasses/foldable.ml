include Foldable_intf

module Make_default (X : MINIMAL) : S with type 'a t := 'a X.t = struct
  let fold_left = X.fold_left

  let fold (type m) ({ append; empty } : m Monoid.t) = fold_left append empty

  (* let fold_right =  *)

  let null l = fold_left (fun _ _ -> false) true l

  let mem (type a) ({ equal } : a Eq.t) elt =
    fold_left (fun a x -> a || equal x elt) false

  let sum _ = failwith "TODO"

  let product _ = failwith "TODO"

  let minimum _ = failwith "TODO"

  let maximum _ = failwith "TODO"

  let fold_right _ = failwith "TODO"

  let length _ = failwith "TODO"
end
