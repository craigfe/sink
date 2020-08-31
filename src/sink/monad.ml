include Monad_intf

module Of_minimal (X : Minimal) = struct
  include X

  let flat_map f x = bind x f
  let kliesli f g x = bind (f x) g
  let join t = flat_map Fun.id t
end
