module type T = sig
  type t
end

module type T1 = sig
  type 'a t
end

module type T2 = sig
  type ('a, 'b) t
end

module type T3 = sig
  type ('a, 'b, 'c) t
end
