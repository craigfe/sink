let ( >> ) f g x = g (f x)

module type Generic = sig
  type (+'a, 'p) t

  val pure : 'a -> ('a, 'p) t
  val apply : ('a -> 'b, 'p) t -> ('a, 'p) t -> ('b, 'p) t [@@infix ( <*> )]
  val lift2 : ('a -> 'b -> 'c) -> ('a, 'p) t -> ('b, 'p) t -> ('c, 'p) t
  val seq : ('a, 'p) t -> ('b, 'p) t -> ('b, 'p) t [@@infix ( *> )]
end
[@@deriving typeclass, infix]

module type S1 = sig
  type +'a t

  include Generic with type ('a, _) t := 'a t
  (** @inline *)
end

module Of_monad (M : Monad.Minimal) :
  Generic with type ('a, 'p) t := ('a, 'p) M.t = struct
  open M

  let pure = return
  let apply ft at = bind ft (fun f -> bind at (f >> return))
  let lift2 f xt yt = bind xt (fun x -> bind yt (f x >> return))
  let seq xt yt = bind xt (fun _ -> yt)
end
