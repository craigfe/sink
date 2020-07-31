let ( >> ) f g x = g (f x)

module type S = sig
  type 'a t

  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t [@@infix ( <*> )]
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val seq : 'a t -> 'b t -> 'b t [@@infix ( *> )]
end
[@@deriving typeclass, infix]

module type S1 = S

module Of_monad (M : Monad.S1) : S1 with type 'a t := 'a M.t = struct
  open M

  let pure = return
  let apply ft at = bind ft (fun f -> bind at (f >> return))
  let lift2 f xt yt = bind xt (fun x -> bind yt (f x >> return))
  let seq xt yt = bind xt (fun _ -> yt)
end
