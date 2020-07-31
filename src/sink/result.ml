open Import

type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e
[@@deriving branded]

let map f = function Ok o -> Ok (f o) | Error _ as e -> e
let return x = Ok x
let bind x f = match x with Ok o -> f o | Error _ as e -> e
let join = function Error _ as e -> e | Ok x -> x
let kliesli f g x = bind (f x) g
let errorf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

module Infix = struct
  let ( >>= ) = bind
  let ( >>| ) x f = map f x
  let ( >=> ) f g x = bind (f x) g
end

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end
