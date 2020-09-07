include Repr_intf
open! Import

class type ['repr] sym =
  object
    method empty : (empty, 'repr) app

    method unit : (unit, 'repr) app

    method int : (int, 'repr) app

    method nativeint : (nativeint, 'repr) app

    method int32 : (int32, 'repr) app

    method int64 : (int64, 'repr) app

    method bool : (bool, 'repr) app

    method char : (char, 'repr) app

    method string : (string, 'repr) app

    method bytes : (bytes, 'repr) app

    method float : (float, 'repr) app

    method lazy_ : ('a, 'repr) app -> ('a Stdlib.Lazy.t, 'repr) app

    method option : ('a, 'repr) app -> ('a option, 'repr) app

    method either :
      ('a, 'repr) app -> ('b, 'repr) app -> (('a, 'b) either, 'repr) app

    method result :
      ('a, 'repr) app -> ('b, 'repr) app -> (('a, 'b) result, 'repr) app

    method list : ('a, 'repr) app -> ('a list, 'repr) app

    method array : ('a, 'repr) app -> ('a array, 'repr) app

    method pair : ('a, 'repr) app -> ('b, 'repr) app -> ('a * 'b, 'repr) app

    method triple :
      ('a, 'repr) app ->
      ('b, 'repr) app ->
      ('c, 'repr) app ->
      ('a * 'b * 'c, 'repr) app
  end

type 'a t = { interp : 'repr. 'repr sym -> ('a, 'repr) app }
[@@unboxed] [@@deriving branded]

(* let t t = { interp = (fun r -> r#repr t) } *)

let empty = { interp = (fun r -> r#empty) }
let unit = { interp = (fun r -> r#unit) }
let int = { interp = (fun r -> r#int) }
let nativeint = { interp = (fun r -> r#nativeint) }
let int32 = { interp = (fun r -> r#int32) }
let int64 = { interp = (fun r -> r#int64) }
let bool = { interp = (fun r -> r#bool) }
let char = { interp = (fun r -> r#char) }
let string = { interp = (fun r -> r#string) }
let bytes = { interp = (fun r -> r#bytes) }
let float = { interp = (fun r -> r#float) }
let option a = { interp = (fun r -> r#option (a.interp r)) }
let either a b = { interp = (fun r -> r#either (a.interp r) (b.interp r)) }
let result a b = { interp = (fun r -> r#result (a.interp r) (b.interp r)) }
let list a = { interp = (fun r -> r#list (a.interp r)) }
let array a = { interp = (fun r -> r#array (a.interp r)) }
let lazy_ a = { interp = (fun r -> r#lazy_ (a.interp r)) }
let pair a b = { interp = (fun r -> r#pair (a.interp r) (b.interp r)) }

let triple a b c =
  { interp = (fun r -> r#triple (a.interp r) (b.interp r) (c.interp r)) }

type 'br generic = { generic : 'a. 'a t -> ('a, 'br) app } [@@unboxed]

let make : type br. (module S with type br = br) -> br generic =
 fun (module S : S with type br = br) ->
  {
    generic =
      (fun t ->
        let open S in
        let interp =
          object
            method empty = S.inj S.empty

            method unit = S.inj S.unit

            method int = S.inj S.int

            method nativeint = S.inj S.nativeint

            method int32 = S.inj S.int32

            method int64 = S.inj S.int64

            method bool = S.inj S.bool

            method char = S.inj S.char

            method bytes = S.inj S.bytes

            method float = S.inj S.float

            method lazy_ : type a. (a, br) app -> (a Stdlib.Lazy.t, br) app =
              fun elt -> S.inj (S.lazy_ (S.prj elt))

            method string = S.inj S.string

            method option : type a. (a, br) app -> (a option, br) app =
              fun elt ->
                let elt = S.prj elt in
                S.inj (S.option elt)

            method result : type a b.
                (a, br) app -> (b, br) app -> ((a, b) result, br) app =
              fun a b ->
                let a = S.prj a in
                let b = S.prj b in
                S.inj (S.result a b)

            method either : type a b.
                (a, br) app -> (b, br) app -> ((a, b) either, br) app =
              fun a b ->
                let a = S.prj a in
                let b = S.prj b in
                S.inj (S.either a b)

            method list : type a. (a, br) app -> (a list, br) app =
              fun elt ->
                let elt = prj elt in
                inj (S.list elt)

            method array : type a. (a, br) app -> (a array, br) app =
              fun elt ->
                let elt = prj elt in
                inj (S.array elt)

            method pair : type a b.
                (a, br) app -> (b, br) app -> (a * b, br) app =
              fun a b ->
                let a = prj a in
                let b = prj b in
                inj (S.pair a b)

            method triple : type a b c.
                (a, br) app -> (b, br) app -> (c, br) app -> (a * b * c, br) app
                =
              fun a b c ->
                let a = prj a in
                let b = prj b in
                let c = prj c in
                inj (S.triple a b c)
          end
        in
        t.interp interp);
  }

module Pp = struct
  type nonrec _ t = Format.formatter -> unit [@@deriving branded]

  let empty = Fmt.fmt "empty"
  let unit = Fmt.fmt "unit"
  let bool = Fmt.fmt "bool"
  let char = Fmt.fmt "char"
  let string = Fmt.fmt "string"
  let bytes = Fmt.fmt "bytes"
  let float = Fmt.fmt "float"
  let int = Fmt.fmt "int"
  let int32 = Fmt.fmt "int32"
  let int64 = Fmt.fmt "int64"
  let nativeint = Fmt.fmt "nativeint"
  let option elt ppf = Fmt.pf ppf "%t option" elt
  let list elt ppf = Fmt.pf ppf "%t list" elt
  let array elt ppf = Fmt.pf ppf "%t array" elt
  let lazy_ elt ppf = Fmt.pf ppf "%t lazy" elt
  let pair a b ppf = Fmt.pf ppf "(%t * %t)" a b
  let triple a b c ppf = Fmt.pf ppf "(%t * %t * %t)" a b c
  let result a b ppf = Fmt.pf ppf "(%t, %t) result" a b
  let either a b ppf = Fmt.pf ppf "(%t, %t) either" a b
end

let pp_interp = make (module Pp)

let pp : type a. Format.formatter -> a t -> unit =
 fun ppf t -> Pp.prj (pp_interp.generic t) ppf

let to_string : type a. a t -> string =
 fun t -> Fmt.to_to_string (fun ppf () -> Pp.prj (pp_interp.generic t) ppf) ()
