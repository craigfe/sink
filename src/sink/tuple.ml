open Import

(*$
  let alpha i = Char.chr (i + 96)

  let rec interleave ~sep = function
    | ([] | [ _ ]) as l -> l
    | h1 :: (_ :: _ as tl) -> h1 :: sep :: interleave ~sep tl

  type component = { fmap : string; proj : string }

  let component = function
    | 1 -> { fmap = "first "; proj = "p1" }
    | 2 -> { fmap = "second"; proj = "p2" }
    | 3 -> { fmap = "third "; proj = "p3" }
    | 4 -> { fmap = "fourth"; proj = "p4" }
    | 5 -> { fmap = "fifth "; proj = "p5" }
    | _ -> assert false

  let pp_tuple ~typ ?(cs = []) () ppf n =
    let component i =
      match List.assoc_opt i cs with
      | Some s -> s
      | None -> String.make 1 (alpha i)
    in
    (match typ with `Str -> Format.fprintf ppf "(" | `Sig -> ());
    for i = 1 to n - 1 do
      Format.fprintf ppf
        (match typ with `Str -> "%s, " | `Sig -> "'%s * ")
        (component i)
    done;
    Format.fprintf ppf
      (match typ with `Str -> "%s)" | `Sig -> "'%s")
      (component n)

  let print_const ?(cs = []) () ppf n =
    let component i =
      match List.assoc_opt i cs with
      | Some s -> s
      | None -> "'" ^ String.make 1 (alpha i)
    in
    Format.fprintf ppf "(";
    for i = 1 to n - 1 do
      Format.fprintf ppf "%s, " (component i)
    done;
    Format.fprintf ppf "%s) t" (component n)

  let pp_projections ~typ ppf n =
    match typ with
    | `Sig ->
        Format.fprintf ppf
          "  (** Projecting each component from the tuple: *)\n";
        for i = 1 to n do
          let cs =
            List.init n (function
              | j when j + 1 = i -> (j + 1, "'" ^ String.make 1 (alpha (j + 1)))
              | j -> (j + 1, "_"))
          in
          Format.fprintf ppf "  val %s : %a -> '%c\n" (component i).proj
            (print_const ~cs ()) n (alpha i)
        done
    | `Str ->
        for i = 1 to n do
          let cs =
            List.init n (function
              | j when j + 1 = i -> (j + 1, String.make 1 (alpha (j + 1)))
              | j -> (j + 1, "_"))
          in
          Format.fprintf ppf "  let %s %a = %c\n" (component i).proj
            (pp_tuple ~typ:`Str ~cs ())
            n (alpha i)
        done

  let pp_mono ~typ ppf n =
    match typ with
    | `Sig ->
        let xs = List.init n (fun j -> (j + 1, "'a")) in
        let ys = List.init n (fun j -> (j + 1, "'b")) in

        Format.fprintf ppf "  val map : ('a -> 'b) -> %a -> %a\n"
          (print_const ~cs:xs ()) n (print_const ~cs:ys ()) n;

        Format.fprintf ppf "  val equal : ord:'a Ord.t -> %a -> %a -> bool\n"
          (print_const ~cs:xs ()) n (print_const ~cs:xs ()) n;

        Format.fprintf ppf "  val minimum : ord:'a Ord.t -> %a -> 'a\n"
          (print_const ~cs:xs ()) n;

        Format.fprintf ppf
          "  val minimum_on : ord:'b Ord.t -> ('a -> 'b) -> %a -> 'a\n"
          (print_const ~cs:xs ()) n;

        Format.fprintf ppf "  val maximum : ord:'a Ord.t -> %a -> 'a\n"
          (print_const ~cs:xs ()) n;

        Format.fprintf ppf
          "  val maximum_on : ord:'b Ord.t -> ('a -> 'b) -> %a -> 'a\n"
          (print_const ~cs:xs ()) n
    | `Str ->
        Format.fprintf ppf "  let map f %a = %a\n"
          (pp_tuple ~typ:`Str ())
          n
          (pp_tuple ~typ:`Str
             ~cs:
               (List.init n (fun i ->
                    (i + 1, Format.sprintf "f %c" (alpha (i + 1)))))
             ())
          n;

        let cs =
          List.init n (fun j -> (j + 1, Format.sprintf "%c" (alpha (j + 1))))
        in
        let xs =
          List.init n (fun j -> (j + 1, Format.sprintf "%c1" (alpha (j + 1))))
        in
        let ys =
          List.init n (fun j -> (j + 1, Format.sprintf "%c2" (alpha (j + 1))))
        in
        let equals =
          List.init n (fun j ->
              Format.sprintf "ord.equal %c1 %c2" (alpha (j + 1)) (alpha (j + 1)))
          |> String.concat "\n    && "
        in

        Format.fprintf ppf
          "  let equal (type o) ~(ord : o Ord.t) %a %a =\n    %s\n\n"
          (pp_tuple ~typ:`Str ~cs:xs ())
          n
          (pp_tuple ~typ:`Str ~cs:ys ())
          n equals;

        let rec fold_left ~f ppf xs =
          let rec inner ~acc = function
            | [] -> Format.fprintf ppf "    acc"
            | x :: xs ->
                Format.fprintf ppf "    let acc = %s in\n" (f acc x);
                inner ~acc:"acc" xs
          in
          inner ~acc:(List.hd xs) (List.tl xs)
        in

        Format.fprintf ppf
          "  let minimum (type o) ~(ord : o Ord.t) %a =\n%a\n\n"
          (pp_tuple ~typ:`Str ~cs ())
          n
          (fold_left ~f:(Format.sprintf "ord.min %s %s"))
          (List.map snd cs);

        Format.fprintf ppf
          "  let maximum (type o) ~(ord : o Ord.t) %a =\n%a\n\n"
          (pp_tuple ~typ:`Str ~cs ())
          n
          (fold_left ~f:(Format.sprintf "ord.max %s %s"))
          (List.map snd cs);

        let f a b =
          Format.sprintf
            "match ord.compare (f %s) (f %s) with Lt | Eq -> %s | Gt -> %s" a b
            a b
        in
        Format.fprintf ppf
          "  let minimum_on (type o) ~(ord : o Ord.t) f %a =\n%a\n\n"
          (pp_tuple ~typ:`Str ~cs ())
          n (fold_left ~f) (List.map snd cs);

        let f a b =
          Format.sprintf
            "match ord.compare (f %s) (f %s) with Gt | Eq -> %s | Lt -> %s" a b
            a b
        in
        Format.fprintf ppf
          "  let maximum_on (type o) ~(ord : o Ord.t) f %a =\n%a\n\n"
          (pp_tuple ~typ:`Str ~cs ())
          n (fold_left ~f) (List.map snd cs)

  let pp_maps ~typ ppf n =
    match typ with
    | `Sig ->
        Format.fprintf ppf
          "  (** Functor instances on each of the components: *)\n";
        for i = 1 to n do
          Format.fprintf ppf "  val %s : ('%c1 -> '%c2) -> %a -> %a\n"
            (component i).fmap (alpha i) (alpha i)
            (print_const ~cs:[ (i, Format.sprintf "'%c1" (alpha i)) ] ())
            n
            (print_const ~cs:[ (i, Format.sprintf "'%c2" (alpha i)) ] ())
            n
        done
    | `Str ->
        for i = 1 to n do
          Format.fprintf ppf "  let %s f %a = %a\n" (component i).fmap
            (pp_tuple ~typ:`Str ())
            n
            (pp_tuple ~typ:`Str ~cs:[ (i, Format.sprintf "f %c" (alpha i)) ] ())
            n
        done

  let pp_typ ppf n =
    Format.printf "type %a = %a" (print_const ()) n (pp_tuple ~typ:`Sig ()) n

  let pp_sig ppf n =
    Format.fprintf ppf "  %a [@implements Eq.S]\n" pp_typ n;
    Format.fprintf ppf "\n%a" (pp_projections ~typ:`Sig) n;
    Format.fprintf ppf "\n%a" (pp_mono ~typ:`Sig) n;
    Format.fprintf ppf "\n%a" (pp_maps ~typ:`Sig) n;
    ()

  let pp_struct ppf n =
    Format.fprintf ppf "  %a\n" pp_typ n;
    Format.fprintf ppf "\n%a" (pp_projections ~typ:`Str) n;
    Format.fprintf ppf "\n%a" (pp_mono ~typ:`Str) n;
    Format.fprintf ppf "\n%a" (pp_maps ~typ:`Str) n;
    ()

  let print_module n =
    Format.printf "\nmodule T%d : sig\n%a\nend = struct\n%aend\n" n pp_sig n
      pp_struct n

  ;;
  for i = 2 to 5 do
    print_module i
  done
*)
module T2 : sig
  type ('a, 'b) t = ('a * 'b[@implements Eq.S])

  val p1 : ('a, _) t -> 'a
  (** Projecting each component from the tuple: *)

  val p2 : (_, 'b) t -> 'b
  val map : ('a -> 'b) -> ('a, 'a) t -> ('b, 'b) t
  val equal : ord:'a Ord.t -> ('a, 'a) t -> ('a, 'a) t -> bool
  val minimum : ord:'a Ord.t -> ('a, 'a) t -> 'a
  val minimum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a) t -> 'a
  val maximum : ord:'a Ord.t -> ('a, 'a) t -> 'a
  val maximum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a) t -> 'a

  val first : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t
  (** Functor instances on each of the components: *)

  val second : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t
end = struct
  type ('a, 'b) t = 'a * 'b

  let p1 (a, _) = a
  let p2 (_, b) = b
  let map f (a, b) = (f a, f b)

  let equal (type o) ~(ord : o Ord.t) (a1, b1) (a2, b2) =
    ord.equal a1 a2 && ord.equal b1 b2

  let minimum (type o) ~(ord : o Ord.t) (a, b) =
    let acc = ord.min a b in
    acc

  let maximum (type o) ~(ord : o Ord.t) (a, b) =
    let acc = ord.max a b in
    acc

  let minimum_on (type o) ~(ord : o Ord.t) f (a, b) =
    let acc = match ord.compare (f a) (f b) with Lt | Eq -> a | Gt -> b in
    acc

  let maximum_on (type o) ~(ord : o Ord.t) f (a, b) =
    let acc = match ord.compare (f a) (f b) with Gt | Eq -> a | Lt -> b in
    acc

  let first f (a, b) = (f a, b)
  let second f (a, b) = (a, f b)
end

module T3 : sig
  type ('a, 'b, 'c) t = ('a * 'b * 'c[@implements Eq.S])

  val p1 : ('a, _, _) t -> 'a
  (** Projecting each component from the tuple: *)

  val p2 : (_, 'b, _) t -> 'b
  val p3 : (_, _, 'c) t -> 'c
  val map : ('a -> 'b) -> ('a, 'a, 'a) t -> ('b, 'b, 'b) t
  val equal : ord:'a Ord.t -> ('a, 'a, 'a) t -> ('a, 'a, 'a) t -> bool
  val minimum : ord:'a Ord.t -> ('a, 'a, 'a) t -> 'a
  val minimum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a, 'a) t -> 'a
  val maximum : ord:'a Ord.t -> ('a, 'a, 'a) t -> 'a
  val maximum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a, 'a) t -> 'a

  val first : ('a1 -> 'a2) -> ('a1, 'b, 'c) t -> ('a2, 'b, 'c) t
  (** Functor instances on each of the components: *)

  val second : ('b1 -> 'b2) -> ('a, 'b1, 'c) t -> ('a, 'b2, 'c) t
  val third : ('c1 -> 'c2) -> ('a, 'b, 'c1) t -> ('a, 'b, 'c2) t
end = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let p1 (a, _, _) = a
  let p2 (_, b, _) = b
  let p3 (_, _, c) = c
  let map f (a, b, c) = (f a, f b, f c)

  let equal (type o) ~(ord : o Ord.t) (a1, b1, c1) (a2, b2, c2) =
    ord.equal a1 a2 && ord.equal b1 b2 && ord.equal c1 c2

  let minimum (type o) ~(ord : o Ord.t) (a, b, c) =
    let acc = ord.min a b in
    let acc = ord.min acc c in
    acc

  let maximum (type o) ~(ord : o Ord.t) (a, b, c) =
    let acc = ord.max a b in
    let acc = ord.max acc c in
    acc

  let minimum_on (type o) ~(ord : o Ord.t) f (a, b, c) =
    let acc = match ord.compare (f a) (f b) with Lt | Eq -> a | Gt -> b in
    let acc = match ord.compare (f acc) (f c) with Lt | Eq -> acc | Gt -> c in
    acc

  let maximum_on (type o) ~(ord : o Ord.t) f (a, b, c) =
    let acc = match ord.compare (f a) (f b) with Gt | Eq -> a | Lt -> b in
    let acc = match ord.compare (f acc) (f c) with Gt | Eq -> acc | Lt -> c in
    acc

  let first f (a, b, c) = (f a, b, c)
  let second f (a, b, c) = (a, f b, c)
  let third f (a, b, c) = (a, b, f c)
end

module T4 : sig
  type ('a, 'b, 'c, 'd) t = ('a * 'b * 'c * 'd[@implements Eq.S])

  val p1 : ('a, _, _, _) t -> 'a
  (** Projecting each component from the tuple: *)

  val p2 : (_, 'b, _, _) t -> 'b
  val p3 : (_, _, 'c, _) t -> 'c
  val p4 : (_, _, _, 'd) t -> 'd
  val map : ('a -> 'b) -> ('a, 'a, 'a, 'a) t -> ('b, 'b, 'b, 'b) t
  val equal : ord:'a Ord.t -> ('a, 'a, 'a, 'a) t -> ('a, 'a, 'a, 'a) t -> bool
  val minimum : ord:'a Ord.t -> ('a, 'a, 'a, 'a) t -> 'a
  val minimum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a, 'a, 'a) t -> 'a
  val maximum : ord:'a Ord.t -> ('a, 'a, 'a, 'a) t -> 'a
  val maximum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a, 'a, 'a) t -> 'a

  val first : ('a1 -> 'a2) -> ('a1, 'b, 'c, 'd) t -> ('a2, 'b, 'c, 'd) t
  (** Functor instances on each of the components: *)

  val second : ('b1 -> 'b2) -> ('a, 'b1, 'c, 'd) t -> ('a, 'b2, 'c, 'd) t
  val third : ('c1 -> 'c2) -> ('a, 'b, 'c1, 'd) t -> ('a, 'b, 'c2, 'd) t
  val fourth : ('d1 -> 'd2) -> ('a, 'b, 'c, 'd1) t -> ('a, 'b, 'c, 'd2) t
end = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let p1 (a, _, _, _) = a
  let p2 (_, b, _, _) = b
  let p3 (_, _, c, _) = c
  let p4 (_, _, _, d) = d
  let map f (a, b, c, d) = (f a, f b, f c, f d)

  let equal (type o) ~(ord : o Ord.t) (a1, b1, c1, d1) (a2, b2, c2, d2) =
    ord.equal a1 a2 && ord.equal b1 b2 && ord.equal c1 c2 && ord.equal d1 d2

  let minimum (type o) ~(ord : o Ord.t) (a, b, c, d) =
    let acc = ord.min a b in
    let acc = ord.min acc c in
    let acc = ord.min acc d in
    acc

  let maximum (type o) ~(ord : o Ord.t) (a, b, c, d) =
    let acc = ord.max a b in
    let acc = ord.max acc c in
    let acc = ord.max acc d in
    acc

  let minimum_on (type o) ~(ord : o Ord.t) f (a, b, c, d) =
    let acc = match ord.compare (f a) (f b) with Lt | Eq -> a | Gt -> b in
    let acc = match ord.compare (f acc) (f c) with Lt | Eq -> acc | Gt -> c in
    let acc = match ord.compare (f acc) (f d) with Lt | Eq -> acc | Gt -> d in
    acc

  let maximum_on (type o) ~(ord : o Ord.t) f (a, b, c, d) =
    let acc = match ord.compare (f a) (f b) with Gt | Eq -> a | Lt -> b in
    let acc = match ord.compare (f acc) (f c) with Gt | Eq -> acc | Lt -> c in
    let acc = match ord.compare (f acc) (f d) with Gt | Eq -> acc | Lt -> d in
    acc

  let first f (a, b, c, d) = (f a, b, c, d)
  let second f (a, b, c, d) = (a, f b, c, d)
  let third f (a, b, c, d) = (a, b, f c, d)
  let fourth f (a, b, c, d) = (a, b, c, f d)
end

module T5 : sig
  type ('a, 'b, 'c, 'd, 'e) t = ('a * 'b * 'c * 'd * 'e[@implements Eq.S])

  val p1 : ('a, _, _, _, _) t -> 'a
  (** Projecting each component from the tuple: *)

  val p2 : (_, 'b, _, _, _) t -> 'b
  val p3 : (_, _, 'c, _, _) t -> 'c
  val p4 : (_, _, _, 'd, _) t -> 'd
  val p5 : (_, _, _, _, 'e) t -> 'e
  val map : ('a -> 'b) -> ('a, 'a, 'a, 'a, 'a) t -> ('b, 'b, 'b, 'b, 'b) t

  val equal :
    ord:'a Ord.t -> ('a, 'a, 'a, 'a, 'a) t -> ('a, 'a, 'a, 'a, 'a) t -> bool

  val minimum : ord:'a Ord.t -> ('a, 'a, 'a, 'a, 'a) t -> 'a
  val minimum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a, 'a, 'a, 'a) t -> 'a
  val maximum : ord:'a Ord.t -> ('a, 'a, 'a, 'a, 'a) t -> 'a
  val maximum_on : ord:'b Ord.t -> ('a -> 'b) -> ('a, 'a, 'a, 'a, 'a) t -> 'a

  val first : ('a1 -> 'a2) -> ('a1, 'b, 'c, 'd, 'e) t -> ('a2, 'b, 'c, 'd, 'e) t
  (** Functor instances on each of the components: *)

  val second :
    ('b1 -> 'b2) -> ('a, 'b1, 'c, 'd, 'e) t -> ('a, 'b2, 'c, 'd, 'e) t

  val third : ('c1 -> 'c2) -> ('a, 'b, 'c1, 'd, 'e) t -> ('a, 'b, 'c2, 'd, 'e) t

  val fourth :
    ('d1 -> 'd2) -> ('a, 'b, 'c, 'd1, 'e) t -> ('a, 'b, 'c, 'd2, 'e) t

  val fifth : ('e1 -> 'e2) -> ('a, 'b, 'c, 'd, 'e1) t -> ('a, 'b, 'c, 'd, 'e2) t
end = struct
  type ('a, 'b, 'c, 'd, 'e) t = 'a * 'b * 'c * 'd * 'e

  let p1 (a, _, _, _, _) = a
  let p2 (_, b, _, _, _) = b
  let p3 (_, _, c, _, _) = c
  let p4 (_, _, _, d, _) = d
  let p5 (_, _, _, _, e) = e
  let map f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

  let equal (type o) ~(ord : o Ord.t) (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)
      =
    ord.equal a1 a2
    && ord.equal b1 b2
    && ord.equal c1 c2
    && ord.equal d1 d2
    && ord.equal e1 e2

  let minimum (type o) ~(ord : o Ord.t) (a, b, c, d, e) =
    let acc = ord.min a b in
    let acc = ord.min acc c in
    let acc = ord.min acc d in
    let acc = ord.min acc e in
    acc

  let maximum (type o) ~(ord : o Ord.t) (a, b, c, d, e) =
    let acc = ord.max a b in
    let acc = ord.max acc c in
    let acc = ord.max acc d in
    let acc = ord.max acc e in
    acc

  let minimum_on (type o) ~(ord : o Ord.t) f (a, b, c, d, e) =
    let acc = match ord.compare (f a) (f b) with Lt | Eq -> a | Gt -> b in
    let acc = match ord.compare (f acc) (f c) with Lt | Eq -> acc | Gt -> c in
    let acc = match ord.compare (f acc) (f d) with Lt | Eq -> acc | Gt -> d in
    let acc = match ord.compare (f acc) (f e) with Lt | Eq -> acc | Gt -> e in
    acc

  let maximum_on (type o) ~(ord : o Ord.t) f (a, b, c, d, e) =
    let acc = match ord.compare (f a) (f b) with Gt | Eq -> a | Lt -> b in
    let acc = match ord.compare (f acc) (f c) with Gt | Eq -> acc | Lt -> c in
    let acc = match ord.compare (f acc) (f d) with Gt | Eq -> acc | Lt -> d in
    let acc = match ord.compare (f acc) (f e) with Gt | Eq -> acc | Lt -> e in
    acc

  let first f (a, b, c, d, e) = (f a, b, c, d, e)
  let second f (a, b, c, d, e) = (a, f b, c, d, e)
  let third f (a, b, c, d, e) = (a, b, f c, d, e)
  let fourth f (a, b, c, d, e) = (a, b, c, f d, e)
  let fifth f (a, b, c, d, e) = (a, b, c, d, f e)
end

(*$*)
