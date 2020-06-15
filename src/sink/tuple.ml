open Import

module T2 = struct
  type ('a, 'b) t = 'a * 'b

  let equal a b (a1, b1) (a2, b2) = a.Eq.equal a1 a2 && b.Eq.equal b1 b2

  let first f (a, b) = (f a, b)

  let second f (a, b) = (a, f b)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let equal a b c (a1, b1, c1) (a2, b2, c2) =
    a.Eq.equal a1 a2 && b.Eq.equal b1 b2 && c.Eq.equal c1 c2

  let first f (a, b, c) = (f a, b, c)

  let second f (a, b, c) = (a, f b, c)

  let third f (a, b, c) = (a, b, f c)
end

module T4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let equal a b c d (a1, b1, c1, d1) (a2, b2, c2, d2) =
    a.Eq.equal a1 a2 && b.Eq.equal b1 b2 && c.Eq.equal c1 c2 && d.Eq.equal d1 d2

  let first f (a, b, c, d) = (f a, b, c, d)

  let second f (a, b, c, d) = (a, f b, c, d)

  let third f (a, b, c, d) = (a, b, f c, d)

  let fourth f (a, b, c, d) = (a, b, c, f d)
end
