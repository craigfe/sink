# Sink – everything and the kitchen sink

<p align="center">(under development)</p>

An OCaml standard library replacement intended for personal use in other
projects. Explores some design space not seen in other OCaml standard libraries.

__Can I use it?__ Of course.

__Should I use it?__ Almost certainly not.

<hr/>

<details>
<summary>Vague design principles: (click to unfold)</summary>

- __Consistent interfaces__. Very similar to the approach taken in [base], type
  interfaces are constructed using a combination of functors and PPX to ensure
  consistency.

- __Higher-kinded polymorphism via [brands][yallop14]__. All higher-kinded types
  `'a t` come alongside a corresponding _brand_ `br` that can be used to
  specialise functions that are polymorphic over _all_ higher-kinded types. With
  some squinting, this enables an OCaml equivalent of the standard Haskell
  typeclass hierarchy (`Monoid`, `Semigroup`, `Functor`, `Monad` etc.).

- __Generic programming__. All higher-kinded types `t` come alonside a _value_
  `t` that is a run-time representation of the type. These representations can
  be used to derive operations on the corresponding types, if you're (_a_)
  allergic to boilerplate, (_b_) can't afford a PPX dependency and (_c_) don't
  care about performance.

- __Function-level programming permitted__. Use of point-free style in OCaml
  code has been somewhat contentious, but I find it useful occasionally.
  Function composition is provided as `( >> )`, function-level monadic
  composition as `( >=> )` etc.
  
- __Name-spaced operators__. Some modules provide `Infix` and `Syntax`
  submodules that are intended to be opened either locally or globally. For
  instance, there are many ways to get at the `bind` operation on lists:

|                | `List`     | `List.Infix` | `List.Syntax`  |
| -------------- |:----------:|:------------:|:--------------:|
| Value-level    | `bind`     | `( >>= )`    | `( let* )`     |
| Function-level | `kliesli`  | `( >=> )`    | —              |

- __Composable error values__.

- __Different datastructure views belong in different namespaces__. For
  instance, viewing an `'a array array` as a _matrix_ (or `('k * 'v) list` as an
  association list) belongs in a different namespace from the main datastructure
  (e.g. `Array.Matrix` or `List.Assoc` respecitvely).

- __Dependencies à la carte__. OCaml library developers have limited solutions
  for isolating their users from library dependency choices. (At time of
  writing, my system contains 38 Opam switches with transitive dependencies on
  [`Base`][js-base].) For lack of a better solution, Sink is intended to be
  vendored, taking advantage of Dune's excellent composability. In Mirage style,
  extra dependencies such as [`Lwt`][lwt], [`Alcotest`][alcotest], and
  [`Pp`][pp] can be opted into explicitly.

<hr/>

If you don't care about any of the above, you probably want the OCaml standard
library instead. Some non-goals of this project:

- __~~Speed~~__. e.g. functions over lists are tail-recursive to minimise
  surprisal.

- __~~Exotic data-structures~~__. Ring buffers, indices, B-trees, tries etc. to
  be found elsewhere. If you want one of these, you probably care about its
  performance (and this library is not about performance). Try
  [`Containers`][containers].

As well as a few minor things:

- All sequencing is _left-to-right_ (e.g. `( *> )` is provided for
  applicatives but `( <* )` is not).
  
</details>

[yallop14]: https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf
[lwt]: https://github.com/ocsigen/lwt
[alcotest]: https://github.com/mirage/alcotest
[pp]: https://github.com/diml/pp
[js-base]: https://github.com/janestreet/base
[containers]: https://github.com/c-cube/ocaml-containers
