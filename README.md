## `Sink` – everything and the kitchen sink

<p align="center">🚧&nbsp;&nbsp;&nbsp;<b><i>Highly unstable and incomplete</i></b>&nbsp;&nbsp;&nbsp;🚧</p>

> _An OCaml standard library replacement intended for personal use in other
> projects. Explores some design space not seen in other OCaml standard
> libraries._

![Photograph of a kitchen sink](.meta/sink.jpg)

**Can I use it?** Of course.

**Should I use it?** Almost certainly not.

<hr/>

<details>
<summary>Vague design principles: (click to unfold)</summary>

- **Consistent interfaces**. Very similar to the approach taken in Jane Street's
  [Base][js-base], type interfaces are built with heavy use of functors and
  destructive substitution.

- **Higher-kinded polymorphism via [brands][yallop14]**. All higher-kinded types
  `'a t` come alongside a corresponding _brand_ `br` that can be used to
  specialise functions that are polymorphic over _all_ higher-kinded types. With
  some squinting, this enables an OCaml equivalent of the standard Haskell
  typeclass hierarchy (`Monoid`, `Semigroup`, `Functor`, `Monad` etc.).

- **Generic programming**. All higher-kinded types `t` come alonside a _value_
  `t` that is a run-time representation of the type. These representations can
  be used to derive operations on the corresponding types, if you're (_a_)
  allergic to boilerplate, (_b_) can't afford a PPX dependency and (_c_) don't
  care about performance. Type representations are implemented in
  tagless-final style, so it's possible to define one's own generic operations
  by supplying a new interpreter for the DSL.

- **Function-level programming permitted**. Use of point-free style in OCaml
  code is contentious, but I find it useful. Function composition is provided as
  `( >> )`, function-level monadic composition as `( >=> )` etc.

- **Name-spaced operators**. Some modules provide `Infix` and `Syntax`
  submodules that are intended to be opened either locally or globally. For
  instance, there are many ways to get at the `bind` operation on lists:

|                |  `List`   | `List.Infix` | `List.Syntax` |
| -------------- | :-------: | :----------: | :-----------: |
| Value-level    |  `bind`   |  `( >>= )`   |  `( let* )`   |
| Function-level | `kliesli` |  `( >=> )`   |       —       |

- **Different datastructure views in different namespaces**. For instance, views
  of an `'a array array` as a _matrix_ or a `('k * 'v) list` as an _association
  list_ are kept in separate namespaces (`Array.Matrix` and `List.Assoc`
  respecitvely).

<hr/>

If you don't care about any of the above, you probably want the OCaml standard
library instead. Some non-goals of this project:

- **~~Speed~~**. e.g. functions over lists are tail-recursive to minimise
  surprisal.

- **~~Exotic data-structures~~**. Ring buffers, indices, B-trees, tries etc. to
  be found elsewhere. If you want one of these, you probably care about its
  performance (and this library is not about performance). Try
  [`Containers`][containers].

All sequencing is _left-to-right_ (e.g. `( *> )` is provided for applicatives
but `( <* )` is not).

</details>

[yallop14]: https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf
[lwt]: https://github.com/ocsigen/lwt
[alcotest]: https://github.com/mirage/alcotest
[pp]: https://github.com/diml/pp
[js-base]: https://github.com/janestreet/base
[containers]: https://github.com/c-cube/ocaml-containers
