# Things to do

## General

- [ ] Group infix operators and syntax into submodules

- [ ] Implement `Hmap`

- [ ] Extract cinaps generation in `tuple.ml` to `tuple.cinaps`

  - [ ] Blocked on [cinaps-2]
  - [ ] OCamlformat `.cinaps` files

- [ ] Allow PPX to trigger from signature-local types (see [ppxlib-160])

  - [ ] Provide basic operations for datastructure views using PPX

- [ ] Implement `Zippable.S` for container types

- [ ] Provide `UTF8` view on strings

[cinaps-2]: https://github.com/ocaml-ppx/cinaps/issues/2
[ppxlib-160]: https://github.com/ocaml-ppx/ppxlib/issues/160

## Documentation

- [ ] Ensure that destructive substitutions are rendered correctly (blocked on
      [odoc-462])

- [ ] Add top-level documentation for modules (see [odoc-478])

[odoc-462]: https://github.com/ocaml/odoc/issues/462
[odoc-478]: https://github.com/ocaml/odoc/issues/478

## Testing

- [ ] Ensure OCamlformat keeps `module%ext` syntax sugar for `ppx_alcotest` (see
      [ocamlformat-410])

- [ ] Add basic test cases for `ppx_alcotest`

- [ ] Implement `[%check]` extension point for `ppx_alcotest` (see
      [alcotest-256])

  - [ ] Extract type reification logic from `ppx_irmin` to a common library
  - [ ] Add support for `check` locations to Alcotest

- [ ] Extend `ppx_alcotest` to support multi-file test suites

- [ ] Implement `[@@deriving testable]`

- [ ] `Array.Matrix`:

  - [ ] Test pretty-printer (fix alignment bug)
  - [ ] Provide iterators / fold functions (instance of `Assoc` with key =
        `int * int`)

[ocamlformat-410]: https://github.com/ocaml-ppx/ocamlformat/issues/410
[alcotest-256]: https://github.com/mirage/alcotest/issues/256

## Generic programming

- [ ] Implement equality for type representations `'a t -> 'b t -> bool`
- [ ] Extend `Repr.S` with algebraic types
- [ ] Fuzz-test generic operations
- [ ] Drop dependency on `Fmt`

## Optics

- [ ] Implement basic optics
- [ ] Derive optics from type representations
