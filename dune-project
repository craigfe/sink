(lang dune 2.5.1)
(name sink)

(using cinaps 1.0)

(generate_opam_files true)
(source (github CraigFe/sink))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name sink)
 (synopsis "An OCaml standard library replacement for personal use.")
 (description "An OCaml standard library replacement for personal use.")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (ppx_brands (= :version))
  (ppx_deriving_typeclass (= :version))
  (ppx_deriving_phantom (= :version))
  (ppx_implements (= :version))
  (alcotest (and (>= 1.0.0) :with-test))
  (cinaps :with-test)
  (ocamlformat (and (= 0.15.0) :with-test))))

(package
 (name sink-unix)
 (synopsis "Unix-specific standard library replacement.")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (sink (= :version))))

(package
 (name sink-lwt)
 (synopsis "Lwt-specific standard library replacement.")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (sink (= :version))
  lwt))

;; PPXes used in the implementation of Sink

(package
 (name ppx_implements)
 (synopsis "Helper for composing interfaces supported by a type")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (ppxlib (= 0.13.0))))

(package
 (name ppx_deriving_typeclass)
 (synopsis "Derive first-class equivalents of module types")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (ppxlib (= 0.13.0))))

(package
 (name ppx_deriving_infix)
 (synopsis "Derive sets of infix operators from module types")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (ppxlib (= 0.13.0))))

(package
 (name ppx_brands)
 (synopsis "Derive type brands for higher-kinded polymorphism")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (ppxlib (= 0.13.0))))

(package
 (name ppx_deriving_phantom)
 (synopsis "Clone a module type with extra phantom type parameters")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (ppxlib (= 0.13.0))))
