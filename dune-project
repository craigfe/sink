(lang dune 2.5.1)
(name sink)

(generate_opam_files true)
(source (github CraigFe/sink))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name sink)
 (synopsis "An OCaml standard library replacement for personal use.")
 (description "An OCaml standard library replacement for personal use.")
 (documentation https://CraigFe.github.io/sink/))

(package
 (name sink-unix)
 (synopsis "Unix-specific standard library replacement.")
 (description "Unix-specific standard library replacement.")
 (documentation https://CraigFe.github.io/sink/))

(package
 (name sink-lwt)
 (synopsis "Lwt-specific standard library replacement.")
 (description "Lwt-specific standard library replacement.")
 (documentation https://CraigFe.github.io/sink/))

;; PPXes used in the implementation of Sink

(package
 (name ppx_deriving_typeclass)
 (synopsis "Derive first-class equivalents of module types")
 (description "Derive first-class equivalends of type signatures")
 (documentation https://CraigFe.github.io/sink/))

(package
 (name ppx_deriving_infix)
 (synopsis "Derive sets of infix operators from module types")
 (description "Derive sets of infix operators from module types")
 (documentation https://CraigFe.github.io/sink/))
