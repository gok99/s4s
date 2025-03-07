(lang dune 3.16)
(using menhir 3.0)

(name source1star)

(generate_opam_files true)

(source
 (github gok99/source1star))

(authors "Gokul Rajiv")

(maintainers "Gokul Rajiv")

(license LICENSE)

(package
 (name source1star)
 (synopsis "An extension of source1 with effect handlers and type inference")
 (depends ocaml dune))
