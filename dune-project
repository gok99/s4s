(lang dune 3.16)
(using menhir 3.0)

(name s1s)

(generate_opam_files true)

(source
 (github gok99/s1s))

(authors "Gokul Rajiv")

(maintainers "Gokul Rajiv")

(license LICENSE)

(package
 (name s1s)
 (synopsis "An extension of source1 with effect handlers and type inference")
 (depends ocaml dune))
