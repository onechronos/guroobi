(lang dune 3.7)

(name guroobi)

(generate_opam_files true)

(source
 (github onechronos/guroobi))

(license MIT)

(authors "Mika Illouz")

(maintainers "mika@illouz.net")

(package
 (name guroobi)
 (synopsis "OCaml bindings to the Gurobi optimizer's C library")
 (description "a wrapper for Gurobi")
 (depends
  dune
  re
  bos
  (odoc :with-doc)
  (ocaml
   (>= "4.08"))))
