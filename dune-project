(lang dune 2.6)
(name fcf)
(using menhir 2.0)
(allow_approximate_merlin)

(package
  (name fcf)
  (synopsis "A small DSL for describing FCFSMs (Functional Compute FSMs) in OCaml")
  (depends
    (dune (>= 2.0))
    (menhir (>= 2.0))
    (ocaml (>= 4.08))))

(generate_opam_files true)
(wrapped_executables false) 
