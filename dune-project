(lang dune 3.7)

(source (github colis-anr/morsmall))

(authors "The CoLiS Project")
(maintainers "Nicolas “Niols” Jeannerod <niols@niols.fr>")

(license GPL-3.0-or-later)

(package
 (name morsmall)
 (version 0.3.0)
 (synopsis "A concise AST for POSIX shell")

 (depends
  (alcotest             :with-test)
  (dune                 :build)
   logs
   menhir
   morbig
  (ocaml               (>= 4.04.0))
  (odoc                 :with-doc)
  (ppx_deriving         :build)
  (ppx_deriving_yojson  :build)
   ppx_import
  (qcheck               :with-test)
  (qcheck-alcotest      :with-test)
   visitors
   yojson))

(generate_opam_files)
