(lang dune 2.5)

(source (github colis-anr/morsmall))

(authors "The CoLiS Project")
(maintainers "Nicolas “Niols” Jeannerod <niols@niols.fr>")

(license GPL-3.0-or-later)

(package
 (name morsmall)
 (version 0.3.0)
 (synopsis "A concise AST for POSIX shell")

 (depends
  (dune                 :build)
  (morbig              (>= 0.10.4))
  (ocaml               (>= 4.04.0))
  (ppx_deriving         :build)
  (ppx_deriving_yojson  :build)
   yojson))

(generate_opam_files)
