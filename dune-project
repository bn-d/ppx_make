(lang dune 2.0)
(name ppx_make)
(version "0.2.0")

(generate_opam_files true)

(source (github erebuxy/ppx_make))
(license MIT)
(authors "Boning <erebuxy@gmail.com>")
(maintainers "Boning <erebuxy@gmail.com>")

(package
 (name ppx_make)
 (synopsis "ppxlib based make deriver")
 (depends
  (dune (>= 2.0.0))
  (ppxlib (>= 0.10.0))
  (ounit2 :with-test)))
