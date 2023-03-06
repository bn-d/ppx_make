(lang dune 2.7)
(name ppx_make)
(version "0.3.3")

(generate_opam_files true)

(source (github bn-d/ppx_make))
(license MIT)
(authors "Boning <me@boni.ng>")
(maintainers "Boning <me@boni.ng>")
(documentation "https://boni.ng/ppx_make/ppx_make/index.html")

(package
 (name ppx_make)
 (synopsis "[@@deriving make]")
 (description "[@@deriving] plugin to generate make functions")
 (depends
  (ppxlib (>= 0.10.0))
  (ounit2 :with-test)))
