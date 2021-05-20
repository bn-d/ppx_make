(lang dune 2.0)

(version "0.0.1")

(package
 (name ppx_make)
 (synopsis "ppxlib based make deriver")
 (depends
  (dune (>= 2.0.0))
  (ppxlib (>= 0.10.0))
  (ounit2 :with-test)))
