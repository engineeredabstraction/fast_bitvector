(lang dune 3.18)
(name fast_bitvector)

(generate_opam_files true)

(source
 (github engineeredabstraction/fast_bitvector))

(authors "Stefan Muenzel <source@s.muenzel.net>")

(maintainers "Stefan Muenzel <source@s.muenzel.net>")

(license MPL-2.0)

(package
  (name     fast_bitvector)
  (synopsis "A bitvector library")
  (description "Bitvector represented as bytes internally")
  ; depends are in separate file
  (tags (bitvector bitset)))
