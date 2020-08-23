(lang dune 2.7)

(name note)
(version 0.0.2)

(generate_opam_files true)

(source (github kevinschoon/note))
(license AGPL3)
(authors "Kevin Schoon <kevinschoon@gmail.com>")

(package
 (name note)
 (synopsis "Note is a simple CLI based note taking program")
 (depends
  (base
   (>= v0.14.0))
  (core
   (>= v0.14.0))
  (dune
   (>= 2.7))
  (ezjsonm
   (>= 1.2.0))
  (stdio ( >= v0.14.0))
  (yaml
   (>= 2.1.0))))
