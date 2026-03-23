(lang dune 3.2)

(name re-nfa)

(generate_opam_files true)

(source
 (github yallop/ocaml-re-nfa))

(authors "Jeremy Yallop" "Nada Amin")
(maintainers "Jeremy Yallop <yallop@gmail.com>")

(license MIT)

(package
 (name re-nfa)
 (synopsis "Convert regular expressions to NFAs")
 (depends 
   ocaml
   (qcheck :with-test)
   (regenerate :with-test)))
