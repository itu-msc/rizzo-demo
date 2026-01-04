(lang dune 3.20)

(name rizzo_demo)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rizzo_demo)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml rizzo)
 (tags
  ("demo" "rizo" "example")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
