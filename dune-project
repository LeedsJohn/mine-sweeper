(lang dune 3.7)

(name mine_sweeper)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mine_sweeper)
 (synopsis "Mine Sweeper Solver")
 (description "In this project, I am experimenting with different methods of solving Mine Sweeper.  As it stands now in its first draft, it can solve easy mode (10x10 grid with 10 mines) with ~75% accuracy.")
 (depends ocaml dune))

