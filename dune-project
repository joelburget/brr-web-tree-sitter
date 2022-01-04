(lang dune 2.9)

(using dune_site 0.1)

(wrapped_executables false)

(package
 (name brr_web_tree_sitter)
 (synopsis "Ocaml (Brr) bindings to web-tree-sitter")
 (sites
  (lib wasm)))
