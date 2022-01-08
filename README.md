# brr-web-tree-sitter

OCaml bindings for [tree-sitter](https://github.com/tree-sitter/tree-sitter)'s JavaScript / WebAssembly implementation, using [brr](https://erratique.ch/software/brr).

## Building / testing

```
> dune build
> cd _build/default/bin
> yarn install
> cp node_modules/web-tree-sitter/tree-sitter.wasm .
> yarn parcel demo.html
```
