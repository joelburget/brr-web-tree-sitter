# brr-web-tree-sitter

OCaml bindings for [tree-sitter](https://github.com/tree-sitter/tree-sitter)'s JavaScript / WebAssembly implementation, using [brr](https://erratique.ch/software/brr).

## Building / testing

brr-web-tree-sitter works only with js_of_ocaml builds, not native code. The resulting javascript has two requirements:

1. It includes `require('web-tree-sitter')`. You need to use a JavaScript build tool like Webpack or Parcel to link it.
2. When initialized, it will issue a request for `tree-sitter.wasm` (relative to where the js was loaded from).

### Example

```
> dune build
> cd _build/default/bin
> yarn install
> cp node_modules/web-tree-sitter/tree-sitter.wasm .
> yarn parcel demo.html
```
