# Tree-Sitter WebAssembly/JS Bindings

These files support parsing Darklang code in the browser.

The `tree-sitter.js` and `tree-sitter.wasm` files were pulled from the releases of `tree-sitter`. The .js file is a wrapper around the .wasm file, and the .wasm file is the actual parser which calls upon language-specific parsers (like `tree-sitter-darklang.wasm`) after loading them.

The actual parsing of Darklang code is done by the `tree-sitter-darklang.wasm` file, which is a compiled version of the `grammar.js` file in the `parser/tree-sitter-darklang` directory. See the README.md in that directory for more information.