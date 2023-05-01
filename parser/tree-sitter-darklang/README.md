# Darklang tree-sitter parser

In-progress / experimental `tree-sitter` parser for Darklang.

- https://tree-sitter.github.io

## Getting started

Run `./scripts/build/build-parser` to build the parser and JS/WASM bindings.

Alternatively, run the scripts included in the package.json file:

- `npm run build-parser` to build the parser from the grammar.js file
- `npm run build-wasm-bindings`
- `npm run test` to run the tests in `test/corpus`

## Grammar and Parser

The grammar of Darklang is defined in `grammar.js`.
This is a `tree-sitter`-specific file, which is used to generate a parser in C code.
The resultant parser is used to tokenize and parse Darklang code into a syntax tree.

## Bindings

"Bindings" are used to interface with the parser. While the parser takes taext and returns a structured syntax tree,
bindings must be created to interface with the parser in a particular language.

Bindings may be used to provide syntax highlighting, code folding, or to turn the syntax tree into an AST (semantic analysis).

At this point, we have bindings for JS/WASM - later, we may need bindings for F# or something else.

### JS/WASM bindings

See `canvases/dark-tree-sitter-demo/README.md` for a JS/WASM usage demo.

Bindings are built with the `./scripts/build/build-parser` script, and the output is
placed in the `backend/scripts/tree-sitter` directory. `tree-sitter.js` and
`tree-sitter.wasm` are pulled from the `web-tree-sitter` package. The `.js` file is
a wrapper around the `.wasm` file, and the `.wasm` file calls upon language-specific
parsers (like `tree-sitter-darklang.wasm`) after loading them.

Relevant documentation: https://github.com/tree-sitter/tree-sitter/tree/master/lib/binding_web
