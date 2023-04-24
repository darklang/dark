# Darklang tree-sitter parser

In-progress / experimental `tree-sitter` parser for Darklang.


- https://tree-sitter.github.io

## Grammar and Parser

The grammar of Darklang is defined in `grammar.js`.
This is a `tree-sitter`-specific file, which is used to generate a parser in C code.
The resultant parser is used to tokenize and parse Darklang code into a syntax tree.

To get set up and familiar with the grammar and parser:

- `npm i` install npm dependencies specific to this project
- `npm i -g tree-sitter-cli`
  install tree-sitter globally via npm
- `tree-sitter init-config`
  this creates a config file at `~/.config/tree-sitter/config.json`
- `code ~/.config/tree-sitter/config.json`
  edit the config; add the path of `dark/parser` to the `parser-directories` array
  for example, I keep my `dark` repo in `~/code/dark`,
  so I added "/home/stachu/code/dark/parser" there.
- `tree-sitter dump-languages` to make sure you see `darklang` there somewhere
- `tree-sitter generate` to generate parser from grammar
- `tree-sitter parse ./demo.dark` to parse a hardcoded file
- `tree-sitter test` to test the parser against a 'corpus' of test cases found in `test/corpus`

Note: `export PATH=$PATH:./node_modules/.bin` may be needed at some point? I forget.

## Bindings

"Bindings" are used to interface with the parser. While the parser takes taext and returns a structured syntax tree,
bindings must be created to interface with the parser in a particular language.

Bindings may be used to provide syntax highlighting, code folding, or to turn the syntax tree into an AST (semantic analysis).
