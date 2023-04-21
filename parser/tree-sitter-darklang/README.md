# Darklang tree-sitter parser

## Relevant Tree-Sitter Docs and Projects
- https://tree-sitter.github.io
- writing a parser (`grammar.js` -> parser in C code)
- using a parser (creating and using bindings)
- JS/WASM bindings https://github.com/tree-sitter/tree-sitter/tree/master/lib/binding_web
  the .d.ts file here in particular is worth a review

## First steps

to get you set up an and familiar with `tree-sitter` and our usage.

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
- `tree-sitter parser demo.dark` to parse a hardcoded file
- `tree-sitter test` to test the parser against a 'corpus' of test cases found in `test/corpus`
- When you need the WASM bindings updated:
  - compile to WASM with `tree-sitter build-wasm . && mv ./tree-sitter-darklang.wasm ./bindings`
  - copy the bindings to `backend/static/parser`
  - use the parser+bindings!
    - see `canvases/dark-tree-sitter-demo` for a JS/WASM usage demo
    - TODO: additional usages

## Usage


## What the files are for



https://github.com/tree-sitter/tree-sitter/blob/master/lib/binding_web/README.md

https://github.com/tree-sitter/tree-sitter/releases/tag/v0.20.8

## TODOs
- [ ] review https://tree-sitter.github.io/tree-sitter/syntax-highlighting#unit-testing
- [ ] write some words about lo
- [ ] I think I can gitignore the whole of the src directory, since it's generated?
  or maybe just certain files

`export PATH=$PATH:./node_modules/.bin`



https://github.com/tree-sitter/tree-sitter/blob/master/lib/binding_web/README.md keep going, testing in browser
 actually this (review the directory)