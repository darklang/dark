# `backend/static`

This directory contains static files that are served by a Dark backend for a webpage.
The `dark_wasm` and `tree-sitter` directories should ideally be hosted in a CDN, long-term.

These assets are currently served by an experimental canvas, `dark-serve-static`.
See the README.md in that directory for more information.

## Tree-Sitter WebAssembly/JS Bindings

If you don't see a `tree-sitter` directory, run `./scripts/build/build-parser`.