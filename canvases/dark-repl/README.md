# dark-repl

- take user input
- use an experimental StdLib function `parseAndSerializeProgram` to parse the code as text,
  and serialize it as JSON such that it can be passed to a function defined in `Wasm.fsproj`.
  (the F# parser which we have been (ab)using does not work out-of-the-box in WASM)
- load WASM'd Dark runtime via the `dark-serve-static` canvas
  note: this canvas _must_ be seeded along with this one, as it serves the assets.
- evaluate the user code via the WASM Darklang runtime,
  which returns the result of the last-provided expr
- display the results

Note: as we use experimental functions, this canvas must be run on the experimental BwdServer, at :11003.

## Demo:
- `./scripts/run-canvas-hack load-from-disk dark-serve-static`
- `./scripts/run-canvas-hack load-from-disk dark-repl`
- go to http://dark-repl.dlio.localhost:11003
