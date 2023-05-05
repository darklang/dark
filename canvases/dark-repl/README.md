# dark-wasm-demo

## What it does
- take user input
- use an experimental StdLib function `parseAndSerializeExpr` to parse the Expr as text,
  and serialize it as JSON such that it can be passed to a function defined in Wasm.fsproj.
  (the F# parser which we have been (ab)using does not work out-of-the-box in WASM)
- load WASM'd F# code via the `dark-serve-static` canvas
  note: this canvas _must_ be seeded along with this one, as it serves the assets.
- call the WASM'd F# code, passing in the serialized Expr
- have the WASM'd F#/Dark code evaluate the expression
- expect the WASM'd F# code to pass back the results of the evaluation
- display the results

Note: as we use experimental functions, this canvas must be run on the experimental BwdServer, at :11003.

## Demo:
- `./scripts/run-canvas-hack load-from-disk dark-serve-static`
- `./scripts/run-canvas-hack load-from-disk dark-repl`
- go to http://dark-repl.dlio.localhost:11003
