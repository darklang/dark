# Darklang browser REPL (WASM)

The parser + runtime compiled to WebAssembly. Dark evaluates entirely in
the browser tab, no server. The publish output is a static site; host it anywhere.

## Build & run (from the repo root)

```
# 1. Generate the package snapshot (re-run when packages change;
#    also refreshes the published copy, no republish needed)
python3 backend/src/Wasm/generate-snapshot.py

# 2. Publish (re-run when F# or index.html changes)
dotnet publish backend/src/Wasm/Wasm.fsproj -c Release -o rundir/wasm-repl

# 3. Serve locally (9090 because the devcontainer only forwards 9090-9099
#    to the host; any port works for a browser inside the container)
./scripts/run-cli serve Darklang.WasmReplServer.router --port 9090
```

Open http://localhost:9090 → `1 + 2` → `3`. To host publicly, upload
`rundir/wasm-repl/wwwroot` to any static host (GitHub Pages, Cloudflare Pages, etc.)

## Notes

- The REPL is stateful across entries: fn/type/`val` declarations become
  in-memory package items under the `Repl` owner (callable unqualified in later
  entries; redefinition wins), and a bare `let x = …` persists its bindings as
  session variables (injected into later entries as pre-loaded VM registers).
  Declarations can't close over session variables — package items are static.
- Builtins: `Builtins.Pure` + `Builtins.Http.Client` (works over fetch; subject
  to CORS) + browser-local `printLine`/`print` (buffered into each result) +
  the `pmGetLocationsBy*` lookups the pretty-printer needs for type names.
  `Stdlib.*` comes from the snapshot. Output matches `dark eval`.
- Keep `<WasmBuildNative>false</WasmBuildNative>`. SQLite comes in through
  `LibParser`/`LibDB` and crashes the native-relink step. The browser never
  uses SQLite, so skipping it is safe.
- Always publish to `rundir/`. Publishing into the project tree makes the next
  publish copy the previous build into itself.

## Next steps

- Hosted package manager to replace the snapshot seam (marked in `Repl.fs`).
- Break `LibParser`'s dependency on `LibDB`. That dependency is what pulls
  SQLite into the browser bundle (and why the `WasmBuildNative` workaround
  exists) — the browser never uses it.
- Make the first load smaller (~10 MB gzipped today). Trim unused code and drop
  the accidental `Expecto` (test framework) reference.
