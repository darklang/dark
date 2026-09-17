# Darklang in the browser (WASM)

The runtime, parser, LibDB and SQLite compiled to WebAssembly and published as a static site.
Live at https://dark-wasm.fly.dev.

- `index.html`: a directory of the pages. With a query it is the answer, raw: `/?cmd=<argv>`
  prints what `dark <argv>` prints, `/?fn=<name>` is `view`.
- `cli.html`: the real Dark CLI in an xterm.js terminal, against a real package store. No params
  opens the workbench with a shell on the right that takes `dark <command>` lines (`?panel=0`
  hides it). `?cmd=<argv>` runs a command then drops to the classic prompt; `?fn=<name>` is
  `view`; `?env=DARK_CLASSIC=1` is the classic prompt.
- `eval.html?e=<expr>`: an expression box and what `dark eval` prints.
- `repl.html`: the older REPL over an in-memory package snapshot.

## Build, serve, deploy (from the repo root, inside the container)

```
dotnet publish backend/src/Wasm/Wasm.fsproj -c Release -o rundir/wasm-repl   # ~6 min (AOT)
backend/src/Wasm/make-store.sh                # the store the CLI boots from; re-run when packages change
python3 backend/src/Wasm/generate-snapshot.py # the REPL's snapshot (repl.html only)

./scripts/run-cli permissions allow http-server 9090
./scripts/run-cli permissions allow file read /home/dark/app/rundir/wasm-repl/wwwroot
./scripts/run-cli serve Darklang.WasmReplServer.router --port 9090   # host port: scripts/dev/host-port

backend/src/Wasm/deploy/deploy.sh             # nginx image of the publish -> fly app dark-wasm
```

Wipe `rundir/wasm-repl` before a publish, or stale fingerprinted files pile up. Nothing in CI
builds or deploys this; `deploy.sh` is run by hand.

## How it is put together

`Host.fs`: `Cli.Boot` fetches `data.db.br` into emscripten's in-memory filesystem, inflates it
through the runtime's own brotli decoder, warms SQLite and runs `growIfNeeded`; `Cli.RunCli`
builds the same execution state the native CLI builds and calls the entry point; `RunCommand`
runs one command with its output captured. `Browser` is the seam to the page: keys in
(`PushKey`/`PushPaste`), output out (`DrainOutput`, drained by the page on a timer), size
(`SetTerminalSize`). `BrowserBuiltins` replaces seven builtins (stdin, terminal size and
session info, clear); everything else is the real `Builtins.Cli`/`CliHost`/`Matter`.
`site.js` holds the pages' shared boot and helpers.

`RunAOTCompilation` is on because the interpreter's F# `task` loop could not suspend a second
time under the mono interpreter. A few big leaf assemblies stay interpreted; interpreting the
BCL wholesale asserts when compiled F# generics call into it. `InvariantGlobalization` drops ICU.

## The store, and the secret

`make-store.sh` copies this clone's `rundir/data.db`, deletes `config_v0` (relay url, push
cursors, and `sync.secret.<url>`, the write secret shared between a person's machines and
production) and the sync tables, asserts `config_v0` is empty, scans the bytes for a stored
secret key, and only then writes the file. It never reads `~/.darklang` or `cli-config.json`.
`deploy.sh` checks the bytes again. Keep it that way.

## Testing headless

```
node backend/src/Wasm/headless-check.mjs <url> <script.js> [timeout-s] [screenshot.png]
```

Drives playwright's chromium over CDP with Node's WebSocket. The script is evaluated in the page
every 2 s until it returns a string starting `DONE` or `FAIL`; other returns are progress.

## Known gaps

- Everything is per tab and in memory; reload and the store is fresh.
- No push from the tab (no secret ships). Pull from a server is untried.
- Posix: cwd, env, mkdir, rmdir, unlink, rename, listDir and stat answer through `System.IO`;
  descriptors, symlinks, chmod, kill and processes answer ENOSYS. HTTP: same-origin and
  CORS-enabled hosts only; no server.
- Cold start: 17.7 MB gzip of runtime plus a 6.6 MB brotli store, then ~1 s of warm-up.
