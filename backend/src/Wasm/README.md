# Darklang in the browser (WASM)

Four pages, one bundle. The runtime, parser, LibDB and SQLite are compiled to WebAssembly and
published as a static site; host it anywhere.

- `index.html` is a directory of the experiences below, with the URL forms spelled out. With a
  query it is the answer, raw: `/?cmd=<argv>` prints what `dark <argv>` prints and nothing
  else, `/?fn=<name>` is view.
- `eval.html?e=<expr>` is an expression box and what `dark eval` prints.
- `cli.html` is the real Dark CLI: `Darklang.Cli.executeCliCommand` against a real package
  store, in an xterm.js terminal. No params opens the workbench, like `dark` with no arguments,
  with a shell on the right that takes `dark <command>` lines against the same store
  (`?panel=0` hides it).
- `repl.html` is the older REPL: one expression at a time, over an in-memory package snapshot.

## Build & run (from the repo root, inside the container)

```
# 1. Publish (re-run when F# or the pages change). AOT, so it takes ~7 minutes.
dotnet publish backend/src/Wasm/Wasm.fsproj -c Release -o rundir/wasm-repl

# 2. Stage it for the local server: the store the CLI boots from (make-store.sh, sync
#    credentials stripped, brotli'd), the pages from the tree, swapped into rundir/wasm-site
backend/src/Wasm/promote.sh

# 3. The REPL's snapshot (only repl.html needs it)
python3 backend/src/Wasm/generate-snapshot.py

# 4. Serve. 9090 because the devcontainer forwards 9090-9099 to the host
#    (`scripts/dev/host-port` says which host port that is).
./scripts/run-cli permissions allow http-server 9090
./scripts/run-cli permissions allow file read /home/dark/app/rundir/wasm-repl/wwwroot
./scripts/run-cli serve Darklang.WasmReplServer.router --port 9090
```

Always publish to `rundir/`; publishing into the project tree makes the next publish copy the
previous build into itself. Wipe `rundir/wasm-repl` first, or stale fingerprinted files pile up.

## The URL contract (cli.html)

- `cli.html` opens the workbench.
- `cli.html?cmd=<argv>` runs that command (URL-encoded, split on whitespace, double quotes
  group), then drops into the classic inline prompt so the output stays in scrollback and
  the visitor keeps a CLI. `?cmd=eval+1+%2B+2`, `?cmd=search+map`, `?cmd=docs+syntax`.
- `cli.html?fn=<Owner.Module.name>` is sugar for `?cmd=view <name>`.
- `?env=NAME=value` sets an environment variable before the CLI starts (`?env=DARK_CLASSIC=1`
  for the classic prompt instead of the workbench).

## How it is put together

- `Host/Cli.fs` is `Cli/Cli.fs:main` minus the process: `Boot` fetches `data.db.br` into
  emscripten's in-memory filesystem, warms SQLite, runs `growIfNeeded`, preloads the harmful
  set. `RunCli argv` builds the same execution state the native CLI builds and calls the
  entry point; `RunCommand argv` runs one command with its output captured (the shell, the raw
  pages).
- `Host/Browser.fs` is the seam to the page: keys come in through `PushKey`/`PushPaste`,
  size through `SetTerminalSize`, output goes to a buffer the page drains with `DrainOutput`
  on a 25 ms timer (a whole TUI frame arrives as one write).
- `Host/BrowserBuiltins.fs` replaces seven builtins (stdin, terminal size and session info,
  clear). Everything else is the real `Builtins.Cli`, `Builtins.CliHost`, `Builtins.Matter`.
- xterm.js comes from jsdelivr (pinned versions).

Things that had to change outside this directory, all guarded on `OperatingSystem.IsBrowser()`:
`HostLibc` takes managed implementations (no `DllImport("libc")` in wasm), `PackageManager`
gained `preloadHarmful`, `Terminal.fs` skips the `CancelKeyPress` subscription,
`NonBlockingConsole` has a browser sink.

`RunAOTCompilation` is on because the interpreter's F# `task` loop could not suspend a second
time under the mono interpreter ("Cannot wait on monitors on this runtime"). It is the reason
the publish takes minutes. A few big leaves (XML, Expecto, Mono.Cecil, Linq.Expressions, the
Blazor component assemblies) are kept interpreted via `_AOT_InternalForceInterpretAssemblies`;
excluding the BCL wholesale made mixed mode assert ("should not be reached") the moment
compiled F# generics called into an interpreted assembly, so that is as far as it goes.
`InvariantGlobalization` drops ICU.

## The store, and the secret

`make-store.sh` copies THIS clone's `rundir/data.db`, deletes `config_v0` (relay url, push
cursors, and `sync.secret.<url>`, the write secret shared between a person's machines and
production), `sync_pushed`, `sync_bases` and `relay_branches`, asserts `config_v0` is empty,
scans the bytes for a stored secret key, and only then writes the file. It never reads
`~/.darklang` or `cli-config.json`. `deploy/deploy.sh` runs it again on the exact bytes that
ship. Keep it that way.

## Deploy (fly.io)

```
backend/src/Wasm/deploy/deploy.sh      # nginx image of rundir/wasm-site/wwwroot -> app dark-wasm
```

CI does the same on a release tag (`v*`), alongside the binaries: `build-wasm` publishes and
stages, `deploy-wasm` ships (needs `WASM_FLY_API_TOKEN` in the project's CircleCI environment).

## Testing headless

```
node backend/src/Wasm/headless-check.mjs <url> <script.js> [timeout-s]
```

Drives playwright's chromium over CDP (Node 22's WebSocket, no npm packages). The script is
evaluated in the page every 2 s until it returns a string starting `DONE` or `FAIL`; other
returns are progress. A fourth argument saves a screenshot on `DONE`.

## Known gaps

- Everything is per tab and in memory: the store dies with the tab. No IndexedDB yet.
- No push from the tab (by design: no secret ships). `pull` from a relay should work and
  has not been tried.
- Posix: cwd, env, mkdir, rmdir, unlink, rename, listDir and stat answer through `System.IO`
  (synthesized modes); descriptors, symlinks, chmod, kill and processes answer ENOSYS.
- HTTP: same-origin and CORS-enabled hosts only (the browser's rules); no server.
- The `?` in the welcome logo is in the source; the native CLI shows it too.
- Cold start: 17.7 MB gzip of runtime plus a 6.6 MB brotli store (fetched in parallel via a
  preload link, inflated by the runtime's own brotli decoder through a P/Invoke, since
  `BrotliStream` refuses the browser), then ~1 s to decode and warm up. First workbench frame
  5-8 s from the fly deploy, ~3 s locally.
