# Darklang in the browser (WASM)

Two pages, one bundle. The runtime, parser, LibDB and SQLite are compiled to WebAssembly and
published as a static site; host it anywhere.

- `cli.html` is the real Dark CLI: `Darklang.Cli.executeCliCommand` against a real package
  store, in an xterm.js terminal. No params opens the workbench, like `dark` with no arguments.
- `index.html` is the older REPL: one expression at a time, over an in-memory package snapshot.

## Build & run (from the repo root, inside the container)

```
# 1. Publish (re-run when F# or the pages change). AOT, so it takes ~7 minutes.
dotnet publish backend/src/Wasm/Wasm.fsproj -c Release -o rundir/wasm-repl

# 2. The store the CLI boots from (re-run when packages change)
backend/src/Wasm/make-store.sh

# 3. The REPL's snapshot (only index.html needs it)
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
  for the classic prompt instead of the workbench). `?trace=1` logs every frame entry to the
  console, `?probe=1` runs the boot probes. Diagnostics, not a promise.

## How it is put together

- `Host/Cli.fs` is `Cli/Cli.fs:main` minus the process: `Boot` fetches `data.db` into
  emscripten's in-memory filesystem, warms SQLite, runs `growIfNeeded`, preloads the harmful
  set. `RunCli argv` builds the same execution state the native CLI builds and calls the
  entry point. `EvalProbe` evaluates one expression under that state (for tests).
- `Host/Browser.fs` is the seam to the page: keys come in through `PushKey`/`PushPaste`,
  size through `SetTerminalSize`, output goes to a buffer the page drains with `DrainOutput`
  on a 25 ms timer (a whole TUI frame arrives as one write).
- `Host/BrowserBuiltins.fs` replaces seven builtins (stdin, terminal size and session info,
  clear). Everything else is the real `Builtins.Cli`, `Builtins.CliHost`, `Builtins.Matter`.
- `wwwroot/vendor/` carries xterm.js (MIT), so the site has no CDN dependency.

Things that had to change outside this directory, all guarded on `OperatingSystem.IsBrowser()`:
`HostLibc` takes managed implementations (no `DllImport("libc")` in wasm), `PackageManager`
gained `preloadHarmful`, `Terminal.fs` skips the `CancelKeyPress` subscription,
`NonBlockingConsole` has a browser sink.

`RunAOTCompilation` is on because the interpreter's F# `task` loop could not suspend a second
time under the mono interpreter ("Cannot wait on monitors on this runtime"). It is the reason
the publish takes minutes and `dotnet.native.wasm` is ~14 MB brotli'd; partial AOT (a profile,
or AOT for LibExecution and FSharp.Core only) is the next size lever.

## The store, and the secret

`make-store.sh` copies THIS clone's `rundir/data.db`, deletes `config_v0` (relay url, push
cursors, and `sync.secret.<url>`, the write secret shared between a person's machines and
production), `sync_pushed`, `sync_bases` and `relay_branches`, asserts `config_v0` is empty,
scans the bytes for a stored secret key, and only then writes the file. It never reads
`~/.darklang` or `cli-config.json`. `deploy/deploy.sh` runs it again on the exact bytes that
ship. Keep it that way.

## Deploy (fly.io)

```
backend/src/Wasm/deploy/deploy.sh      # nginx image of rundir/wasm-repl/wwwroot -> app dark-wasm
```

## Testing headless

```
node backend/src/Wasm/headless-check.mjs <url> <script.js> [timeout-s]
```

Drives playwright's chromium over CDP (Node 22's WebSocket, no npm packages). The script is
evaluated in the page every 2 s until it returns a string starting `DONE` or `FAIL`; other
returns are progress. `headless-stack.mjs <url> <secs>` pauses a busy tab and prints its
stack, for when the main thread never yields.

## Known gaps

- Everything is per tab and in memory: the store dies with the tab. No IndexedDB yet.
- No push from the tab (by design: no secret ships). `pull` from a relay should work and
  has not been tried.
- `Posix.stat` and friends answer through `System.IO` with synthesized modes; `kill`,
  `spawn`, `exec` fail cleanly (no processes in a tab).
- The `?` in the welcome logo is in the source; the native CLI shows it too.
- Cold start: ~19 MB brotli of runtime plus a 14 MB gzip'd store, then ~3 s of SQLite and
  package-manager warm-up.
