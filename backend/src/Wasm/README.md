# Darklang in the browser (WASM)

The CLI's runtime, store and builtins compiled to WebAssembly and published as a static site.
Dark code runs in a Web Worker, so the page stays live while it runs.

- `index.html`: a directory of the pages. With a query it is the answer, raw: `/?cmd=<argv>`
  prints what `dark <argv>` prints, `/?fn=<name>` is `view`.
- `cli.html`: the real Dark CLI in an xterm.js terminal, against a real package store. No params
  opens the workbench with a shell on the right that takes `dark <command>` lines (`?panel=0`
  hides it). `?cmd=<argv>` runs a command then drops to the classic prompt; `?fn=<name>` is
  `view`; `?env=DARK_CLASSIC=1` is the classic prompt.
- `eval.html?e=<expr>`: an expression box and what `dark eval` prints.

## Build, serve, deploy (from the repo root, inside the container)

```
backend/src/Wasm/build.sh                     # setup.sh, the publish and the store; ~6 min (AOT)
backend/src/Wasm/build.sh --store-only        # just the store, when only packages changed

python3 -m http.server 9090 -d rundir/wasm-repl/wwwroot   # host port: scripts/dev/host-port 9090

backend/src/Wasm/deploy/deploy.sh             # nginx image of the publish -> fly app dark-wasm
```

`setup.sh` installs the `wasm-tools` workload (emscripten, the browser runtime packs, the AOT
compiler) once per container; the dev image leaves it out. `build.sh` wipes `rundir/wasm-repl`
first, or stale fingerprinted files pile up, and writes the publish log to a file
(`publish-notes.md` says why).

## How it is put together

Two projects. `Wasm.csproj` is the site: a `Microsoft.NET.Sdk.WebAssembly` app whose
`Exports.cs` holds the `[JSExport]` functions JavaScript calls. It is C# only because F# has no
generator for those attributes; every export passes straight through to `Lib/`, the F# host.

- `Lib/Host.fs`: `Cli.Boot` fetches the store into emscripten's in-memory filesystem, warms
  SQLite and runs `growIfNeeded`; `Cli.RunCli` builds the execution state the native CLI builds
  and calls the entry point; `RunCommand` runs one command with its output captured. `Browser`
  is the seam to the page: keys in (`PushKey`/`PushPaste`), output out (`DrainOutput`), size
  (`SetTerminalSize`). `BrowserBuiltins` replaces the builtins that read a tty (stdin, terminal
  size, isatty, `hostAwait`); everything else is the real `Builtins.Cli`/`CliHost`.
- `wwwroot/worker.js` owns the runtime: it boots .NET, keeps the tab's store in IndexedDB, and
  answers `{ id, call, args }` messages by calling the exports.
- `wwwroot/site.js` is the page side: `dark.boot`, `dark.run(argv)`, `dark.invoke(name, ...args)`.

The site is built one way, AOT; the publish refuses `RunAOTCompilation=false`. A few big leaf
assemblies stay interpreted in the AOT build; interpreting the BCL wholesale asserts when compiled
F# generics call into it. `InvariantGlobalization` drops ICU.

## Keeping a tab's work

The store lives under `/dark` in emscripten's in-memory filesystem. The worker keeps a copy in
IndexedDB, one database per site path (`darklang-tab:/pulls/12/`), puts it back before Boot,
and Boot uses it, brings it up to this build, or sets it aside and starts fresh. `?fresh` sets
the saved work aside; `?restore=<name>` puts a set-aside copy back.

## The store, and the secret

`make-store.sh` copies this clone's `rundir/data.db`, deletes `config_v0`, `accounts_v0`, the
test results and the trace tables, asserts `config_v0` is empty, scans the bytes for a stored
secret key, and only then writes the file. It never reads `~/.darklang` or `cli-config.json`.
`deploy.sh` checks the bytes again. Keep it that way. It also sweeps orphan `package_blobs`
(`pm-sweep-blobs`, against the snapshot) before the final VACUUM.

The page fetches `data.db`; the host sends `data.db.br` (brotli_static) or `data.db.gz`
(gzip_static) in its place and the browser's fetch inflates it. .NET 11's browser runtime has
no brotli decoder of its own, so the store is never decoded in .NET.

## A preview per PR

CI publishes this site and puts it on the preview host (the `wasm` workflow in
`.circleci/continue.yml`; `deploy/preview/ci-deploy.sh` has the rules). The workflow runs only
for a branch that changes `backend/src/Wasm/` (`deploy/ci-changed`) and for a release tag, so
no other PR waits on it or pays for it:

    https://wasm.darklang.com/            directory of what is deployed
    https://wasm.darklang.com/latest/     the newest release tag
    https://wasm.darklang.com/pulls/<n>/  the CLI built from open PR #n

`latest/` comes from the release tag's own pipeline (`scripts/deployment/release` pushes the
tag), so it moves at the release cadence. An older tag's pipeline finishing late never
overwrites a newer one.

Who gets a preview is decided by CircleCI, not by anything in this repo, since a PR can edit
this repo. A build deploys when its CircleCI project holds `PREVIEW_FLY_API_TOKEN`, and
CircleCI never gives project secrets to a forked PR's build. So:

- a branch pushed to darklang/dark with an open PR: darklang's pipeline deploys it.
- a PR from your fork: your fork's own CircleCI project, holding its own token, deploys it;
  the PR is found by asking GitHub for the open PR whose head is `<owner>:<branch>`.
- a PR from anyone else's fork: nothing deploys, and the publish never runs.

The store is a fresh migration in the runner, so no secret can be in it. The PR gets one
comment with the link, edited on later pushes. The host keeps `latest/` and open PRs; every
release deploy removes everything else. A removed preview or a typo is a 404 whose page points
at `latest/`.

The host is the fly app `dark-preview` (`deploy/preview/`): Alpine's nginx with the brotli
module over one volume, a directory per site, `_framework` kept as `.gz` only.
`host.sh upload|remove|cleanup` works from a laptop with a logged-in flyctl. Every preview
shares that origin, so nothing with a cookie or a token may ever be served from it.

### Saying what a branch is for

- A URL in the PR description: `cli.html?cmd=<argv>` runs a command and hands over the prompt,
  `?fn=<name>` lands on an item, `eval.html?e=<expr>` is an expression box, `/?cmd=<argv>`
  prints the answer and nothing else.
- A `showcase.json` in `wwwroot/`, read by the branch's index page and by the directory:

      { "title": "Traits",
        "blurb": "Eq and Display as stdlib traits.",
        "landing": "cli.html?cmd=view Darklang.Stdlib.Eq",
        "tries": [ { "label": "an expression", "url": "eval.html?e=1 + 2",
                     "what": "what it prints" } ] }

  `landing` is where the directory card goes (default `cli.html`); `tries` become cards at the
  top of the branch's index. Delete the file before merging.
- A page of your own in `wwwroot/`, and `landing` pointing at it. `site.js` gives it
  `dark.boot`, `dark.run(argv)` and `dark.invoke`; `eval.html` is the smallest example.

What a preview cannot show is what a tab cannot do: `serve` (no listening socket), OS
processes, `Exec.spawn`, push. Say so in the PR rather than let a reviewer hunt for it.

## Testing headless

```
node backend/src/Wasm/headless-check.mjs <url> <script.js> [timeout-s] [screenshot.png]
```

Drives playwright's chromium over CDP with Node's WebSocket. The script is evaluated in the page
every 2 s until it returns a string starting `DONE` or `FAIL`; other returns are progress.
The worker's console comes through too, marked `worker`.

## Known gaps

- No push from the tab (no secret ships). Pull from a server is untried.
- Posix: cwd, env, mkdir, rmdir, unlink, rename, listDir and stat answer through `System.IO`;
  descriptors, symlinks, chmod, kill and processes answer ENOSYS. HTTP: CORS-enabled public
  hosts only; the SSRF guard refuses loopback and private ranges here as it does natively.
- One thread: `Exec.spawn` and anything else that wants a second thread refuses.
