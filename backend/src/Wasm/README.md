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

Wipe `rundir/wasm-repl` before a publish, or stale fingerprinted files pile up. `deploy.sh` is
the hand deploy of the stable site; CI does the per-branch previews below.

## A preview per PR

CI publishes this site and puts it on the preview host (`build-wasm`, `deploy-preview`;
`deploy/preview/ci-deploy.sh` has the rules). Two things are hosted, nothing else:

    https://wasm.darklang.com/            directory of what is deployed
    https://wasm.darklang.com/latest/     the newest release tag
    https://wasm.darklang.com/pulls/<n>/  the CLI built from open PR #n

`latest/` comes from the tag's own pipeline, and `tag-release` tags every green merge to
main, so it moves at the release cadence. A red main is never on it, and an older tag's
pipeline finishing late never overwrites a newer one.

Who gets a preview is decided by CircleCI, not by anything in this repo, since a PR can
edit this repo. A build deploys when its CircleCI project holds `PREVIEW_FLY_API_TOKEN`,
and CircleCI never gives project secrets to a forked PR's build (the "Pass secrets to
builds from forked pull requests" setting stays off; everything else in CI already relies
on that). So:

- a branch pushed to darklang/dark with an open PR: darklang's pipeline deploys it.
- a PR from your fork: darklang's pipeline for it has no token and halts before the
  publish. Your fork's own CircleCI project, holding its own token, deploys it; the PR is
  found by asking GitHub for the open PR whose head is `<owner>:<branch>`.
- a PR from anyone else's fork: nothing deploys, and the publish never runs.

The store is a fresh migration in the runner, so no secret can be in it. The PR gets one
comment with the link, edited on later pushes.

The host keeps `latest/` and open PRs, ~25 MB each. Every release deploy removes everything
else on the volume. A removed preview or a typo is a 404 whose page points at `latest/`.
The tab keeps nothing: the store is in memory and a reload is a fresh one.

The host is the fly app `dark-preview` (`deploy/preview/`: `fly.toml`, `Dockerfile`,
`nginx.conf`, `regen-index`, `gone.html`, `preview.css`): one nginx over one volume, a
directory per site, `_framework` kept as `.gz` only. `host.sh upload|remove|cleanup` works
from a laptop with a logged-in flyctl, which is how to test any of this without a PR. Every
preview shares that origin, so nothing with a cookie or a token may ever be served from it.

### Saying what a branch is for

By default the preview is this site, built from the branch. Three ways to point a reviewer at
the thing, cheapest first:

- A URL in the PR description: `cli.html?cmd=<argv>` runs a command and hands over the prompt
  (`?cmd=outliner`, `?cmd=view Stdlib.Eq`), `?fn=<name>` lands on an item, `eval.html?e=<expr>`
  is an expression box, `/?cmd=<argv>` prints the answer and nothing else.
- A `showcase.json` in `wwwroot/`, read by the branch's index page and by the directory:

      { "title": "Traits",
        "blurb": "Eq and ToString as stdlib traits; sum and product over Zero and One.",
        "landing": "cli.html?cmd=view Darklang.Stdlib.Eq",
        "tries": [ { "label": "sum over a trait", "url": "eval.html?e=List.sum [1L; 2L]",
                     "what": "resolves Zero and Add at the call" } ] }

  `landing` is where the directory card goes (default `cli.html`); `tries` become cards at
  the top of the branch's index. Delete the file before merging, or leave it if main should
  say the same.
- A page of your own in `wwwroot/`, and `landing` pointing at it. `site.js` gives it
  `dark.boot`, `dark.run(argv)` and `dark.invoke`; `eval.html` is the smallest example.

What a preview cannot show is what the browser cannot do. The tab has one thread and
cannot block, so anything that needs a real thread, a blocking wait, a socket or an OS
process is out: `serve` (no listening socket in a tab), OS processes, persistence across
reload, push to the relay. Say so in the PR rather than let a reviewer hunt for it.

## How it is put together

`Host.fs`: `Cli.Boot` fetches `data.db.br` into emscripten's in-memory filesystem, inflates it
through the runtime's own brotli decoder, warms SQLite and runs `growIfNeeded`; `Cli.RunCli`
builds the same execution state the native CLI builds and calls the entry point; `RunCommand`
runs one command with its output captured. `Browser` is the seam to the page: keys in
(`PushKey`/`PushPaste`), output out (`DrainOutput`, drained by the page on a timer), size
(`SetTerminalSize`). `BrowserBuiltins` replaces seven builtins (stdin, terminal size and
session info, clear); everything else is the real `Builtins.Cli`/`CliHost`/`Matter`.
`site.js` holds the pages' shared boot and helpers.

`RunAOTCompilation` is on for what ships, because it runs faster. To iterate, publish with
`-p:RunAOTCompilation=false`: about two minutes against four to eight, and the interactive prompt
works under the mono interpreter now that `driveToEnd` has no `task` state machine to block in. A
few big leaf assemblies stay interpreted in the AOT build; interpreting the BCL wholesale asserts
when compiled F# generics call into it. `InvariantGlobalization` drops ICU.

## The store, and the secret

`make-store.sh` copies this clone's `rundir/data.db`, deletes `config_v0` (relay url, push
cursors, and `sync.secret.<url>`, the write secret shared between a person's machines and
production), the sync tables and the trace tables, asserts `config_v0` is empty, scans the
bytes for a stored secret key, and only then writes the file. It never reads `~/.darklang` or
`cli-config.json`. `deploy.sh` checks the bytes again. Keep it that way.

It also sweeps orphan `package_blobs` (`pm-sweep-blobs`, against the snapshot) before the
final VACUUM. Those rows are live, so no VACUUM reclaims them, and on a clone that has been
worked in they are the biggest thing in the file. The sweep only sees `package_values`, which
is why the traces go first and why it is skipped when `user_data_v0` has rows.

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
