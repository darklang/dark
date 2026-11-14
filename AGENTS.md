# Darklang Monorepo

Darklang is a local CLI for managing a few repositories of your own code across your own
machines: you navigate and edit a tree of package items by keyboard, and the language they are
written in is Darklang. This repo holds the F# runtime (`backend/`) and the Darklang standard
library and CLI, written in Darklang itself (`packages/`).

It used to say "a language, editor and infrastructure in one", and that was true of a bigger
product than this tree now contains. Anything hosted, networked or shared either does not exist
here or does not exist yet: there is no relay, no sync, no accounts, no user databases and no
editor extension. Do not build on the assumption that this is a cloud platform.

## Quickstart

Everything runs inside this clone's dev container, and `./scripts/*` enter it for you. Run
from the clone's root. In-container paths: `/home/dark/app` is the clone.

    scripts/dev/start                     bring the container up; a no-op if it is. First run: minutes
    ./scripts/run-cli docs for-ai         language syntax, naming and workflow. Always read it

    scripts/dev/build                     build what changed, by content; blocks, nonzero on failure
    scripts/dev/status                    did it work, is one running, is the tree ahead of it
    ./scripts/run-cli <args>              the dev CLI, e.g. ./scripts/run-cli eval '1 + 1'

    scripts/dev/build                     reload packages: any .dark change reloads the whole set
    scripts/build/reload-packages         the reload alone, e.g. after truncating package-ref-hashes.txt
    scripts/dev/build backend/global.json a genuine full build: compile, migrate, reload

    ./scripts/run-tests --published                         every suite CI tests, at once, one summary
    ./scripts/run-fsharp-tests --find <text>                find an F# test, and the command to run it
    ./scripts/run-fsharp-tests --filter tests/Interpreter   one F# group (a path prefix); Debug is fine
    ./scripts/run-cli test run Stdlib.Float64                  Dark package tests, by whole name segments
    ./scripts/run-cli test run --force Stdlib.Float64          ... ignoring cached passes

    scripts/testing/gates list            the gates, one line each
    scripts/testing/gates <name>          one gate (first-day, package-tests, workbench-scm, ...)
    scripts/testing/gates all             every gate at once, against the Debug CLI

What CI runs, on `darklang/dark`. Locally, run what your change touches before the push that
opens or updates a PR:

    build    scripts/linting/shellchecker
             scripts/formatting/format check        fix with `format format`, then build again
             scripts/testing/test-build-planning.py
             scripts/dev/build --optimize
             scripts/build/build-release-cli-exes.sh
             ./scripts/run-fsharp-tests --published --split
             scripts/testing/gates first-day $(ls clis/darklang-*-linux-x64)
             CLI=$(ls clis/darklang-*-linux-x64) scripts/testing/gates ci --parallel
             CLI=$(ls clis/darklang-*-linux-x64) scripts/testing/gates package-tests

Is a pushed commit green? Ask `darklang/dark`, even for a branch pushed to a fork; nothing runs on
the fork, so asking it answers `pending` forever:

    curl -s https://api.github.com/repos/darklang/dark/commits/<sha>/status   # state + one row per job

`ci/circleci: ok-to-deploy` passes only when `build` did. On main the release-matrix jobs can report
there too, so read the rows, not just the overall `state`.

The traps that cost the most, one line each. The stories are in `docs/agent-traps.md`.

- An exit 0 is not a pass. `build --optimize --test` skips the tests when nothing it compiles
  changed; run `./scripts/run-fsharp-tests --published` and read its `EXPECTO!` line AND exit code.
- `run-fsharp-tests` does not compile. Build first, or the `.fs` change is not in the run.
- A test run that returns in 0s is a held lock, not a pass. Never wrap the runner in a host
  `timeout`; stop a run by pid inside the container.
- Wait on `scripts/dev/status`, never on `ps`/`pgrep`, and never send a build's output to
  `/dev/null` or through `| tail`: the refusal disappears and the exit code with it.
- `--optimize` builds Release INSTEAD of Debug, and the gates default to Debug: pass them `CLI=`.
  Once `--optimize` has built a change, only `scripts/build/clear-dotnet-build && scripts/dev/build`
  brings Debug up to it; a plain build says "nothing has changed".
- Order: build, publish, gates, THEN edit. A gate run after an edit fails on the edit's staleness.
- `Function <hash> couldn't be found` / `FnNotFound` on everything is a stale binary or seed, not a
  missing fn: `scripts/run-local-exec export-seed rundir/seed.db`, then rebuild.
- `PackageRefs: X hash not found`: `> backend/src/LibExecution/package-ref-hashes.txt &&
  ./scripts/build/reload-packages`. That reload reports 0 trait calls moved; believe the next one.
- A build re-authors the store from `packages/`: anything authored by hand is gone, and hand
  experiments in `rundir/data.db` break every gate that commits. Use a throwaway HOME.
- `backend/Build` is a container volume, so from the host it reads as EMPTY. Look from inside.
- Read the FIRST F# compiler error, not the last; an unclosed construct cascades as `FS0058`.
- A claim about main needs main: `git grep <pattern> upstream/main -- <path>`.
- Never conclude an absence from output you truncated or filtered. Re-run without the filter.
- Bracket `pgrep -f`/`pkill -f` patterns (`[s]erve`), and stop processes by pid, not pattern.
- `run-cli` reads stdin: inside a `while read` loop give it `< /dev/null`.
- A Dark call site is not checked until it runs; after a rename, rebase or deletion, sweep for it.
- A number you quote or gate on comes from an AOT build (`clis/<binary>.mode` says which), and is
  compared only with AOT. R2R and Debug are for running things, not for measuring them.

## Clones, containers and ports

We keep several clones checked out side by side, one per task or agent, each with its own
container. Nothing is shared between them: containers are matched to clones by directory,
each gets its own `backend/Build` volume, and each gets its own block of host ports.

`./scripts/*` all auto-enter this clone's container (see
`scripts/devcontainer/_assert-in-container`). If there's no container for the clone you're
in, they stop and tell you, rather than running in someone else's. Don't reach for
`docker exec` to work around that; run `scripts/dev/start` instead.

The container mounts the clone's `rundir` at `~/.darklang`, where every dark keeps its store,
so `rundir/data.db` is the dev store. A container made before that mount existed keeps its own
`~/.darklang` until it is recreated, even after the clone is rebased, and the gates would then
copy a stale `rundir/data.db` and fail with `FnNotFound`. The scripts refuse to run in such a
container and print the fix:

    scripts/dev/start --recreate
    scripts/dev/build backend/global.json   # the build index may already call the old store current

Scripts starting with `_` are meant to be called by other scripts. The rest are for you.

Ports. Inside the container, 9090-9099 are reserved for ad-hoc `darklang serve`. That's the same
in every clone.

On the host, each clone gets its own block: the first clone up gets 9090-9099, the next
9100-9109, and so on. `scripts/dev/start` prints yours. To get one later:

    scripts/dev/host-port        # host port for container 9090
    scripts/dev/host-port 9091   # ...for 9091

## Builds

Builds are explicit. Edit as many files as you like, then:

    scripts/dev/build            # what's changed since the last good build
    scripts/dev/build <paths>    # just these
    scripts/dev/plan             # what that would do, without doing it
    scripts/dev/status           # did it work, and is the tree ahead of it?

`build` blocks, prints the steps it chose, and exits nonzero if any fails. Measured on
an idle machine in August 2026, and again in October on a box at load ~20, where the
reload came out at 28-39s and a `.fs` change at 78-97s:

    .dark change    ~34s   the whole package set reloads, not just your file
    .fs change      ~74s   39s compiling, then that same ~34s reload
    nothing changed  0.2s  compared by content, so a `touch` or a branch switch is free

The cost and history of those numbers are in `docs/agent-traps.md`, under "Builds".

The container builds once when it starts. Rebuild-on-save is available but off by
default, because a five-file change under a watcher pays for five rebuilds, four of them
on half-finished states that produce real-looking failures:

    scripts/dev/watch            # foreground, Ctrl+C to stop
    scripts/dev/watch --stop

Don't infer build state from logs. `rundir/build-state.json` records what ran, what
failed and when, and `scripts/dev/status` reads it. Everything that used to grep
`packages.log` for "Exception" now asks that file instead, which is why `run-cli` can
tell you the tree has moved on rather than silently running a stale binary.

`scripts/build/clear-dotnet-build` is the escape when the planner will not rebuild something you
know is stale; `docs/agent-traps.md` says when you need it.

## Tests

    ./scripts/run-tests --published                   everything CI tests: the F# tests, the Dark
                                                      package tests and `gates ci`, all at once,
                                                      with one summary. Without --published, Debug
    ./scripts/run-fsharp-tests                        the F# tests, a few minutes
    ./scripts/run-fsharp-tests --published --split    the F# tests, faster: the one-at-a-time
                                                      tests run in a second process at the
                                                      same time as the rest. What CI runs
    scripts/dev/build --optimize --test               the same, published and much faster
    ./scripts/run-fsharp-tests --groups               the tree, with counts
    ./scripts/run-fsharp-tests --groups Interpreter   just that part of it
    ./scripts/run-fsharp-tests --find mergeFavoring   what matches, and how to run it
    ./scripts/testing/gates package-tests             the Dark package tests (`dark test run`)
    ./scripts/testing/test-build-planning.py          tests of the build itself
    ./scripts/testing/gates list                      the gate scripts, one line each
    ./scripts/testing/gates <name>                    one gate (first-day, workbench-scm, ...)
    ./scripts/testing/gates ci                        the subset CI runs, each bounded by 5m. Serial;
                                                      `--parallel` locally
    ./scripts/testing/gates all                       every gate except gates-are-clean, the slow
                                                      meta-gate that re-runs the rest itself. All at
                                                      once; package-tests sets the wall time (about
                                                      6m, measured in November 2026 at load ~30).
                                                      `--serial` for the old way
    ./scripts/perf/gate --published                   every perf reading vs budget; NOT in CI
    ./scripts/perf/gate                               the same, debug: reports, does NOT gate
    scripts/perf/measure-release <tag>                every perf number for one release, on the host
    scripts/perf/track --history H --out-dir D        new releases into a history, charted, printed
    ./scripts/perf/suite                              seven workloads, allocation per iteration

Find what you want before guessing at a filter: `--groups` and `--find` need no
database and no package reload, and print the exact command for what they found.

Run the WHOLE suite published. Debug compiles F# with optimisations off, and the interpreter
the suite spends its time in is F#, so the suite takes several times longer in Debug than the
extra build costs. `--optimize` builds Release INSTEAD of Debug, so `run-cli` will say the debug
tree is behind, and a plain build will not catch it up (see "After `--optimize`, nothing will
rebuild the debug tree" in `docs/agent-traps.md`). While iterating on one group, stay in Debug -- a
filtered run is seconds either way.

A Release Cli embeds `rundir/seed.db`, so `--optimize` exports it from the dev store first when
it is missing or older than the store (under a second, with the Debug LocalExec the container's
first build left behind). CI exports its own. By hand it is
`scripts/run-local-exec export-seed rundir/seed.db`.

## Directories

    backend/src/          # F# source
      LibExecution/       # execution engine
        Host/             #   the checked host boundary: the only code that may
                          #   touch the OS (enforced by tests/hostBoundary)
      LibParser/          # parser
      LibDB/              # package DB, branches, SCM ops, SQLite
                          # plumbing (LibDB.Sqlite), tracing recorder
      Builtins/           # Cli, CliHost, Http.Client, Http.Server, Language,
                          # Matter, Pure, Random, Time
    packages/darklang/    # .dark files
      cli/                # the CLI app: registry, loop, workbench, etc.
      scm/                # SCM library (branches, merge, conflicts, propagation, packageOps)
      stdlib/             # standard library
        cli/stdin.dark    #   reads keys
        cli/tui/          #   paints: view types, frame diffing, terminal session
        cli/ui/           #   composes: widgets, layout, the palette
      tests/              # all package tests, one folder per area (stdlib/, language/,
                          # scm/, cli/, ...); module names follow what they test
                          # (`Darklang.SCM.Tests`), so `dark test run Darklang.SCM` runs one area
    backend/migrations/   # schema/, the from-scratch shape; changes to an existing store go in
                          # LibDB/Releases.fs
    rundir/logs/          # log files
    scripts/dev/          # start, build, plan, status, watch, host-port
    scripts/build/        # the build itself; `_` ones are called by other scripts
    scripts/perf/         # perf tools, and workloads/ for the scripts they run
    scripts/testing/      # gates (the one runner) + _gates-* (the gates), fixtures/, hand tools
    benchmarks/           # the committed benchmark record, rendered to results.md
    scripts/              # everything else
    later/                # NOT the product; see below. Mirrors the repo paths it came from.

## later/

A root-level directory holding code that is not part of the product. Nothing in it is built,
loaded, tested, linted or formatted: the F# is out of `backend/fsdark.sln`, the `.dark` is outside
`packages/` so the reload never sees it, and seven separate tools prune it (`_buildplan.py`,
`_dev.py`, `format`, `shellchecker`, `yamllinter`, `.fantomasignore`, `.dockerignore`). It is
allowed to rot. `later/README.md` is the whole story and the only place that needs to tell it.

What it means for you: a name you grep for may exist only in there, in which case it is not a
caller, not a definition and not evidence. `--include=*.dark` over the whole repo will find it.

## Logs (rundir/logs/)

    cli.log              # CLI runtime issues
    packages.log         # .dark loading from disk
    build.log            # the last scripts/dev/build
    watch.log            # a detached scripts/dev/watch
    post-start.log       # what the container did on startup
    migrations.log       # migrations, if recently changed

Logs are for reading when something went wrong. For "did it work", use
`scripts/dev/status`.

## Adding a builtin (F#)

Pick the right `Builtins` subproject: `Pure` (pure stdlib), `Matter` (package store,
traces), `Language` (reflection, parser, language tools), `Http.Server`, `Http.Client`,
`CliHost` (eval, script entry), `Cli` (file, terminal, other side effects), `Time`,
`Random`. Add the fn to the `fns` list in the relevant `Libs/<module>.fs`, save, wait for
the rebuild.

A builtin that touches a scoped OS resource goes through the host boundary, not directly to `System.IO`/`System.Net`: add an `Operation` in `HostTypes.fs`, implement its check and execution in `Host.fs`, and call `PermissionCheck.performHost state vm op`. If a resource cannot be scoped honestly, classify it as `Native`. `tests/hostBoundary` enforces this.

Return structured Dark values (enums, records), not pre-rendered strings or string tags.
Dark-side code formats for display. Don't build `"superseded-by:<hash>"` in F# for a Dark
caller to parse; return the `DeprecationKind` enum. F#-side stringify is only right when
the value genuinely has no Dark-side type, which is rare.

Then wrap it, once. `tests/builtin` asserts both halves of that: a builtin with two or
more `Builtin.x` references anywhere in `packages/` fails, and so does one with no Dark
caller anywhere in the repo. So give each builtin exactly one Dark wrapper -- a stdlib
or CLI fn that names it, types it and documents it -- and route callers through the
wrapper. The wrapper is where the raw builtin's `List<'a>` becomes `List<TraceSummary>`.
The allowlists in `backend/tests/Tests/Builtin.Tests.fs` are for cases that genuinely
can't work that way. `multiUseAllowlist` is empty and `unusedAllowlist` has one entry;
before adding to either, check whether a wrapper already exists that the new caller has
simply not been pointed at, which is what a second `Builtin.x` reference usually means.

The shared kernel StdLib is an intentional exception: private `syscallX` and
`runtimeX` primitives are called directly from existing modules' `__` helpers.
Do not add forwarding-only syscall/runtime packages to satisfy the reference-count
rule. Every primitive still needs a real Dark caller; unused primitives are removed.

## Adding a CLI command (Darklang)

1. `packages/darklang/cli/<name>.dark`
2. Implement `execute`, `help`, `complete`
3. Register in `Registry.allCommands` in `cli/registry.dark`, with what it accepts on the command
   line (`cli/accepts.dark`): its flags, value flags and how many words. The dispatcher checks
   that before `execute` runs and refuses anything else with exit 1, so `execute` never sees an
   unknown flag or a word too many and does not check for them itself.

## Style

`///` for doc comments on types, DU cases and fns, in both F# and Dark. `//` for inline
notes. 85 columns for F#; Dark is 100.

`scripts/formatting/format` holds the F# side to it. **It is a pre-merge step, not a
per-commit one.** Run it once when the branch is being readied to merge; do not run it
between intermediate commits, and do not hand-apply what it would have done. Checking
formatting on every commit while more work is coming costs a cycle each time and changes
nothing a reviewer sees. Pre-merge does not mean optional: CI runs `format check` and fails on
it, so run `scripts/formatting/format format` before the push that opens or updates a PR, then
build once, because the formatter can change the program. Bare `scripts/formatting/format` prints
usage and exits 255. It reports `.dark` as `ignored` unless given `--dark-only`, which runs
`dark format` over `packages/`. Nothing checks `.dark` formatting.

Don't take a Bool that callers pass as a bare `true`/`false`; use a two-case enum named
for what it means (see #6150). `backend/tests/bool-flag-sites.txt` lists the calls left, and a
test fails on a new one.

## Where the rest is

    docs/agent-traps.md      every trap the quickstart lists, and the rest, with what each cost
    docs/agent-testing.md    sweeping the CLI, interactive testing, servers in tests, CI's
                             terminal, tests for code moved into Dark, debugging, performance
    docs/internals.md        the schema, SCM and branches, traits, measuring text
    docs/unittests.md        running and filtering both test suites
    docs/processes.md        the scheduler: processes, the loop, `Exec.spawn`, executions, `dark ps`
    docs/live.md             live programming: the host loop, `serve` following edits, live values
    docs/perf/playbook.md    how to do perf work here. Read it before starting

`dark docs processes` and `dark docs live` are the short forms of those two.

## Keep this file honest

If you learn something that cost 30+ minutes and isn't written down here, in `docs/agent-traps.md`
or in `docs for-ai`, add a short entry before you move on. These compound. A trap goes in
`docs/agent-traps.md`; it earns a line in the quickstart only if it keeps biting. When a tool is
fixed so a trap can no longer happen, delete the entry.

## Elsewhere

    ~/vaults/Darklang Dev    # team notes
    wip.darklang.com         # website WIP
    blog.darklang.com        # blog
