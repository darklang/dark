# Darklang Monorepo

Darklang is a language, editor and infrastructure in one. This repo holds the F# runtime
(`backend/`) and the Darklang standard library and CLI, written in Darklang itself
(`packages/`).

## Start the dev container first

Everything runs inside this clone's dev container. Nothing works until it's up:

    scripts/dev/start

Safe to re-run; it's a no-op if the container is already going. The first run pulls and
builds, so expect a few minutes. It also picks this clone's host ports and prints them.

Then read `./scripts/run-cli docs for-ai` for language syntax, naming and workflow. Always
read that one.

## Several clones at once

We keep several clones checked out side by side, one per task or agent, each with its own
container. Nothing is shared between them: containers are matched to clones by directory,
each gets its own `backend/Build` volume, and each gets its own block of host ports.

`./scripts/*` all auto-enter this clone's container (see
`scripts/devcontainer/_assert-in-container`). If there's no container for the clone you're
in, they stop and tell you, rather than running in someone else's. Don't reach for
`docker exec` to work around that; run `scripts/dev/start` instead.

Scripts starting with `_` are meant to be called by other scripts. The rest are for you.

## Ports

Inside the container, 9090-9099 are reserved for ad-hoc `darklang serve`. That's the same
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
an idle machine:

    .dark change    ~34s   the whole package set reloads, not just your file
    .fs change      ~74s   39s compiling, then that same ~34s reload
    nothing changed  0.2s  compared by content, so a `touch` or a branch switch is free

Note what the second line means: half the cost of any F# change is reloading packages
that usually didn't need reloading. It's unconditional because a `.fs` change *can*
alter the serialized package format, and there's no cheap way to ask whether this one
did. Narrowing it is the biggest remaining win in the loop, and it's entangled with
`package-ref-hashes.txt`, so coordinate before starting.

The container builds once when it starts. Rebuild-on-save is available but off by
default, because a five-file change under a watcher pays for five rebuilds, four of them
on half-finished states that produce real-looking failures:

    scripts/dev/watch            # foreground, Ctrl+C to stop
    scripts/dev/watch --stop

Don't infer build state from logs. `rundir/build-state.json` records what ran, what
failed and when, and `scripts/dev/status` reads it. Everything that used to grep
`packages.log` for "Exception" now asks that file instead, which is why `run-cli` can
tell you the tree has moved on rather than silently running a stale binary.

    rundir/logs/build.log           # the last explicit build
    rundir/logs/packages.log        # .dark reload
    rundir/logs/watch.log           # a detached watcher

## Tests

    ./scripts/run-backend-tests                       all of them, a few minutes
    ./scripts/run-backend-tests --groups              the tree, with counts
    ./scripts/run-backend-tests --groups Interpreter  just that part of it
    ./scripts/run-backend-tests --find mergeFavoring  what matches, and how to run it
    ./scripts/testing/test-build-planning.py          tests of the build itself
    ./scripts/perf/gate                               reference workload, allocation vs budget
    ./scripts/perf/suite                              six workloads, allocation per iteration
    ./scripts/perf/checks                             by-hand interpreter and error-message checks

Find what you want before guessing at a filter: `--groups` and `--find` need no
database and no package reload, and print the exact command for what they found.

The one trap worth knowing here: a filter that matches nothing used to be reported as
`0 tests run - Success!` with exit 0. It fails now. `docs/unittests.md` has the rest,
including what the three filter flags actually do and why they used to disagree with
their own help text.

### Performance

Everything perf lives in `scripts/perf/` (tools) and `docs/perf/` (writing):

    docs/perf/playbook.md    how to do perf work here, and the traps. Read before starting.
    docs/perf/roadmap.md     what's worth doing next, ranked, with measured vs estimated marked
    docs/perf/history.md     the numbers round by round, and facts not worth re-deriving

    scripts/perf/gate        the CI assertion: one workload, allocation against a checked-in budget
    scripts/perf/suite       six workloads, allocation and time per iteration
    scripts/perf/checks      by-hand interpreter and error-message checks
    scripts/perf/http        a real server under concurrent load
    scripts/perf/bench       repeatable CLI timing, with an A/B mode
    scripts/perf/alloc-profile   allocation by type name
    scripts/perf/crosslang   the reference workload in node and python

The playbook is the one to read cold. Its recurring lesson: nearly all wasted effort came from
trusting a measurement nobody had checked.

Decide with allocation, not time. Allocation for a fixed workload repeats to a tenth of a percent
and doesn't care how loaded the box is; time drifts by more than most individual wins are worth. So
`gate` asserts allocation and only allocation, against `scripts/perf/budget.json`, and CI runs it
after the backend tests. When a change earns a lower number, lower the budget in the same commit
with `scripts/perf/gate --update`, or it stops being a gate and becomes a ceiling to drift up to.

`suite` is the wider view and asserts nothing -- it is for seeing whether a change that helped one
shape of program hurt another. The six differ by more than an order of magnitude per iteration, so
tuning against any one of them proves little.

Two runs in the same clone destroy each other, so `run-backend-tests` takes a lock.
Two runs in different clones are fine; each has its own container, so its own PID
namespace, network and `rundir`.

Logs go to `rundir/logs/fsharp-tests.log`.

## Directories

    backend/src/          # F# source
      LibExecution/       # execution engine
      LibParser/          # parser
      LibDB/              # package DB, branches, SCM ops, user DB, SQLite
                          # plumbing (LibDB.Sqlite), tracing recorder
      Builtins/           # Cli, CliHost, Http.Client, Http.Server, Language,
                          # Matter, Pure, Random, Time
    packages/darklang/    # .dark files
      cli/                # CLI code
      scm/                # SCM library (branch, rebase, merge, packageOps)
      stdlib/             # standard library
    backend/migrations/   # schema.sql, plus incremental/ for additive migrations
    rundir/logs/          # log files
    scripts/dev/          # start, build, plan, status, watch, host-port
    scripts/build/        # the build itself; `_` ones are called by other scripts
    scripts/perf/         # perf tools, and workloads/ for the scripts they run
    benchmarks/           # the committed benchmark record, rendered to results.md
    scripts/              # everything else

## Logs (rundir/logs/)

    cli.log              # CLI runtime issues
    lsp.log              # LSP input/output
    packages.log         # .dark loading from disk
    build.log            # the last scripts/dev/build
    watch.log            # a detached scripts/dev/watch
    post-start.log       # what the container did on startup
    migrations.log       # migrations, if recently changed

Logs are for reading when something went wrong. For "did it work", use
`scripts/dev/status`.

## Adding a builtin (F#)

Pick the right `Builtins` subproject: `Pure` (pure stdlib), `Matter` (DB, package store,
traces), `Language` (reflection, parser, language tools), `Http.Server`, `Http.Client`,
`CliHost` (eval, script entry), `Cli` (file, terminal, other side effects), `Time`,
`Random`. Add the fn to the `fns` list in the relevant `Libs/<module>.fs`, save, wait for
the rebuild.

Return structured Dark values (enums, records), not pre-rendered strings or string tags.
Dark-side code formats for display. Don't build `"superseded-by:<hash>"` in F# for a Dark
caller to parse; return the `DeprecationKind` enum. F#-side stringify is only right when
the value genuinely has no Dark-side type, which is rare.

## Adding a CLI command (Darklang)

1. `packages/darklang/cli/<name>.dark`
2. Implement `execute`, `help`, `complete`
3. Register in `Registry.allCommands` in `cli/registry.dark`

## SCM / branches

    LibDB/Branches.fs         # branch CRUD, getBranchChain
    LibDB/Rebase.fs           # conflict detection, rebase
    LibDB/Merge.fs            # merge into parent
    LibDB/Inserts.fs          # ops take branchId
    LibDB/Queries.fs          # branch-aware SQL
    LibDB/PackageManager.fs   # pt(branchId) constructs PM
    Builtins/Builtins.Matter/Libs/PM/{Branches,Rebase,Merge}.fs

`PackageManager.pt` takes a branchId and pre-computes the branch chain for name resolution.
Items are global (content-addressed); locations (name bindings) are branch-scoped.

## Gotchas

**PackageRefs stale hash.** `backend/src/LibExecution/package-ref-hashes.txt` isn't in git.
Empty is tolerated; non-empty with a missing key crashes at startup with "PackageRefs: X
hash not found". After adding a ref:
`> backend/src/LibExecution/package-ref-hashes.txt && ./scripts/build/reload-packages`

**Name resolution in test files.** `backend/testfiles/` is parsed with owner "Tests", so
`Darklang.*` names need full qualification or the `Stdlib.` shortcut. `Stdlib.Json.ParseError.toString`
and `Darklang.SCM.Branch.mainBranchId` resolve; `SCM.Branch.mainBranchId` doesn't. Impl:
`backend/src/LibParser/NameResolver.fs` and `packages/darklang/languageTools/nameResolver.dark`.

**No `PACKAGE.` source prefix.** `PACKAGE.` is internal runtime/debug notation, not a
Dark namespace. Write `Stdlib.List.map` or `Darklang.Stdlib.List.map`, never
`PACKAGE.Darklang.Stdlib.List.map`; the same rule applies to search queries.

**Enum construction across modules.** Name the DU type, not just the module:
`ProgramTypes.Reference.PackageFn hash` works, `ProgramTypes.PackageFn hash` doesn't. In a
match arm the bare case is fine, since the matched value's type resolves it.

**Cross-module pipes.** Dark parses pipes greedily, so
`Stdlib.List.length xs |> Stdlib.Int.toString` raises "Pipe: LongIdent". Parenthesize the
left side.

**Nested functions.** Only the fully annotated form works: `let helper (x: Int) : Int = ...`.
It desugars to a local lambda, can close over outer bindings and can call itself, but mutual
recursion between nested functions isn't supported. There's no `rec` keyword; top-level
functions are self-recursive already.

**Record update takes no type tag.** `{ state with field = v }` is right.
`MyType { state with field = v }` looks like F# but parses as function application.

**Value bindings take no type annotation.** `let xs = [...]`, not `let xs : List<String> = [...]`,
which fails with "Value annotations are not supported". Function bindings do take them, and
nested functions require them.

## Interactive CLI testing

The interactive CLI (`run-cli` with no args) needs a real TTY. Use `expect`:

    ./scripts/run-in-docker expect scripts/testing/test-interactive.expect

Telemetry lands in `rundir/logs/telemetry.jsonl`. Full guide: `docs interactive-testing`.

## Debugging

    Builtin.debug "label" value   # prints DEBUG: label: <repr> to stdout
    eval <expr>                   # test small pieces

## Style

`///` for doc comments on types, DU cases and fns, in both F# and Dark. `//` for inline
notes. 85 columns.

## Keep this file honest

If you learn something that cost 30+ minutes and isn't written down here or in
`docs for-ai`, add a short entry before you move on. These compound.

## Elsewhere

    ~/vaults/Darklang Dev    # team notes
    wip.darklang.com         # website WIP
    blog.darklang.com        # blog
