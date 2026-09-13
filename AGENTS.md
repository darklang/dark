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
    scripts/dev/typecheck <paths> # compile ONLY the projects owning these files
    scripts/dev/plan             # what that would do, without doing it
    scripts/dev/status           # did it work, and is the tree ahead of it?

`typecheck` is for the inner loop and produces nothing runnable. It IS recorded in
`build-state.json`, as `fsharp_typecheck`, which is not in `_buildstate.BUILD_ACTIONS` -- the same
mechanism a lint-only build uses, so the run is remembered but the last-success mark does not move
and `status` still says the tree has moved on. It refuses while a test run is in flight, for the
same reason `build` does: it writes assemblies, so it takes the file lock and kills the suite. That
looks like a hang, or worse like a truncated pass, because the log simply stops.

**A typecheck followed by a revert strands the binary, and `build` will not fix it.** Because
typecheck does not move the last-success mark, reverting leaves the source identical to the last
successful build while the assembly on disk came from the edit you just undid; `build` compares
source to last-success, sees nothing, and does nothing. `--force` does not help (it bypasses the
tests-running guard, not change detection). `touch` the file and typecheck again. You reach this by
editing, typechecking, and reverting, which is exactly what verifying a test's failure mode looks
like.

**Build cost is linear in the number of projects in the closure you ask for**, paid whether or not
anything changed. A no-op `LibExecution` build is a second or two; a no-op solution build is tens of
seconds, and the ordering between them is the project COUNT, not the depth. Touching
`LibExecution/RuntimeTypes.fs`, which everything depends on, costs more than a no-op solution build.
So building LESS is the only lever, and `typecheck` is it. Run `scripts/dev/plan` for the current
numbers rather than trusting any written here. Node reuse is not available (the SDK disables it under
`DOTNET_RUNNING_IN_CONTAINER`, and MSBuild nodes cannot outlive the `docker exec` anyway; measured,
no difference).

`build` blocks, prints the steps it chose, and exits nonzero if any fails. The shape of the cost,
which matters more than the seconds:

    .dark change      the whole package set reloads, not just your file
    .fs change        compiling, then that same whole-package reload
    nothing changed   near-instant; compared by content, so a `touch` or branch switch is free

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
    ./scripts/testing/gates list                      the gate scripts, one line each
    ./scripts/testing/gates <name>                    one gate (setup, relay-routes, first-day, ...)
    ./scripts/testing/gates ci                        the subset CI runs, each bounded by 5m
    ./scripts/testing/gates all                       every gate except gates-are-clean, the slow
                                                      meta-gate that re-runs the rest itself
    ./scripts/perf/gate                               reference workload, allocation vs budget
    ./scripts/perf/suite                              six workloads, allocation per iteration
    ./scripts/perf/checks                             by-hand interpreter and error-message checks

Find what you want before guessing at a filter: `--groups` and `--find` need no
database and no package reload, and print the exact command for what they found.

`run-backend-tests` does NOT compile. It reloads packages and runs the test binary that is
already there, so an `.fs` change you have not built yet is simply not in the run. It looks
exactly like a pass, and a red test you "fixed" stays red with its old message, which is the
tell. Build first:

    scripts/dev/build && ./scripts/run-backend-tests

The other trap: a filter that matches nothing used to be reported as `0 tests run - Success!`
with exit 0. It fails now. `docs/unittests.md` has the rest, including what the three filter
flags actually do and why they used to disagree with their own help text.

### Sweeping the CLI after a change

A Dark call site is not type-checked until it executes, so a rename or a type change across
`packages/` leaves holes a green suite cannot see. The bugs that get found here are found by
running commands, not by reading them, and always the same four ways:

1. every command BARE
2. every command with `--help`
3. every command with valid arguments, on main AND on a branch
4. every command with arguments a person would get wrong (missing, misspelled, wrong type)

`scripts/testing/sweep-shape3` is a hand-curated sample of shape 3, which
cannot be driven off the registry the way the others are.

Grep the output for `Encountered a Runtime Error`, `expects .* but got`, `No matching case
found`, `couldn't be found`. Shape 4 is the one people skip and it finds the most: a
fall-through arm answers plausibly instead of refusing, so `dark commits zzznope` listed
main's commits as though nothing had been asked, and `dark branch rename` created a branch
called "rename".

Shapes 2 and 4 are automated in `CliSurface.Tests.fs`, driven off the command registry rather
than a list, so a new command is swept the day it is registered. A command that must not be
RUN goes in `notSweepable` there, with the reason; a name in that list that is no longer
registered fails its own test, because an exclusion nobody revisits is how a sweep quietly
stops covering the thing it was written for.

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

Decide with allocation, not time. Allocation for a fixed workload is far steadier than time and
doesn't care how loaded the box is; time drifts by more than most individual wins are worth. It is
not byte-identical though, and the store it runs against matters as much as the binary --
`docs/perf/playbook.md` has the measured noise floor. So
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
        Host/             #   the checked host boundary: the only code that may
                          #   touch the OS (enforced by tests/hostBoundary)
      LibParser/          # parser
      LibDB/              # package DB, branches, SCM ops, user DB, SQLite
                          # plumbing (LibDB.Sqlite), tracing recorder
      Builtins/           # one assembly per platform: Pure, Time, Random, Cli,
                          # Http.Client, Http.Server, Language, Store, Data, CliHost
      Platforms/          # the catalog: the ONE place the full platform list lives
    packages/darklang/    # .dark files
      cli/                # the CLI app: registry, loop, workbench, outliner, etc.
      scm/                # SCM library (branches, merge, conflicts, propagation, packageOps)
      sync/               # sync, on top of SCM: wire codec, import planning, the hosted relay
      stdlib/             # standard library
        cli/stdin.dark    #   reads keys
        cli/tui/          #   paints: view types, frame diffing, terminal session
        cli/ui/           #   composes: widgets, layout, the palette
    backend/migrations/   # schema.sql, plus incremental/ for additive migrations
    rundir/logs/          # log files
    scripts/dev/          # start, build, plan, status, watch, host-port
    scripts/build/        # the build itself; `_` ones are called by other scripts
    scripts/perf/         # perf tools, and workloads/ for the scripts they run
    scripts/testing/      # gates (the one runner) + _gates-* (the gates), fixtures/, hand tools
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

Pick the right `Builtins` subproject: `Pure` (pure stdlib), `Store` (package store, ops,
branches, approvals), `Data` (user DB, raw SQLite, traces, accounts), `Language` (reflection,
parser, language tools), `Http.Server`, `Http.Client`, `CliHost` (eval, script entry), `Cli`
(file, terminal, other side effects), `Time`, `Random`. Add the fn to the `fns` list in the
relevant `Libs/<module>.fs`, save, wait for the rebuild.

Each of those assemblies IS a platform, and declares one at the bottom of its `Builtin.fs`. A new
assembly needs a `Platform` record there and a line in `Platforms/Sets.fs`; a new fn in an existing
one needs neither.

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

## Adding a CLI command (Darklang)

1. `packages/darklang/cli/<name>.dark`
2. Implement `execute`, `help`, `complete`
3. Register in `Registry.allCommands` in `cli/registry.dark`

## Platforms

A **platform** is a named, versioned bundle of builtins plus the effects they may perform, and a
statement of whether it needs a store on disk. Declared at the bottom of an assembly's `Builtin.fs`;
`Platforms/Sets.fs` is the catalog, and the only place the full list lives. They range from `Core`,
which is most of the builtins and reaches no effects, down to platforms with a single function.

Which ones a SESSION gets is a separate question from which ones the binary links: see
`docs/platforms.md`, and keep activation and permission apart when you touch either.

**A platform is a value, not an assembly, and the two do not have to line up.** Four assemblies ship
several platforms each, over subsets of their own `Libs`, sharing every line of implementation:

    Builtins.Cli   -> Terminal, Files, Process, Posix
    Builtins.Data  -> Db, Traces, Accounts, Sqlite
    Builtins.Admin -> Instance, Seed, Policy
    Builtins.Store -> Store, Authoring

That is the lever when a platform is too coarse to grant: splitting the record costs nothing at build
time, splitting the project costs a second or two on every build that reaches it. `Builtins.Store` splits on
DECLARED EFFECTS rather than file layout, because reads and writes interleave in the same files;
adding `PackageWrite` to a builtin there MOVES it between platforms, which is intended -- it changes
the fingerprint and forces a re-review.

    LibExecution/Platform.fs   the types, `PlatformSet.make`, the manifest fingerprint
    Platforms/Sets.fs          the catalog and the named sets an executable runs
    SealedHost/                an executable that links `Core` and nothing else. `scripts/run-sealed`.
                               Its confinement is a LINK-TIME fact: there is no `fileRead` in the
                               binary to deny. A test pins its four project references.

Read-only reports, for deciding what to narrow next:

    scripts/run-local-exec platforms            what ships, and the manifest fingerprint
    scripts/run-local-exec platforms tighten    every effect a platform reaches, and which
                                                builtins are responsible. An effect with ONE
                                                contributor is one function away from being gone.
    scripts/run-local-exec platforms wrappers   which package areas wrap each platform's builtins,
                                                and which single builtins strayed elsewhere
    scripts/run-local-exec platforms cost       what building the set allocates, per platform and
                                                per fn. Startup cost every invocation pays.
                                                `cost core` breaks Core down by library;
                                                `cost parts` says what one BuiltInFn is made of.
    scripts/run-local-exec platforms unused     builtins whose one Dark wrapper nothing calls.
                                                Reads builtin refs off the compiled RT instructions,
                                                so comments and strings cannot fool it.
    scripts/run-local-exec platforms unreachable
                                                builtins no package function in the repo reaches,
                                                by walking every listed fn's call graph. Very few,
                                                and the ones there are tend to be operators nobody
                                                types. It also prints where the call graph and the
                                                TEXTUAL scan (`PackageSurface.referencesBuiltin`,
                                                which the wrapper-home test rests on) disagree.
                                                Two methods for one question, with the
                                                disagreement printed rather than one of them
                                                picked, is the shape worth copying.
    scripts/run-local-exec platforms audit <Owner.Module>
                                                every function under that prefix reaching effects
                                                its module's baseline does not allow. Walks a
                                                call-graph closure per function, so it is slow and
                                                deliberate. The baseline is a written table in
                                                `PlatformReport.Audit`: opinionated on purpose,
                                                since one derived from the code can never be
                                                violated. Effects are per BUILTIN here, not per
                                                platform, which is what makes it a layering question
                                                rather than a granting one.
    scripts/run-local-exec platforms collapsible
                                                the numeric tower: how much of the builtin set is
                                                one table copied per numeric type, and how much of
                                                that already has a working polymorphic twin in
                                                `NoModule`. Per operation, with a presence matrix.
                                                Width conversions are not in it; `convert` and
                                                `tryConvert` already cover those.
    scripts/run-local-exec platforms needed <Owner.Module.fn>
                                                the MINIMUM platform set that fn needs, by walking
                                                its call graph. The question asked from the
                                                program's end rather than the binary's, and the
                                                install-time review surface. Says `INCOMPLETE` when
                                                a call leaves through a function-typed parameter,
                                                so treat those answers as lower bounds.

`dark platforms` and `dark platforms needed <Owner.Module.fn>` ask the same two questions from the
shipped CLI, over `Builtin.platformsInstalled` and `Builtin.platformsNeededBy`. Both are wrapped in
`packages/darklang/languageTools/platforms.dark`, and the rendering is Dark, in
`packages/darklang/cli/platforms.dark`.

The gap between what `needed` reports and what granting a platform costs is the thing to watch, and
the way to find them all at once is to sweep it over every command handler in
`packages/darklang/cli/registry.dark` and sort by how many effects granting costs. That sweep is
what found `Host` (a `dark clear` that calls one builtin was granted eight effects), `getBuildHash`
sitting in `Posix` while declaring no effects at all, and `Store` handing `package-write` to
thirteen commands that only read.

Five tests hold the model to reality, all in `Platform.Tests.fs`, and all of them want a deliberate
edit rather than a green light:

- `declaredEffectsMatchReality` holds every platform to `promisedEffects`, a written list of what it
  may reach, and fails in BOTH directions. An extra effect usually means a new builtin landed in the
  wrong assembly; a missing one is a promise to delete.
- `builtinsAreWrappedInTheirPlatformsHome` holds every builtin's Dark wrapper to its platform's
  declared area, with the current exceptions pinned by name in `knownStrays`. Deleting a stray that
  no longer exists is required, not optional.
- `requiresIsJustCore` pins the fact that every platform but `Core` requires exactly `Core`. A new
  entry has to be justified by naming the builtin that calls another platform's.
- `operatorDispatchedBuiltinsArePure` pins that every builtin in the infix table is pure. That USED
  to be the only thing making capability analysis sound; since `analysisVersion` 4 the analyzer sees
  operators, so this is now the second line rather than the only one. Kept because it states an
  intent (an operator should not act) and because it is what made the hole visible.
- `sealedHostLinksOnlyCore` reads `SealedHost.fsproj` and pins its reference list. It reads the
  PROJECT FILE rather than the compiled closure on purpose: the failure worth catching is somebody
  adding a line because they wanted one function out of it.

**Three ways to ask "does anything reference this builtin", with different blind spots.** Pick by
what you are actually asking:

    textual regex over `.dark`   `platforms wrappers`, wrapper-home test   blind to operators
    RT instructions              `platforms unused`                        sees operators
    PT call graph                `platforms needed`/`audit`/`unreachable`  sees them since v4

For a question about what code REACHES, use the instruction stream or the call graph. Use the text
only for questions about how code is WRITTEN, which is what the wrapper-home test asks (where does
the wrapper live) and why a regex is right there. `platforms unreachable` prints where the first and
third disagree, which in practice is operators and nothing else in either direction.

**`gates lsp-branches` leaks the LSP server, and it is not small.** The gate starts
`dark eval 'LspServer.runServerCli ()'`, which dies on its log write (see below) but leaves the
PROCESS alive reading a pipe nobody writes to, growing without bound: left overnight one will eat
**most of the machine's memory**. This box is Stachu's desktop, so after running that gate:

    ps -eo pid,rss,etime,cmd --sort=-rss | head
    # anything matching `Cli eval Darklang.LanguageTools.LspServer` is an orphan; kill it

Attribute it to your clone first (`cat /proc/<pid>/cgroup`, then match the container's volumes to
`dark_build_<clone>`), because every clone's processes are visible from the host.

**A policy denial and an I/O failure leave a builtin by different doors.**
`PermissionCheck.performHost` answers `Error failure` for an I/O failure, which Dark code matches on
like any `Result`, but `Host.Outcome.Denied` calls `raiseDenial` and unwinds. So a builtin returning
`Result<_, FileError>` looks as though every failure is catchable and denials are not. **Best-effort
host access is therefore not expressible today**: `let _ = appendToFile path text` reads as
best-effort and dies under a policy that says no. That is what kills the LSP server under
`gates lsp-branches`. Deliberate (a program should not probe the policy by catching denials), but
know it before writing anything that logs.

**`CallGraph.analyze` records the builtin behind an operator, as of `analysisVersion` 4.** It did
not before: `a + b` is an `EInfix` in ProgramTypes and lowers to `Builtin.add` only at RT, so every
static walk built on `CallGraph` -- `platforms needed`, `platforms audit`, and crucially
`LibDB.PackagePermissions`, which decides what a package fn is APPROVED for -- missed every operator.
It was sound only because every builtin in the infix table is pure, which nothing was checking;
`Tests.Platform.operatorDispatchedBuiltinsArePure` now does.

Bumping `analysisVersion` invalidates every stored approval fingerprint, and that is the POINT of
the constant rather than a cost of touching it: an analyzer that got better must not silently bless
a review taken against the narrower one. Bump it whenever completeness or reachability changes.

`PlatformSet.make` raises rather than composing when two platforms claim one builtin name, or when a
member's `requires` names a platform the set does not contain. Neither is caught below it:
`Builtin.combine` is last-write-wins over a `Dictionary`, so a collision there is silent.

Two things to know before touching this:

**The `pm` parameter is load-bearing.** `Platforms.Sets.everythingFor pm` threads a package manager
into `Store`'s builtins, and the tests pass an ephemeral one holding the declarations of the file
under test. Hardwiring `LibDB.PackageManager.pt` instead breaks the round-trip tests, and not with an
error: names resolve to something plausible and the printer emits `<hash:Tests..M>` where a type name
belongs.

**Compose a subset at the call site rather than linking the catalog.** `Platforms` references every
platform, including the ones whose SQLite and LibCloud dependencies a browser cannot link.
`Wasm/Repl.fs` builds its own `PlatformSet` from the two platform records it references plus one of
its own, and that is the pattern for any smaller executable.

**`Wasm` is not in `fsdark.sln`, so nothing you build will tell you that you broke it.** It is the
only executable that composes a SUBSET of platforms, which makes it the one that proves the split
works, and also the one that silently rots: adding a field to `Platform` compiles everywhere and
leaves `Wasm/Repl.fs` red until someone tries it months later. After changing `Platform`,
`RuntimeTypes` or anything else `Wasm` links, check it:

    ./scripts/run-in-docker dotnet build backend/src/Wasm/Wasm.fsproj -c Debug

It builds in Debug without the wasm-tools workload; only `dotnet publish` needs that, which is why
the project stays out of the solution.

`PlatformSet.fingerprint` is a content hash of every builtin's owner, identity, signature and
effects. Descriptions and parameter names are deliberately out of it: a doc fix must not invalidate
every cached package and every capability approval on the machine.


## SCM / branches

`package_ops` is canonical and append-only; an op's id IS its content hash. Everything else -- `locations`,
`package_functions`, `package_dependencies`, `propagation_policy` -- is a projection you can drop and
re-fold from the log. That is why a schema change to a projection costs nothing and a change to a canonical
table needs `LibDB/Releases.fs`.

The decisions live in Dark; F# does what only F# can do (parse, hash, serialize, execute, store bytes).

    packages/darklang/scm/     # the silos, each owning the SQL for its own tables
      packageOps.dark          #   package_ops: the log, and branches as overlays
      branches.dark            #   branches, op_branches, branch_name_bases; canMerge lives here
      commits.dark             #   commits
      conflicts.dark           #   conflicts, sync_bases; the base-agnostic detector
      constraints.dark         #   standing findings (outdated usages)
      propagation.dark         #   propagation_policy: pin and follow
      draft.dark               #   one answer to "what have I changed"
      storeHealth.dark         #   what can be wrong with the STORE

    LibDB/Lww.fs               # THE last-writer-wins rule. One place, on purpose; see below
    LibDB/PackageOpPlayback.fs # THE FOLD: ops -> projections. Read this first.
    LibDB/Inserts.fs           # author: mint the op id, insert, fold
    LibDB/Draft.fs             # discard / un-stage; the only code that edits `locations` outside the fold
    LibDB/Branches.fs          # branch tables + the merge MECHANISM (the gate is in Dark)
    LibDB/Propagation.fs       # the cascade: who depends on what moved
    LibDB/Releases.fs          # shape changes to canonical tables on existing stores

**Last-writer-wins lives in `LibDB/Lww.fs`, and asking it twice is the bug.** Two different things need
the rule: the fold decides which binding survives, and conflict recording decides which side to NAME as
the winner (`SCM.Conflicts.incomingWins`, in Dark, because the recording is in Dark). If those disagree, a
recorded conflict names a winner the fold did not pick and two instances converge on different content
with nothing to say so. The F# side has exactly one copy and the fold calls it. The Dark side is held to
it by matching tables in `Tests/Lww.Tests.fs` and `testfiles/execution/scm/lww.dark`: change one, change
both, and both test tables. Inverting either tie-break turns those red, which is checked.

**A branch is an overlay, not a copy.** Its ops live in the same table, stored `effective = 0` and tagged in
`op_branches`. A branch's package manager is main's with those ops layered on top.

**A synced store's own log holds ops it cannot read.** A peer on a different build sends ops this
binary's deserializer rejects. They are STORED and left unapplied deliberately, so a later build can
apply them, which means they sit in the local log where every local reader meets them. So "off the
wire it may be garbage, but the LOCAL log is ours, raise if it will not parse" is false. A reader
that raises dies on the first such op and stays dead (`dark propagate pin` with a raw
`BinaryFormatException`); a writer that deletes main's log and re-inserts what it read destroys them.
The rule: ONE decoder, it returns an Option, every reader tolerates, and no writer deletes what it
could not decode (`Inserts.wholeMainDeletes` excludes them by id).

**A pull cursor can point past ops you do not have.** The cursor is a position in the RELAY's log, and
nothing ties it to what you actually applied. `dark pull` then answers "Pulled 0 new op(s)" forever
while `dark sync status` correctly says you are thousands of ops behind. The client rewinds on its own
(relay-instance mismatch, or an empty bundle while the relay holds more ops than you), but the
CAUSE is still there, and it is narrower than it looks. `LocalExec` calls `LibDB.Purge.purge ()` before
a package reload: it wipes the store and re-authors every item from the `.dark` files on disk, and a
colleague's ops are not in those files. Measured: 28,372 ops before a build, 12,692 after. `LocalExec`
is dev tooling, so this is a dev-clone fact, not a product one, and we are deliberately not fixing it.
If you sync with someone from a dev clone, expect to lose their ops on every build and to re-pull them.

**Authoring looks like the same bug and is not.** `WipRefresh.refresh` runs on EVERY author, in any
binary, and it also deletes the whole non-branch log and re-inserts. The difference is where the
re-insert reads from: `Queries.getWipOps` reads the DATABASE, and it carries the same
`WHERE effective = 1 AND id NOT IN (SELECT op_id FROM op_branches)` as `Inserts.draftDeletes` and
`Inserts.wholeMainDeletes`. Delete and read cover the same set, so a peer's op goes out and comes back.
That symmetry is the whole safety property. Break it in either direction and authoring silently eats ops
that arrived over the wire: without the id exclusion the undecodable ops go, and without `effective = 1`
a self-hosted relay's hosted ops go.

**This is the trap.** `locations` has NO `branch_id`. A branch has no rows there at all, so any read that
goes straight to `locations` answers about MAIN while you are standing on a branch -- and it answers
plausibly, which is why it is hard to spot. Go through the overlay helpers in `SCM.PackageOps`, or read the
op log directly.

## Method, paid for

These came out of the platform work and none of them is specific to it.

**Every wrong number came from a script reading source text; every right one came from the running
system.** A grep whose name pattern excluded digits understated a builtin total by a third. Another
counted a builtin named in a doc comment as a stray somewhere else. The reports here read compiled
RT instructions and the live builtin set for exactly this reason. If a figure came from a grep over
`.fs` or `.dark` text, re-derive it before printing it.

**Measure before building.** A case for a build-time change rested on one number. Measuring first
produced a better result from three small edits and made the build step unnecessary.

**A check can cost more than the thing it checks.** A collision check allocated 0.2 MB per process
start to answer "no", turning its own change into a measured regression.

**Sweep, do not sample.** Asking a question one command at a time finds gaps one at a time. Run it
over every registered handler at once, sorted by what the answer costs, and read from the top.

**Read the artifact end to end, once.** Thirty-odd surgical edits went into a page, each verified
only by whether the page count changed. Reading it whole found defects no page count would have, on
every artifact it was tried on.

**Break each test on purpose and watch which one catches it.** A test that passes because it checks
nothing is worse than no test.

**Write the contract where the next person stands.** What a piece of work learned about a subsystem
belongs in that subsystem's module doc, not in a note nobody will open.

## Gotchas

**`Prelude` shadows `List.groupBy` to answer a `Map`.** `List.groupBy f xs` here is
`Map<'k, List<'a>>`, not F#'s `('k * List<'a>) list`, so the usual `|> List.map (fun (k, vs) -> ...)`
after it fails to typecheck with an error naming a `Map` you never wrote. Use `Map.map` (Prelude's,
which takes the value only). Same file redefines a few other List functions; check before assuming
FSharp.Core semantics. In particular **`List.head` answers an `Option`**, so `xs |> List.head |> snd`
does not compile and the error names an `Option` type you did not write, which reads as a mistake
somewhere else entirely. `List.last` is the same.

**PackageRefs stale hash.** `backend/src/LibExecution/package-ref-hashes.txt` isn't in git.
Empty is tolerated; non-empty with a missing key crashes at startup with "PackageRefs: X
hash not found". After adding a ref:
`> backend/src/LibExecution/package-ref-hashes.txt && ./scripts/build/reload-packages`

**Name resolution in test files.** `backend/testfiles/` is parsed with owner "Tests", so
`Darklang.*` names need full qualification or the `Stdlib.` shortcut. `Stdlib.Json.ParseError.toString`
and `Darklang.SCM.Branch.mainBranchId` resolve; `SCM.Branch.mainBranchId` doesn't. Impl:
`backend/src/LibParser/NameResolver.fs` and `packages/darklang/languageTools/nameResolver.dark`.

**A published artifact older than your tree fails like a broken product.** Every command dies with
"Function <hash> couldn't be found", because reloading packages regenerates the pinned ref hashes but does
NOT re-export `rundir/seed.db`, and a binary built on that seed can't produce the refs it was pinned to. It
only fails outside the source tree, since inside it the working store answers.
`scripts/build/check-seed-carries-refs` names it in one run; fix with
`scripts/run-local-exec export-seed rundir/seed.db` and rebuild. `gates first-day` and
`scripts/perf/gate --published` refuse an artifact older than the tree rather than
reporting on it.

**No `PACKAGE.` source prefix.** `PACKAGE.` is internal runtime/debug notation, not a
Dark namespace. Write `Stdlib.List.map` or `Darklang.Stdlib.List.map`, never
`PACKAGE.Darklang.Stdlib.List.map`; the same rule applies to search queries.

**Enum construction across modules.** Name the DU type, not just the module:
`ProgramTypes.Reference.PackageFn hash` works, `ProgramTypes.PackageFn hash` doesn't. In a
match arm the bare case is fine, since the matched value's type resolves it.

**Cross-module pipes.** Dark parses pipes greedily, so
`Stdlib.List.length xs |> Stdlib.Int.toString` raises "Pipe: LongIdent". Parenthesize the
left side.

**A literal inside a tuple pattern silently falls through.** `| Some(_, _, "propagation") ->`
against an `Option` of a 3-tuple parses and never matches; destructure first, then compare. Same
family as the wildcard trap below.

**`Stdlib.Dict.set` raises on an existing key.** Check-then-set, or use the merge helpers.

**A builtin's wrapper can lie about Int width.** A builtin returning `Result<Int, _>` wrapped as
`Result<Int64, _>` loads clean and fails at the first call. Match the builtin's declared types
exactly; the loader will not catch it.

**AOT disables System.Text.Json.** A path that is green on every dev-build test can die only in
the published binary. Publish before the gates, always; `gates first-day` exists for exactly this.

**`branch create` while standing on a branch creates a CHILD of that branch.** Switch to main
first if you meant a sibling.

**A wildcard doesn't match a multi-field DU case.** `| ExportPath _ ->` silently fails to match
`ExportPath of String * TextField.State`; you need `| ExportPath(_ext, _field) ->`. It's a
runtime error ("No matching case found") at the moment that case comes up, not a load
error, so it waits until you press the key that reaches it.

**`nonInteractive` means "run as `dark <cmd>`", not "no terminal".** `AppState.nonInteractive`
is set for any command given on the command line (entry.dark), so it's true even with a
perfectly good terminal in front of you. To decide whether a TUI can start, ask
`Stdlib.Cli.Tui.TerminalSupport.current ()`.

**Nested functions.** Only the fully annotated form works: `let helper (x: Int) : Int = ...`.
It desugars to a local lambda, can close over outer bindings and can call itself, but mutual
recursion between nested functions isn't supported. There's no `rec` keyword; top-level
functions are self-recursive already.

**A capped listing decides on stdout, but the TTY depends on stdin.** `Listing.shouldCap`
asks whether stdout is a terminal, and `run-in-docker` allocates a TTY only when *stdin*
is one. Authoring reads the declaration from stdin (`fn X - <<'EOF'`), so there's no TTY,
so `shouldCap` is false and nothing caps -- on exactly the path that produces the most
output. For a status report from an authoring command, cap unconditionally and point the
footer at the command that prints everything.

**A `val` holding a custom type can go stale.** `val forMain = forBranch ""` stores a *value*,
and that value carries the type identity it was built against. Reload packages and a caller can
be handed a `Context` the callee no longer recognises: `FnParameterNotExpectedType` on a
parameter whose type you never touched. Confusingly it reproduces only where the package set is
rebuilt (the LibExecution testfile harness) and not under `eval`. Call the function instead of
reaching for the `val` when the result is a custom type.

**Record update takes no type tag.** `{ state with field = v }` is right.
`MyType { state with field = v }` looks like F# but parses as function application.

**Permissions are runtime authorization, not OS isolation.** Host effects cross
`Host.perform` and are checked against policy; this does not confine a compromised
runtime or limit resources. OS sandboxing remains future work in `docs/permissions-todos.md`.

**Instance policy.** First run seeds `~/.darklang/policy/policies.bin` with a safe
default: package loading, computation, local storage, and printing work, while
filesystem, network, process, and environment access are denied. Missing or corrupt
policy data fails closed. `permissions allow all` is available for development.

**`run-in-docker` ignores stdin unless something's on it.** Fd 0 is forwarded through a `cat`
that needs an EOF, and an agent harness hands you a socket that never gives one, which used to
hang the call well past its timeout. If you pipe real input and it gets dropped, `DARK_STDIN=1`
forces it through.

**`let f () = <a literal>` rebuilds it on every call.** A nullary function whose body is a constant is
not a constant; `val` is evaluated once. If the body doesn't depend on anything, make it a `val`.

**Value bindings take no type annotation.** `let xs = [...]`, not `let xs : List<String> = [...]`,
which fails with "Value annotations are not supported". Function bindings do take them, and
nested functions require them.

**Package `val`s must not depend on the host.** They are evaluated once and stored;
trusted seed builds can therefore hide permission failures and freeze build-machine
state. Use a function for environment-dependent work. `PermissionEscape.Tests` checks
shipped values under guest policy.

**A forwarding wrapper has no frame.** `let map list fn = Builtin.listMap list fn`
is elided on both the first call and the cached `Apply` path. Both must establish
`vm.activeAccess`, the wrapper's package policy and its ceiling. A wrapper is thin only
when its signature exactly matches the builtin; `sameType` even compares type-variable
names, so tests must use the builtin's `'a` and `'b`. All paths share
`Interpreter.packageEntryAccess`.

**A permission test must make the value in one frame and run it in another.** Deferred
values keep their producer's restrictions and intersect them with the runner's. A broad
driver must call the producer and pass the value into a separate restricted runner;
otherwise the producer inherits the restriction and the test proves nothing. Load partial-
application references in the driver's value position so they are stamped broad first.

**Seeded package values are code, not captured authority.** A function reference stored in a seeded `val` must use `access = None` (`Seed.stripCapturedAccess`); otherwise it can decode as deny-all. Static approval treats opaque `EValue` bodies as incomplete.

**Compiled function references are serialized code.** `EFnName` becomes a serialized
`DApplicable(AppNamedFn …)` in package instructions. Keep code constants distinct from
captured values when changing applicable decoding; attenuating code references can lock
down the entire CLI.

## Scratch stores carry the real relay

A copy of `rundir/data.db` inherits its config: `sync.relay`, the write secret, the push cursors. So a
throwaway store is pointed at the PRODUCTION relay until you say otherwise, and one sync-shaped command
reaches it. `dark review pull` with no url does exactly that, quietly, because the url is a stored default.

    sqlite3 "$scratch/data.db" 'DELETE FROM config_v0;'   # all of it, not just current_branch

There are THREE config stores, and that line only clears one: `config_v0` in sqlite, `cli-config.json`
beside the db (instance id and name), and `$HOME/.darklang/capabilities.bin`.

That third one is keyed on **HOME**, not `DARK_CONFIG_RUNDIR`, so an isolated store does NOT isolate it. A
`dark caps grant ...` in a throwaway store writes the real grant for the whole container. Worse, the file's
ABSENCE is what makes the host permissive (`hostCaps` returns allCaps only while there is no file), so
creating one narrows every process under that HOME. One `caps grant` from a sweep turns ten suite tests
red with "capability denied: `sqliteQuery` needs file (read)", and the fix is to delete the file rather
than to grant more. Set `HOME` as well as `DARK_CONFIG_RUNDIR` when a test touches `caps`.

Clearing only `current_branch%` is the trap: it looks like isolation and leaves the relay wired up.

One consequence worth knowing: an isolated store usually looks like a FIRST RUN, and Home shows its welcome
PANEL instead of a row's detail, so a test waiting for anything a populated Home draws waits forever. Do not
anchor a workbench test on the greeting either way: "Welcome, <name>" is on every Home, and the panel is the
part that distinguishes a new instance. The context row (`instance:`) is the stable "it started" marker.

## Standing up a relay in a test

Two traps, both of which read as product bugs and are not:

**A stray relay answers on the port you expected.** One left over from an earlier run holds the port with
a secret you have forgotten, your new relay never binds, and every push comes back `HTTP 401: that write
secret isn't the one this relay expects`. It looks exactly like broken auth. Do not reach for
`pkill -f Matter.router`: it takes out every relay in the container, including one somebody is running
by hand in tmux. Pick a port of your own, refuse to start if it is taken, trap the pid you started, and
check that pid is the one alive:

    PORT=$(( 9200 + RANDOM % 300 ))
    ss -ltn | grep -q ":$PORT " && { echo "port $PORT in use; re-run"; exit 2; }
    ... start it ... & RPID=$!; trap 'kill $RPID 2>/dev/null' EXIT
    kill -0 $RPID || { echo "the relay we started is not running"; exit 2; }

**A bare `wait` also waits on the relay.** The relay is a background job of the same shell, and it never
exits, so `wait` after a couple of parallel pushes hangs forever with no output and no CPU. Name the pids:
`wait $PA $PB`. A hung script with an empty log looks like a hung PRODUCT, so this one is worth ruling
out first.

The sync-multi-instance gate (`scripts/testing/_gates-sync`) does both correctly and is the place
to copy from.

## Interactive CLI testing

**A key pressed while a frame is painting is lost.** In an `expect` script, wait a beat after the text you
matched before sending the next key, or the key lands mid-render and is dropped. The symptom is not "that
key did nothing", it's the NEXT assertion timing out, which reads as a broken view.
`fixtures/_workbench-scm.expect`
has a `press` helper for this.

The interactive CLI (`run-cli` with no args) needs a real TTY. Use `expect`:

    ./scripts/run-in-docker expect scripts/testing/fixtures/test-interactive.expect
    ./scripts/run-in-docker expect scripts/testing/fixtures/test-workbench.expect

`run-cli` with no args opens the WORKBENCH, so that second one covers the default experience:
switching views, resize, the too-small guard, and quitting cleanly. None of it is reachable from
the test suite -- the views render fine when called directly; what needs a terminal is the keyboard,
the alternate screen and SIGWINCH.

Telemetry lands in `rundir/logs/telemetry.jsonl`. Full guide: `docs interactive-testing`.

For poking at it by hand, or driving something `expect` would be awkward for -- an editor,
a pager, anything that takes over the screen -- `tmux` is more reliable:

    tmux new-session -d -s work -x 200 -y 50
    tmux send-keys -t work:0 'scripts/run-in-docker bash' Enter
    tmux send-keys -t work:0 './scripts/run-cli ...' Enter
    tmux capture-pane -t work:0 -p          # read the screen
    tmux kill-session -t work

Inside a pane, stdin IS a terminal, so `run-in-docker` allocates a TTY and everything that
needs one works: `dark edit` really opens `$EDITOR`, and you drive it with more `send-keys`
(`:%s/a/b/` then `:wq`, or `:cq` to exit non-zero and test the cancel path).

Poll `capture-pane` in a loop rather than sleeping between steps; a command that shells out
per invocation takes a second or more, and the pane is the only thing that tells you it is done.

For a command that just asks QUESTIONS (`dark sync setup`, `dark conflicts walk`), reach for `script`
before `expect`. It gives a pty and takes the answers on stdin, so there is no pattern matching
to get wrong:

    printf 'name\nhttp://localhost:9099\n<secret>\n' \
      | script -qec "$CLI sync setup" /dev/null

`expect` is worth it only when you must react to what comes back. Used for a plain question list it
is easy to get subtly wrong, and the failure looks like the program hanging: an `expect` block with
no `eof` branch returns IMMEDIATELY when the spawned process ends, matching nothing and printing
nothing, so a script that exits 0 in silence means the process died, not that it hung. Give every
block an `eof` branch, and don't call `wait` after one has already fired.

A command that reads a line still reads a line under a pty: `Stdlib.Cli.Stdin.readLine` returns ""
on a bare Enter. If Enter appears not to advance a prompt, suspect the harness first.

## CI has a terminal, and that changes behaviour

`Builtin.stdinIsInteractive ()` is false on a pipe and TRUE on a pty. CircleCI gives each
step a pty, so anything gated on "is a human here?" answers YES in CI, and a command that
then reads stdin blocks on a terminal nobody types into. Forever.

It presents as a hang, not a failure: no error, no output, and the step killed for
silence with nothing in the log naming the culprit.

So anything unattended passes `--yes` explicitly. Never rely on being detected as
non-interactive, and never test a destructive command without it. Reproducing this class
needs a real terminal, which `tmux` gives you (see above); the same run from a pipe
passes, which is why it survives local testing.

Three things make the next one findable, all in place:

    scripts/run-backend-tests        under CI, a heartbeat every 2min, so a live run
                                     never looks silent to CI's no-output timeout
    scripts/run-backend-tests --debug   names every test as it starts, which is how you
                                     find WHICH one is stuck
    .circleci/config.yml             `when: always` on store_artifacts, so a failing step
                                     still uploads rundir, and `timeout 20m` on the test
                                     step so it dies somewhere known

## Testing code that moved from F# to Dark

When a function moves into `packages/`, its F# test has to move with it, or it goes on asserting about a
copy nobody runs. Drive the real one from the test instead: `TestUtils.evalDarkExpr` parses a Dark
expression, builds an execution state and executes it; destructure the `Dval` it hands back.

    match! evalDarkExpr code with
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DInt n ])) -> ...
    | Error(rte, _) -> return failtest $"the Dark call raised: {rte}"

`Draft.Tests.fs` and `BranchOverlay.Tests.fs` both have a small set of these, one per result shape
(`Result<Int,_>`, `Result<Unit,_>`, `List<String>`), plus a helper that spells a `BranchId` as Dark
source. Copy those rather than writing a fourth variant.

Three things that will cost you a build cycle each:

- **Say what went wrong.** `Exception.raiseInternal "the Dark call raised" [ "rte", rte ]` prints the
  message and swallows the tag, so every failure looks identical. Put the error IN the message:
  `failtest $"the Dark call raised: {rte}"`. The error is usually a name resolution failure that names
  the exact missing module, and it is useless if it is swallowed.
- **The test file is parsed with owner `Tests`**, so the Dark you embed needs `Darklang.` in full. A
  find-and-replace over the F# call sites will also rewrite the module path inside those strings, and
  the result is a `ParseTimeNameResolution` at run time rather than a compile error.
- **A `let` needs the newline after it.** Building Dark source by string concatenation loses that
  silently and you get `VariableNotFound`. Write the expression without a `let`.

The point of all this is that a green F# build says nothing about Dark, which resolves names lazily.

## Debugging

    Builtin.debug "label" value   # prints DEBUG: label: <repr> to stdout
    eval <expr>                   # test small pieces

## Style

`///` for doc comments on types, DU cases and fns, in both F# and Dark. `//` for inline
notes. 85 columns, for both languages.

`scripts/formatting/format` holds the F# side to it; run it before you commit. It reports
`.dark` as `ignored`, so Dark is on you. Aim for 85 there anyway. Some existing Dark files
don't: `scm/packageOps.dark` and `sync/relay/protocol.dark` are written wider, and are not worth
reflowing just to close the gap.

## Measuring text, and what may go native

Pretty-printing lives in Dark; `Execution.fs` looks the printers up through `PackageRefs` and
calls them. **F# owns measurement, Dark owns layout.** How wide a character is, is a table lookup
(`Prelude/TextWidth.fs`). Where a line breaks is not.

The trap: a Dark loop pays a builtin call per character. `Stdlib.Pretty` measures once per `Doc`
node and is fine in Dark; the row fitters in `Builtins.Cli/Libs/TerminalText.fs` walked characters
and cost seconds per keystroke, which is why they're native.

"Characters bad, spans fine" is the wrong test, though. `Canvas.compose` only ever touches spans, a
couple of hundred per frame, and is still the largest thing in a view build -- because it runs about a
dozen interpreted operations on each one.

So count **operations per frame**, not what they operate on. A Dark loop over 250 items doing a dozen
calls each is 3,000 operations, and that is the number that decides whether something belongs in F#.

Widths are display cells, never `String.length`. `Stdlib.String.displayWidth` measures them and is
medium-independent; `Cli.Tui.Text` is only for rows that carry escape sequences.
`padEndToWidth`/`padStartToWidth` pad by cell, `padEnd`/`padStart` count characters and go ragged on
the first wide glyph.

## Keep this file honest

If you learn something that cost 30+ minutes and isn't written down here or in
`docs for-ai`, add a short entry before you move on. These compound.

## Elsewhere

    ~/vaults/Darklang Dev    # team notes
    wip.darklang.com         # website WIP
    blog.darklang.com        # blog
