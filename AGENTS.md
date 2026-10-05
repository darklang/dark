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

**And `scripts/dev/build --optimize --test` exits 0 when it decided not to run the tests.**
If nothing a build cares about has changed since the last successful one, it prints
"nothing has changed since the last successful build" and skips every step including
`backend_test`, so the zero means "did not run" rather than "ran and passed". Worse, it
does not re-record anything, so `scripts/dev/status` goes on showing whatever the PREVIOUS
run left there: a green exit code beside `failed: backend_test` at an older commit, which
is the shape that nearly got reported as a passing suite. A doc-only commit is enough to
put you in it.

To actually run the suite against a published build without compiling, use
`./scripts/run-backend-tests --published`, and read its own `EXPECTO!` line and exit code.
That is the third shape of the same failure in one night: a filter that matched less than
intended, a field that resolves at execution, and now a zero that means the step was
skipped. Whenever a number is suspiciously good or suspiciously fast, ask whether the thing
you think produced it actually ran.

    rundir/logs/build.log           # the last explicit build
    rundir/logs/packages.log        # .dark reload
    rundir/logs/watch.log           # a detached watcher

## Tests

    ./scripts/run-backend-tests                       all of them, a few minutes
    scripts/dev/build --optimize --test               the same, published and much faster
    ./scripts/run-backend-tests --groups              the tree, with counts
    ./scripts/run-backend-tests --groups Interpreter  just that part of it
    ./scripts/run-backend-tests --find mergeFavoring  what matches, and how to run it
    ./scripts/testing/test-build-planning.py          tests of the build itself
    ./scripts/testing/gates list                      the gate scripts, one line each
    ./scripts/testing/gates <name>                    one gate (setup, relay-routes, first-day, ...)
    ./scripts/testing/gates ci                        the subset CI runs, each bounded by 5m
    ./scripts/testing/gates all                       every gate except gates-are-clean, the slow
                                                      meta-gate that re-runs the rest itself
    ./scripts/perf/gate --published                   reference workload, allocation vs budget
    ./scripts/perf/gate                               the same, debug: reports, does NOT gate
    ./scripts/perf/suite                              six workloads, allocation per iteration
    ./scripts/perf/checks                             by-hand interpreter and error-message checks

Find what you want before guessing at a filter: `--groups` and `--find` need no
database and no package reload, and print the exact command for what they found.

Run the WHOLE suite published. Debug compiles F# with optimisations off, and the interpreter
the suite spends its time in is F#, so the suite takes several times longer in Debug than the
extra build costs. `--optimize` builds Release INSTEAD of Debug, so `run-cli` will say the debug
tree is behind until the next plain build. While iterating on one group, stay in Debug -- a
filtered run is seconds either way.

`run-backend-tests` does NOT compile. It reloads packages and runs the test binary that is
already there, so an `.fs` change you have not built yet is simply not in the run. It looks
exactly like a pass, and a red test you "fixed" stays red with its old message, which is the
tell. Build first:

    scripts/dev/build && ./scripts/run-backend-tests

The other trap: a filter that matches nothing used to be reported as `0 tests run - Success!`
with exit 0. It fails now. `docs/unittests.md` has the rest, including what the three filter
flags actually do and why they used to disagree with their own help text.

It fails by PRINTING OVER a success, though, which is its own trap for anyone reading the run
through a grep. Expecto still emits `EXPECTO! 0 tests run ... Success!`; the runner then explains
itself and exits 1. So a `grep -oE "[0-9,]+ tests run.*"` captures Expecto's "Success!" and
throws away both the override and the exit code, and an empty run reads as a passing one. Twice
in one session that produced a confident, wrong report about the tooling.

**Never report a conclusion about the tooling from matched text alone; show the exit code beside
it.** Same family as "don't infer build state from logs" above, one level out: that one is about
reading the wrong file, this one is about reading the right file too narrowly.

**And a claim about `main` needs `main`, not the tree you are standing in.** `git grep <pattern>
upstream/main -- <path>` costs nothing and settles it. A clone is a branch, and the interesting
branches here are the ones that change defaults: this one flips `DARK_CONFIG_TRACE_DETAIL` in
`config/dev` from `off` to `on`, adds a stored `trace.record` that beats the environment, and
renames flags that never existed upstream. Grep the working tree for any of those and you will
report your own branch as though it were the world. Three times in one session, each time while
deciding whether something was separable, which is the decision that depends on it most.

**A host-side `timeout` around `run-backend-tests` leaves the real test process running, holding
the lock.** `timeout 300 ./scripts/run-backend-tests ...` kills the wrapper on the host; the
`Tests` binary inside the container is not its child and keeps going. It still holds the suite
lock, so every run you start afterwards exits IMMEDIATELY, with no output and nothing resembling
an error. Measured: a filtered run that should take seconds returned in 0s, four times in a row,
and each one read as a pass.

So two rules, and the second is the one that saves you:

- Stop a run by pid from inside the container, never by killing the host wrapper and never by
  pattern (see the `pkill -f` entry below):

      source scripts/devcontainer/_container-for-clone
      C=$(container_for_clone "$PWD")
      P=$(docker exec "$C" ps -eo pid,args | awk '/[T]ests --colours/ {print $1}')
      for p in $P; do docker exec "$C" kill $p; done

- **Treat a zero-second test run as a lock, not a result.** An instant green is the most
  believable wrong answer the suite can give you, because it looks exactly like a fast pass on a
  narrow filter. If a run came back in under a second, check for a surviving `Tests` process
  before you believe a word of it.

### Sweeping the CLI after a change

### Processes and live programming

`docs/processes.md` is the scheduler: what a process is, the loop, reads in flight, `Exec.spawn`,
executions (resume, fork, export), `dark ps`. `docs/live.md` is live programming: the host loop,
`serve` following edits, the `Node` tree, live values. `dark docs processes` and `dark docs live`
are the short forms.

A Dark call site is not type-checked until it executes, so a rename or a type change across
`packages/` leaves holes a green suite cannot see. The bugs that get found here are found by
running commands, not by reading them, and always the same four ways:

1. every command BARE
2. every command with `--help`
3. every command with valid arguments, on main AND on a branch
4. every command with arguments a person would get wrong (missing, misspelled, wrong type)

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

**AOT or R2R, and why it decides a number.** `scripts/build/build-release-cli-exes.sh` can publish
either. They are about FIVE TIMES apart on startup (`dark eval 1L`: 40 ms AOT, 276 ms R2R,
measured), different sizes, and different allocation, so a number taken against one says nothing
about the other -- and nothing in the binary's filename says which you have.

The rule: **anything that produces a number, or gates a release, is AOT. Anything you are only
running is R2R.** So `perf/gate --published`, `perf/suite`, `perf/bench`, `gates first-day`, and
any figure you quote to a person or write into a document: AOT. Iterating on a bug that only
happens in a published build, or just checking the thing starts: R2R, and say which it was if you
quote a number from it.

The default is `--mode=auto`, which is AOT on every runtime that can do it and R2R on the ones
that cannot (Windows, from a Linux host), so the default is AOT here. `--mode=r2r` is the fast
path, about 40 seconds cheaper on the one-runtime default. `--mode=aot` refuses a runtime this
host cannot AOT-build rather than falling back, which is why it is not the default.

The build writes the mode to `clis/<binary>.mode`, `scripts/perf/budget.json` records the
`publishedMode` its numbers were taken in, and `perf/gate` REFUSES a binary from the other mode
rather than reporting a number that compares across them.

Decide with allocation, not time. Allocation for a fixed workload is far steadier than time and
doesn't care how loaded the box is; time drifts by more than most individual wins are worth. It is
not byte-identical though, and the store it runs against matters as much as the binary --
`docs/perf/playbook.md` has the measured noise floor. So
`gate` asserts allocation and only allocation, against `scripts/perf/budget.json`, and CI runs it
after the backend tests. When a change earns a lower number, lower the budget in the same commit
with `scripts/perf/gate --published --update`, or it stops being a gate and becomes a ceiling to
drift up to. Only the published build is budgeted: the debug budget was retired because most of
its overage was an interpreter hook the Release optimiser elides, so it tracked a configuration
nobody ships.

`suite` is the wider view and asserts nothing -- it is for seeing whether a change that helped one
shape of program hurt another. The six differ by more than an order of magnitude per iteration, so
tuning against any one of them proves little.

Two runs in the same clone destroy each other, so `run-backend-tests` takes a lock.
Two runs in different clones are fine; each has its own container, so its own PID
namespace, network and `rundir`.

Logs go to `rundir/logs/fsharp-tests.log`.

Everything perf lives in `scripts/perf/` (tools) and `docs/perf/` (writing):

    docs/perf/playbook.md    how to do perf work here, and the traps. Read before starting.
    docs/perf/roadmap.md     what's worth doing next, ranked, with measured vs estimated marked
    docs/perf/history.md     the numbers round by round, and facts not worth re-deriving

    scripts/perf/gate --published   the CI assertion: one workload, allocation against a budget.
                             Bare (debug) there is no budget, so it reports and exits 2 rather
                             than looking like a check that passed
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
      Builtins/           # Cli, CliHost, Http.Client, Http.Server, Language,
                          # Matter, Pure, Random, Time
    packages/darklang/    # .dark files
      cli/                # the CLI app: registry, loop, workbench, outliner, etc.
      scm/                # SCM library (branches, merge, conflicts, propagation, packageOps)
      sync/               # sync, on top of SCM: wire codec, import planning, the hosted relay
      stdlib/             # standard library
        cli/stdin.dark    #   reads keys
        cli/tui/          #   paints: view types, frame diffing, terminal session
        cli/ui/           #   composes: widgets, layout, the palette
    backend/migrations/   # schema/, the from-scratch shape; changes to an existing store go in
                          # LibDB/Releases.fs
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

Pick the right `Builtins` subproject: `Pure` (pure stdlib), `Matter` (DB, package store,
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

## Adding a CLI command (Darklang)

1. `packages/darklang/cli/<name>.dark`
2. Implement `execute`, `help`, `complete`
3. Register in `Registry.allCommands` in `cli/registry.dark`

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

## Gotchas

## Changing the schema

**A `migrations/schema/*.sql` file is frozen once it has merged to main.** Those files declare the shape
a FRESH store is born with, and they are read by every store that has ever been made from them. Editing
one after it has merged makes the same filename mean two different things depending on when you pulled.

So:

- A NEW TABLE goes in a NEW numbered file (`10-executions.sql`), never appended to a merged one.
- A file that has not merged yet is still yours: edit it in place until the PR lands.
- A NEW COLUMN on an already-merged table is the one exception, and it takes both halves:
  - declare it in the file that declares the table, for fresh stores, with a comment saying why;
  - carry it to existing stores with a step in `LibDB/Releases.fs`.
  There is no third option: a schema file only ever runs `CREATE TABLE` / `CREATE INDEX` /
  `INSERT OR IGNORE` statements (`Releases.applySchemaTables`), so a patch file cannot `ALTER`.

Adding a file changes the schema hash, which drops and re-folds the projection tables. That is cheap and
expected; canonical tables are never dropped, which is the reason a column needs the `Releases` step.

**Two branches adding an instruction both take the next tag.** An `Instruction` case needs a
number in three places: `Opcode.index` (RuntimeTypes.fs) and the read and write halves of
`LibSerialization/Binary/Serializers/RT/Instructions.fs`. When two branches each add a case,
both pick the same next number, and merging them is asymmetric in a way that bites: the READ
side conflicts, because both arms start `| 23uy ->`, while the WRITE side merges CLEANLY into
two arms that both `w.Write 23uy`. That compiles, passes every test that round-trips through one
process, and writes instructions that a later reader decodes as the wrong case. After any merge
or rebase that brings in a new instruction, grep both halves for a duplicated tag before
trusting the build.

**A build wipes anything you authored by hand, and the next measurement looks like a fast pass.**
`reload-packages` re-authors the store from the `.dark` files on disk, so a module you made with
`dark module /Demo.X -` is gone after any build, including the one inside
`scripts/dev/build --optimize --test`. That part is expected. The trap is what it does to timing
work: `dark eval 'Demo.X.f 16'` then fails in milliseconds, the trace it leaves is a FAILED one,
and timing `traces show` against it reports the startup baseline. A 0.18 s reading that should
have been 23 s looks like a win rather than a mistake. Check the eval's own output, or the
trace's `status`, before trusting any number taken against a hand-authored fixture. A fixture you
intend to keep belongs in a file the walkthrough pipes in (`docs/walkthrough-fixture.dark`), so
re-authoring it is one command rather than remembering what it was.

**`Defined N declarations` is not evidence your edit took effect.** Two different mechanisms
drop an edit while printing the same success line, so treat the line as "parsed", not "live":

- AN APPROVED NAME FOLLOWS THE APPROVED VERSION. `permissions approve` says so out loud
  ("the name follows this version"). Re-authoring does write the new body and does repoint the
  name, but a previously approved name keeps resolving to the approved version until you approve
  again. Editing bodies in `docs/browser-concurrency-demo.dark` and re-running
  `dark module /Demo.Browser - < ...` left every approved function on its OLD body; a
  `permissions approve Demo.Browser.overlapping` made the new one live immediately. A newly
  added name in the same file (a `val`) landed straight away, having no approval to pin it,
  which is what makes it confusing: part of the file takes effect and part does not.
- A DOC-ONLY EDIT NEVER LANDS AT ALL, for an unrelated reason: doc comments are deliberately
  excluded from the content hash (`Canonical.fs`), so the write produces nothing new to store.
  Written up in `notes/doc-comment-edits-dropped-2026-10-02.md`.

These are NOT the same bug, and the first one is not a bug at all once you know it. What they
share is the output.

The tell, and the useful part, because it works without knowing which one you are in: A COUNT IN
THE OUTPUT DISAGREED WITH THE FILE. A five-element list reported "10 reads", the old body's
`repeatUnsafe 10` still running. Put something countable in what you are editing (a length, a
name, a number in a string) and check the output moved. To test an edited module without
re-approving, load it under a fresh module path, which needs `permissions approve` per function
anyway since a fresh name has none.

**Never conclude an ABSENCE from output you truncated or filtered.** This is the one that gets
through, because it produces a confident negative, and a negative is the result nobody re-checks:
you look for a thing, do not see it, and move on. Every other harness mistake here produced a
wrong positive that something eventually contradicted. This family produces silence, and silence
agrees with whatever you already believed. Measured instances:

- `git remote -v | head -4` cuts alphabetically after `oceanoak` and before `origin`, so `origin`
  looks absent and you conclude the clone does not follow the remote convention
- `grep -c` counts LINES, not occurrences, so a count comes back wrong and the "correction" you
  then make is the error
- a prefix regex over-matches (`Stdlib\.[A-Za-z0-9]+\.toString` also matches
  `toStringISO8601BasicDate`), inventing call sites that do not exist
- `head -c N` on JSON truncates mid-structure, so a valid payload fails to parse and reads as a
  malformed response
- a context pattern like `grep -o '.\{260\}NEEDLE.\{120\}'` requires 260 characters BEFORE the
  match, so any hit near the start of a line silently fails to match and the needle reads as
  absent. Reported "zero occurrences" of a number that was in the file 15 times. Count with a
  fixed string first (`grep -c -F`), then go looking for context
- a pipeline hides the exit code of the thing you care about. `./scripts/dev/build ... | tail -40`
  reported exit 0 over a build that printed "Failed in 64.85s". Use `${PIPESTATUS[0]}`, or do not
  pipe
- `ls backend/Build/...` from the HOST returns nothing whether or not the file is there, because
  that path is a container volume. See the entry below. This is the one variant that does not
  announce itself: the others cut a real answer short, this one shows you an empty directory with
  no error at all

A HARNESS manufactures a negative the same way a filter does, and it is harder to see because
running a control FEELS like the check. Headless chromium never reached a prompt in the browser
build; the deployed site did not either, so the conclusion drawn was "not a regression", when the
available conclusion was equally "not a working instrument". A real browser with a person in
front of it gets a prompt in seconds. A control agreeing with your negative only rules out one of
the two explanations, and the instrument is the one nobody suspects.

**Bracket every pattern you hand to `pgrep -f` / `pkill -f`.** The pattern appears in your own
shell's command line, so an unbracketed one matches the process doing the matching. A waiter waits
on itself forever; a `pkill -f "serve Foo"` kills the backgrounded shell whose command line
contains "serve Foo", which looks exactly like the publish you just started dying for no reason.
`[s]erve Foo` matches the target and not the matcher. Note that bracketing is not enough on its
own: if the same shell also RUNS `serve Foo`, the bracketed pattern still matches it, so kill and
start belong in separate commands. That last clause caught me three more times in one night, each
time in a command that started a fixture server and tidied up after an earlier one, so take the
recipe rather than the rule: start background fixtures with `docker exec -d` (or `setsid` plus a
redirect, so the exec can return), and STOP them by pid read from `ps`, never by pattern:

    P=$(ps -eo pid,args | awk '/python3 \/tmp\/delay.py/ && !/awk/ {print $1}')
    for p in $P; do kill $p; done

`docker exec` without `-d` also hangs on a server that never exits, which looks like the command
failing rather than the server working. The same family: `2>&1` on a command whose stdout you are about
to parse as JSON merges a warning into the payload and the parse failure reads as a product bug.
All of these presented as product failures here and all of them were the harness.

**A wasm publish piped to `tail` looks like a 40-minute hang.** `dotnet publish` spawns MSBuild
worker nodes with `/nodeReuse:true`. They inherit stdout and outlive the parent, so the pipe never
closes, `tail` never gets EOF, and whatever error dotnet printed sits in its buffer unseen: the
child exits, the shell stays, and `rundir/wasm-repl` stays empty with no exit code and no
diagnostic. Run it to a FILE with node reuse off, and nothing else building in that container,
since a Release wasm publish rebuilds the same project references and two of them fight over
obj/bin:

    MSBUILDDISABLENODEREUSE=1 dotnet publish backend/src/Wasm/Wasm.fsproj -c Release \
      -o rundir/wasm-repl -nodeReuse:false > rundir/logs/wasm-publish.log 2>&1

A real publish is 4 to 8 minutes and leaves hundreds of MB in `backend/Build/obj/Wasm` within the
first couple of minutes; `obj` still tiny means it never got past restore, which is not slowness.
Progress markers in the log, in order: restore, `Wasm -> ... Darklang.Wasm.dll`, `AOT'ing N
assemblies`, then the emscripten link and `wasm-opt`, which is single-threaded and the long tail.
(Diagnosed by the wasm-preview session, which named it from a one-line symptom.)

**`backend/Build` is a container volume, so from the host it reads as EMPTY.** `ls`, `du` and
`find` against it from the host return nothing at all, with no error, whether or not the binary is
there. Everything here is driven through `./scripts/*` from the host, which is exactly the habit
that leaves you inspecting host paths, and this one lies. To look at build output, look from
inside:

    source scripts/devcontainer/_container-for-clone
    docker exec "$(container_for_clone "$PWD")" \
      ls -la /home/dark/app/backend/Build/out/Cli/Debug/net10.0/Cli

That helper exists for this and is meant to be sourced; its own comment explains why the
`/home/dark/app` mount source is the only key that holds (names vary, the `local_folder` label is
empty on some containers, and `docker ps --last 1` sorts by creation time, which says nothing about
which clone you are in). `workspace-tools/clones` shows a container's STATE and ports, not its name.

Cost of not knowing: twenty minutes on a missing-binary theory for a binary that was sitting there
the whole time, immediately after a control had just saved me from a different wrong theory.

**After `--optimize`, nothing will rebuild the debug tree.** `scripts/dev/build --optimize` builds
Release INSTEAD of Debug, and `--help` says the debug tree is left behind "until the next plain
build". A plain build does not do it. The build index tracks SOURCES, not outputs, so with no `.fs`
change it reports "nothing has changed since the last successful build" and stops. Naming paths
does not do it either: "1 path(s) given, but none of it changes what gets built". `dev/build` does
force a full build when binaries are MISSING, but after `--optimize` the Debug binary is not
missing, it is merely from whatever commit built it last, so the check passes and you are left with
a Debug CLI and a package store that disagree about which commit they came from, silently. The
supported escape, whose own comment explains the asymmetry:

    scripts/build/clear-dotnet-build && scripts/dev/build

What holds that state is `rundir/build-index.json`, which is why neither a plain build nor
named paths can talk the planner out of it: both ask the index, and the index says the
sources are current, because they are. It is the OUTPUT that is stale and nothing tracks
that.

That wipes Release too and costs a full rebuild, so do not reach for `--optimize` unless you are
about to run the whole suite and then stop.

If a search comes back empty and you are about to act on the emptiness, re-run it without the
filter. If a harness reports that nothing happened, reproduce it by hand once before you write it
down.

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

**A change to what the test harness grants does not reach an existing `rundir`.**
`CliTestHarness` seeds its policy with `seedInstanceIfMissing`, into
`rundir/test-policy`, which persists. So adding a capability to the harness is a no-op
on any clone whose rundir already has one, and the tests go on failing with
`permission denied by instance policy` while the code that grants it is right there.
CI never sees this, because CI's rundir is fresh. `rm -rf rundir/test-policy` and
re-run. Cost an hour of believing a correct fix had not worked.

**`run-in-docker` ignores stdin unless something's on it.** Fd 0 is forwarded through a `cat`
that needs an EOF, and an agent harness hands you a socket that never gives one, which used to
hang the call well past its timeout. If you pipe real input and it gets dropped, `DARK_STDIN=1`
forces it through.

**For deep tree walks in Dark, avoid recursion inside builtin callbacks.** For example,
`Stdlib.List.map children (fun child -> visit child)` starts a nested interpreter run at
each level and can fail with "Out of stack". Use direct recursion or an explicit work list.

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
part that distinguishes a new instance. The header's `who @ where` (` @ `) is the stable "it started" marker.

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

**A key pressed while a frame is painting is lost under `expect`.** The runtime queues a key that arrives
while nobody waits for the next `Key` subscriber, but the terminal `expect` drives does not: wait a beat
after the text you matched before sending the next key, or the key lands mid-render and is dropped. The
symptom is not "that key did nothing", it's the NEXT assertion timing out, which reads as a broken view.
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
