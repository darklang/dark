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
    ./scripts/testing/test-setup                      `dark sync setup`, end to end, own rundir
    ./scripts/testing/test-lsp-branches                the `dark/*` branch surface, as an editor drives it
    ./scripts/testing/test-workbench-scm               the workbench SCM view with real work in the store
    ./scripts/testing/test-workbench-views             every workbench view, ditto, one session each
    ./scripts/testing/test-relay-routes                the relay's HTTP surface, given bad input
    ./scripts/testing/test-sync-hostile-relay          the sync/branch CLIENTS, given a relay that lies
    ./scripts/testing/test-sync-multi-instance         four instances, branches, review queues, agents
    ./scripts/testing/test-first-day                   a published binary, empty home, whole SCM workflow
    ./scripts/testing/test-gates-are-clean             that none of the above touch the shared dev store (slow)
    ./scripts/perf/gate                               reference workload, allocation vs budget
    ./scripts/perf/suite                              six workloads, allocation per iteration
    ./scripts/perf/checks                             by-hand interpreter and error-message checks

Find what you want before guessing at a filter: `--groups` and `--find` need no
database and no package reload, and print the exact command for what they found.

`run-backend-tests` does NOT compile. It reloads packages and runs the test binary that is
already there, so an `.fs` change you have not built yet is simply not in the run. It looks
exactly like a passing test, and a red test you "fixed" stays red with its old message,
which is the tell. Build first:

    scripts/dev/build && ./scripts/run-backend-tests

The other trap: a filter that matches nothing used to be reported as
`0 tests run - Success!` with exit 0. It fails now. `docs/unittests.md` has the rest,
including what the three filter flags actually do and why they used to disagree with
their own help text.

### Sweeping the CLI after a conversion

A Dark call site is not type-checked until it executes, so a rename or a type change across `packages/`
leaves holes a green suite cannot see. Every serious bug on the kernel-substrate branch was found by
running commands, not by reading them, and always the same four ways:

1. every command BARE
2. every command with `--help`
3. every command with valid arguments, on main AND on a branch
4. every command with arguments a person would get wrong (missing, misspelled, wrong type)

Grep the output for `Encountered a Runtime Error`, `expects .* but got`, `No matching case found`,
`couldn't be found`. Shape 4 is the one people skip and it found the most: a fall-through arm answers
plausibly instead of refusing, so `dark branch rename` created a branch called "rename".
`everyCommandSurvivesABogusArgument` in `CliTraces.Tests.fs` is shape 4, automated.

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

Decide with allocation, not time. Allocation doesn't care how loaded the box is; time drifts by more
than most individual wins are worth. So `gate` asserts allocation and only allocation, against
`scripts/perf/budget.json`, and CI runs it after the backend tests. When a change earns a lower
number, lower the budget in the same commit with `scripts/perf/gate --update`, or it stops being a
gate and becomes a ceiling to drift up to.

It repeats to a tenth of a percent only WITHIN A MODE, which this file used to state without the
qualifier. Measured on `steady.dark` in debug, same tree, same box, runs back to back: readings fall
into tight clusters that agree to 0.0003%-0.03%, and the clusters sit 0.3%-1.0% apart. One 8-sample
set spanned 1.05% end to end. So the repeatability is real and better than a tenth of a percent, and
it tells you which MODE you landed in rather than what the code allocates.

What picks the mode is unknown. It is NOT the shared dev store: an isolated copy of `data.db` shows
the same clustering (0.73% across 8 runs) as the shared one (0.31%), so don't spend time isolating
the store again. It is not obviously warmup either -- the modes interleave rather than settling.

Practical rule: one reading is not a measurement. Take 5+ of each side and compare the minimum or
the modal cluster, and treat any difference under ~0.7% as unproven until it survives that. Do not
`--update` off a single sample: it bakes in whichever mode you hit. The gate's 3% tolerance sits
above the whole band, so the gate itself is still meaningful.

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
      cli/                # the CLI app: registry, loop, workbench, outliner
      scm/                # SCM library (branch, rebase, merge, packageOps)
      stdlib/             # standard library
        cli/stdin.dark    #   reads keys
        cli/tui/          #   paints: view types, frame diffing, terminal session
        cli/ui/           #   composes: widgets, layout, the palette
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

Then wrap it, once. `tests/builtin` asserts both halves of that: a builtin with two or
more `Builtin.x` references anywhere in `packages/` fails, and so does one with no Dark
caller anywhere in the repo. So give each builtin exactly one Dark wrapper -- a stdlib
or CLI fn that names it, types it and documents it -- and route callers through the
wrapper. The wrapper is where the raw builtin's `List<'a>` becomes `List<TraceSummary>`.
The allowlists in `backend/tests/Tests/Builtin.Tests.fs` are for cases that genuinely
can't work that way; they're down to one entry each, so adding a third needs a reason
written next to it.

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

**This is the trap.** `locations` has NO `branch_id`. A branch has no rows there at all, so any read that
goes straight to `locations` answers about MAIN while you are standing on a branch -- and it answers
plausibly, which is why it is hard to spot. Go through the overlay helpers in `SCM.PackageOps`, or read the
op log directly. Three call sites had already drifted this way.

## Gotchas

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
`scripts/run-local-exec export-seed rundir/seed.db` and rebuild. `test-first-day` and
`scripts/perf/gate --published` now refuse an artifact older than the tree rather than
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

**`run-in-docker` ignores stdin unless something's on it.** Fd 0 is forwarded through a `cat`
that needs an EOF, and an agent harness hands you a socket that never gives one, which used to
hang the call well past its timeout. If you pipe real input and it gets dropped, `DARK_STDIN=1`
forces it through.

**`let f () = <a literal>` rebuilds it on every call.** A nullary function whose body is a constant is
not a constant; `val` is evaluated once. If the body doesn't depend on anything, make it a `val`.

**Value bindings take no type annotation.** `let xs = [...]`, not `let xs : List<String> = [...]`,
which fails with "Value annotations are not supported". Function bindings do take them, and
nested functions require them.

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
creating one narrows every process under that HOME. Running `caps grant random` in a sweep turned ten suite
tests red with "capability denied: `sqliteQuery` needs file (read)", and the fix is to delete the file
rather than to grant more. Set `HOME` as well as `DARK_CONFIG_RUNDIR` when a test touches `caps`.

Clearing only `current_branch%` is the trap: it looks like isolation and leaves the relay wired up. It has
happened twice here from ad-hoc scripts, both reads, both avoidable.

One consequence worth knowing: an isolated store usually looks like a FIRST RUN, and Home shows its welcome
PANEL instead of a row's detail, so a test waiting for anything a populated Home draws waits forever. Do not
anchor a workbench test on the greeting either way: "Welcome, <name>" is on every Home, and the panel is the
part that distinguishes a new instance. The context row (`instance:`) is the stable "it started" marker.

## Standing up a relay in a test

Two traps, both of which read as product bugs and are not:

**A stray relay answers on the port you expected.** One left over from an earlier run holds the port with
a secret you have forgotten, your new relay never binds, and every push comes back `HTTP 401: that write
secret isn't the one this relay expects`. It looks exactly like broken auth. Kill first, wait for the
kill, pick a port of your own, and check the process you started is the one alive:

    pkill -9 -f "Matter.router"; for _ in 1 2 3 4 5; do pgrep -f "[M]atter.router" >/dev/null || break; sleep 1; done
    PORT=$(( 9200 + RANDOM % 300 ))
    ... start it ... ; kill -0 $RPID || { echo "not ours"; exit 2; }

**A bare `wait` also waits on the relay.** The relay is a background job of the same shell, and it never
exits, so `wait` after a couple of parallel pushes hangs forever with no output and no CPU. Name the pids:
`wait $PA $PB`. This cost two debugging rounds in one session, twice, because a hung script with an empty
log looks like a hung PRODUCT.

`scripts/testing/test-sync-multi-instance` does both correctly and is the place to copy from.

## Interactive CLI testing

**A key pressed while a frame is painting is lost.** In an `expect` script, wait a beat after the text you
matched before sending the next key, or the key lands mid-render and is dropped. The symptom is not "that
key did nothing", it's the NEXT assertion timing out, which reads as a broken view. `_workbench-scm.expect`
has a `press` helper for this.


The interactive CLI (`run-cli` with no args) needs a real TTY. Use `expect`:

    ./scripts/run-in-docker expect scripts/testing/test-interactive.expect

Telemetry lands in `rundir/logs/telemetry.jsonl`. Full guide: `docs interactive-testing`.

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

## Debugging

    Builtin.debug "label" value   # prints DEBUG: label: <repr> to stdout
    eval <expr>                   # test small pieces

## Style

`///` for doc comments on types, DU cases and fns, in both F# and Dark. `//` for inline
notes. 85 columns, for both languages.

`scripts/formatting/format` holds the F# side to it; run it before you commit. It reports
`.dark` as `ignored`, so Dark is on you. Aim for 85 there anyway. Some existing Dark files
don't: `scm/packageOps.dark` and `matter/relay.dark` are written wider, and are not worth
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
