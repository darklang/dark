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
3. every command with valid arguments
4. every command with arguments a person would get wrong (missing, misspelled, wrong type)

Grep the output for `Encountered a Runtime Error`, `expects .* but got`, `No matching case
found`, `couldn't be found`. Shape 4 is the one people skip and it finds the most: a
fall-through arm answers plausibly instead of refusing, so `dark commits zzznope` listed
main's commits as though nothing had been asked.

Shapes 2 and 4 are automated in `CliTraces.Tests.fs`, driven off the command registry rather
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
      Builtins/           # Cli, CliHost, Http.Client, Http.Server, Language,
                          # Matter, Pure, Random, Time
    packages/darklang/    # .dark files
      cli/                # the CLI app: registry, loop, workbench, outliner, review
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
can't work that way; they're down to one entry each, so adding a third needs a reason
written next to it.

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

**Seeded package values are code, not captured authority.** A function reference stored in a seeded `val` must use `access = None` (`Seed.stripCapturedAccess`); otherwise it can decode as deny-all. Static approval treats opaque `EValue` bodies as incomplete.

**Compiled function references are serialized code.** `EFnName` becomes a serialized
`DApplicable(AppNamedFn …)` in package instructions. Keep code constants distinct from
captured values when changing applicable decoding; attenuating code references can lock
down the entire CLI.

## Interactive CLI testing

The interactive CLI (`run-cli` with no args) needs a real TTY. Use `expect`:

    ./scripts/run-in-docker expect scripts/testing/test-interactive.expect
    ./scripts/run-in-docker expect scripts/testing/test-workbench.expect

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

## Debugging

    Builtin.debug "label" value   # prints DEBUG: label: <repr> to stdout
    eval <expr>                   # test small pieces

## Style

`///` for doc comments on types, DU cases and fns, in both F# and Dark. `//` for inline
notes. 85 columns.

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
