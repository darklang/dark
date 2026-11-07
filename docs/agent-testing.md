# Testing and measuring, for agents

How to check a change beyond the suite: sweeping the CLI, driving the interactive CLI, servers in
tests, what CI's terminal changes, tests for code that moved into Dark, debugging, and
performance. The commands for the suite itself are in `AGENTS.md` and `docs/unittests.md`; the
traps are in `docs/agent-traps.md`.

## Sweeping the CLI after a change

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

## Interactive CLI testing

**To ask what a view CONTAINS, call its row builder with `eval`. Drive the TUI only for the
keyboard and the screen.** `_workbench-views.expect` says it itself: the workbench paints as a
diff, and after two or three interactions the renderer stops re-emitting enough for a pattern to
catch, so asserting deeper measures the renderer rather than the view. That makes `expect` and
`tmux` the wrong instrument for content. The right one is the function that produces the rows:

    dark eval 'Darklang.Cli.Workbench.moduleRows(Darklang.SCM.Branch.mainBranchId,
                 Darklang.Cli.Packages.PackageLocation.Module ["Darklang", "Stdlib", "Int64"], false)
               |> Stdlib.List.map(fun r -> r.kind)'

That turns a question about pixels into a question about a list, and then into a COUNT you can
predict before you look. The Code view was drawing seventeen implementations and seventeen folders
of the same names where it should have drawn seventeen and none; nothing about the rendered frame
said so, and the count said so immediately.

**Check a count rather than reading, wherever you can turn a claim into one.** Both of the hardest
finds in the traits work came that way and neither came from careful reading: a PR description that
had been claiming `impl Divide for Int64 = let divide = Stdlib.Int64.divide` is an alias block,
caught by verifying "fourteen of seventeen" and finding `Stdlib.Int64.divide` does not exist; and
the duplicate rows above. It works on prose as well as on views: every number in a document is a
claim you can check, and the wrong ones travel in the company of other wrong ones.

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

### Telemetry from the interactive loop

When `packages/darklang/cli/telemetry.dark` is loaded, the interactive loop appends JSON lines to
`rundir/logs/telemetry.jsonl`, one per event:

    {"event":"preInput","ms":169,"ctx":{}}
    {"event":"readKey","ms":2014,"ctx":{"char":"h"}}
    {"event":"render","ms":33,"ctx":{"page":"MainPrompt"}}

The events, per loop iteration: `completionHint` (the ghosted suggestion), `render` (a page draw),
`preInput` (all the work before blocking on a keystroke), `readKey` (time waiting for the person,
so idle), `update` (state after a keystroke), `loopWork` (preInput + update, the non-idle total),
`commandExec` (on Enter), `tabComplete`, and `session_start` as a marker.

`preInput` is the one to read: it is the lag a person feels between pressing a key and seeing
anything.

    jq -r 'select(.event=="preInput") | .ms' rundir/logs/telemetry.jsonl

    jq -r 'select(.event=="completionHint") | .ms' rundir/logs/telemetry.jsonl \
      | awk '{s+=$1;n++} END{print s/n"ms"}'

`scripts/perf/view-telemetry.py` reads the same file and is easier for a whole run.

Three traps worth knowing when driving the loop under `expect`: give it about 3 seconds after
spawn, because packages load before the first prompt; `send "x"` sends one character and the loop
reads one at a time, so a sleep between them is what a real typing speed looks like; and the
Release and AOT builds behave differently enough from Debug that a timing conclusion from one says
nothing about the other.

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

For a command that just asks QUESTIONS (`dark conflicts walk`, `dark commit`), reach for `script`
before `expect`. It gives a pty and takes the answers on stdin, so there is no pattern matching
to get wrong:

    printf 'y\n' | script -qec "$CLI conflicts walk" /dev/null

`expect` is worth it only when you must react to what comes back. Used for a plain question list it
is easy to get subtly wrong, and the failure looks like the program hanging: an `expect` block with
no `eof` branch returns IMMEDIATELY when the spawned process ends, matching nothing and printing
nothing, so a script that exits 0 in silence means the process died, not that it hung. Give every
block an `eof` branch, and don't call `wait` after one has already fired.

A command that reads a line still reads a line under a pty: `Stdlib.Cli.Stdin.readLine` returns ""
on a bare Enter. If Enter appears not to advance a prompt, suspect the harness first.

## Standing up a server in a test

Two traps, both of which read as product bugs and are not. They are about background processes
in tests, and apply to anything a test starts, `dark serve` included.

**A stray server answers on the port you expected.** One left over from an earlier run holds the
port, your new one never binds, and every request gets the old one's answers. It looks exactly
like the thing under test being wrong. Do not reach for `pkill -f <name>`: it takes out every one
in the container, including one somebody is running by hand in tmux. Pick a port of your own,
refuse to start if it is taken, trap the pid you started, and check that pid is the one alive:

    PORT=$(( 9200 + RANDOM % 300 ))
    ss -ltn | grep -q ":$PORT " && { echo "port $PORT in use; re-run"; exit 2; }
    ... start it ... & RPID=$!; trap 'kill $RPID 2>/dev/null' EXIT
    kill -0 $RPID || { echo "the server we started is not running"; exit 2; }

**A bare `wait` also waits on the server.** It is a background job of the same shell and it never
exits, so `wait` after a couple of parallel requests hangs forever with no output and no CPU. Name
the pids: `wait $PA $PB`. A hung script with an empty log looks like a hung PRODUCT, so this one is
worth ruling out first.

## CI has a terminal, and that changes behaviour

CircleCI gives each step a pty, so stdin is a terminal there and a terminal alone says a
human is here. A command that then reads stdin blocks on a terminal nobody types into. Forever.

It presents as a hang, not a failure: no error, no output, and the step killed for
silence with nothing in the log naming the culprit.

So every prompt goes through `Stdlib.Cli.UI.Prompt.canAsk`, which also needs stderr to be a
terminal and none of `CI`, `DARK_NO_INPUT` or `--no-input` to be on. A TUI refuses only under
`DARK_NO_INPUT`/`--no-input`, since the workbench gates drive one on CI's pty.

Still pass `--yes` for anything unattended, and never test a destructive command without it.
Reproducing this class needs a real terminal, which `tmux` gives you (see "Interactive CLI
testing"); the same run from a pipe passes, which is why it survives local testing.

Three things make the next one findable, all in place:

    scripts/run-backend-tests        under CI, a heartbeat every 2min, so a live run
                                     never looks silent to CI's no-output timeout
    scripts/run-backend-tests --debug   names every test as it starts, which is how you
                                     find WHICH one is stuck
    .circleci/continue.yml           `store-ci-diagnostics`, so a failing step still
                                     uploads its logs, test reports and the databases of
                                     the checks that failed, and `timeout 20m` on the test
                                     step so it dies somewhere known

## Testing code that moved from F# to Dark

When a function moves into `packages/`, its F# test has to move with it, or it goes on asserting about a
copy nobody runs. Drive the real one from the test instead: `TestUtils.evalDarkExpr` parses a Dark
expression, builds an execution state and executes it; destructure the `Dval` it hands back.

    match! evalDarkExpr code with
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DInt64 n ])) -> ...
    | Error(rte, _) -> return failtest $"the Dark call raised: {rte}"

`Draft.Tests.fs` and `BranchOverlay.Tests.fs` both have a small set of these, one per result shape
(`Result<Int64,_>`, `Result<Unit,_>`, `List<String>`), plus a helper that spells a `BranchId` as Dark
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

    Stdlib.debug("label", value)  # prints DEBUG: label: <repr> to stderr
    eval <expr>                   # test small pieces

## Performance

**AOT or R2R, and why it decides a number.** `scripts/build/build-release-cli-exes.sh` can publish
either. They are about FIVE TIMES apart on startup (`dark eval 1`: 40 ms AOT, 276 ms R2R,
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
in the `build-cli` job, on the AOT binary that job builds. When a change earns a lower number,
lower the budget in the same commit with `scripts/perf/gate --published --update`, or it stops
being a gate and becomes a ceiling to drift up to. Only the published build is budgeted: the debug budget was retired because most of
its overage was an interpreter hook the Release optimiser elides, so it tracked a configuration
nobody ships.

`suite` is the wider view and asserts nothing -- it is for seeing whether a change that helped one
shape of program hurt another. The seven differ by more than an order of magnitude per iteration, so
tuning against any one of them proves little.

Everything perf lives in `scripts/perf/` (tools) and `docs/perf/` (writing):

    docs/perf/playbook.md    how to do perf work here, and the traps. Read before starting.
    docs/perf/roadmap.md     what's worth doing next, ranked, with measured vs estimated marked
    docs/perf/history.md     the numbers round by round, and facts not worth re-deriving

    scripts/perf/README.md   what each tool is, which workload it runs, and the recipes. Read
                             this one first; it is the map.
    scripts/perf/gate --published   the CI assertion: one workload, allocation against a budget.
                             Bare (debug) there is no budget, so it reports and exits 2 rather
                             than looking like a check that passed
    scripts/perf/suite       seven workloads, allocation and time per iteration
    scripts/perf/bench       repeatable CLI timing, with an A/B mode
    scripts/perf/http        a real server under concurrent load
    scripts/perf/keystroke   one keypress in the interactive CLI, end to end (needs a TTY)
    scripts/perf/alloc-profile   allocation by type name
    scripts/perf/fnprofile   per-package-fn time, ranked, hashes resolved to names
    scripts/perf/crosslang   the reference workload in node and python
    scripts/perf/view-telemetry.py  where a run's time went

The playbook is the one to read cold. Its recurring lesson: nearly all wasted effort came from
trusting a measurement nobody had checked.
