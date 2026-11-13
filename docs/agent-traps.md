# Agent traps

The long form of the traps the quickstart in `AGENTS.md` lists one line each. Every entry here
cost somebody real time, and most of them present as a product bug when they are not. Grouped
by where you meet them. Testing techniques are in `docs/agent-testing.md`, and the store, SCM
and traits internals in `docs/internals.md`.

## Builds

**Half of every F# build is a package reload.** A `.fs` change pays its compile and then the
same reload of the whole package set a `.dark` change pays (timings in `AGENTS.md`, under
"Builds"). So half the cost of any F# change is reloading packages that usually didn't need
reloading. A `.fs` change *can* alter the serialized package format,
and there's no cheap way to ask whether this one did, so the build asks a narrower question:
whether anything the reload reads changed (LocalExec's build, the `.dark` files, the schema, the
settings; `scripts/build/_reloadstamp.py`), and whether any op was written to the store since. If
neither, it skips the reload and says so. An edit that leaves LocalExec's assemblies alone (the
Cli, the F# tests) costs its compile and no reload; anything else still reloads.

The reload includes a whole-tree type check for trait calls, split across the cores
(`TraitCalls.resolveIn`). It is about a quarter of the reload (the
`Resolved trait calls: ... in Ns` line), so what is left to win is reloading less, not
checking faster.

**`scripts/dev/build --optimize --test` exits 0 when it decided not to run the tests.**
If nothing a build cares about has changed since the last successful one, it prints
"nothing has changed since the last successful build" and skips every step including
`backend_test`, so the zero means "did not run" rather than "ran and passed". Worse, it
does not re-record anything, so `scripts/dev/status` goes on showing whatever the PREVIOUS
run left there: a green exit code beside `failed: backend_test` at an older commit. A doc-only commit is enough to
put you in it.

The same skip happens when the change was to `backend/testfiles/**` or to a `.dark` docs page:
`--test` plans by what gets COMPILED, so it reports "nothing has changed since the last
successful build" and skips the tests entirely, even though those files are exactly the test
inputs you edited. When the change was to testfiles or packages rather than F#, run the suite
directly.

To actually run the suite against a published build without compiling, use
`./scripts/run-fsharp-tests --published`, and read its own `EXPECTO!` line and exit code.
Whenever a number is suspiciously good or suspiciously fast, ask whether the thing
you think produced it actually ran.

**`scripts/dev/status` is the only thing that knows whether a build is running.** Waiting on a
host-side process pattern does not work: the build execs into the container, so `pgrep -f dev/build`
finds nothing while `status` still says `running`, and a waiter keyed on the pattern falls straight
through and reads the PREVIOUS build's result. That previous result is a timestamp, so it looks
current. Wait on `status`, not on `ps`.

The build can also REFUSE, correctly, which reads the same if you only check the exit code:
"Tests are running in this clone (pid N). Building now would overwrite the binaries they're
executing." It is `rundir/test.lock` with a liveness check; a refusal and a failure are
different things and only the message says which.

That liveness check is not fully reliable: a run killed mid-flight can leave `test.lock` naming a
dead pid, and the build goes on refusing. Check `[ -d /proc/<pid> ]` before believing the lock,
then `--force` or delete it.

And never send the build's own output to `/dev/null`. A refusal there is invisible, the next
`dev/status` answers from the LAST successful build, and that answer is a timestamp, so it looks
current. Compare the
timestamp against when you started, not against nothing.

**Change detection is by CONTENT over the whole repo, so `touch` does nothing, and deleting the
store does not make the build rebuild it.** `rundir/data.db` is not among the outputs
`missing_outputs()` watches, so deleting it to force a package reload gets "nothing has changed
since the last successful build" with no store on disk at all. Removing `rundir/build-state.json`
does not help either. To force a genuine full build, hand it the path the script itself uses for
one:

    scripts/dev/build backend/global.json

**A stale binary against a re-authored store fails as `FnNotFound` on a hash that exists
nowhere.** The hash is computed at run time, so grepping the tree AND querying `locations` both
answer "absent", which reads like a missing function rather than a mismatched pair. Every `eval`
fails, `1 + 1` included, and the error may not even render, because rendering it needs a second
unresolvable fn. Compare the two mtimes before theorising (in-container paths):

    stat -c %y backend/Build/out/Cli/Debug/net11.0/Cli
    stat -c %y rundir/data.db

If the binary is older than the store, that is the bug, whatever `dev/status` says. The full build
above relinks the two together.

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

**Read the FIRST compiler error, not the last.** F# reports an unclosed construct as a
cascade: one real error, then an `FS0058` "offside of context started at position (N,c)"
for every following definition. `tail` on the log shows you the end of that cascade,
which points at whichever definition came last rather than at the mistake. So sort the errors by line and read the top, or grep the log for the first
`FS0010`; the `(N,c)` an `FS0058` names is the thing that never closed.

**PackageRefs stale hash.** `backend/src/LibExecution/package-ref-hashes.txt` isn't in git.
Empty is tolerated; non-empty with a missing key crashes at startup with "PackageRefs: X
hash not found". After adding a ref:
`> backend/src/LibExecution/package-ref-hashes.txt && ./scripts/build/reload-packages`

**And the FIRST build after truncating that file reports a legitimate zero for trait
resolution.** Switching branches makes the hash file stale, the documented fix is to truncate it,
and the build that regenerates it runs the whole reload with the trait hashes still empty. So
`NumericTraits` maps no operator to any trait, the at-rest checker correctly finds nothing to
resolve, and the reload prints `Resolved trait calls: 0 op(s) moved` where the same tree prints
some thousands once the file is populated. Do not compare against a remembered figure: the count
moves with the tree. ZERO is the tell,
not any particular number. The second build is the one to believe. Two tells separate the
artifact from a real regression, and the duration is the clearer of them: the reload takes about
20 seconds instead of about 100, because the expensive whole-tree check is finding nothing rather
than not running.

When a number collapses, test the instrument against a known-good tree BEFORE reading any
diffs. A branch cannot regress against itself, so if both trees agree the cause is
environmental.

If you need to tell a real zero from this one, the three counts inside `Decode.report` in
`Builtins.Matter/Libs/PM/TraitCalls.fs` are what distinguish them: `rawItems` is what the checker
returned, `decoded` is what matched the item pattern, and `withResolutions` is what carried
anything. `rawItems=0` means the checker reported nothing, and three numbers in the thousands with a
non-zero `withResolutions` is a healthy run. The absolute values move with the size of the tree,
so read the shape rather than matching the figures.

## Published binaries and the seed

**A published artifact older than your tree fails like a broken product.** Every command dies with
"Function <hash> couldn't be found", because reloading packages regenerates the pinned ref hashes but does
NOT re-export `rundir/seed.db`, and a binary built on that seed can't produce the refs it was pinned to. It
only fails outside the source tree, since inside it the working store answers.
Fix it with `scripts/run-local-exec export-seed rundir/seed.db` and rebuild. `gates first-day`
and `scripts/perf/gate --published` refuse an artifact older than the tree rather than reporting
on it.

**`check-seed-carries-refs` answers about the TREE, not about an artifact.** It compares
`rundir/seed.db` against the ref file as it stands NOW, so it says "ok, N refs, all carried by
the seed" while a binary published ten minutes earlier cannot resolve its own refs: that binary
was pinned against a ref file the seed has since caught up with. Nothing short of running an
artifact tells you about THAT artifact, and the symptom is `Function <hash> couldn't be found` on
every command. So the order matters and is not interchangeable: reload, then export the seed,
then build, then publish. Re-exporting after a publish checks out clean and fixes nothing.

The build does catch it, in the one place that can: `Cli.fsproj` fails with
`error : rundir/seed.db cannot produce this binary's package refs`. Note the shape of that line,
because it defeats the obvious filter -- it is `error :` with no FS code, and the summary says
`Failed in 60.16s` rather than `FAILED`, so a `grep -E 'error FS|FAILED'` over the build output
matches neither and prints nothing. Ask `scripts/dev/status`, which reads `build-state.json` and
says `failedAction: backend_full_build`.

**AOT disables System.Text.Json.** A path that is green on every dev-build test can die only in
the published binary. Publish before the gates, always; `gates first-day` exists for exactly this.

**Operationally: publish, THEN `gates all`, never the other way round.** `first-day` refuses rather
than reporting when the published binary is older than something you changed, naming the file, so a
`gates all` run after an edit ends in `FAILED: first-day` that is the gate protecting itself and not
a result. The sequence that does not waste ten
minutes is `scripts/build/build-release-cli-exes.sh && scripts/testing/gates all`.

## Tests and gates

`run-fsharp-tests` does NOT compile. It reloads packages and runs the test binary that is
already there, so an `.fs` change you have not built yet is simply not in the run. It looks
exactly like a pass, and a red test you "fixed" stays red with its old message, which is the
tell. Build first:

    scripts/dev/build && ./scripts/run-fsharp-tests

The other trap: a filter that matches nothing fails the run. `docs/unittests.md` has the rest,
including what the three filter flags actually do.

It fails by PRINTING OVER a success, though, which is its own trap for anyone reading the run
through a grep. Expecto still emits `EXPECTO! 0 tests run ... Success!`; the runner then explains
itself and exits 1. So a `grep -oE "[0-9,]+ tests run.*"` captures Expecto's "Success!" and
throws away both the override and the exit code, and an empty run reads as a passing one.

**Never report a conclusion about the tooling from matched text alone; show the exit code beside
it.** Same family as "Don't infer build state from logs" (`AGENTS.md`, "Builds"), one level
out: that one is about reading the wrong file, this one is about reading the right file too
narrowly.

**And a claim about `main` needs `main`, not the tree you are standing in.** `git grep <pattern>
upstream/main -- <path>` costs nothing and settles it. A clone is a branch, and a branch can
change defaults and rename flags that main never had. Grep the working tree for those and you will
report your own branch as though it were the world, which matters most when deciding whether
something is separable.

**A host-side `timeout` around `run-fsharp-tests` leaves the real test process running, holding
the lock.** `timeout 300 ./scripts/run-fsharp-tests ...` kills the wrapper on the host; the
`Tests` binary inside the container is not its child and keeps going. It still holds the suite
lock, so every run you start afterwards exits IMMEDIATELY, with no output and nothing resembling
an error: a filtered run that should take seconds returns in 0s and reads as a pass.

So two rules, and the second is the one that saves you:

- Stop a run by pid from inside the container, never by killing the host wrapper and never by
  pattern (see the `pkill -f` entry under "Harness and shell"):

      source scripts/devcontainer/_container-for-clone
      C=$(container_for_clone "$PWD")
      P=$(docker exec "$C" ps -eo pid,args | awk '/[T]ests --colours/ {print $1}')
      for p in $P; do docker exec "$C" kill $p; done

- **Treat a zero-second test run as a lock, not a result.** An instant green is the most
  believable wrong answer the suite can give you, because it looks exactly like a fast pass on a
  narrow filter. If a run came back in under a second, check for a surviving `Tests` process
  before you believe a word of it.

Two runs in the same clone destroy each other, so `run-fsharp-tests` takes a lock.
Two runs in different clones are fine; each has its own container, so its own PID
namespace, network and `rundir`. That is a statement about CORRECTNESS, not about the box: several
clones publishing Release at once can run the machine out of memory and kill suite runs. The lock
protects a clone from itself and nothing protects the host.

Logs go to `rundir/logs/fsharp-tests.log`.

**A change to what the test harness grants does not reach an existing `rundir`.**
`CliTestHarness` seeds its policy with `seedInstanceIfMissing`, into
`rundir/test-policy`, which persists. So adding a capability to the harness is a no-op
on any clone whose rundir already has one, and the tests go on failing with
`permission denied by instance policy` while the code that grants it is right there.
CI never sees this, because CI's rundir is fresh. `rm -rf rundir/test-policy` and
re-run.

**The gates run against the DEBUG binary, so do not run them straight after `--optimize`.**
Every gate defaults to `backend/Build/out/Cli/Debug/net11.0/Cli`, and `--optimize` builds Release
instead of Debug, so `gates all` right after it fails gates that have nothing wrong with them: one
exits in a second with no CLI to run, and `first-day` refuses because the published artifact is
older than the tree. Build Debug first, or pass `CLI=<path>` the way the CI step does.

**Two ways to make every gate fail for a reason that is not there, both of them sequencing.**
**In a gates summary, 0 seconds means it never ran.** A gate that fails fast and a gate whose
prerequisites were already broken look identical in the `FAILED:` line, and the elapsed time is
the only thing that separates them: with a stale published artifact, nothing a gate invokes can
start, and it reports `0m00s`. Read the per-gate times before
believing a gate tested anything, and read the FIRST failure rather than the list at the end,
because one broken prerequisite reports as a dozen unrelated gates.

**A gate may write nothing another gate reads, and the source tree counts.** `gates all` runs
every gate at once, which is safe only because each has its own rundir, HOME and ports, and
because the source tree does not change under them: every CLI in the checkout reads
`backend/src/LibExecution/package-ref-hashes.txt` at startup whatever its HOME, so a reload leaves
that file alone unless its contents change. A new gate that
writes anywhere outside its own rundir breaks the parallel mode the same way.

`gates all` re-execs `$0 <name>` once per gate, so editing `scripts/testing/gates` or a
`_gates-*` file while a run is in flight breaks every invocation that starts after the edit,
with a bash syntax error and no gate output at all. And the gates run the binary directly
through `CLI=`, which skips the staleness check `run-cli` does, so a `.dark` file edited after
the last build leaves them against a store whose hashes have moved: each one dies in about a
second with `Function <hash> couldn't be found` and nothing else. Build, run the gates, then
edit.

## Rebasing and merging

**A rebase that REMOVES a syntax form merges silently where main added uses of it.** Git marks a
conflict only where both sides touched the same lines, so where main added `++` uses while this
branch was deleting the operator, the branch has no text there: it merged with no marker, no
warning and a clean F# build, then failed as a package-load PARSE error in a file the branch never
touched. Sweep for the form across `packages/` and `backend/testfiles/` after the rebase instead of
fixing the first one that fails; they cluster in whatever main wrote while the form was still legal.
When the fix is mechanical, make the script ASSERT it converted every occurrence in each file it
touches, so a string literal spelled the same way (`"++"` in a parser for another language, `" ++ "`
in a printer, "C/C++" in prose) fails the run loudly rather than being corrupted quietly. Same root
cause as the deleted-public-fn case that follows, one level earlier: that one breaks name
resolution, this one breaks the parse.

**A clean rebase can still fail at run time, and git cannot tell you.** Dark resolves names
when a call EXECUTES, so deleting a public fn breaks every caller silently. Git only marks a
conflict where both sides changed the same lines; where main ADDED a call to a fn your branch
deleted, your branch has no text there, so it merges with no marker and no warning. A green
build says nothing either, because nothing type-checks a Dark call site until it runs. The
first you hear of it is a red testfile you did not touch.

So after any rebase that deletes or renames a public fn, sweep for the class rather than
fixing the ones you trip over:

1. For each module, list the top-level `let`s in the MERGE BASE and in your tree; the
   difference is what your branch removes.
2. Grep the merged tree for each removed name, qualified (`Stdlib.<Module>.<fn>`), across
   `packages/`, `backend/` and `docs/`.
3. Eyeball the hits. Prose mentions in docs and comments quoting an error message are
   fine and will show up; a call in a `.dark` testfile is a real break.

Same root cause as the CLI sweep in `docs/agent-testing.md`: the bugs are found by running, not by
reading.

**Check a mechanical resolution by replaying it against the base.** When a commit applies one
transformation across many files (an operator rewrite, a rename) and the rebase conflicts in
all of them, write the transformation as a script instead of hand-editing. Then prove the
script IS your commit: run it over the MERGE BASE and require the output to equal your own
side byte for byte. Once it does, run it over main's text and you get main's new cases with
your change applied, rather than a hand-merge that quietly drops one.

**Reading a resolution twice is not a check, and a build is not the whole check either.**
One rebase of this repo produced four broken resolutions, and reading caught none of them,
including two that were read twice and reported as done. What caught each is the useful part,
because the stages are not interchangeable:

- the compiler caught two: a `.fsproj` still listing a file the branch MOVED to another
  project, and a dropped `}` that left a `test { ... }` block unclosed. Note the second
  presented as a wall of errors naming the wrong place; see "Read the FIRST compiler error"
  under "Builds".
- the SUITE caught one the compiler cannot: a duplicate wrapper around one builtin, left
  behind when main MOVED that wrapper and the resolution kept both copies. It compiles
  perfectly. `tests/builtin` is what fails, because a builtin may have exactly one Dark
  wrapper, and the dead copy still counted as a second reference to it.
- the fourth surfaced only because the same file conflicted a second time, which showed
  that a structural resolution had left the tail of the form it replaced sitting underneath:
  an `if/else` head swapped for a `match`, with the old `else` branch still there, calling
  an op with the arity main had just changed.

So the order is build, then the full suite, then believe it. A clean build on a rebase that
deleted or MOVED anything public says less than it looks like it says, and "I read it twice"
says nothing at all.

**Two branches adding an instruction both take the next tag.** An `Instruction` case needs a
number in three places: `Opcode.index` (RuntimeTypes.fs) and the read and write halves of
`LibSerialization/Binary/Serializers/RT/Instructions.fs`. When two branches each add a case,
both pick the same next number, and merging them is asymmetric in a way that bites: the READ
side conflicts, because both arms start `| 23uy ->`, while the WRITE side merges CLEANLY into
two arms that both `w.Write 23uy`. That compiles, passes every test that round-trips through one
process, and writes instructions that a later reader decodes as the wrong case. After any merge
or rebase that brings in a new instruction, grep both halves for a duplicated tag before
trusting the build.

## The dev store

**A build wipes anything you authored by hand, and the next measurement looks like a fast pass.**
`reload-packages` re-authors the store from the `.dark` files on disk, so a module you made with
`dark module /Demo.X -` is gone after any build, including the one inside
`scripts/dev/build --optimize --test`. That part is expected. The trap is what it does to timing
work: `dark eval 'Demo.X.f 16'` then fails in milliseconds, the trace it leaves is a FAILED one,
and timing `traces show` against it reports the startup baseline, which looks like a win rather
than a mistake. Check the eval's own output, or the
trace's `status`, before trusting any number taken against a hand-authored fixture. A fixture you
intend to keep belongs in a file the walkthrough pipes in (`docs/walkthrough-fixture.dark`), so
re-authoring it is one command rather than remembering what it was.

**A doc-only edit keeps the item's hash.** Doc comments are excluded from the content hash
(`Canonical.fs`), so `dark fn`, `dark module` and `dark edit` write the change as an `UpdateDoc`
op and add "; updated N doc comments" to the output. If a doc edit seems not to land, that line
says whether it was written.

**`branch create` while standing on a branch creates a CHILD of that branch.** Switch to main
first if you meant a sibling.

**A scratch store inherits config you did not mean to give it.**
A copy of `rundir/data.db` carries its config with it: which branch it is on, this instance's
identity, and whatever else `config_v0` holds. So a throwaway store is not a fresh store, and the
difference shows up as a test standing somewhere it did not expect to be.

    sqlite3 "$scratch/data.db" 'DELETE FROM config_v0;'   # all of it, not just current_branch

Clearing only `current_branch%` is the trap: it looks like isolation and is not.

`config_v0` is the only config store, but an instance id and name in a `cli-config.json` beside
the db are moved in the first time anything asks for `instance.*`, and the file is left alone, so a
cleared copy that still has an old `cli-config.json` beside it takes that
file's identity. Copy the store alone, which is the shape to prefer for a scratch store anyway,
with `scripts/testing/_copy-store` rather than `cp`: the newest writes sit in `data.db-wal`.

It lives in `~/.darklang`, so a process given its own HOME gets its own. In the dev
container the clone's `rundir` is mounted there.

**Your own experiments in the dev store break every gate that commits.** The gates start from a copy
of `rundir/data.db`, and `dark commit` refuses a store holding unresolved references. So one
throwaway item authored by hand, referring to something that never resolved, fails
`workbench-scm` with symptoms that look nothing like the cause: a fixture times out waiting for
its own commit to appear. The gates guard against the CONFIG a
copied store inherits and not against its draft. Author experiments under their own HOME, or
`dark discard -y` before running gates; `dark status` says whether the draft is clean.

One consequence worth knowing: an isolated store usually looks like a FIRST RUN, and Home shows its welcome
PANEL instead of a row's detail, so a test waiting for anything a populated Home draws waits forever. Do not
anchor a workbench test on the greeting either way: "Welcome, <name>" is on every Home, and the panel is the
part that distinguishes a new instance. The stable "it started" marker is the frame's top border
(`╭`), because it is the one thing every frame has at every size. A marker has to survive
expect's default 80 columns, where there is no nav strip at all
and the header is just the branch and the instance, so neither `1 Home` nor a fixed name works.

## Harness and shell

**Never conclude an ABSENCE from output you truncated or filtered.** This is the one that gets
through, because it produces a confident negative, and a negative is the result nobody re-checks:
you look for a thing, do not see it, and move on. Every other harness mistake here produced a
wrong positive that something eventually contradicted. This family produces silence, and silence
agrees with whatever you already believed. Instances:

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
  absent. Count with a fixed string first (`grep -c -F`), then go looking for context
- a pipeline hides the exit code of the thing you care about. `./scripts/dev/build ... | tail -40`
  reported exit 0 over a build that printed "Failed in 64.85s". Use `${PIPESTATUS[0]}`, or do not
  pipe
- `ls backend/Build/...` from the HOST returns nothing whether or not the file is there, because
  that path is a container volume. See "`backend/Build` is a container volume" further down. This is the one variant
  that does not announce itself: the others cut a real answer short, this one shows you an
  empty directory with no error at all
- `tail -3` on a save that printed an error above its success line showed only the success, so
  a refused item read as a clean one
- a container listing read three rows deep can say this clone has no container up when it has

**A truncated answer is worse than no answer, because it justifies doing nothing.** Truncation
turns an unseen positive into a confident negative, and a negative leaves nothing behind to
contradict it later. When a command's output decides whether to act, read all of it, and prefer
`grep` for the thing you are deciding on over a positional `head`/`tail` that may not contain it.
If a search comes back empty and you are about to act on the emptiness, re-run it without the
filter. If a harness reports that nothing happened, reproduce it by hand once before you write it
down.

A HARNESS manufactures a negative the same way a filter does, and it is harder to see because
running a control FEELS like the check. If headless chromium reaches no prompt in the browser
build and none on the deployed site either, that says "not a regression" no more than it says "not
a working instrument". A control agreeing with your negative only rules out one of
the two explanations, and the instrument is the one nobody suspects.

**An exclusion list that never fired looks exactly like one that did, and `find` will hand you
one for free.** A prune placed AFTER the `-type f` test is unreachable, so the walk descends into
whatever is there. In `find`, prunes come first:
`find . \( -path ./x -o -name y \) -prune -o -type f -print`.

The second half is the general one. A tool you have excluded something from is worth RUNNING once
to see that the exclusion took, and worth an assertion that the walk found what it must:
`scripts/linting/yamllinter` refuses a walk that does not include `.circleci/config.yml`. A check
that compares a report against a number from inside the same report cannot fail.

**A `while read` loop over a heredoc loses every line after the first if the body calls
`run-cli`.** `run-cli` forwards stdin, so it consumes the rest of the heredoc the loop was reading
from, and the loop ends after one iteration. It prints no error and exits 0, so a sweep written this
way reports confidently on one item while looking like it covered all of them. Feed the inner
command `< /dev/null`, and count what the loop actually examined.

**Bracket every pattern you hand to `pgrep -f` / `pkill -f`.** The pattern appears in your own
shell's command line, so an unbracketed one matches the process doing the matching. A waiter waits
on itself forever; a `pkill -f "serve Foo"` kills the backgrounded shell whose command line
contains "serve Foo", which looks exactly like the publish you just started dying for no reason.
`[s]erve Foo` matches the target and not the matcher. Note that bracketing is not enough on its
own: if the same shell also RUNS `serve Foo`, the bracketed pattern still matches it, so kill and
start belong in separate commands. So take the recipe rather than the rule: start background fixtures with `docker exec -d` (or `setsid` plus a
redirect, so the exec can return), and STOP them by pid read from `ps`, never by pattern:

    P=$(ps -eo pid,args | awk '/python3 \/tmp\/delay.py/ && !/awk/ {print $1}')
    for p in $P; do kill $p; done

`docker exec` without `-d` also hangs on a server that never exits, which looks like the command
failing rather than the server working. The same family: `2>&1` on a command whose stdout you are about
to parse as JSON merges a warning into the payload and the parse failure reads as a product bug.
All of these present as product failures and all of them are the harness.

**`run-in-docker` ignores stdin unless something's on it.** Fd 0 is forwarded through a `cat`
that needs an EOF, and an agent harness hands you a socket that never gives one. If you pipe real input and it gets dropped, `DARK_STDIN=1`
forces it through.

**`backend/Build` is a container volume, so from the host it reads as EMPTY.** `ls`, `du` and
`find` against it from the host return nothing at all, with no error, whether or not the binary is
there. Everything here is driven through `./scripts/*` from the host, which is exactly the habit
that leaves you inspecting host paths, and this one lies. To look at build output, look from
inside:

    source scripts/devcontainer/_container-for-clone
    docker exec "$(container_for_clone "$PWD")" \
      ls -la /home/dark/app/backend/Build/out/Cli/Debug/net11.0/Cli

That helper exists for this and is meant to be sourced; its own comment explains why the
`/home/dark/app` mount source is the only key that holds (names vary, the `local_folder` label is
empty on some containers, and `docker ps --last 1` sorts by creation time, which says nothing about
which clone you are in). `workspace-tools/clones` shows a container's STATE and ports, not its name.

## Dark, the language

**No `PACKAGE.` source prefix.** `PACKAGE.` is internal runtime/debug notation, not a
Dark namespace. Write `Stdlib.List.map` or `Darklang.Stdlib.List.map`, never
`PACKAGE.Darklang.Stdlib.List.map`; the same rule applies to search queries.

**Enum construction across modules.** Name the DU type, not just the module:
`ProgramTypes.Reference.PackageFn hash` works, `ProgramTypes.PackageFn hash` doesn't. In a
match arm the bare case is fine, since the matched value's type resolves it.

**Pipes and `if`.** You don't need parentheses around a call before a pipe:
`Stdlib.List.length(xs) |> Stdlib.display` works as written. You do need them around an
`if` or a `match`. Without them, `if c then a else b |> f` only pipes `b`, as if you had
written `if c then a else (b |> f)`. Write `(if c then a else b) |> f` instead.

**A literal inside a tuple pattern silently falls through.** `| Some(_, _, "propagation") ->`
against an `Option` of a 3-tuple parses and never matches; destructure first, then compare. Same
family as the wildcard trap that follows.

**A wildcard doesn't match a multi-field DU case.** `| ExportPath(_) ->` silently fails to match
`ExportPath(String, TextField.State)`; you need `| ExportPath(_ext, _field) ->`. It's a
runtime error ("No matching case found") at the moment that case comes up, not a load
error, so it waits until you press the key that reaches it.

**Dicts keep the last value on a duplicate key.** `Dict.set`, `fromEntries`, `merge`
and dict literals all replace. To refuse instead, use `insert`, `fromEntriesUnique` or
`mergeUnique`, which return `Error (DuplicateKey key)`; `mergeWith` takes a resolver.
A literal that repeats a constant key (`Dict { "a": 1, "a": 2 }`) gets a
`DuplicateDictKey` error from the at-rest checker, like a record field given twice,
but it still saves and still runs, keeping the last value.

**Nested functions.** Only the fully annotated form works: `let helper(x: Int64): Int64 = ...`.
It desugars to a local lambda, can close over outer bindings and can call itself, but mutual
recursion between nested functions isn't supported. There's no `rec` keyword; top-level
functions are self-recursive already.

**A `val` holding a custom type can go stale.** `val forMain = forBranch ""` stores a *value*,
and that value carries the type identity it was built against. Reload packages and a caller can
be handed a `Context` the callee no longer recognises: `FnParameterNotExpectedType` on a
parameter whose type you never touched. Confusingly it reproduces only where the package set is
rebuilt (the LibExecution testfile harness) and not under `eval`. Call the function instead of
reaching for the `val` when the result is a custom type.

**Record update takes no type tag.** `{ state with field = v }` is right.
Do not prefix the update with `MyType`; the record type comes from `state`.

**`let f() = <a literal>` rebuilds it on every call.** A nullary function whose body is a constant is
not a constant; `val` is evaluated once. If the body doesn't depend on anything, make it a `val`.

**Except on the at-rest checker's path, where a `val` is a bug.** The checker runs inside
`reloadPackages` before `evaluateAllValues`, so reading a package value raises `ValueNotFound`,
`guarded` empties the whole batch, and the reload records no trait-call resolutions and still
prints Success. Use a function there.

**Value bindings take no type annotation.** `let xs = [...]`, not `let xs : List<String> = [...]`,
which fails with "Value annotations are not supported". Function bindings do take them, and
nested functions require them.

**For deep tree walks in Dark, avoid recursion inside builtin callbacks.** For example,
`Stdlib.List.map(children, fun child -> visit(child))` starts a nested interpreter run at
each level and can fail with "Out of stack". Use direct recursion or an explicit work list.

**Name resolution in test files.** `backend/testfiles/` is parsed with owner "Tests", so
`Darklang.*` names need full qualification or the `Stdlib.` shortcut. `Stdlib.Json.ParseError.toString`
and `Darklang.SCM.Branch.mainBranchId` resolve; `SCM.Branch.mainBranchId` doesn't. Impl:
`backend/src/LibParser/NameResolver.fs` and `packages/darklang/languageTools/nameResolver.dark`.

**`nonInteractive` means "run as `dark <cmd>`", not "no terminal".** `AppState.nonInteractive`
is set for any command given on the command line (entry.dark), so it's true even with a
perfectly good terminal in front of you. To decide whether a TUI can start, ask
`Stdlib.Cli.Tui.TerminalSupport.current()`.

**A capped listing decides on stdout, but the TTY depends on stdin.** `Listing.shouldCap`
asks whether stdout is a terminal, and `run-in-docker` allocates a TTY only when *stdin*
is one. Authoring reads the declaration from stdin (`fn X - <<'EOF'`), so there's no TTY,
so `shouldCap` is false and nothing caps -- on exactly the path that produces the most
output. For a status report from an authoring command, cap unconditionally and point the
footer at the command that prints everything.

## F# and Dark together

**A builtin's wrapper can lie about BigInt width.** A builtin returning `Result<BigInt, _>` wrapped as
`Result<Int64, _>` loads clean and fails at the first call. Match the builtin's declared types
exactly; the loader will not catch it.

**A Dark record literal written inside an F# STRING is invisible to every tool you would sweep
with.** A test that drives Dark through `evalDarkExpr` holds its Dark source as an F# string, so
the F# compiler sees string contents and a sweep over `packages/**/*.dark` never reads the file at
all. When a record widens, such a literal raises `Missing field` at run time, in a test whose
name is about something else, and a `record_fields` sweep reports nothing. What finds them is
grepping the F# for the Dark constructor:

    grep -rn 'ProgramTypes.Definitions {' backend/ --include=*.fs

So when you widen a record, sweep `backend/**/*.fs` for its CONSTRUCTOR as well as the Dark tree,
and remember the same applies to testfiles, demo tapes and walkthrough prose. Same family as the
other sweeps here: a tool reporting success over something it had no way to read.

**A TUPLE pattern that loses an element is the same defect as a DU case that loses a field, and
nothing catches it either.** A destructure that takes four of `searchNamesAndHashes`'s six lists
raises, the per-batch guard converts the raise into an empty report, and `dark typecheck <module>`
then answers "nothing to audit" about a module full of declarations. The tell is the same as every
other instance: a confident zero. So when a search or any wide tuple grows, sweep
the destructures, and treat a zero from a guarded path as unexplained until you have seen the guard
not fire.

## Permissions

**Permissions are runtime authorization, not OS isolation.** Host effects cross
`Host.perform` and are checked against policy; this does not confine a compromised
runtime or limit resources. OS sandboxing remains future work in `docs/permissions-todos.md`.

**Instance policy.** First run seeds `~/.darklang/policy/policies.bin` with a safe
default: package loading, computation, local storage, and printing work, while
filesystem, network, process, and environment access are denied. Missing or corrupt
policy data fails closed. `permissions allow all` is available for development.

**Package `val`s must not depend on the host.** They are evaluated once and stored;
trusted seed builds can therefore hide permission failures and freeze build-machine
state. Use a function for environment-dependent work. `PermissionEscape.Tests` checks
shipped values under guest policy.

**A forwarding wrapper has no frame.** `let map(list, fn) = Builtin.listMap(list, fn)`
is elided on both the first call and the cached `Apply` path. Both must establish
`vm.activeAccess` with the invocation's grants and restrictions. A wrapper is thin only
when its signature exactly matches the builtin; `sameType` even compares type-variable
names, so tests must use the builtin's `'a` and `'b`. All paths share
`Interpreter.packageEntryAccess`.

**A permission test must make the value in one frame and run it in another.** Deferred
values keep their producer's restrictions and intersect them with the runner's. A broad
driver must call the producer and pass the value into a separate restricted runner;
otherwise the producer inherits the restriction and the test proves nothing. Load function
references in the driver's value position so they are stamped broad first.

**Seeded package values are code, not captured authority.** A function reference stored in a seeded `val` must use `access = None` (`Seed.stripCapturedAccess`); otherwise it can decode as deny-all. Static approval follows a reference to a package value only when its stored value holds no code (`Dval.isInertData`); a value holding a function, or one not evaluated yet, stays incomplete.

**Compiled function references are serialized code.** `EFnName` becomes a serialized
`DApplicable(AppNamedFn …)` in package instructions. Keep code constants distinct from
captured values when changing applicable decoding; attenuating code references can lock
down the entire CLI.
