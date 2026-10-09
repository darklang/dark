# Unit tests

The canonical description of the F# test suite: how to run some of it, what the filter
flags do, and what used to go wrong. `AGENTS.md` has the short version.

## Running them

```
scripts/run-backend-tests
```

A full run is a few minutes and logs to `rundir/logs/fsharp-tests.log`. The entry point
is `backend/tests/Tests/Tests.fs`; tests are not discovered automatically, they have to
be added there.

## Running some of them

Finding what to run used to be the hard part, so start there rather than guessing at a
filter:

```
scripts/run-backend-tests --groups              the test tree, with counts
scripts/run-backend-tests --groups Interpreter  just that part of it
scripts/run-backend-tests --find mergeFavoring  which tests match, and how to run them
```

Neither touches the database or reloads packages, and both print the command that runs
what they found. The underlying listing is slow, so it's cached until the next build.

Then there are three filter flags, and they do three different things:

```
--filter <path>            a prefix of the slash-separated path, from the root
--filter-test-list <sub>   substring, matches test lists, case-sensitive
--filter-test-case <sub>   substring, matches test cases, case-sensitive
```

Everything is nested under `testList "tests"`, so paths start with `tests/`.
`--filter tests/Interpreter` runs 90 tests; `--filter Interpreter` runs none.

### Why this was worse than it looked

Three things compounded, and each hid the next.

`--filter`'s help says it takes "a hierarchy that's slash (/) separated", but Expecto's
default separator is a dot. So the filter you wrote after reading the help matched
nothing. We now pass `JoinWith "/"` so the documented form is the working one.

A filter that matches nothing was not an error. Expecto reports it as
`0 tests run - Success!` and exits 0, so a filter with a typo in it looked exactly like
a suite that passed. `run-backend-tests` now fails when a filter you supplied matched
nothing.

A filter that matches LESS than you meant is still not an error, and cannot be. It is a
substring match, so `--filter-test-case "stripping markers"` matches the test called
"stripping markers leaves every jump landing on the same instruction" and not the one
called "jumps land where they did with any number of markers", which was the sibling test
in the same module added in the same commit. The run passed, honestly, about a subset.
Two tests were written, one was verified, and the other reached a commit unexecuted; the
full published suite is what found it.

So read the COUNT the run prints, not just its colour. One test when you wrote two is the
tell, and `--find <substring>` lists what a filter will match before you trust it.

`--list-tests`, the obvious way out, ignores every filter and prints ten thousand lines
after a slow startup. That's what `--groups` and `--find` are for.

## Running two at once

Two runs in the same clone destroy each other: they share `test-data.db`, the httpclient
port, and a `killall -9 Tests`. `run-backend-tests` takes a lock and refuses instead.

Two runs in different clones are fine. Each clone has its own container, and so its own
PID namespace, bridge network and bind-mounted `rundir`. This was genuinely forbidden
until recently, when every clone's scripts re-execed into whichever container was newest
and four clones' runs really did land in one.

## Dark tests

`backend/testfiles/README.md` covers the `.dark` test files, which are a separate thing
from the F# tests here.

## Isolated native package tests

To run the repository's full package suite, use its disposable test installation:

```sh
scripts/testing/gates package-tests
```

Build Debug first with `scripts/dev/build`, or set `CLI` to a current published
binary using a repository-relative path. The gate copies the store, configures
a deny-all saved policy in that copy, and runs every test with
`--force`. Its full output is `rundir/package-tests/test.log`.

`./scripts/run-cli test --force` runs tests in the current installation. Test
execution uses an allow-all instance policy in memory, so native workers,
filesystem/environment access, HTTP and subprocesses need no installation grants.
The saved installation policy is unchanged, and ordinary `eval`/`run` commands
continue to use it. Package approvals, declared function ceilings and captured
caller restrictions still apply. The gate starts from a deny-all saved policy to
verify that tests need no setup grants.

Every package test execution runs in its own disposable process and store.
Use an ordinary test declaration, including when changing local state:

```dark
test writesConfig =
  let _ = Stdlib.LocalStore.configSet "example" "child"
  Stdlib.LocalStore.configGet "example" |> Stdlib.Test.equal "child"
```

The runner executes the test's own hash in a disposable process and store.
No callback function or central registry is needed. Each `dark test` run takes
one starting snapshot when its first uncached test executes. Every executed test
gets a private writable copy of that baseline and a fresh process. Cached-only
runs create no snapshot; later runs take a new one.
Proven-safe passing results can still be cached;
`test --force` executes every selected test. Expected-error assertions retain
their typed errors across the worker boundary. Worker and cleanup failures fail
the test, even when its body expects an error.
The defaults are the current branch, 120 seconds, and an 80x24 terminal fallback.

`Stdlib.Test.Process.run options callback argument` also supports typed arguments
and results, and captures stdout/stderr separately. `defaults ()` selects the
current branch, a 120-second timeout, and an 80x24 terminal fallback. Override
`branch` for a test specifically about main, or the dimensions for a rendering
scenario. The worker receives an allow-all disposable instance policy, the
caller's package approvals, and active restrictions. Tests have no permission
options or effect rows of their own. Package functions still use ordinary
`permissions approve`; test execution supplies the instance grants. Callbacks
must be named, unapplied, non-generic package functions;
arguments and results must be serializable runtime values. The API requires Native
and is available only during package-test execution.

The host takes the baseline through SQLite's online backup API and closes it
before copying its file. The run owns and cleans up the baseline even when its
Dark callback fails; no snapshot is kept between runs. Explicit nested
`Stdlib.Test.Process.run` calls instead snapshot the calling test's current store,
so they see its writes. Standalone test execution also takes its own snapshot.

For each worker the host creates a private home/policy/tmp
folder, and launches one worker. The worker executes the test or explicit callback
by its immutable hash. No Dark source is generated and stdout is never used as the result protocol.
Stdin is EOF; commands that need input must supply an explicit pipe. The temporary
store disables live autopush, the relay secret is removed from the environment,
and HTTP follows the test instance policy and inherited function restrictions.
The worker timeout covers process exit and draining both output pipes. Killing a
live worker has a separate bounded cleanup wait. Snapshot creation happens before
the worker timeout. Cleanup is attempted on success, failure, and timeout;
`Output.cleanupErrors` reports cleanup failures alongside the callback result.
The test runner preserves assertion failures and reports cleanup errors or a
nonzero worker exit, even when a worker wrote a passing result before exiting.
Cleanup stops the worker's POSIX process group or Windows job, including
descendants still in that group or job after the worker exits. This provides
test-state separation; native code still has OS access.

Branch changes inside a child cannot affect the parent or the next test. Tests
that previously required a shared runner to restore its branch no longer need
that cleanup convention. `dark test` prints `Running:` before each uncached test.
