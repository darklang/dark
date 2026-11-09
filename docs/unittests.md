# Unit tests

The repository has an F# suite and Dark package tests. This guide covers
running and filtering them. See [Package tests](package-tests.md) for the design,
including storage, isolation, permissions and caching.

## Everything at once

```sh
scripts/dev/build --optimize
scripts/run-tests --published
```

This runs the F# tests, the package tests and the CI gate subset at the same time,
then prints each suite's counts and exits nonzero if any failed. Without
`--published` it uses the Debug build. Each suite's output is in
`rundir/logs/run-tests/`.

## Taking turns on a shared machine

The heavy runs take a machine-wide test slot first: `scripts/run-tests`, a whole
`scripts/run-fsharp-tests`, and `gates ci`, `gates all` and `gates package-tests`.
Clones on one box queue for it in the order they asked, and a waiter says who it is
waiting on:

```
waiting for a test slot (held by suite-slot (run-tests) since 10:42:07 UTC)
```

`DARK_SUITE_SLOTS` says how many may run at once; 0 turns it off. The default is
one per 16 hardware threads, at least 1, so 3 on a 48-thread box and 1 on a
laptop: on a shared box a running suite averaged 6 to 10 busy cores. A filtered run
(`--filter`, `--filter-test-list`, a single gate other than package-tests) skips the queue, and
a suite started by another (`run-tests` starts three) runs under its parent's
slot. The slots are files in a host directory every container mounts at
`/home/dark/.suite-slots`; a container made before that mount says so and runs
without waiting (`scripts/dev/start --recreate` adds it). CI has a machine per
job and never waits.

To run some of the suites in one turn, name them: `scripts/run-tests --published
package-tests fsharp` waits once, not once per suite. For the last run before a
merge, `--final` (on `run-tests`, `run-fsharp-tests`, or `gates --final ...`) or
`DARK_SUITE_PRIORITY=final` waits ahead of the others; a waiter that has waited 3
hours counts as final too.

Don't wrap a suite in `timeout`: the wait for a slot counts toward it, and a run
that times out while queued goes to the back when it starts again.

Builds, package reloads, the release build and the perf scripts run at niceness 10
above their caller, so a running suite gets the CPU first. Instruction counts and
allocation are unaffected; wall times on a busy box are slower.

## F# tests

The runner uses existing binaries, so build first. Use the published build for a
full suite run:

```sh
scripts/dev/build --optimize
scripts/run-fsharp-tests --published
```

For a focused run, a Debug build is also useful:

```sh
scripts/dev/build
scripts/run-fsharp-tests --filter tests/Interpreter
```

The F# suite includes runtime, integration and raw HTTP tests. Its entry
point is [Tests.fs](../backend/tests/Tests/Tests.fs); add new F# test groups there.
See the [testfiles README](../backend/testfiles/README.md) for the HTTP test files.

Output goes to `rundir/logs/fsharp-tests.log`, with JUnit results in
`rundir/test_results/backend.xml`.

CI runs the whole suite with `--split`, which is faster. Most tests run in parallel, but
some must run one at a time (they share state inside the test process). Normally those
wait until all the parallel tests finish. With `--split` they run at the same time instead,
in two more test processes, each with its own copy of the database and its own folder,
`rundir/split-sequenced/` and `rundir/split-sequenced-2/`, so they can't interfere. Each takes
whole groups; which groups go to the second is a list in `scripts/run-fsharp-tests`, chosen to
even out their times.

Their output is prefixed `[sequenced]` and `[sequenced-2]`, logged to
`rundir/logs/fsharp-tests-sequenced.log` and `fsharp-tests-sequenced-2.log`, and reported in
`rundir/test_results/backend-sequenced.xml` and `backend-sequenced-2.xml`. `--split` doesn't
take a filter. To run just one part, use `--kind sequenced` or `--kind parallel`, and
`--only-groups A,B` or `--skip-groups A,B` to pick top-level groups.

Local runs reload packages into a fresh test database. CI copies the seed built
earlier in the job instead. Outside CI the reloaded store is kept and reused until
its inputs change; `rm -rf rundir/test-store-cache` forces a fresh one.

## Finding and filtering F# tests

Use the discovery commands to find a group or case before choosing a filter:

```sh
scripts/run-fsharp-tests --groups              # test groups and counts
scripts/run-fsharp-tests --groups Interpreter  # one part of the tree
scripts/run-fsharp-tests --find mergeFavoring  # matching cases and run commands
```

Discovery uses the Debug test binary, so it needs a Debug build even if you run
full suites with `--published`. The list is cached until that binary changes.

The runner supports three filters:

```text
--filter <path>            prefix of the full slash-separated test path
--filter-test-list <text>  substring of a test-list name, case-sensitive
--filter-test-case <text>  substring of a test-case name, case-sensitive
```

Full paths start with `tests/`. For example, `--filter tests/Interpreter` selects
that group; `--filter Interpreter` does not. Add `--published` when running a
filter against the published build.

Check both the reported test count and the command's exit code. A filter matching
nothing fails, but one matching fewer tests than intended can still pass.

## Package tests

Package tests live in `packages/darklang/tests`. To run the full suite in a
disposable installation:

```sh
scripts/dev/build
scripts/testing/gates package-tests
```

The gate defaults to the Debug CLI and forces every test to execute. To use a
published CLI, set `CLI` to its repository-relative path. CI uses NativeAOT to
reduce the cost of starting a worker for each test.

The full output is saved to `rundir/package-tests/test.log`.

To run a selected group against the current installation's packages:

```sh
scripts/run-cli test run Stdlib.Float
scripts/run-cli test run --force Stdlib.Float
```

Filters match whole name segments. `--force` bypasses cached passes; each executed
test still gets its own process and store. The summary distinguishes tests that
ran from cached results. Assertions combined with `Stdlib.Test.all` count as one
test declaration.

Tests run in parallel, with up to four workers (limited by the available cores).
Use `--jobs <count>` to change the limit, or `--jobs 1` for a serial run.

Some tests deliberately contain invalid programs, so `dark typecheck` in a
normal development store can report intentional test errors. Run the production
check separately:

```sh
scripts/testing/gates shipped-tree-typechecks
```

That gate checks the normal package load and excludes test modules from its
verdict. Shared declarations with production names are still checked. It defaults
to Debug; set `CLI` to a repository-relative published binary path to use that
build instead.

## Avoiding conflicting runs

Run only one backend suite at a time in a clone. Runs share the test database and
HTTP ports, so the runner takes a lock and refuses a competing run. Use separate
clones with their own dev containers for simultaneous backend suites.

Finish a test run before rebuilding the binaries it is using.
