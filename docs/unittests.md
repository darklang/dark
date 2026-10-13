# Unit tests

The repository has an F# backend suite and Dark package tests. This guide covers
running and filtering them. See [Package tests](package-tests.md) for the design,
including storage, isolation, permissions and caching.

## Backend tests

The runner uses existing binaries, so build first. Use the published build for a
full suite run:

```sh
scripts/dev/build --optimize
scripts/run-backend-tests --published
```

For a focused run, a Debug build is also useful:

```sh
scripts/dev/build
scripts/run-backend-tests --filter tests/Interpreter
```

The backend suite includes runtime, integration and raw HTTP tests. Its entry
point is [Tests.fs](../backend/tests/Tests/Tests.fs); add new F# test groups there.
See the [testfiles README](../backend/testfiles/README.md) for the HTTP test files.

Output goes to `rundir/logs/fsharp-tests.log`, with JUnit results in
`rundir/test_results/backend.xml`.

Local runs reload packages into a fresh test database. CI copies the seed built
earlier in the job instead.

## Finding and filtering backend tests

Use the discovery commands to find a group or case before choosing a filter:

```sh
scripts/run-backend-tests --groups              # test groups and counts
scripts/run-backend-tests --groups Interpreter  # one part of the tree
scripts/run-backend-tests --find mergeFavoring  # matching cases and run commands
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
