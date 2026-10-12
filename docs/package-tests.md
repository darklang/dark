# Package tests

Tests are package items, alongside functions, types and values. Their bodies run through the existing interpreter. Dark code discovers tests, judges their results and reports what happened; F# handles storage, execution and process isolation.

This is a guide to the design and the main parts of the implementation. See [Unit tests](unittests.md) for commands, CI setup and the remaining backend tests.

## From a declaration to a stored test

```text
test declaration -> WrittenTypes -> PT.PackageTest -> AddTest + SetName -> package store
```

We added `PT.PackageTest` to store the test's body, description, any expected
error, and any DBs declared for the test.

A test is its own kind of package item. It can depend on functions, types and
values, but nothing can call a test as a function.

Most tests return `Stdlib.Test.Result.Pass` or `Fail`. Tests using `=> raises`
or `=> sqlerror` pass when the body raises the expected error. If the body
returns a value instead, the test fails.

Tests use the existing package machinery: `AddTest` records immutable content,
and `SetName` binds a name to its hash. Tests are serialized, hashed, tracked,
propagated, searched and synced through the same mechanisms as other package
items.

## What lives in SQL

Test definitions and run results live in separate tables:

- `package_ops`: The existing package history table. Tests use AddTest for their definitions and SetName for their names.
- `package_tests`: Test definitions and descriptions, ready to read. This table can be rebuilt from `package_ops`.
- `package_test_results`: Results from local runs, used to show previous outcomes and reuse cached passes. Results are not versioned or synced.

Test bodies are stored as PT and converted into interpreter instructions when they run. For package functions, those instructions are generated and stored ahead of time.

Tests can declare private Dark databases with `[<DB>]` and use them through
`Stdlib.DB`. Each run gets fresh DB IDs, and their data is deleted afterward.

## What happens when you run `dark test`

`dark test` runs all tests, or you can filter them by module or test name. It skips
deprecated tests and starts them in name order, with up to four running at once
(or fewer on smaller machines). Results are printed as tests finish. Use
`--jobs <count>` to change the limit, or `--jobs 1` to run serially. Each declaration
counts as one test, even when `Stdlib.Test.all` combines several checks. The
command fails if any test fails or no tests match.

The worker is the separate process running the test. It loads the saved version
by hash, sets up any private DBs the test declares, and resets the test counter.
F# then converts the body into instructions and runs them through the normal
Dark interpreter.

Tests can use the counter to check how many times a callback or expression runs.
It starts at zero for each test. Calls to `Stdlib.Test.incrementCounter` increase
it, and `Stdlib.Test.counterValue` reads it. Tests that don't need to count
evaluations leave it unused.

The worker runs the test; Dark decides whether it passed. For an ordinary test,
it checks for `Pass` or `Fail`. For `=> raises` or `=> sqlerror`, it checks that
the expected error occurred.

Errors are returned as structured data so Dark can inspect their type and details.

## Isolation and permissions

Before the first uncached test runs, `dark test` takes one snapshot of the package
store. SQLite's backup API creates the snapshot. The backup connection is then
closed, leaving a complete database file that can be copied for each test.
Each test that needs to run gets a fresh process, its own writable copy of the
store, and separate directories for its home, temporary files and permission
policies. If all selected tests have reusable cached passes, no tests run and no
snapshot is created.

Tests start from the same snapshot, so one test's changes don't affect another.
The Dark runner passes an explicit run handle to each execution. Test support
owns that handle and its snapshot; neither is part of interpreter state.

The CLI's `--entry-point` option selects the Dark worker by its exact function
hash for this invocation. The saved entry point stays unchanged, so commands
launched by the test still use it. The worker is a normal
dependency of the runner, so its changes also invalidate cached results.
The worker reads the request and writes the result. Native support executes the
exact hash with inherited permissions and preserves runtime values and errors.
The host layer only manages file snapshots and supervised child processes; it
doesn't know the test protocol or how to configure a test store.

Some tests use `Stdlib.Test.Process.run` to start another process and check its
output or timeout behavior. That process gets a copy of the test's current
store, including any changes the test has already made. Changes made by the
child process stay in its own copy.

For example, our CLI rendering tests start a child process, capture what it
prints, and compare that output with the expected screen.

The worker reads its request and writes its result through separate files,
leaving stdout and stderr available for output. The host handles timeouts and
removes temporary data. POSIX process groups and Windows jobs let it stop the
worker's child processes too.

If the worker crashes, returns no result, or cleanup fails, the test fails—even
if its body returned `Pass`. Tests that expect an error still fail if the worker
crashes or cleanup fails.

Workers have two minutes to finish by default. The timer starts after the
snapshot is prepared.

Tests run with an allow-all instance policy, without changing the installation's
saved permissions. Package approvals still apply, along with any permission
limits declared by a function or inherited from its caller.

Tests get their own store and temporary directories, but this isn't a security
sandbox. They can still access files elsewhere or contact real servers when
permissions allow it. For example, a test with file-write permission could modify
a file outside its temporary directory.

## Assertions and expected errors

`Stdlib.Test` provides helpers such as `expect`, `equal`, `notEqual` and `all`.
These produce `Pass` or `Fail` results. `all` combines several checks, but the
declaration still counts as one test.

`=> raises` checks that the runtime error message exactly matches the expected
text.

`=> sqlerror` checks that a SQL compiler error occurs and its message exactly
matches the expected text.

In the terminal output from `dark test`, failed assertions are shown as `FAIL`
and unexpected runtime errors as `ERROR`, before the test's name.

## How caching works

The runner can reuse a previous passing result instead of executing the test.
A cached pass can only be reused when these still match:

- **Test hash:** identifies the test's content, including the dependency versions
  it references. Changing either changes the hash.
- **Runtime and result checker:** the build and the Dark code that decides
  whether the test passed.
- **Branch:** cached results are kept separate for each branch.
- **Result type:** the branch still uses the same `Stdlib.Test.Result` type that
  the cached result was checked against.

The last two are explicit because the test hash alone does not capture them.
The runtime hash includes the .NET version and hashes the executable and
its installed dependencies. It changes when those files change, including
uncommitted runtime edits. Rebuilding
identical files keeps the same key; no separate build ID is generated.

The hash changes when the code changes. But the same code can return a different
result if a file, database value or network response changes. That's why the
runner also checks whether the result is safe to reuse.

F# checks the test, the functions it calls and the values it uses. If it finds
effects, changing state or behavior that depends on the branch or cannot finish
the analysis, the test runs again. Tests with private DBs or expected errors
always rerun.

Only passing results can skip a run. Failed assertions are saved for display,
but the test runs again next time. Runtime errors are not cached.

Tests using `equal` or `notEqual` currently aren't cached. These helpers can call
`toRepr` to build a failure message, and that formatting can depend on the branch.

The analyzer flags that possible call even when the test passes and no message is
needed. Better analysis could allow those passing results to be cached.

Reusing a cached result avoids starting a process or copying the store.
`dark test --force` runs tests even if their results are cached. CI uses this flag
to run all package tests.

## How this fits with the old tests and CI

The old execution tests have moved to package tests, and their runner has been
removed. Raw HTTP tests and backend integration tests still run in F#.

The old runner shared one process and database and could run independent tests
in parallel. Package tests also run in parallel, each with its own process
and store. This keeps tests from affecting each other's store, but adds process
startup and copying costs. CI uses the NativeAOT CLI to make startup faster.

Package reloads include tests and their helpers. Some tests intentionally contain
type errors, so `dark typecheck` can report those errors. The CI audit checks the
same loaded packages but excludes declarations named only under modules from
`tests/` source directories. A declaration with a production name is still checked,
even if a test module also names it. Package tests run separately.

The old runner compared floats approximately and handled NaN and blob contents
specially. Those assertions needed rewriting when the tests moved to Dark.

Unicode normalization checks are still commented out because the CLI uses
`InvariantGlobalization=true`. To restore them, switch it to `false` and verify
ICU support.
