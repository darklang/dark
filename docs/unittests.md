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

For application and standard-library tests, use ordinary Dark functions returning
`Stdlib.Test.T`:

```dark
let doubles () : Stdlib.Test.T =
  Stdlib.Test.table double [ (0, 0), (4, 8), (-3, -6) ]
```

`dark test MyApp.Math` runs a module; `dark test MyApp.Math.doubles` runs one test.
`dark test new MyApp.Math.Tests.doubles --for MyApp.Math.double` creates a failing
template and opens your editor when a terminal is available. Add `--no-editor`
to create it without opening an editor. `dark docs testing` has the full guide.

Checks return values: return each check or combine them with `Stdlib.Test.all`.
Empty tables fail; table row errors retain the other failures and allow later
rows to run.

The CLI integration suite also runs every discovered test under `Darklang`, so
adding a standard-library test requires no CI registration.

`backend/testfiles/README.md` covers the separate execution testfile harness,
which supports language/runtime checks such as expected interpreter errors.
