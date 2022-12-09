# Unit tests

## Overview

Unit tests run automatically on the client and backend, as part of
the builder script. Run it with `--test` to run tests.

## Backend

The entry point is `backend/tests/Tests/Tests.fs`. Run tests from the
command line using:

`scripts/run-fsharp-tests`

Run `scripts/run-fsharp-tests --help` for options. In particular, to run only
tests with XXX in their names:

`scripts/run-fsharp-tests --filter-test-case XXX`

Or to run only testlists with XXX in their names:

`scripts/run-fsharp-tests --filter-test-list XXX`

Tests are _not_ automatically discovered; they must be added to `Tests.fs`.

We also have a number of property-based tests, which we currently keep separate
in `backend/tests/FuzzTests`. Run them with `scripts/run-fsharp-fuzzer`.

## Client-side

To run tests, run `scripts/run-client-tests` or `npm run test` (slower).
Run `scripts/run-client-tests --help` for options.

Tests are _not_ automatically discovered; they must be added to `run` in the
file in question to run automatically, and new files need to be added to
`unittests.res`.

Our test harness is a tiny homegrown test suite, in `client/test/tester.res`.
We initially used jest; Unfortunately, it had such poor performance that a
rewrite was faster than figuring out why it was bad.

## Integration tests

There are also integration tests, see `integration-tests/README.md`.
