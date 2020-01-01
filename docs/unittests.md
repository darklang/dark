# Unit tests

## Overview

Unit tests run automatically on the client and backend, as part of
the compile script. Run it with --test to run tests.

## Backend

The entry point is backend/test/test.ml. Run tests from the command
line using:

  `run-in-docker backend/_build/default/test/test.exe`

Tests are _not_ automatically discovered; they must be added to the
list at the bottom of the test file. New test files must be added
to test.ml.

## Client-side

To run tests, run `scripts/runtests` or `npm run test` (slower).
Run `scripts/runtests --help` for options.

Tests are _not_ automatically discovered; they must be added to
`run` in the file in question to run automatically, and new files
need to be added to unittest.ml. 

Our test harness is a tiny homegrown test suite, in
client/test/tester.ml. We initially used jest; Unfortunately,
it had such poor performance that a rewrite was faster than
figuring out why it was bad. 

## Integration tests

There are also integration tests, see integration-tests/README.md.

