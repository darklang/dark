# FuzzTests

This aims to find test cases that violate certain properties that we expect
hold true, such as successful serialization round-tripping, and to ensure that
regressions don't happen when updating the interpreter.

Some generators written here may be used in the adjacent `../Tests` project,
for tackling specific corner cases.

At time of writing, these tests are not run as part of CI, but are only
manually run by developers.

## Writing Tests

[FsCheck](https://fscheck.github.io/FsCheck/) is used to generate test cases
and encode properties that we expect to be true.

## Running Tests

[Expecto](https://github.com/haf/expecto) is used to run tests.

You can trigger test runs by running `./scripts/run-fsharp-fuzzer` within the
dev container

(View the project's root `README.md` for details on how to run within the dev
container.)

If you only want to run some of the tests,
- include flag `--filter-test-case 'case-name'` to run specific tests that
  match the pattern
- include flag `--filter-test-list 'list-name'` to run test lists that match
  the pattern