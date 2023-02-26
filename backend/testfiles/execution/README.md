# Dark Execution test files

The files in this directory are unit tests for the Dark language and standard
libraries. A typical test is a single line, such as:

`String.length_v0 "abcdef" = 6`

This is written in a Dark-like syntax, and asserts that calling the function
`String::length_v0` on `"abcdef"` is evaluated to 6. More details on the syntax
of these tests may be found in `testfiles/README.md`.

Most tests are written as single lines, however, we support multi-line tests,
test groups, functions, Datastores and workers.

The implementation of the tests is in `LibExecution.Tests.fs`.

All tests must be of the format `x = y` (or `x <> y`, though that's rarely
used), that is, they must have a single expression on the left, one on the
right, and an equals sign in between. You can use parens around multiple
expressions to group them into a single expression, eg `(5 |> toString)`.

# Test file format

Test file format is as follows:

Lines with just comments or whitespace are ignored. Single-line tests are made
up of code optionally followed by a comment (if present, the comment is used as
the test name).

We also support more complex tests, involving multiple lines, test groups,
datastores and functions. To do so, add a test indicator (see below) - all
code after a test indicator is put into that construct.

## Test indicators:

`[tests.name]` denotes that the following lines (until the next test indicator)
are single line tests, that are all part of the test group named "name". Single
line tests should evaluate to true, and may have a comment (`// ...`) at the
end, which will be the test name.

`[test.name]` indicates that the following lines, up until the next test
indicator, are all a single test named "name", and should be parsed as one.

`[db.name json_desc_of_schema]` creates a User DB, which can be used by tests
which say "with DB DBNAME". (Only give DBs to tests which need them, as these
tests need to be isolated and that's much slower)

`[test.name] with DB MyDB` is like `[test.name]`, where the DB previously
defined as MyDB is available to the test. Cannot currently be used with
`with Worker MyWorker` syntax.

`[test.name] with Worker MyWorker` is like `[test.name]`, where the Worker
MyWorker is available to the test, without prior setup required. Cannot be used
with `with DB MyDb` syntax.

`[fn.name arg1:int arg2:str]` creates a function which is available to all
subsequent tests. The following lines are part of the function body (until we
hit another test indicator)

`[packagefn.name arg1:int arg2:str]` creates a function which is available to
all subsequent tests. The following lines are part of the function body (until
we hit another test indicator). Package functions call be called as
`Test.Test.Test.myFn`.