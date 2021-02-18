# Dark unit tests

The files in this directory are unit tests for the Dark language and standard libraries. A typical test is a single line, such as:

`String.length_v0 "abcdef" = 6`

This is written in a Dark-like syntax, and asserts that calling the function `String::length_v0` on `"abcdef"` is 6.

Most tests are written as single lines, however, we support multi-line tests,
test groups, functions, and Datastores.

# Code Syntax

The code is written using F#, which is very similar to Dark. It is parsed by the F# parser, then converted from F# to Dark. The error messages around this are not very good, but there are a few things to be careful of:

- all tests must be of the format `x = y`, that is, they must have a single
  expression on the left, one on the right, and an equals sign in between. You can use parens around multiple expressions to group them into a single expression, eg `(5 |> toString)`.

- be explicit around pipes, wrapping them in parens to make sure the right expression is being piped.

# Test file format

Test file format is as follows:

Lines with just comments or whitespace are ignored. Single-line tests are made
up of code optionally followed by a comment (if present, the comment is used as
the test name).

You can also constructs too, such as multi-line tests, test groups, datastores
and functions. To do so, add a test indicator (see below) - all code after a
test indicator is put into that construct.

## Test indicators:

`[tests.name]` denotes that the following lines (until the next test
indicator) are single line tests, that are all part of the test group
named "name". Single line tests should evaluate to true, and may have a
comment at the end, which will be the test name

`[test.name]` indicates that the following lines, up until the next test
indicator, are all a single test named "name", and should be parsed as
one.

`[db.name json_desc_of_schema]` creates a DB, which can be used by tests
which say "with DB DBNAME". (Only give DBs to tests which need them, as
that these tests need to be isolated and that's much slower)

`[test.name] with DB MyDB` indicates that the following lines, up until
the next test indicator, are all a single test named "name", and should be
parsed as one. The DB previously defined as MyDB is available to the test.

`[fn.name argnames]` creates a function which is available to all subsequent
tests. The following lines are part of the function body (until we hit
another test indicator)
