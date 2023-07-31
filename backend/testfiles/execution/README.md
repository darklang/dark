# Dark Execution test files

The files in these directories are unit tests for the Dark language and standard
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
datastores and functions. These use regular F# syntax, occasionally with some minor tweaks.

## Groups

Use F# modules to make test groups:

```fsharp
module MyTestGroup =
  // test1
  // test2
```

Nested modules/groups are supported.

Any type, function, or DB declared in a group will be made available to all tests in
that group, or in a nested group.

## Functions

Functions are declared in the normal way:

```fsharp
let myFn (x : int) (y : string) : bool =
  ...
```

## Constant

Constants are declared in the normal way:

```fsharp
let myConstant = 5
```

## Types

Types are declared in the normal way. Note that Darklang types are capitalized.

```fsharp
type MyType = {
  anInt : string
}
```

## DBs

You can create a DB by making a type alias with a DB annotation:

```fsharp
[<DB>]
type MyTypeDB = MyDB
```

## Packages

Packages should be defined in the packages directory, perhaps in the Darklang.Test namespace.
