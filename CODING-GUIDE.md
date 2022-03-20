# Coding guide / style guide

## File layout

- Every file should start with a comment describing it (this is currently not done
  very much at all).

- all files have a formatter, which should be setup automatically in VSCode. Use
  `scripts/format format` to format otherwise. Unformatted files fail in CI.

- imports should be ordered:
  - First stdlib and language builtins
  - Then the `Prelude` library and `tablecloth`
  - then other Dark modules

## Names

- Avoid use of `foo`, `bar`, and `baz` in names (including as sample data in tests).

## Comments

- All files should begin with a comment explaining the purpose of the file

- All directories should have a README describing their purpose

- All types, fields in records, constructors, functions, and modules, should have a
  comment unless extremely obvious. If unsure, add a comment. The comment does not need
  to be long, describing the purpose of the thing is usually enough.

## F#

- `ignore` should always use a type signature (this should be enforced by the
  compiler)

- use `print` instead of `Console.WriteLine` or similar, the latter deadlocks

- ensure that `try` do not have a `uply` in the body unless you know what you are
  doing and provide a comment. Typically, the `uply` should be on the outside, which
  causes the compiler to compile the `try` into a version which supports Tasks.
  Otherwise, it won't catch an exception thrown within the `uply`.

- you can only use Tasks (aka `Ply`) once. Using it a second time is undefined.

- use `///` for function comments

- For file header comments, use `///` and add them to the first line of the file
  before the module declaration

## SQL migrations

- add `set statement_timeout = '1s'` or `set lock_timeout = '1s'` to the first line
  of your script, so that it fails instead of taking the service down.

## Initialization

- initialization code should be in a function called `init` in a file called `Init.fs`

- Library initialization code can rely on the DB but the services (eg ApiServer) must
  ensure to call them in the right order to ensure the DB is available and in the right
  shape (as migrations will not necessarily have run in tests at that point).

- Do not block in library initialization code, instead return `Lazy` or `Task`, and
  let the service resolve it.

- remove unused `Init.fs` files - they create cognitive load

### Creating types

When creating a type:

- create a module with the name of the type, and type T
- instead of members on the type, add functions in the module. These are then first
  class and can be used in eg `List.map`
- the `T` (the object) should go last in function signatures
- the exception is the `ToString()` method (the `string` function calls the
  overridden `ToString` method)

For example:

```
module RuleEngine =
  type T =
  | Rule1 of string
  | Rule2 of int
    override this.ToString() : string =
      match this with
      | Rule1 s -> s
      | Rule2 i -> string i
    let parse (str : string) : T =
      ...
```
