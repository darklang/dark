# Coding guide / style guide

## File layout

- Every file should start with a comment describing it.

- all files have a formatter, which should be setup automatically in VSCode. Use
  `./scripts/formatting/format format` to format otherwise. Unformatted files fail in CI.

- imports should be ordered:
  - First dotnet and F# builtins
  - Then the `Prelude` library
  - then other Dark modules

## Names

- Avoid use of `foo`, `bar`, and `baz` in names (including as sample data in tests).

- Be consistent with built-in names. Always use the "correct" form below:
  - Int (not Integer)
  - String (not Str)
  - Float
  - Bool (not Boolean)
  - Char (not Character)
  - Uuid (not UUID)
  - Dict (not Dictionary)

## Comments

- All files should begin with a comment explaining the purpose of the file

- All directories should have a README describing their purpose

- All types, fields in records, constructors, functions, and modules, should have a
  comment unless extremely obvious. If unsure, add a comment. The comment does not need
  to be long, describing the purpose of the thing is usually enough.

## HTTP API

- Use REST where possible, don't hang on ceremony if it doesn't fit nicely
  - We haven't been good about doing REST, so migrate to it where possible
- JSON objects should use camelCase
  - in the past, they used snake_case, so we should switch

## F#

- `ignore` should always use a type signature (this should be enforced by the
  compiler)

- use `print` instead of `Console.WriteLine` or similar; the latter deadlocks

- ensure that `try` do not have a `uply`/`task` in the body unless you know what you
  are doing and provide a comment. Typically, the `uply`/`task` should be on the
  outside, which causes the compiler to compile the `try` into a version which supports
  Tasks. Otherwise, it won't catch an exception thrown within the `uply`/`task`.

- you can only use Tasks (aka `Ply`) once. Using it a second time is undefined.

- When writing SQL, ensure that expensive operations to not happen while in the
  reading loop. That is, don't do this:

  ```
  someSqlStuff
  |> Sql.executeAsync (fun read -> read.string "value" |> expensiveOperation)
  ```

  Instead do this:

  ```
  let! results =
    someSqlStuff
    |> Sql.executeAsync (fun read -> read.string "value")
  return
    results |> (List.map (fun str -> expensiveOperation str)
  ```

- use `///` for function comments

- For file header comments, use `///` and add them to the first line of the file
  before the module declaration

### Telemetry

- use `camel_case` names for tags
- prefer adding more attributes to a span vs events (events cost money and you can't
  search across them in honeycomb)

### SQL migrations

- add `set statement_timeout = '1s'` or `set lock_timeout = '1s'` to the first line
  of your script, so that it fails instead of taking the service down.
  (CLEANUP: make this happen automatically)

- migrations are run manually before deployment (using `ProdExec migrations run`)

### Initialization

- initialization code should be in a function called `init` in a file called `Init.fs`

- Library initialization code can rely on the DB but the services (eg BwdServer) must
  ensure to call them in the right order to ensure the DB is available and in the right
  shape (as migrations will not necessarily have run in tests at that point).

- Do not block in library initialization code, instead return `Lazy` or `Task`, and
  let the service resolve it.

- remove unused `Init.fs` files - they create cognitive load

### Types

- Avoid using bools for function parameters to configure. Instead use a type with two
  cases. `match` on the type to ensure exhaustive checks

- Unless impossible or impractical to do so, avoid using wildcards in pattern
  matches. When changing a type we would like the compiler to tell us everywhere that
  has to be changed.

- Include types for parameters, as well as the return type, of all functions.

#### Creating types

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
