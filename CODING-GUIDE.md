# Coding guide / style guide

## File layout

- Every file should start with a comment describing it.

- all files have a formatter, which should be setup automatically in VS Code. Use
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

## JSON

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

- there are exactly two places, and which one you want depends on whether the store
  already exists:

  - `backend/migrations/schema/*.sql` declares the shape a NEW store is born with.
    Ordered by filename, concatenated, hashed; a change drops the regenerable
    projections and replays. Every statement is `IF NOT EXISTS`, so it adds nothing
    to a table that is already there.

  - `backend/src/LibDB/Releases.fs` is how a change reaches a store that ALREADY
    exists: an append-only list of named steps, each safe to run against a store of
    any age, recorded in `system_migrations_v0`. A new column, a new table, a
    backfill -- all of it goes here, in addition to declaring it in `schema/`.

  There used to be a third (`migrations/incremental/*.sql`) with no written rule for
  choosing, which is how one migration got written twice and failed both ways. A raw
  `.sql` file cannot look at the store before acting, and every change to an existing
  store has to.

- `scripts/migrations/new <tag>` scaffolds BOTH halves and cross-references them, which is
  the point: forgetting the second is the mistake this is built to stop. A new column goes
  in its table's numbered subsystem file rather than the stamped one, so a table stays
  defined in one place.

- they run as part of `scripts/dev/build`; there's no separate step

### Initialization

- initialization code should be in a function called `init` in a file called `Init.fs`

- Library initialization code can rely on the DB but the entry points (Cli, Tests,
  LocalExec) must ensure to call them in the right order so the DB is available and
  in the right shape (migrations may not have run yet in tests).

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
