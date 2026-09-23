# `let!`

`let! pat = e` binds the success of a Result or Option. On failure it returns
that failure from the nearest enclosing function or lambda, immediately.

```dark
let total (a: String) (b: String) : Stdlib.Result.Result<Int, Stdlib.Int.ParseError> =
  let! x = Stdlib.Int.parse a
  let! y = Stdlib.Int.parse b
  Ok (x + y)

let firstLength (items: List<String>) : Stdlib.Option.Option<Int> =
  let! first = Stdlib.List.head items
  Some (Stdlib.String.length first)
```

## Behavior

| Value of `e` | `let! pat = e` | Control flow |
| --- | --- | --- |
| `Ok value` | binds `value` to `pat` | Continue |
| `Error error` | — | Return `Error error` |
| `Some value` | binds `value` to `pat` | Continue |
| `None` | — | Return `None` |

`e` is evaluated exactly once. Failure skips the rest of that function or
lambda, including later statements. Completed effects are not undone. Only a
branch that runs can fail. Propagation is an ordinary return, not an exception.

Success stays explicit: a Result-returning function writes `Ok value`, and an
Option-returning function writes `Some value`. There is no automatic wrapping.

To fail on purpose, bind a failure: `let! _ = if n < 0 then Error "negative"
else Ok ()`. Patterns destructure as with `let`: `let! (a, b) = pair`. On one
line, use `in`: `let! n = parse s in Ok (n * 2)`.

## Types and boundaries

- In `let! x = e`, `e : Result<A, E>` binds `x : A`; the enclosing function
  must return `Result<B, E>`.
- `e : Option<A>` binds `x : A`; the enclosing function must return
  `Option<B>`.
- `A` and `B` may differ: the success is consumed and a new one built. Result
  error types must agree, since the error is passed through unchanged. Aliases
  are resolved.
- Mixing Option and Result requires an explicit conversion, such as
  `let! user = maybeUser |> Stdlib.Option.toResult "user missing"`.
- To add context to an error, map it first:
  `let! x = Stdlib.Int.parse a |> Stdlib.Result.mapError (fun _ -> "a: not a number")`.
- Inside a lambda or nested function, failure returns from that, never its
  caller. The caller may use another `let!`.
- `let!` outside a function or lambda is rejected, including in package value
  bodies.
- `let!` binds a value: `let! f (x: Int) = …` is rejected, and `let !x` (with a
  space) is not `let!`.

These rules are enforced by the at-rest type checker, like every other type
error: `dark typecheck`, the LSP and authoring report them. The runtime needs no
inferred types. A failure becomes the frame's result, which then goes through
the frame's ordinary return-type check.

Code that skips the checker (scripts, `eval`) still gets a container check where
the compiler can see the container: a function's declared return type, or a
lambda ending in `Some`/`None` or `Ok`/`Error` (through `let`, statements, and
every branch of an `if` or `match` that shows one). There, a `let!` of the other
container fails on the spot, whatever the value:

    (fun v -> let! n = v in Some n) (Ok 7)
    // `let!` got a Result, but the enclosing function or lambda returns an
    // Option; convert it first with Stdlib.Result.toOption: Ok(7)

Where nothing shows it (a lambda ending in `wrap n`, or a declared return type
that is an alias), nothing is checked at the `let!`:

| Code | Wrong-kind success | Wrong-kind failure |
| --- | --- | --- |
| a function returning `MaybeInt`, ending `wrap n` | bound; returns normally | rejected by the return-type check |
| a lambda ending `wrap n` | bound; returns normally | returned as it is |

A declared function never lets a wrong-kind value out; the mistake surfaces
only on the error path. A lambda has no declared type, so it returns the
failure as it is, as any lambda may return any value. The at-rest checker
rejects all of these as package code.

A value that is not an Option or Result fails at the `let!`:
`` `let!` needs an Option or Result, but got 4 (an Int) ``.

## How it's built

`let! pat = e` is parser sugar: a `let` whose value is a propagation expression
(`EPropagate`). Type checking, the runtime, serialization and hashing see only
that. The printer turns it back: a `let` whose value propagates prints as
`let!`.

Propagation has its own expression and runtime instruction, with new
serialization tags; existing tags and the hashes of unchanged expressions stay
stable. Older binaries cannot execute code containing the new tags. The local
package reference generator must regenerate the `ProgramTypes.PackageOp`
reference because that type contains expressions.

SQL query compilation rejects propagation with an explicit unsupported-operation
error; it does not silently ignore the early return.

## Known gaps

- Scripts and `eval` skip the static checker, so a lambda whose ending doesn't
  show its container can return a wrong-kind failure. Tracked by the `CLEANUP`
  in `backend/src/Builtins/Builtins.CliHost/Libs/Cli.fs`.
- Chained field access through an inferred lambda parameter
  (`fun row -> … row.child.value`) is reported as incomplete by the checker.
  This predates `let!` and is a separate follow-up.

This version adds no `throws`, catch syntax, or custom propagation protocol.
