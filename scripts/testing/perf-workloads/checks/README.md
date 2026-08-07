# By-hand checks for interpreter changes

The backend suite has twice passed while the interpreter returned a quietly wrong answer, so
anything that touches execution gets run past these as well. They are not automated: read the
output.

    ./scripts/run-cli run scripts/testing/perf-workloads/checks/semantics.dark
    ./scripts/run-cli run scripts/testing/perf-workloads/checks/type-error.dark
    ./scripts/run-cli run scripts/testing/perf-workloads/checks/lambda-callstack.dark
    ./scripts/run-cli run scripts/testing/perf-workloads/checks/record-errors.dark
    ./scripts/run-cli run scripts/testing/perf-workloads/checks/type-identity.dark

- `semantics.dark` covers self-recursion, enums (including a recursive case), records, lambdas,
  matches, map/filter/fold, and a polymorphic `Json.serialize`/`parse` round trip in both
  directions. `fib 18` is 2584 and the areas are `[12.56636, 12.0, 3.0]`.
- `type-error.dark` passes a String where an Int64 is wanted. The message must name the parameter
  and both types.
- `record-errors.dark` covers the four ways building a record fails. Run it four times, swapping
  which line is uncommented; the expected messages are listed in the file. These paths are only
  reached once something has already gone wrong, so they are easy to break invisibly.
- `type-identity.dark` passes a record of the wrong type. The type checker has a fast path that
  answers same-type-no-type-args without consulting the store, so this is the one that would catch
  it accepting anything. It also documents why two structurally identical records are the *same*
  type in Dark, which makes the obvious version of this test useless.
- `lambda-callstack.dark` raises inside a lambda nested two deep. The call stack must show the
  package functions and then both `Lambda` frames -- worth checking whenever frames start sharing
  `ExecutionPoint`s.
