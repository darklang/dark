# By-hand checks for interpreter changes

The backend suite has twice passed while the interpreter returned a quietly wrong answer, so
anything that touches execution gets run past these as well. They are not automated: read the
output.

    ./scripts/run-cli run scripts/testing/perf-workloads/checks/semantics.dark
    ./scripts/run-cli run scripts/testing/perf-workloads/checks/type-error.dark
    ./scripts/run-cli run scripts/testing/perf-workloads/checks/lambda-callstack.dark

- `semantics.dark` covers self-recursion, enums (including a recursive case), records, lambdas,
  matches, map/filter/fold, and a polymorphic `Json.serialize`/`parse` round trip in both
  directions. `fib 18` is 2584 and the areas are `[12.56636, 12.0, 3.0]`.
- `type-error.dark` passes a String where an Int64 is wanted. The message must name the parameter
  and both types.
- `lambda-callstack.dark` raises inside a lambda nested two deep. The call stack must show the
  package functions and then both `Lambda` frames -- worth checking whenever frames start sharing
  `ExecutionPoint`s.
