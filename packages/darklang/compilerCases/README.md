# Compiler cases

Inputs for the compiler coverage report (`scripts/compiler/report`). Each case is a
nullary fn whose body calls one target fn with literal arguments, so the compiled and
interpreted results can be diffed byte for byte. They are ordinary package fns: the
type checker sees them, `dark view` shows them, and the interpreter's tests can run
them too.

The module path mirrors the target's, minus `Darklang.CompilerCases`, and the fn name
is `<target>_<case>`:

    module Darklang.CompilerCases.Stdlib.List
    let map_basic () : List<Int64> = Stdlib.List.map [1L, 2L, 3L] (fun x -> x * 2L)

names the target `Darklang.Stdlib.List.map` and the case `basic`. A target with no
cases here falls back to synthesized arguments in the report, and is marked as such.

Cases are for what the SYNTHESIZED arguments cannot reach: anything with a function
or custom-typed parameter, empty inputs, the error path of a Result, unicode in
strings. One case per distinct shape; the point is coverage of behaviour, not volume.
