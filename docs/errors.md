# Error Handling

Within the Darklang F# codebase, there are a number of different ways to handle errors.
This document explains which error handling mechanism to use.

Note, this is not for user code or packages, just for the execution engine and the
rest of the darklang runtime (aka stuff written in F#).

## Which error handler to use and when?

- if the error should _never_ happen, `Exception.raiseInternal`
- if an error is unexpected but possible (like integer overflows), raise a `RT.RuntimeError`
  - these generally represent problems within the users' code.
- if an error is relatively common, use a `Result<?,?>`

## Runtime Errors & Type Errors

The primary way of indicating an error in the runtime and during code execution is a
`RuntimeError`. This indicates there was an exceptional - though anticipated - error
in execution, typically caused by the user's code.

Example:

At the end of the `match`, a `RuntimeError` is raised by the Interpreter if no cases are matched,
as otherwise it's unclear how to proceed.

```fsharp
match 45 with
| 1 -> "only thing matched"
```

Our goal is to fully prevent `RuntimeError`s in our ahead-of-time type checker;
if the code type-checks, it should never fail in unexpected ways.

### Using RuntimeError

If you need to raise a RuntimeError, call `RT.raiseRTE vm.threadID`, or
`RT.raiseUntargetedRTE` where there is no VM at hand. Both throw a
`RuntimeErrorException` holding the error, which is caught at the execution boundary
(`LibExecution.Execution.execute`, which `executeExpr` and `executeFunction` go
through). The boundary returns an `RT.ExecutionResult`, a
`Result<Dval, RuntimeError.Error * CallStack>`. To show the error to a user,
`Execution.runtimeErrorToString` converts it to a `Dval` with
`RuntimeTypesToDarkTypes.RuntimeError.toDT` and renders it with the Dark function
`Darklang.PrettyPrinter.RuntimeTypes.RuntimeError.toString`.

#### Tips for using RuntimeErrors

We prefer to raise `RuntimeErrorException`s using `raiseRTE`, instead of returning
F# `Result<_, RuntimeError.Error>`.

Never catch a `RuntimeErrorException` except at the outer boundaries of code
execution: `Execution.execute`, the scheduler when a process fails, and the CLI's
`guestTry`, which reports an evaluated expression's error. Catching it anywhere else
hides the error from the user.

#### Where the error happened

A `RuntimeError` does not carry its location. The boundary pairs it with the call
stack of the VM that raised it (`Execution.callStackFromVM`), and that is what tells
the user where it came from.

### Builtin functions

When writing a builtin function, typically we handle errors by returning a
`Stdlib.Result.Result` value. The `Error` case should use a custom type appropriate
for that function or for general use in the module, defined in a package.

This sort of error handling is appropriate where the user calls the function
incorrectly, or something bad happens beyond our control.

Example: `Stdlib.HttpClient.get("http://mywebsiteisdowntoday.invalid", [])` gives
`Error(NetworkError(DnsFailure(Destination("mywebsiteisdowntoday.invalid"))))`
(with cases qualified under `Stdlib.HttpClient`). `Stdlib.HttpClient.toString`
formats it as `Could not resolve host: mywebsiteisdowntoday.invalid`.

Example: `Stdlib.Float.parse("not a float")` gives `Error(Stdlib.Float.ParseError.BadFormat)`

#### Raise RuntimeErrors for type errors in builtins

If a builtin is given a value that is not the type its signature says, raise
`RTE.Applications.FnParameterNotExpectedType` instead of returning a `Result` (see
how `httpClientRequest` checks its headers). Use `Result`s for errorable conditions
like trying to read a file which doesn't exist.

When a builtin's arguments don't match the patterns it expects at all, the type
checker should already have stopped the call, so that is a bug in the runtime:
`RT.incorrectArgs ()`, which raises an internal exception.

The interpreter checks what a lambda returns, so a builtin does not need to
(`RTE.Applications.LambdaResultNotExpectedType`).

#### Custom errors in Builtins

It's important not to use `Result<_, String>` in Builtins, as those errors are
difficult for a user to handle resiliently. Instead, create a darklang type for the
error.

For example `Builtin.httpClientRequest` returns
`Stdlib.Result.Result<Stdlib.HttpClient.Response, Stdlib.HttpClient.RequestError>`.

This is the same best-practice as writing code in a darklang package or user code.

Note that the error type should be created in a package even though it's used from a
builtin - that's by design and works fine.

#### Catch F# exceptions in builtins

If code raises a dotnet `System.Exception`-based Exception, it should be caught at
the callsite and should not spread any further. Do not use the error message
(`e.Message`) from dotnet exceptions in messages to users.

### Exception.raiseInternal

`Exception.raiseInternal` is for things that should not happen. It indicates a bug in
our code, not in the user's code.

Don't use it for things where the user can trigger it by making a coding error - use a
`RuntimeError` for that. This is for things where the user should not have been able
to make this happen.

For example, if we call a builtin function where the parameter type is
`List<String>`, we will still need to go through the list and extract the dotnet
`string`s from the `DString`s in the list. Since the typechecker has already checked
that the list should only have `DString`s in it, finding anything else in there
should be impossible and is a bug in the runtime.

`Exception.raiseInternal` cases are all bugs, and so they are things we wish to know
about.

`Exception.raiseInternal` carries a stacktrace and a set of metadata for debugging.
Nothing collects them at the moment. Treat an internal exception as something you
find out about by hitting it, not something anyone gets alerted to.

Connecting the CLI runtime to a reporting flow, so users can send these
semi-automatically, is still the intention.

## RuntimeError implementation

`RuntimeError.Error` is an F# union in `LibExecution/RuntimeTypes.fs`
(`module RuntimeError`). Its cases are either single errors (`VariableNotFound`,
`FnNotFound`, ...) or wrap the `Error` type of a submodule for one area (`Bools`,
`BigInts`, `Lists`, `Records`, `Applications`, ...).

The same type is declared in Dark, as `Darklang.LanguageTools.RuntimeTypes.RuntimeError`
in `packages/darklang/languageTools/runtimeErrors.dark`, so that Dark code can render
it.

### Making a new RuntimeError

Add a case to the F# type: to one of the submodule `Error` types, or to the top-level
`Error`, or a new submodule with an `Error` and a case wrapping it. Then, in step:

- the same case in `runtimeErrors.dark`, with the same name and fields
- its `toDT` and `fromDT` in `RuntimeTypesToDarkTypes.fs` (`module RuntimeError`)
- for a new submodule, its type in `PackageRefs.fs` (`RuntimeError`)
- its message in `toSegments`, in `packages/darklang/prettyPrinter/runtimeError.dark`

### Prefer a specific RuntimeError

`UncaughtException(msg, metadata)` is the catch-all for very unexpected things, and
anything a builtin throws that isn't caught ends up there. Prefer a specific case
for your module (or an extra case in the module's existing `Error` type, if that
feels more appropriate), so the user gets a message written for the error.

### What to put in a RuntimeError

`RuntimeError`s often have fields that can be used to provide errors to the user.
When deciding what fields to include, a good approach is to try to write the error
message you'd like the user to see, and to capture any values you need for it. The
location comes from the call stack, so you do not need to capture the expression
being executed.
