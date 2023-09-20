# Error Handling

Within the Darklang codebase, consistent and clear error handling is paramount. This guide provides a straightforward set of practices to follow when dealing with different types of errors.

The codebase has a handful of different ways to handle errors. In the runtime we have:
- F# results
- darklang results (user-facing version)
- Exception.raiseInternal
- Type Errors
- other runtime errors

## Runtime Errors & Type Errors

The primary way of indicating an error in the runtime and during code execution is a
`RuntimeError`.

### Using RuntimeError

If you need to raise a RuntimeError, call `RT.Error.raiseRTE`. This throws an
exception with the RuntimeError in it, which is caught at the execution boundary (eg
`LibExecution.Execution.executeExpr`). At the execution boundaries, the functions
returns an F# `Result<Dval, RuntimeError>`, which are then converted to Darklang and
can be stringified using `@darklang.languageTools.runtimeErrors.error.toString`.

A very common RuntimeError to use is a `TypeChecker.Error.ValueNotExpectedType`,
which indicates that we have the wrong type. There are helper functions like
`TypeChecker.raiseValueNotExpectedType` and
`TypeChecker.raiseFnValResultNotExpectedType` that can make this easier (the latter
for checking return values from first-class functions called from builtin functions).

We prefer to raise `RuntimeErrorException`s using `raiseRTE` or something similar, vs
returning F# `Result<_, RuntimeError>`s.

Never catch a `RuntimeError` except in `LibExecution.Execution`. That's not how
they're intended to be used.

### Getting rid of string-based RuntimeErrors

Many `RuntimeError`s in the codebase at the moment use `string`s, usually via
`raiseString` or `Runtime.oldError`. We really really want to move away from that.
Prefer to make a new user-facing `RuntimeError` for your module, or add an extra
variant to the existing Error enum in a module.

### DvalSource, Location, and raiseUntargetedRTE

We're trying to get to a place where we always include the `DvalSource` of an RuntimeError, but often it can be hard. We plan to ensure we pass around enough information so that RuntimeErrors always have a `DvalSource` - without which we do not have enough


## Exception.raiseInternal

`Exception.raiseInternal` is for things that should not happen. Note: not for things
where the user can trigger it by making a mistake - use a `RuntimeError` for that.
This is for things where the user should not have been able to make this happen. It
indicates a bug in our code, not in the user's code.

For example, if we call a builtin function where the parameter type is
`List<String>`, we will still need to go through the list and extract the F# `string`
from the `DString`s in the list. Since the typechecker has already checked that the
`DList` should only have `DString`s in it, finding anything else in there should be
impossible and is a bug in the runtime.

`Exception.raiseInternal`, in the cloud runtime, creates an exception that we can
track and get alerts from Rollbar, including a stacktrace and a set of useful
metadata for debugging the issue. We should debug and solve these.

## Errors in Builtin functions

When writing a builtin function, typically we handle errors by returning a
`@darklang.stdlib.Result.Result` value.

We have been in the habit of creating `Result<_, String>` - don't do that. Instead,
create a darklang type for the error. That type will be created in a package even
though it's used from a builtin - that's fine. Eg `Builtin.HttpClient.request`
returns `@darklang.stdlib.HttpClient.Request`, which is `Result<_,
@darklang.stdilib.HttpClient.Error>`.

Types are cheap: it is better to create a new type that will be clear and obvious to
users, rather than trying to reuse a general type or a string.

### Raise RuntimeErrors for type errors in builtins

If a builtin has a type error, such as the wrong type being returned by
`Interpreter.applyFnVal`, use a `TypeChecker.Error` instead of a `Result`.

If you expect a type, as indicated by the type signature, and it is not that type, a
TypeChecker.Error is appropriate. Use `Result`s for errorable conditions like trying
to read a file which doesn't exist.

### F# exceptions in builtins

If code raises a dotnet `System.Exception`-based Exception, it should be caught at
the callsite and should not spread any further. Do not use the error message from
dotnet exceptions darklang-specific error mechanisms.

## RuntimeError implementation

Unusually, a `RuntimeError` is actually a wrapper around a `Dval`, which corresponds
to a type in `@darklang.languageTools.runtimeErrors`. Using a `Dval` here allows us
work around some of the problems with the F# module ordering, and define/create
`RuntimeError`s from anywhere.

### Making a new RuntimeError

To make a new `RuntimeError`, create a package type for it in
`@darklang.languageTools.runtimeErrors`. Then you will need a set of functions in F#
to produce it and convert it to a `Dval`, as well as handling the
stringification in `@darklang.languageTools.runtimeErrors.error.toString`.

A good approach is to have an F# type that matches it, and then create `MyError.toDT`
functions to convert it to a Darklang value. We commonly also have `toRuntimeError`
functions, as well as convenience functions to raise the exceptions.

Most modules will have an Error mmodule, such as `TypeChecking.Error`, or
`Interpreter.ExecutionError`, which have types and functions to allow creating
RuntimeErrors. Convenience functions for raising these errors are helpful.

### What to put in a RuntimeError

`RuntimeError`s often have fields that can be used to provide errors to the user.
When deciding what fields to include, a good approach is to try to write the error
message you'd like the user to see, and to capture any values you need for it. Note
that RuntimeErrors are captured with `DvalSource` (tlid and id of the expression/etc
it happens in), so you do not need to capture this.

