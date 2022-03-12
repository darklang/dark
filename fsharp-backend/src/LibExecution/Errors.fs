/// Reusable errors and messages used mostly in the standard library
module LibExecution.Errors

open Prelude
open RuntimeTypes

// ------------------
// Exception types
// ------------------

type StdlibError =
  | IncorrectArgs
  /// When we encounter a fakeDval, this exception allows us to jump out of the
  /// computation immediately, and the caller can return the dval. This is useful
  /// for jumping out of folds and other complicated constructs.
  | FakeDvalFound of Dval
  | StringError of string

/// Error made in a standard library call. This allows us to call them when we
/// don't have the information to make a RuntimeException, and they are
/// converted to runtimeExceptions at the call site.
exception StdlibException of StdlibError

/// Error in DB::query when we don't support something yet
exception DBQueryException of string

/// Error in DB::query when there's a fakeval in the query
exception FakeValFoundInQuery of Dval



// ------------------
// Messages
// ------------------
let expectedLambdaType (fnName : string) (typ : DType) (actual : Dval) : string =
  let actual = DvalReprExternal.toDeveloperReprV0 actual
  let typ = DvalReprExternal.typeToDeveloperReprV0 typ
  $"Expected `{fnName}` to return a {typ}, but it returned `{actual}`"

let expectedLambdaValue
  (fnName : string)
  (expected : string)
  (actual : Dval)
  : string =
  let actual = DvalReprExternal.toDeveloperReprV0 actual
  $"Expected `{fnName}` to return {expected}, but it returned `{actual}`"


/// Used for values which are outside the range of expected values for some
/// reason. Really, any function using this should have a Result type instead.
let argumentWasnt (expected : string) (paramName : string) (dv : Dval) : string =
  let actual = DvalReprExternal.toDeveloperReprV0 dv
  $"Expected the argument `{paramName}` to be {expected}, but it was `{actual}`"

/// Used for lists which contain invalid values for some reason.
let argumentMemberWasnt (typ : DType) (paramName : string) (dv : Dval) : string =
  let actual = DvalReprExternal.toDeveloperReprV0 dv
  let typ = DvalReprExternal.typeToDeveloperReprV0 typ
  $"Expected `{paramName}` to be a list of {typ}s, but the list contained `{actual}`"

let queryCompilerErrorTemplate =
  "You're using our new experimental Datastore query compiler. It compiles your lambdas into optimized (and partially indexed) Datastore queries, which should be reasonably faster.\n\nUnfortunately, we hit a snag while compiling your lambda. We only support a subset of Dark's functionality, but will be expanding it in the future.\n\nSome Dark code is not supported in DB::query lambdas for now, and some of it won't be supported because it's an odd thing to do in a datastore query. If you think your operation should be supported, let us know in #general.\n\nError: "

let typeErrorMsg (colName : string) (expected : DType) (actual : Dval) : string =
  let expected = DvalReprExternal.typeToDeveloperReprV0 expected
  let actualType = DvalReprExternal.prettyTypename actual

  $"Expected a value of type {expected} but got a {actualType} (`{actual}`)"
  + $" in column {colName}"

// ------------------
// Extremely common exceptions
// ------------------
let throw (str : string) : 'a = raise (StdlibException(StringError str))

/// When a function in called with the wrong number of arguments.
/// Used in almost every function signature.
let incorrectArgs () = raise (StdlibException IncorrectArgs)

let intInfixFns = Set [ "+"; "-"; "*"; ">"; ">="; "<="; "<"; "^"; "%" ]

let incorrectArgsMsg (name : FQFnName.T) (p : Param) (actual : Dval) : string =
  let actualRepr = DvalReprExternal.toDeveloperReprV0 actual
  let actualType = Dval.toType actual
  let actualTypeRepr = DvalReprExternal.typeToDeveloperReprV0 actualType
  let expectedTypeRepr = DvalReprExternal.typeToDeveloperReprV0 p.typ

  let conversionMsg =
    match p.typ, actualType, name with
    | TInt, TFloat, FQFnName.Stdlib std when
      std.module_ = "Int"
      || (std.module_ = "" && Set.contains std.function_ intInfixFns)
      ->
      let altfn = { std with module_ = "Float" }

      $" Try using {FQFnName.StdlibFnName.toString altfn}, or use Float::truncate to truncate Floats to Ints."
    | TInt, TStr, FQFnName.Stdlib std when
      (std.module_ = "Int" && std.function_ = "add")
      || (std.module_ = "" && std.function_ = "+")
      ->
      " Use ++ to concatenate"
    | _ -> ""
  $"{FQFnName.toString name} was called with a {actualTypeRepr} ({actualRepr}), but `{p.name}` expected "
  + $"a {expectedTypeRepr}.{conversionMsg}"


/// When a function has been removed (rarely happens but does happen occasionally)
let removedFunction (state : ExecutionState) (fnName : string) : DvalTask =
  state.notify state "function removed" [ "fnName", fnName ]
  Ply(DError(SourceNone, $"{fnName} was removed from Dark"))

/// When you have a fakeval, you typically just want to return it.
let foundFakeDval (dv : Dval) : 'a = raise (StdlibException(FakeDvalFound dv))
