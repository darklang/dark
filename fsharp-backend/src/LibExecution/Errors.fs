/// Reusable errors and messages used mostly in the standard library
module LibExecution.Errors

open Prelude
open RuntimeTypes

// ------------------
// Exception types
// ------------------
// We have a number of special exceptions that are used as control-flow to jump out
// of Stdlib execution.
// ------------------

/// Standard error raised for calling a function with arguments of an incorrect type
exception IncorrectArgs

/// When we encounter a fakeDval, this exception allows us to jump out of the
/// computation immediately, and the caller can return the dval. This is useful
/// for jumping out of folds and other complicated constructs.
exception FakeDvalFound of Dval


// ------------------
// Messages
// ------------------
let expectedLambdaType (fnName : string) (typ : DType) (actual : Dval) : string =
  let actual = DvalReprDeveloper.toRepr actual
  let typ = DvalReprDeveloper.typeName typ
  $"Expected `{fnName}` to return a {typ}, but it returned `{actual}`"

let expectedLambdaValue
  (fnName : string)
  (expected : string)
  (actual : Dval)
  : string =
  let actual = DvalReprDeveloper.toRepr actual
  $"Expected `{fnName}` to return {expected}, but it returned `{actual}`"


/// Used for values which are outside the range of expected values for some
/// reason. Really, any function using this should have a Result type instead.
let argumentWasnt (expected : string) (paramName : string) (dv : Dval) : string =
  let actual = DvalReprDeveloper.toRepr dv
  $"Expected the argument `{paramName}` to be {expected}, but it was `{actual}`"

/// Used for lists which contain invalid values for some reason.
let argumentMemberWasnt (typ : DType) (paramName : string) (dv : Dval) : string =
  let actual = DvalReprDeveloper.toRepr dv
  let typ = DvalReprDeveloper.typeName typ
  $"Expected `{paramName}` to be a list of {typ}s, but the list contained `{actual}`"


let typeErrorMsg (colName : string) (expected : DType) (actual : Dval) : string =
  let expected = DvalReprDeveloper.typeName expected
  let actualType = DvalReprDeveloper.dvalTypeName actual

  $"Expected a value of type {expected} but got a {actualType} (`{actual}`)"
  + $" in column {colName}"

// ------------------
// Extremely common exceptions
// ------------------

/// When a function in called with the wrong number of arguments.
/// Used in almost every function signature.
let incorrectArgs () = raise IncorrectArgs

let intInfixFns = Set [ "+"; "-"; "*"; ">"; ">="; "<="; "<"; "^"; "%" ]

let incorrectArgsMsg (name : FQFnName.T) (p : Param) (actual : Dval) : string =
  let actualRepr = DvalReprDeveloper.toRepr actual
  let actualType = Dval.toType actual
  let actualTypeRepr = DvalReprDeveloper.typeName actualType
  let expectedTypeRepr = DvalReprDeveloper.typeName p.typ

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

let incorrectArgsToDError (source : DvalSource) (fn : Fn) (argList : List<Dval>) =
  let paramLength = List.length fn.parameters
  let argLength = List.length argList

  if paramLength <> argLength then
    (Dval.errSStr
      source
      ($"{FQFnName.toString fn.name} has {paramLength} parameters,"
       + $" but here was called with {argLength} arguments."))

  else
    let invalid =
      List.zip fn.parameters argList
      |> List.filter (fun (p, a) -> not (Dval.typeMatches p.typ a))

    match invalid with
    | [] -> Dval.errSStr source $"unknown error calling {FQFnName.toString fn.name}"
    | (p, actual) :: _ ->
      let msg = incorrectArgsMsg fn.name p actual
      Dval.errSStr source msg


/// When a function has been removed (rarely happens but does happen occasionally)
let removedFunction (state : ExecutionState) (fnName : string) : DvalTask =
  state.notify state "function removed" [ "fnName", fnName ]
  Ply(DError(SourceNone, $"{fnName} was removed from Dark"))

/// When you have a fakeval, you typically just want to return it.
let foundFakeDval (dv : Dval) : 'a = raise (FakeDvalFound dv)
