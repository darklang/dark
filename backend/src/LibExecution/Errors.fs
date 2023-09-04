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
// CLEANUP add word 'exception' to exception names


// ------------------
// Messages
// ------------------
let expectedLambdaType
  (fnName : string)
  (typ : TypeReference)
  (actual : Dval)
  : string =
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
  $"Expected `{paramName}` to be {expected}, but it was `{actual}`"

/// Used for lists which contain invalid values for some reason.
let argumentWasntType
  (expected : TypeReference)
  (paramName : string)
  (dv : Dval)
  : string =
  let actual = DvalReprDeveloper.toRepr dv
  let expected = DvalReprDeveloper.typeName expected
  $"Expected `{paramName}` to be a `{expected}`, but it was `{actual}`"

let resultWasntType (expected : TypeReference) (dv : Dval) : string =
  let actual = DvalReprDeveloper.toRepr dv
  let expected = DvalReprDeveloper.typeName expected
  $"Expected result to be a `{expected}`, but it was `{actual}`"


// ------------------
// Extremely common exceptions
// ------------------

/// When a function in called with the wrong number of arguments.
/// Used in almost every function signature.
let incorrectArgs () = raise IncorrectArgs

let intInfixFns = Set [ "+"; "-"; "*"; ">"; ">="; "<="; "<"; "^"; "%" ]

let incorrectArgsMsg (name : FnName.FnName) (p : Param) (actual : Dval) : string =
  let actualRepr = DvalReprDeveloper.toRepr actual
  let expectedTypeRepr = DvalReprDeveloper.typeName p.typ

  let conversionMsg =
    match p.typ, actual, name with
    | TInt, DFloat _, FQName.BuiltIn({ name = FnName.FnName name } as b) when
      b.modules = [ "Int" ] || (b.modules = [] && Set.contains name intInfixFns)
      ->
      let altfn = { b with modules = [ "Float" ] }

      $". Try using {FnName.builtinToString altfn}, or use Float.truncate to truncate Floats to Ints."
    | TInt, DString _, FQName.BuiltIn({ name = FnName.FnName name } as b) when
      (b.modules = [ "Int" ] && name = "add") || (b.modules = [] && name = "+")
      ->
      ". Use ++ to concatenate"
    | _ -> ""
  $"{FnName.toString name} was expected to be called with a `{expectedTypeRepr}`"
  + $" in {p.name}, but was actually called with {actualRepr}"
  + conversionMsg

let incorrectArgsToDError (source : DvalSource) (fn : Fn) (argList : NEList<Dval>) =
  let paramLength = NEList.length fn.parameters
  let argLength = NEList.length argList

  if paramLength <> argLength then
    (Dval.errSStr
      source
      ($"{FnName.toString fn.name} has {paramLength} parameters,"
       + $" but here was called with {argLength} arguments."))

  else
    let invalid =
      NEList.zip fn.parameters argList
      |> NEList.filter (fun (p, a) -> not (Dval.typeMatches p.typ a))

    match invalid with
    | [] ->
      Dval.errSStr
        source
        $"unknown error calling {FnName.toString fn.name}, with args {argList} and params {fn.parameters}"
    | (p, actual) :: _ ->
      let msg = incorrectArgsMsg fn.name p actual
      Dval.errSStr source msg


/// When you have a fakeval, you typically just want to return it.
let foundFakeDval (dv : Dval) : 'a = raise (FakeDvalFound dv)
