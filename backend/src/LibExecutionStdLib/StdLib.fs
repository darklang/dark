module LibExecutionStdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes


let fn = FQFnName.stdlibFnName

let renames : List<FQFnName.StdlibFnName * FQFnName.StdlibFnName> =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []


let prefixFns : List<BuiltInFn> =
  [ LibBool.fns
    LibBytes.fns
    LibChar.fns
    LibDateTime.fns
    LibDict.fns
    LibFloat.fns
    LibHttp.fns
    LibHttpClient.fns
    LibHttpClientAuth.fns
    LibJson.fns
    LibMath.fns
    LibUuid.fns
    LibInt.fns
    LibList.fns
    // LibMiddleware.fns
    LibNoModule.fns
    LibOption.fns
    LibResult.fns
    LibCrypto.fns
    LibString.fns
    LibTuple2.fns
    LibTuple3.fns ]
  |> List.concat
  |> renameFunctions renames

// -------------------------
// Infix fns
// -------------------------

// Map of prefix names to their infix versions
let infixFnMapping : Map<FQFnName.StdlibFnName, (FQFnName.StdlibFnName * Deprecation)> =
  [ ("Int", "add", 0), ("+", NotDeprecated)
    ("Int", "subtract", 0), ("-", NotDeprecated)
    ("Int", "multiply", 0), ("*", NotDeprecated)
    ("Int", "greaterThan", 0), (">", NotDeprecated)
    ("Int", "greaterThanOrEqualTo", 0), (">=", NotDeprecated)
    ("Int", "lessThanOrEqualTo", 0), ("<=", NotDeprecated)
    ("Int", "lessThan", 0), ("<", NotDeprecated)
    ("Int", "power", 0), ("^", NotDeprecated)
    ("Int", "mod", 0), ("%", NotDeprecated)
    ("Float", "divide", 0), ("/", NotDeprecated)
    ("String", "append", 1), ("++", NotDeprecated)
    ("", "equals", 0), ("==", NotDeprecated)
    ("", "notEquals", 0), ("!=", NotDeprecated) ]
  |> List.map (fun ((module_, name, version), (opName, deprecation)) ->
    FQFnName.stdlibFnName module_ name version,
    (FQFnName.stdlibFnName "" opName 0, deprecation))
  |> Map

// set of infix names
let infixFnNames : Set<FQFnName.StdlibFnName> =
  infixFnMapping
  |> Map.toSeq
  |> Seq.map FSharpPlus.Operators.item2
  |> Seq.map FSharpPlus.Operators.item1
  |> Set

// Is this the name of an infix function?
let isInfixName (fnName : string) =
  infixFnNames.Contains { module_ = ""; function_ = fnName; version = 0 }

let infixFns : List<BuiltInFn> =
  let fns =
    prefixFns
    |> List.choose (fun (builtin : BuiltInFn) ->
      builtin.name
      |> infixFnMapping.TryFind
      |> Option.map (fun (newName, deprecation) ->
        { builtin with name = newName; deprecated = deprecation }))

  assertEq "All infixes are parsed" fns.Length infixFnMapping.Count // make sure we got them all
  fns



// -------------------------
// All fns
// -------------------------
let fns = infixFns @ prefixFns
