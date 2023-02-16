module LibExecutionStdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes

module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal

let fn = FQFnName.stdlibFnName

let renames =
  // old names, new names
  [ fn "Http" "respond" 0, fn "Http" "response" 0
    fn "Http" "respondWithHtml" 0, fn "Http" "responseWithHtml" 0
    fn "Http" "respondWithText" 0, fn "Http" "responseWithText" 0
    fn "Http" "respondWithJson" 0, fn "Http" "responseWithJson" 0
    fn "Http" "respondWithHeaders" 0, fn "Http" "responseWithHeaders" 0
    fn "" "assoc" 0, fn "Dict" "set" 0
    fn "" "dissoc" 0, fn "Dict" "remove" 0
    fn "JSON" "read" 1, fn "JSON" "parse" 0
    fn "Object" "empty" 0, fn "Dict" "empty" 0
    fn "Object" "merge" 0, fn "Dict" "merge" 0
    fn "Object" "toJSON" 1, fn "Dict" "toJSON" 0
    fn "Date" "subtract" 0, fn "Date" "subtractSeconds" 0
    fn "List" "contains" 0, fn "List" "member" 0
    fn "String" "toUUID" 1, fn "Uuid" "parse" 0
    fn "String" "toFloat" 1, fn "Float" "parse" 0 ]


let prefixFns : List<BuiltInFn> =
  [ LibBool.fns
    LibBytes.fns
    LibChar.fns
    LibDate.fns
    LibDict.fns
    LibFloat.fns
    LibHttp.fns
    LibHttpClient.fns
    LibHttpClientAuth.fns
    LibJson.fns
    LibMath.fns
    LibObject.fns
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
  [ ("Int", "add", 0), (("", "+"), NotDeprecated)
    ("Int", "subtract", 0), (("", "-"), NotDeprecated)
    ("Int", "multiply", 0), (("", "*"), NotDeprecated)
    ("Int", "greaterThan", 0), (("", ">"), NotDeprecated)
    ("Int", "greaterThanOrEqualTo", 0), (("", ">="), NotDeprecated)
    ("Int", "lessThanOrEqualTo", 0), (("", "<="), NotDeprecated)
    ("Int", "lessThan", 0), (("", "<"), NotDeprecated)
    ("Int", "power", 0), (("", "^"), NotDeprecated)
    ("Int", "mod", 0), (("", "%"), NotDeprecated)
    ("Float", "divide", 0), (("", "/"), NotDeprecated)
    ("Date", "lessThan", 0), (("Date", "<"), NotDeprecated)
    ("Date", "greaterThan", 0), (("Date", ">"), NotDeprecated)
    ("Date", "lessThanOrEqualTo", 0), (("Date", "<="), NotDeprecated)
    ("Date", "greaterThanOrEqualTo", 0), (("Date", ">="), NotDeprecated)
    ("String", "append", 1), (("", "++"), NotDeprecated)
    ("", "equals", 0), (("", "=="), NotDeprecated)
    ("", "notEquals", 0), (("", "!="), NotDeprecated) ]
  |> List.map (fun ((module_, name, version), ((newMod, opName), deprecation)) ->
    FQFnName.stdlibFnName module_ name version,
    (FQFnName.stdlibFnName newMod opName 0, deprecation))
  |> Map

// set of infix names
let infixFnNames : Set<FQFnName.StdlibFnName> =
  infixFnMapping
  |> Map.toSeq
  |> Seq.map FSharpPlus.Operators.item2
  |> Seq.map FSharpPlus.Operators.item1
  |> Set

// Is this the name of an infix function?
let isInfixName (module_ : string) (fnName : string) =
  infixFnNames.Contains { module_ = module_; function_ = fnName; version = 0 }

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
