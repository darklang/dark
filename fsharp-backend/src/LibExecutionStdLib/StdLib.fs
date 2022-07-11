module LibExecutionStdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes

module DvalReprExternal = LibExecution.DvalReprExternal

let fn = FQFnName.stdlibFnName

let renames =
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
    fn "Date" "subtract" 0, fn "Date" "subtractSeconds" 0 ]


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
    LibString.fns ]
  |> List.concat
  |> renameFunctions renames

// -------------------------
// Infix fns
// -------------------------

// Map of prefix names to their infix versions
let infixFnMapping =
  [ ("Int", "add", 0), ("", "+")
    ("Int", "subtract", 0), ("", "-")
    ("Int", "multiply", 0), ("", "*")
    ("Int", "greaterThan", 0), ("", ">")
    ("Int", "greaterThanOrEqualTo", 0), ("", ">=")
    ("Int", "lessThanOrEqualTo", 0), ("", "<=")
    ("Int", "lessThan", 0), ("", "<")
    ("Int", "power", 0), ("", "^")
    ("Int", "mod", 0), ("", "%")
    ("Float", "divide", 0), ("", "/")
    ("Date", "lessThan", 0), ("Date", "<")
    ("Date", "greaterThan", 0), ("Date", ">")
    ("Date", "lessThanOrEqualTo", 0), ("Date", "<=")
    ("Date", "greaterThanOrEqualTo", 0), ("Date", ">=")
    ("String", "append", 1), ("", "++")
    ("", "equals", 0), ("", "==")
    ("", "notEquals", 0), ("", "!=")
    ("Bool", "and", 0), ("", "&&")
    ("Bool", "or", 0), ("", "||") ]
  |> List.map (fun ((module_, name, version), (newMod, opName)) ->
    FQFnName.stdlibFnName module_ name version, FQFnName.stdlibFnName newMod opName 0)
  |> Map

// set of infix names
let infixFnNames =
  infixFnMapping |> Map.toSeq |> Seq.map FSharpPlus.Operators.item2 |> Set

// Is this the name of an infix function?
let isInfixName (module_ : string) (fnName : string) =
  infixFnNames.Contains { module_ = module_; function_ = fnName; version = 0 }

let infixFns : List<BuiltInFn> =
  let fns =
    prefixFns
    |> List.choose (fun (builtin : BuiltInFn) ->
      let opName = infixFnMapping.TryFind builtin.name
      Option.map (fun newName -> { builtin with name = newName }) opName)

  assertEq "All infixes are parsed" fns.Length infixFnMapping.Count // make sure we got them all
  fns



// -------------------------
// All fns
// -------------------------
let fns = infixFns @ prefixFns
