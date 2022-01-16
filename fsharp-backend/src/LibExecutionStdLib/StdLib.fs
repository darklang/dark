module LibExecutionStdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes

module DvalReprExternal = LibExecution.DvalReprExternal

let prefixFns : List<BuiltInFn> =
  List.concat [ LibBool.fns
                LibBytes.fns
                LibChar.fns
                LibDate.fns
                LibDict.fns
                LibFloat.fns
                LibHttp.fns
                LibHttpClient.fns
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
                LibString.fns ]

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
let isInfixName (name : FQFnName.StdlibFnName) = infixFnNames.Contains name

let infixFns : List<BuiltInFn> =
  let fns =
    List.choose
      (fun (builtin : BuiltInFn) ->
        let opName = infixFnMapping.TryFind builtin.name
        Option.map (fun newName -> { builtin with name = newName }) opName)
      prefixFns

  assertEq "All infixes are parsed" fns.Length infixFnMapping.Count // make sure we got them all
  fns


// -------------------------
// renamed fns
// -------------------------

// To cut down on the amount of code, when we rename a function and make no other
// changes, we don't duplicate it. Instead, we rename it and add the rename to this
// list. At startup, the renamed functions are created and added to the list.
let renamed =
  let fn = FQFnName.stdlibFnName
  // old name first, new name second. The new one should still be in the codebase
  [ fn "DB" "query" 3, fn "DB" "queryExactFields" 0
    fn "DB" "query" 2, fn "DB" "queryExactFields" 3 // don't know why
    fn "DB" "queryWithKey" 2, fn "DB" "queryExactFieldsWithKey" 0
    fn "DB" "get" 1, fn "DB" "get" 2
    fn "DB" "queryOne" 2, fn "DB" "queryOneWithExactFields" 0
    fn "DB" "queryOneWithKey" 2, fn "DB" "queryOneExactFieldsWithKey" 0
    fn "x" "x" 0, fn "x" "x" 0 ]

let renamedFunctions = []


// -------------------------
// All fns
// -------------------------
let fns = infixFns @ prefixFns @ renamedFunctions
