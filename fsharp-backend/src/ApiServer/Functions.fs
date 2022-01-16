module ApiServer.Functions

// Functions

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

// CLEANUP
// These types are to match the existing OCaml serializations that the frontend
// can read
type ParamMetadata =
  { name : string
    tipe : string
    block_args : string list
    optional : bool
    description : string }

type PreviewSafety =
  | Safe
  | Unsafe

type FunctionMetadata =
  { name : string
    parameters : ParamMetadata list
    description : string
    return_type : string
    infix : bool
    preview_safety : PreviewSafety
    deprecated : bool
    is_supported_in_query : bool }

let functionsWithoutRenames =
  LibExecutionStdLib.StdLib.fns @ BackendOnlyStdLib.StdLib.fns

// -------------------------
// renamed fns
// -------------------------

// To cut down on the amount of code, when we rename a function and make no other
// changes, we don't duplicate it. Instead, we rename it and add the rename to this
// list. At startup, the renamed functions are created and added to the list.
let renamed =
  let fn = RT.FQFnName.stdlibFnName
  // old name first, new name second. The new one should still be in the codebase,
  // the old one should not. If a function is renamed multiple times, add the latest
  // rename first.
  [ fn "DB" "query" 3, fn "DB" "queryExactFields" 0
    fn "DB" "query" 2, fn "DB" "query" 3 // don't know why
    fn "DB" "queryWithKey" 2, fn "DB" "queryExactFieldsWithKey" 0
    fn "DB" "get" 1, fn "DB" "get" 2
    fn "DB" "queryOne" 2, fn "DB" "queryOneWithExactFields" 0
    fn "DB" "queryOneWithKey" 2, fn "DB" "queryOneWithExactFieldsWithKey" 0 ]


let renamedFunctions : List<RT.BuiltInFn> =
  let existing = functionsWithoutRenames |> List.map (fun fn -> fn.name, fn) |> Map
  renamed
  |> List.fold Map.empty (fun renamedFns (oldName, newName) ->
    debuG "old" oldName
    debuG "new" newName
    let newFn =
      Map.tryFind newName (Map.mergeFavoringLeft renamedFns existing)
      |> Exception.unwrapOptionInternal
           $"all fns should exist {oldName} -> {newName}"
           [ "oldName", oldName; "newName", newName ]
    Map.add
      oldName
      { newFn with name = oldName; deprecated = RT.RenamedTo newName }
      renamedFns)
  |> Map.values


let allFunctions = functionsWithoutRenames @ renamedFunctions


// CLEANUP not needed anymore
let fsharpOnlyFns : Lazy<Set<string>> =
  lazy
    ([] // LibExecutionStdLib.LibMiddleware.fns
     |> List.map (fun (fn : RT.BuiltInFn) -> string fn.name)
     |> Set)


let typToApiString (typ : RT.DType) : string =
  match typ with
  | RT.TVariable _ -> "Any"
  | RT.TInt -> "Int"
  | RT.TFloat -> "Float"
  | RT.TBool -> "Bool"
  | RT.TNull -> "Nothing"
  | RT.TChar -> "Character"
  | RT.TStr -> "Str"
  | RT.TList _ -> "List"
  | RT.TRecord _
  | RT.TDict _ -> "Dict"
  | RT.TFn _ -> "Block"
  | RT.TIncomplete -> "Incomplete"
  | RT.TError -> "Error"
  | RT.THttpResponse _ -> "Response"
  | RT.TDB _ -> "Datastore"
  | RT.TDate -> "Date"
  // | TDbList tipe ->
  //     "[" ^ tipe_to_string tipe ^ "]"
  | RT.TPassword -> "Password"
  | RT.TUuid -> "UUID"
  | RT.TOption _ -> "Option"
  | RT.TErrorRail -> "ErrorRail"
  | RT.TResult _ -> "Result"
  | RT.TUserType (name, _) -> name
  | RT.TBytes -> "Bytes"
// | TDeprecated1
// | TDeprecated2
// | TDeprecated3
// | TDeprecated4 _
// | TDeprecated5 _
// | TDeprecated6 ->
// Exception.internal "Deprecated type"

let convertFn (fn : RT.BuiltInFn) : FunctionMetadata =
  { name =
      // CLEANUP: this is difficult to change in OCaml, but is trivial in F# (we
      // should just be able to remove this line with no other change)
      let n = string fn.name

      if n = "DB::add" then "DB::add_v0"
      else if n = "JSON::parse" then "JSON::parse_v0"
      else n
    parameters =
      List.map
        (fun (p : RT.Param) ->
          ({ name = p.name
             tipe = typToApiString p.typ
             block_args = p.blockArgs
             optional = false
             description = p.description } : ParamMetadata))
        fn.parameters
    description = fn.description
    return_type = typToApiString fn.returnType
    preview_safety = if fn.previewable = RT.Pure then Safe else Unsafe
    infix = LibExecutionStdLib.StdLib.isInfixName fn.name
    deprecated = fn.deprecated <> RT.NotDeprecated
    is_supported_in_query = fn.sqlSpec.isQueryable () }


let functionsToString (fns : RT.BuiltInFn list) : string =
  fns
  |> List.filter (fun fn ->
    not (Set.contains (string fn.name) (Lazy.force fsharpOnlyFns)))
  |> List.map convertFn
  |> List.sortBy (fun fn -> fn.name)
  |> Json.Vanilla.prettySerialize

let adminFunctions : Lazy<string> = lazy (allFunctions |> functionsToString)

let nonAdminFunctions : Lazy<string> =
  lazy
    (allFunctions
     |> List.filter (function
       | { name = { module_ = "DarkInternal" } } -> false
       | _ -> true)
     |> functionsToString)


let functions (includeAdminFns : bool) : Lazy<string> =
  if includeAdminFns then adminFunctions else nonAdminFunctions
