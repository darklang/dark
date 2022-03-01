///
module ApiServer.Functions

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
  | RT.TPassword -> "Password"
  | RT.TUuid -> "UUID"
  | RT.TOption _ -> "Option"
  | RT.TErrorRail -> "ErrorRail"
  | RT.TResult _ -> "Result"
  | RT.TUserType (name, _) -> name
  | RT.TBytes -> "Bytes"

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

let adminFunctions : Lazy<string> =
  lazy
    (LibRealExecution.RealExecution.stdlibFns.Force()
     |> Map.values
     |> functionsToString)

let nonAdminFunctions : Lazy<string> =
  lazy
    (LibRealExecution.RealExecution.stdlibFns.Force()
     |> Map.values
     |> List.filter (function
       | { name = { module_ = "DarkInternal" } } -> false
       | _ -> true)
     |> functionsToString)

/// Returns a list of all standard library Functions
///
/// Depending on `includeAdminFns` flag, may exclude Dark admin-only fns
let functions (includeAdminFns : bool) : Lazy<string> =
  if includeAdminFns then adminFunctions else nonAdminFunctions
