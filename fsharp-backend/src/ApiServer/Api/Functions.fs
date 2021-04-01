module ApiServer.Functions

// Functions and API endpoints for the API

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Prelude
open Tablecloth

open Npgsql.FSharp.Tasks
open Npgsql
open LibBackend.Db

module PT = LibBackend.ProgramTypes
module OT = LibBackend.OCamlInterop.OCamlTypes
module ORT = LibBackend.OCamlInterop.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibBackend.OCamlInterop.Convert

module Account = LibBackend.Account
module Stats = LibBackend.Stats
module Traces = LibBackend.Traces
module Auth = LibBackend.Authorization
module Canvas = LibBackend.Canvas
module Config = LibBackend.Config
module RT = LibExecution.RuntimeTypes
module SA = LibBackend.StaticAssets
module Session = LibBackend.Session
module TFA = LibBackend.TraceFunctionArguments
module TFR = LibBackend.TraceFunctionResults
module TI = LibBackend.TraceInputs

// FSCLEANUP
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

let allFunctions = LibExecution.StdLib.StdLib.fns @ LibBackend.StdLib.StdLib.fns

let fsharpOnlyFns : Lazy<Set<string>> =
  lazy
    (LibExecution.StdLib.LibMiddleware.fns
     |> List.map (fun (fn : RT.BuiltInFn) -> (fn.name).ToString())
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
      let n = fn.name.ToString()
      if n = "DB::add" then "DB::add_v0" else n
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
    infix = LibExecution.StdLib.StdLib.isInfixName fn.name
    deprecated = fn.deprecated <> RT.NotDeprecated
    is_supported_in_query = fn.sqlSpec.isQueryable () }


let functionsToString (fns : RT.BuiltInFn list) : string =
  fns
  |> List.filter
       (fun fn -> not (Set.contains (toString fn.name) (Lazy.force fsharpOnlyFns)))
  |> List.map convertFn
  |> List.sortBy (fun fn -> fn.name)
  |> Json.Vanilla.prettySerialize

let adminFunctions : Lazy<string> = lazy (allFunctions |> functionsToString)

let nonAdminFunctions : Lazy<string> =
  lazy
    (allFunctions
     |> List.filter
          (function
          | { name = { module_ = "DarkInternal" } } -> false
          | _ -> true)
     |> functionsToString)


let functions (includeAdminFns : bool) : Lazy<string> =
  if includeAdminFns then adminFunctions else nonAdminFunctions
