/// Types used to send functions to the client
///
/// Many of these types exactly match runtime types, but often there are small
/// changes
module ApiServer.Functions

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module CTRuntime = ClientTypes.Runtime
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime

module Param =
  type T =
    { name : string
      ``type`` : CTRuntime.DType
      args : List<string>
      description : string }

  let fromRT (p : RT.Param) : T =
    { name = p.name
      ``type`` = CT2Runtime.DType.toCT p.typ
      args = p.blockArgs
      description = p.description }

module Previewable =
  type T =
    | Pure
    | ImpurePreviewable
    | Impure

  let fromRT (p : RT.Previewable) =
    match p with
    | RT.Pure -> Pure
    | RT.ImpurePreviewable -> ImpurePreviewable
    | RT.Impure -> Impure

module StdlibFnName =
  type T = { module_ : string; function_ : string; version : int }

  let fromRT (fnName : RT.FQFnName.StdlibFnName) =
    { function_ = fnName.function_
      module_ = fnName.module_
      version = fnName.version }

module Deprecation =
  type T =
    | NotDeprecated
    | RenamedTo of StdlibFnName.T
    | ReplacedBy of StdlibFnName.T
    | DeprecatedBecause of string

  let fromRT (p : RT.Deprecation) =
    match p with
    | RT.NotDeprecated -> NotDeprecated
    | RT.RenamedTo f -> RenamedTo(StdlibFnName.fromRT f)
    | RT.ReplacedBy f -> ReplacedBy(StdlibFnName.fromRT f)
    | RT.DeprecatedBecause str -> DeprecatedBecause str

module SqlSpec =
  type T =
    | Unknown
    | NotQueryable
    | QueryFunction
    | SqlUnaryOp of string
    | SqlBinOp of string
    | SqlFunction of string
    | SqlFunctionWithPrefixArgs of string * List<string>
    | SqlFunctionWithSuffixArgs of string * List<string>
    | SqlCallback2

  let fromRT (s : RT.SqlSpec) : T =
    match s with
    | RT.NotYetImplementedTODO -> Unknown
    | RT.NotQueryable -> NotQueryable
    | RT.QueryFunction -> QueryFunction
    | RT.SqlUnaryOp str -> SqlUnaryOp str
    | RT.SqlBinOp str -> SqlBinOp str
    | RT.SqlFunction str -> SqlFunction str
    | RT.SqlFunctionWithPrefixArgs (name, args) ->
      SqlFunctionWithPrefixArgs(name, args)
    | RT.SqlFunctionWithSuffixArgs (name, args) ->
      SqlFunctionWithSuffixArgs(name, args)
    | RT.SqlCallback2 _ -> SqlCallback2


module BuiltInFn =
  type T =
    { name : StdlibFnName.T
      parameters : List<Param.T>
      returnType : CTRuntime.DType
      description : string
      previewable : Previewable.T
      deprecated : Deprecation.T
      isInfix : bool
      sqlSpec : SqlSpec.T }


  let fromRT (fn : RT.BuiltInFn) : T =
    { name = StdlibFnName.fromRT fn.name
      parameters = List.map Param.fromRT fn.parameters
      description = fn.description
      returnType = CT2Runtime.DType.toCT fn.returnType
      previewable = Previewable.fromRT fn.previewable
      isInfix =
        LibExecutionStdLib.StdLib.isInfixName fn.name.module_ fn.name.function_
      deprecated = Deprecation.fromRT fn.deprecated
      sqlSpec = SqlSpec.fromRT fn.sqlSpec }


let functionsToString (fns : RT.BuiltInFn list) : string =
  fns
  |> List.map BuiltInFn.fromRT
  |> List.sortBy (fun fn -> fn.name)
  |> Json.Vanilla.prettySerialize

let adminFunctions : string =
  LibRealExecution.RealExecution.stdlibFns |> Map.values |> functionsToString

let nonAdminFunctions : string =
  LibRealExecution.RealExecution.stdlibFns
  |> Map.values
  |> List.filter (function
    | { name = { module_ = "DarkInternal" } } -> false
    | _ -> true)
  |> functionsToString

/// Returns a list of all standard library Functions
///
/// Depending on `includeAdminFns` flag, may exclude Dark admin-only fns
let functions (includeAdminFns : bool) : string =
  if includeAdminFns then adminFunctions else nonAdminFunctions
