/// Types used to send functions to the client
///
/// Many of these types exactly match runtime types, but often there are small
/// changes
module ApiServer.Functions

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

module DType =
  type T =
    | TInt
    | TFloat
    | TBool
    | TNull
    | TStr
    | TList of T
    | TTuple of T * T * List<T>
    | TDict of T
    | TIncomplete
    | TError
    | THttpResponse of T
    | TDB of T
    | TDate
    | TChar
    | TPassword
    | TUuid
    | TOption of T
    | TErrorRail
    | TUserType of string * int
    | TBytes
    | TResult of T * T
    | TVariable of string
    | TFn of List<T> * T
    | TRecord of List<string * T>

  let rec fromRT (t : RT.DType) =
    let r = fromRT
    let rl = List.map fromRT
    match t with
    | RT.TInt -> TInt
    | RT.TFloat -> TFloat
    | RT.TBool -> TBool
    | RT.TNull -> TNull
    | RT.TStr -> TStr
    | RT.TList t -> TList(r t)
    | RT.TTuple (t1, t2, ts) -> TTuple(r t1, r t2, rl ts)
    | RT.TDict t -> TDict(r t)
    | RT.TIncomplete -> TIncomplete
    | RT.TError -> TError
    | RT.THttpResponse t -> THttpResponse(r t)
    | RT.TDB t -> TDB(r t)
    | RT.TDate -> TDate
    | RT.TChar -> TChar
    | RT.TPassword -> TPassword
    | RT.TUuid -> TUuid
    | RT.TOption t -> TOption(r t)
    | RT.TErrorRail -> TErrorRail
    | RT.TUserType (str, version) -> TUserType(str, version)
    | RT.TBytes -> TBytes
    | RT.TResult (ok, error) -> TResult(r ok, r error)
    | RT.TVariable (name) -> TVariable(name)
    | RT.TFn (ts, returnType) -> TFn(rl ts, r returnType)
    | RT.TRecord (pairs) -> TRecord(List.map (fun (k, t) -> (k, r t)) pairs)

module Param =
  type T =
    { name : string
      ``type`` : DType.T
      args : List<string>
      description : string }

  let fromRT (p : RT.Param) : T =
    { name = p.name
      ``type`` = DType.fromRT p.typ
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
  type T = { ``module`` : string; ``function`` : string; version : int }

  let fromRT (fnName : RT.FQFnName.StdlibFnName) =
    { ``function`` = fnName.function_
      ``module`` = fnName.module_
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
      returnType : DType.T
      description : string
      previewable : Previewable.T
      deprecated : Deprecation.T
      isInfix : bool
      sqlSpec : SqlSpec.T }


  let fromRT (fn : RT.BuiltInFn) : T =
    { name = StdlibFnName.fromRT fn.name
      parameters = List.map Param.fromRT fn.parameters
      description = fn.description
      returnType = DType.fromRT fn.returnType
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
