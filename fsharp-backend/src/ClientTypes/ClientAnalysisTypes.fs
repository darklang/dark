/// Types used in Analysis of Dark code in the client.
///
/// These should match directly with AnalysisTypes.res in the client, and are
/// a simple translation layer between those types and AnalysisTypes.fs in the
/// backend.
module ClientTypes.Analysis

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module AT = LibExecution.AnalysisTypes
module CTRuntime = ClientTypes.Runtime

open ClientTypes.Runtime

module ExecutionResult =
  type T =
    | ExecutedResult of Dval.T
    | NonExecutedResult of Dval.T

  let fromAT (er : AT.ExecutionResult) : T =
    match er with
    | AT.ExecutedResult (dv) -> ExecutedResult(Dval.fromRT dv)
    | AT.NonExecutedResult (dv) -> NonExecutedResult(Dval.fromRT dv)

module AnalysisResults =
  type T = Dictionary.T<id, ExecutionResult.T>

  let fromAT (ar : AT.AnalysisResults) : T =
    ar
    |> Dictionary.toList
    |> List.map (fun (k, v) -> (k, ExecutionResult.fromAT v))
    |> Dictionary.fromList


type InputVars = List<string * Dval.T>

type FunctionArgHash = string
type HashVersion = int
type FnName = string
type FunctionResult = FnName * id * FunctionArgHash * HashVersion * Dval.T

type TraceID = System.Guid

module TraceData =
  type T =
    { input : InputVars
      timestamp : NodaTime.Instant
      functionResults : List<FunctionResult> }

  let toAT (td : T) : AT.TraceData =
    { input = List.map (fun (k, v) -> (k, Dval.toRT v)) td.input
      timestamp = td.timestamp
      function_results =
        List.map
          (fun (name, id, hash, version, dval) ->
            (name, id, hash, version, Dval.toRT dval))
          td.functionResults }


type Trace = TraceID * TraceData.T

type HandlerAnalysisParam =
  { requestID : int
    requestTime : NodaTime.Instant
    handler : PT.Handler.T
    traceID : TraceID
    traceData : TraceData.T
    dbs : List<PT.DB.T>
    userFns : list<PT.UserFunction.T>
    userTypes : list<PT.UserType.T>
    packageFns : list<PT.Package.Fn>
    secrets : list<PT.Secret.T> }

type FunctionAnalysisParam =
  { requestID : int
    requestTime : NodaTime.Instant
    func : PT.UserFunction.T
    traceID : TraceID
    traceData : TraceData.T
    dbs : List<PT.DB.T>
    userFns : list<PT.UserFunction.T>
    userTypes : list<PT.UserType.T>
    packageFns : list<PT.Package.Fn>
    secrets : list<PT.Secret.T> }

type PerformAnalysisParams =
  | AnalyzeHandler of HandlerAnalysisParam
  | AnalyzeFunction of FunctionAnalysisParam

type AnalysisEnvelope = TraceID * AnalysisResults.T * int * NodaTime.Instant
