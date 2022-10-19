/// Types used in Analysis of Dark code in the client.
///
/// These should match directly with AnalysisTypes.res in the client, and are
/// a simple translation layer between those types and AnalysisTypes.fs in the
/// backend.
module ClientTypes.Analysis

open Prelude
open Tablecloth

// todo: reference ClientProgramTypes instead.
module PT = LibExecution.ProgramTypes

open ClientTypes.Runtime

type ExecutionResult =
  | ExecutedResult of Dval.T
  | NonExecutedResult of Dval.T

type AnalysisResults = Dictionary.T<id, ExecutionResult>

type InputVars = List<string * Dval.T>

type FunctionArgHash = string
type HashVersion = int
type FnName = string
type FunctionResult = FnName * id * FunctionArgHash * HashVersion * Dval.T

type TraceID = System.Guid

type TraceData =
  { input : InputVars
    timestamp : NodaTime.Instant
    functionResults : List<FunctionResult> }

type Trace = TraceID * TraceData

type HandlerAnalysisParam =
  { requestID : int
    requestTime : NodaTime.Instant
    handler : PT.Handler.T
    traceID : TraceID
    traceData : TraceData
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
    traceData : TraceData
    dbs : List<PT.DB.T>
    userFns : list<PT.UserFunction.T>
    userTypes : list<PT.UserType.T>
    packageFns : list<PT.Package.Fn>
    secrets : list<PT.Secret.T> }

type PerformAnalysisParams =
  | AnalyzeHandler of HandlerAnalysisParam
  | AnalyzeFunction of FunctionAnalysisParam

type AnalysisEnvelope = TraceID * AnalysisResults * int * NodaTime.Instant
