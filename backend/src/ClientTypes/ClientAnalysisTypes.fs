/// Types used in Analysis of Dark code in the client.
///
/// These should match directly with AnalysisTypes.res in the client, and are
/// a simple translation layer between those types and AnalysisTypes.fs in the
/// backend.
module ClientTypes.Analysis

open Prelude
open Tablecloth

module PT = ClientTypes.Program
module RT = ClientTypes.Runtime

type ExecutionResult =
  | ExecutedResult of RT.Dval.T
  | NonExecutedResult of RT.Dval.T

type AnalysisResults = Dictionary.T<id, ExecutionResult>

type InputVars = List<string * RT.Dval.T>

type FunctionArgHash = string
type HashVersion = int
type FnName = string
type FunctionResult = FnName * id * FunctionArgHash * HashVersion * RT.Dval.T

type TraceID = System.Guid

type TraceData = { input : InputVars; functionResults : List<FunctionResult> }

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
    secrets : list<PT.Secret> }

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
    secrets : list<PT.Secret> }

type PerformAnalysisParams =
  | AnalyzeHandler of HandlerAnalysisParam
  | AnalyzeFunction of FunctionAnalysisParam

type AnalysisEnvelope = TraceID * AnalysisResults * int * NodaTime.Instant

type AnalysisResult = Result<AnalysisEnvelope, string>
