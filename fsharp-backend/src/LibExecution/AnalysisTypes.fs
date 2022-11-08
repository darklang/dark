/// Types used during program analysis/traces
module LibExecution.AnalysisTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

module RT = LibExecution.RuntimeTypes

// --------------------
// Dval store - save per-tl analysis results
// --------------------
type ExecutionResult =
  | ExecutedResult of RT.Dval
  | NonExecutedResult of RT.Dval

// --------------------
// Analysis result
// --------------------
type InputVars = List<string * RT.Dval>

type FunctionArgHash = string
type HashVersion = int
type FnName = string
type FunctionResult = FnName * id * FunctionArgHash * HashVersion * RT.Dval

type TraceID = System.Guid

type TraceData =
  { input : InputVars
    timestamp : NodaTime.Instant
    function_results : List<FunctionResult> }

type Trace = TraceID * TraceData

type AnalysisRequest =
  { requestID : int
    requestTime : NodaTime.Instant
    tlid : tlid
    traceID : TraceID
    traceData : TraceData
    userFns : List<RT.UserFunction.T>
    userTypes : List<RT.UserType.T>
    dbs : List<RT.DB.T>
    expr : RT.Expr
    packageFns : List<RT.Package.Fn>
    secrets : List<RT.Secret.T> }

type AnalysisResults = System.Collections.Generic.Dictionary<id, ExecutionResult>
