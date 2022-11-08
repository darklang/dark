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

// Dictionarys are mutable
type AnalysisResults = System.Collections.Generic.Dictionary<id, ExecutionResult>

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
