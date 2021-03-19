module LibExecution.AnalysisTypes

// Types used during program analysis/traces

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

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
    timestamp : System.DateTime
    function_results : List<FunctionResult> }

type Trace = TraceID * Option<TraceData>

// type tlid_traces = tlid * trace list [@@deriving to_yojson]
//
// type tlid_traceid = tlid * traceid [@@deriving to_yojson]
//
// type traceid_tlids = traceid * tlid list [@@deriving to_yojson]
//
