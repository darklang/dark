module LibExecution.AnalysisTypes

// Types used during program analysis/traces

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
    timestamp : System.DateTime
    // CLEANUP remove underscore
    function_results : List<FunctionResult> }

type Trace = TraceID * TraceData

// The option was removed to make it compatible with the OCaml version of the
// API. Although OCaml also wraps this with an option, the
// yojson_deriving_ppx-generated serialized automatically unwraps options. We
// could do that too, but we just Option in DOption (which the OCaml version
// does not). This could lead to problems but let's see how it goes for now.
// type Trace = TraceID * Option<TraceData>
