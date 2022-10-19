/// Types used in Analysis of Dark code in the client.
///
/// These should match directly with AnalysisTypes.res in the client, and are
/// a simple translation layer between those types and AnalysisTypes.fs in the
/// backend.
module ClientTypes2ExecutionTypes.Analysis

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module AT = LibExecution.AnalysisTypes
module CTRuntime = ClientTypes.Runtime
module CTAnalysis = ClientTypes.Analysis

open ClientTypes.Runtime

module ExecutionResult =
  let toCT (er : AT.ExecutionResult) : CTAnalysis.ExecutionResult =
    match er with
    | AT.ExecutedResult (dv) ->
      CTAnalysis.ExecutionResult.ExecutedResult(Dval.fromRT dv)
    | AT.NonExecutedResult (dv) ->
      CTAnalysis.ExecutionResult.NonExecutedResult(Dval.fromRT dv)


module AnalysisResults =
  let toCT (ar : AT.AnalysisResults) : CTAnalysis.AnalysisResults =
    ar
    |> Dictionary.toList
    |> List.map (fun (k, v) -> (k, ExecutionResult.toCT v))
    |> Dictionary.fromList


module TraceData =
  let fromCT (td : CTAnalysis.TraceData) : AT.TraceData =
    { input = List.map (fun (k, v) -> (k, Dval.toRT v)) td.input
      timestamp = td.timestamp
      function_results =
        List.map
          (fun (name, id, hash, version, dval) ->
            (name, id, hash, version, Dval.toRT dval))
          td.functionResults }
