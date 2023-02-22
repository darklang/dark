/// Handles requests for evaluating expressions
module LibAnalysis

open System.Threading.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution
module AT = LibExecution.AnalysisTypes
module DvalReprInternalHash = LibExecution.DvalReprInternalHash

module Eval =
  let loadFromTrace
    (results : AT.FunctionResult list)
    ((_tlid, fnName, callerID) : RT.FunctionRecord)
    (args : List<RT.Dval>)
    : Option<RT.Dval * NodaTime.Instant> =
    let hashes =
      DvalReprInternalHash.supportedHashVersions
      // Laziness is an optimization to avoid computing hashes we don't need (which is almost certainly all but one)
      |> List.map (fun key -> (key, lazy (DvalReprInternalHash.hash key args)))
      |> Map

    results
    |> List.filterMap (fun (rFnName, rCallerID, hash, hashVersion, dval) ->
      if RT.FQFnName.toString fnName = rFnName
         && callerID = rCallerID
         && hash = (Map.get hashVersion hashes
                    |> Exception.unwrapOptionInternal
                         "Could not find hash"
                         [ "hashVersion", hashVersion; "hashes", hashes ]
                    |> Lazy.force) then
        Some dval
      else
        None)
    |> List.head
    // We don't use the time, so just hack it to get the interface right.
    |> Option.map (fun dv -> (dv, NodaTime.Instant.now ()))


  let runAnalysis (request : AT.AnalysisRequest) : Task<AT.AnalysisResults> =
    task {
      let program : RT.ProgramContext =
        { accountID = System.Guid.NewGuid()
          canvasID = System.Guid.NewGuid()
          canvasName = CanvasName.createExn "todo"
          userFns = request.userFns |> List.map (fun fn -> fn.name, fn) |> Map
          userTypes =
            request.userTypes |> List.map (fun t -> (t.name, t.version), t) |> Map
          dbs = request.dbs |> List.map (fun t -> t.name, t) |> Map
          secrets = request.secrets }

      let stdlib =
        LibExecutionStdLib.StdLib.fns
        |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)

      let packageFns =
        request.packageFns |> Map.fromListBy (fun fn -> RT.FQFnName.Package fn.name)

      let libraries : RT.Libraries = { stdlib = stdlib; packageFns = packageFns }
      let results, traceDvalFn = Exe.traceDvals ()
      let functionResults = request.traceData.function_results

      let tracing =
        { LibExecution.Execution.noTracing RT.Preview with
            traceDval = traceDvalFn
            loadFnResult = loadFromTrace functionResults }

      let state =
        Exe.createState
          libraries
          tracing
          RT.consoleReporter
          RT.consoleNotifier
          request.tlid
          program

      let inputVars = Map request.traceData.input
      let! (_result : RT.Dval) = Exe.executeExpr state inputVars request.expr
      return results
    }


module CTAnalysis = ClientTypes.Analysis
module CT2Analysis = ClientTypes2ExecutionTypes.Analysis

let performAnalysis
  (args : CTAnalysis.PerformAnalysisParams)
  : Task<CTAnalysis.AnalysisEnvelope> =

  let analysisRequest = CT2Analysis.AnalysisRequest.fromCT args

  Eval.runAnalysis analysisRequest
  |> Task.map (fun analysisResponse ->
    (AT.TraceID.toUUID analysisRequest.traceID,
     CT2Analysis.AnalysisResults.toCT analysisResponse,
     analysisRequest.requestID,
     analysisRequest.requestTime))


let initSerializers () =
  // allow universally-serializable types
  Json.Vanilla.allow<pos> "Prelude"

  // allow Analysis-specific serializable types
  Json.Vanilla.allow<ClientTypes.Analysis.PerformAnalysisParams> "Analysis"
  Json.Vanilla.allow<ClientTypes.Analysis.AnalysisResult> "Analysis"
