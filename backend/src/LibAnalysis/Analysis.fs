/// Handles requests for evaluating expressions
module LibAnalysis.Analysis

open System.Threading.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module Exe = LibExecution.Execution
module DvalReprInternalHash = LibExecution.DvalReprInternalHash

module CAT = ClientAnalysisTypes


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
        { canvasID = System.Guid.NewGuid()
          internalFnsAllowed = false
          userFns = request.userFns |> List.map (fun fn -> fn.name, fn) |> Map
          userTypes = request.userTypes |> List.map (fun t -> t.name, t) |> Map
          dbs = request.dbs |> List.map (fun t -> t.name, t) |> Map
          secrets = request.secrets }

      let stdlibTypes : Map<RT.FQTypeName.T, RT.BuiltInType> =
        StdLibExecution.StdLib.types
        |> Map.fromListBy (fun typ -> RT.FQTypeName.Stdlib typ.name)

      let stdlibFns =
        StdLibExecution.StdLib.fns
        |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)

      let packageFns =
        request.packageFns |> Map.fromListBy (fun fn -> RT.FQFnName.Package fn.name)

      let libraries : RT.Libraries =
        { stdlibTypes = stdlibTypes; stdlibFns = stdlibFns; packageFns = packageFns }
      let results, traceDvalFn = Exe.traceDvals ()
      let functionResults = request.traceData.functionResults

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


let performAnalysis
  (args : ClientAnalysisTypes.PerformAnalysisParams)
  : Task<ClientAnalysisTypes.AnalysisEnvelope> =

  let analysisRequest = ClientAnalysisTypes.AnalysisRequest.fromCT args

  Eval.runAnalysis analysisRequest
  |> Task.map (fun analysisResponse ->
    (AT.TraceID.toUUID analysisRequest.traceID,
     CAT.AnalysisResults.toCT analysisResponse,
     analysisRequest.requestID,
     analysisRequest.requestTime))


let initSerializers () =
  // allow Analysis-specific serializable types
  Json.Vanilla.allow<CAT.PerformAnalysisParams> "Analysis"
  Json.Vanilla.allow<CAT.AnalysisResult> "Analysis"
