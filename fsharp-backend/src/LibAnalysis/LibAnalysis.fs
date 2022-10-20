/// Handles requests for evaluating expressions
module LibAnalysis

open System.Threading.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module AT = LibExecution.AnalysisTypes
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated
module CT2Analysis = ClientTypes2ExecutionTypes.Analysis

module Eval =

  let loadFromTrace
    (results : AT.FunctionResult list)
    ((_tlid, fnName, callerID) : RT.FunctionRecord)
    (args : List<RT.Dval>)
    : Option<RT.Dval * NodaTime.Instant> =
    let hashes =
      DvalReprInternalDeprecated.supportedHashVersions
      |> List.map (fun key -> (key, DvalReprInternalDeprecated.hash key args))
      |> Map

    results
    |> List.filterMap (fun (rFnName, rCallerID, hash, hashVersion, dval) ->
      if RT.FQFnName.toString fnName = rFnName
         && callerID = rCallerID
         && hash = (Map.get hashVersion hashes
                    |> Exception.unwrapOptionInternal
                         "Could not find hash"
                         [ "hashVersion", hashVersion; "hashes", hashes ]) then
        Some dval
      else
        None)
    |> List.head
    // We don't use the time, so just hack it to get the interface right.
    |> Option.map (fun dv -> (dv, NodaTime.Instant.now ()))


  let runAnalysis
    (tlid : tlid)
    (traceData : AT.TraceData)
    (userFns : List<RT.UserFunction.T>)
    (userTypes : List<RT.UserType.T>)
    (dbs : List<RT.DB.T>)
    (expr : RT.Expr)
    (packageFns : List<RT.Package.Fn>)
    (secrets : List<RT.Secret.T>)
    : Task<AT.AnalysisResults> =
    task {
      let program : RT.ProgramContext =
        { accountID = System.Guid.NewGuid()
          canvasID = System.Guid.NewGuid()
          canvasName = CanvasName.createExn "todo"
          userFns = userFns |> List.map (fun fn -> fn.name, fn) |> Map
          userTypes = userTypes |> List.map (fun t -> (t.name, t.version), t) |> Map
          dbs = dbs |> List.map (fun t -> t.name, t) |> Map
          secrets = secrets }

      let stdlib =
        LibExecutionStdLib.StdLib.fns
        |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)

      let packageFns =
        packageFns |> Map.fromListBy (fun fn -> RT.FQFnName.Package fn.name)

      let libraries : RT.Libraries = { stdlib = stdlib; packageFns = packageFns }
      let results, traceDvalFn = Exe.traceDvals ()
      let functionResults = traceData.function_results

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
          tlid
          program

      let inputVars = Map traceData.input
      let! (_result : RT.Dval) = Exe.executeExpr state inputVars expr
      return results
    }

module CTRuntime = ClientTypes.Runtime
module CTAnalysis = ClientTypes.Analysis
module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes

let performAnalysis
  (args : CTAnalysis.PerformAnalysisParams)
  : Task<CTAnalysis.AnalysisEnvelope> =
  let runAnalysis
    (requestID : int)
    (requestTime : NodaTime.Instant)
    (tlid : tlid)
    (traceID : CTAnalysis.TraceID)
    (traceData : CTAnalysis.TraceData)
    (userFns : List<PT.UserFunction.T>)
    (userTypes : List<PT.UserType.T>)
    (dbs : List<PT.DB.T>)
    (expr : PT.Expr)
    (packageFns : List<PT.Package.Fn>)
    (secrets : List<PT.Secret.T>)
    : Task<CTAnalysis.AnalysisEnvelope> =
    task {
      let traceData = CT2Analysis.TraceData.fromCT traceData
      let userFns = List.map PT2RT.UserFunction.toRT userFns
      let userTypes = List.map PT2RT.UserType.toRT userTypes
      let dbs = List.map PT2RT.DB.toRT dbs
      let expr = PT2RT.Expr.toRT expr
      let packageFns = List.map PT2RT.Package.toRT packageFns
      let secrets = List.map PT2RT.Secret.toRT secrets
      let! result =
        Eval.runAnalysis tlid traceData userFns userTypes dbs expr packageFns secrets

      return (traceID, CT2Analysis.AnalysisResults.toCT result, requestID, requestTime)
    }

  match args with
  | CTAnalysis.AnalyzeHandler ah ->
    runAnalysis
      ah.requestID
      ah.requestTime
      ah.handler.tlid
      ah.traceID
      ah.traceData
      (List.map CT2Program.UserFunction.fromCT ah.userFns)
      (List.map CT2Program.UserType.fromCT ah.userTypes)
      (List.map CT2Program.DB.fromCT ah.dbs)
      (CT2Program.Expr.fromCT ah.handler.ast)
      (List.map CT2Program.Package.Fn.fromCT ah.packageFns)
      (List.map CT2Program.Secret.fromCT ah.secrets)

  | CTAnalysis.AnalyzeFunction af ->
    runAnalysis
      af.requestID
      af.requestTime
      af.func.tlid
      af.traceID
      af.traceData
      (List.map CT2Program.UserFunction.fromCT af.userFns)
      (List.map CT2Program.UserType.fromCT af.userTypes)
      (List.map CT2Program.DB.fromCT af.dbs)
      (CT2Program.Expr.fromCT af.func.body)
      (List.map CT2Program.Package.Fn.fromCT af.packageFns)
      (List.map CT2Program.Secret.fromCT af.secrets)

// todo: move this init to ClientTypes
let initSerializers () =
  do Json.Vanilla.allow<CTAnalysis.PerformAnalysisParams> "LibAnalysis"
  do Json.Vanilla.allow<CTAnalysis.AnalysisResult> "LibAnalysis"
