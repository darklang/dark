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

module CRT = ClientTypes.Runtime // is this used?
module CAT = ClientTypes.Analysis

let performAnalysis (args : CAT.PerformAnalysisParams) : Task<CAT.AnalysisEnvelope> =
  let runAnalysis
    (tlid : tlid)
    (traceID : CAT.TraceID)
    (traceData : CAT.TraceData.T)
    (userFns : List<PT.UserFunction.T>)
    (userTypes : List<PT.UserType.T>)
    (dbs : List<PT.DB.T>)
    (expr : PT.Expr)
    (packageFns : List<PT.Package.Fn>)
    (secrets : List<PT.Secret.T>)
    : Task<CAT.AnalysisEnvelope> =
    task {
      let traceData = CAT.TraceData.toAT traceData
      let userFns = List.map PT2RT.UserFunction.toRT userFns
      let userTypes = List.map PT2RT.UserType.toRT userTypes
      let dbs = List.map PT2RT.DB.toRT dbs
      let expr = PT2RT.Expr.toRT expr
      let packageFns = List.map PT2RT.Package.toRT packageFns
      let secrets = List.map PT2RT.Secret.toRT secrets
      let! result =
        Eval.runAnalysis tlid traceData userFns userTypes dbs expr packageFns secrets

      return (traceID, CAT.AnalysisResults.fromAT result)
    }

  match args with
  | CAT.AnalyzeHandler ah ->
    runAnalysis
      ah.handler.tlid
      ah.traceID
      ah.traceData
      ah.userFns
      ah.userTypes
      ah.dbs
      ah.handler.ast
      ah.packageFns
      ah.secrets

  | CAT.AnalyzeFunction af ->
    runAnalysis
      af.func.tlid
      af.traceID
      af.traceData
      af.userFns
      af.userTypes
      af.dbs
      af.func.body
      af.packageFns
      af.secrets


type AnalysisResult = Result<CAT.AnalysisEnvelope, string>

let initSerializers () =
  do Json.Vanilla.allow<AnalysisResult> "LibAnalysis"
  do Json.Vanilla.allow<CAT.PerformAnalysisParams> "LibAnalysis"
