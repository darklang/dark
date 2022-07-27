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

/// Used to map from F# types to the OCaml types that the Client expects
module ClientInterop =

  // -----------------------
  // High-level defs
  // -----------------------
  type HandlerAnalysisParam =
    { handler : PT.Handler.T
      traceID : AT.TraceID
      traceData : AT.TraceData
      dbs : List<PT.DB.T>
      userFns : list<PT.UserFunction.T>
      userTypes : list<PT.UserType.T>
      secrets : list<PT.Secret.T> }

  type FunctionAnalysisParam =
    { func : PT.UserFunction.T
      traceID : AT.TraceID
      traceData : AT.TraceData
      dbs : List<PT.DB.T>
      userFns : list<PT.UserFunction.T>
      userTypes : list<PT.UserType.T>
      secrets : list<PT.Secret.T> }

  type PerformAnalysisParams =
    /// This is called `performHandlerAnalysisParam` in the client
    | AnalyzeHandler of HandlerAnalysisParam

    /// This is called `performHandlerFunctionParam` in the client
    | AnalyzeFunction of FunctionAnalysisParam

  type AnalysisEnvelope = AT.TraceID * AT.AnalysisResults

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
    (traceID : AT.TraceID)
    (traceData : AT.TraceData)
    (userFns : List<PT.UserFunction.T>)
    (userTypes : List<PT.UserType.T>)
    (dbs : List<PT.DB.T>)
    (expr : PT.Expr)
    (secrets : List<PT.Secret.T>)
    : Task<ClientInterop.AnalysisEnvelope> =
    task {
      let program : RT.ProgramContext =
        { accountID = System.Guid.NewGuid()
          canvasID = System.Guid.NewGuid()
          canvasName = CanvasName.createExn "todo"
          userFns =
            userFns
            |> List.map PT2RT.UserFunction.toRT
            |> List.map (fun fn -> fn.name, fn)
            |> Map
          userTypes =
            userTypes
            |> List.map PT2RT.UserType.toRT
            |> List.map (fun t -> (t.name, t.version), t)
            |> Map
          dbs = dbs |> List.map PT2RT.DB.toRT |> List.map (fun t -> t.name, t) |> Map
          secrets = secrets |> List.map (PT2RT.Secret.toRT) }

      let stdlib =
        LibExecutionStdLib.StdLib.fns
        |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)

      // TODO: get packages from caller
      let libraries : RT.Libraries = { stdlib = stdlib; packageFns = Map.empty }
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

      let ast = expr |> PT2RT.Expr.toRT
      let inputVars = Map traceData.input
      let! (_result : RT.Dval) = Exe.executeExpr state inputVars ast
      return (traceID, results)
    }

  let performAnalysis
    (args : ClientInterop.PerformAnalysisParams)
    : Task<ClientInterop.AnalysisEnvelope> =
    match args with
    | ClientInterop.AnalyzeHandler ah ->
      runAnalysis
        ah.handler.tlid
        ah.traceID
        ah.traceData
        ah.userFns
        ah.userTypes
        ah.dbs
        ah.handler.ast
        ah.secrets
    | ClientInterop.AnalyzeFunction af ->
      runAnalysis
        af.func.tlid
        af.traceID
        af.traceData
        af.userFns
        af.userTypes
        af.dbs
        af.func.body
        af.secrets

type AnalysisResult = Result<ClientInterop.AnalysisEnvelope, string>

let initSerializers () =
  do Json.Vanilla.allow<AnalysisResult> "LibAnalysis"
  do Json.Vanilla.allow<ClientInterop.PerformAnalysisParams> "LibAnalysis"
