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
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated

/// Used to map from F# types to the OCaml types that the Client expects
module ClientInterop =
  // -----------------------
  // DB types
  // -----------------------
  // CLEANUP There is a subtly different definition of DBs, which is that the
  // type should be a DType, but the client gives us a string instead.
  type client_col = string OT.or_blank * string OT.or_blank

  type client_db_migration =
    { starting_version : int64
      version : int64
      state : ORT.DbT.db_migration_state
      rollforward : ORT.fluidExpr
      rollback : ORT.fluidExpr
      cols : client_col list }

  type client_db =
    { tlid : tlid
      name : string OT.or_blank
      cols : client_col list
      version : int64
      old_migrations : client_db_migration list
      active_migration : client_db_migration option }


  let convert_col ((name, tipe) : client_col) : ORT.DbT.col =
    match tipe with
    | OT.Blank id -> (name, OT.Blank id)
    | OT.Partial (id, str) -> (name, OT.Partial(id, str))
    | OT.Filled (id, tipe) ->
      let typ =
        PTParser.DType.parse tipe
        |> Exception.unwrapOptionInternal "cannot parse col" [ "type", tipe ]
        |> OT.Convert.pt2ocamlTipe
      (name, (OT.Filled(id, typ)))

  let convert_migration (m : client_db_migration) : ORT.DbT.db_migration =
    { starting_version = m.starting_version
      version = m.version
      state = m.state
      rollforward = m.rollforward
      rollback = m.rollback
      cols = List.map convert_col m.cols }


  let convert_db (db : client_db) : ORT.DbT.db =
    { tlid = db.tlid
      name = db.name
      cols = List.map convert_col db.cols
      version = db.version
      old_migrations = List.map convert_migration db.old_migrations
      active_migration = Option.map convert_migration db.active_migration }

  // -----------------------
  // Analysis types
  // -----------------------
  // Cleanup: Dvals are different, and there are dvals in analysis results
  type InputVars = List<string * ORT.dval>

  type FunctionResult =
    AT.FnName * id * AT.FunctionArgHash * AT.HashVersion * ORT.dval

  type TraceData =
    { input : InputVars
      timestamp : NodaTime.Instant
      function_results : List<FunctionResult> }

  let convert_trace_data (td : TraceData) : AT.TraceData =
    { input = List.map (fun (s, dv) -> (s, OT.Convert.ocamlDval2rt dv)) td.input
      timestamp = td.timestamp
      function_results =
        List.map
          (fun (a, b, c, d, dv) -> (a, b, c, d, OT.Convert.ocamlDval2rt dv))
          td.function_results }

  // -----------------------
  // High-level defs
  // -----------------------
  type handler_analysis_param =
    { handler : ORT.HandlerT.handler
      trace_id : AT.TraceID
      trace_data : TraceData
      // don't use a trace as this isn't optional
      dbs : client_db list
      user_fns : ORT.user_fn list
      user_tipes : ORT.user_tipe list
      secrets : OT.secret list }

  type function_analysis_param =
    { func : ORT.user_fn
      trace_id : AT.TraceID
      trace_data : TraceData
      // don't use a trace as this isn't optional
      dbs : client_db list
      user_fns : ORT.user_fn list
      user_tipes : ORT.user_tipe list
      secrets : OT.secret list }

  type performAnalysisParams =
    /// This is called `performHandlerAnalysisParam` in the client
    | AnalyzeHandler of handler_analysis_param

    /// This is called `performHandlerFunctionParam` in the client
    | AnalyzeFunction of function_analysis_param

  type ExecutionResult =
    | ExecutedResult of ORT.dval
    | NonExecutedResult of ORT.dval

  // Dictionaries are mutable
  type AnalysisResults = System.Collections.Generic.Dictionary<id, ExecutionResult>

  type AnalysisEnvelope = AT.TraceID * AnalysisResults

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
    (userFns : List<ORT.user_fn>)
    (userTypes : List<ORT.user_tipe>)
    (dbs : List<ORT.DbT.db>)
    (expr : ORT.fluidExpr)
    (secrets : List<OT.secret>)
    : Task<ClientInterop.AnalysisEnvelope> =
    task {
      let program : RT.ProgramContext =
        { accountID = System.Guid.NewGuid()
          canvasID = System.Guid.NewGuid()
          canvasName = CanvasName.createExn "todo"
          userFns =
            userFns
            |> List.map OT.Convert.ocamlUserFunction2PT
            |> List.map PT2RT.UserFunction.toRT
            |> List.map (fun fn -> fn.name, fn)
            |> Map
          userTypes =
            userTypes
            |> List.map OT.Convert.ocamlUserType2PT
            |> List.map PT2RT.UserType.toRT
            |> List.map (fun t -> (t.name, t.version), t)
            |> Map
          dbs =
            dbs
            |> List.map (OT.Convert.ocamlDB2PT { x = 0; y = 0 })
            |> List.map PT2RT.DB.toRT
            |> List.map (fun t -> t.name, t)
            |> Map
          secrets = secrets |> List.map (OT.Convert.ocamlSecret2RT) }

      let stdlib =
        LibExecutionStdLib.StdLib.fns
        |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)

      // TODO: get packages from caller
      let libraries : RT.Libraries = { stdlib = stdlib; packageFns = Map.empty }
      let dvalResults, traceDvalFn = Exe.traceDvals ()
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

      let ast = expr |> OT.Convert.ocamlExpr2PT |> PT2RT.Expr.toRT
      let inputVars = Map traceData.input
      let! (_result : RT.Dval) = Exe.executeExpr state inputVars ast
      let ocamlResults =
        dvalResults
        |> Dictionary.toList
        |> List.map (fun (k, v) ->
          k,
          match v with
          | AT.ExecutedResult dv ->
            ClientInterop.ExecutedResult(OT.Convert.rt2ocamlDval dv)
          | AT.NonExecutedResult dv ->
            ClientInterop.NonExecutedResult(OT.Convert.rt2ocamlDval dv))
        |> Dictionary.fromList

      return (traceID, ocamlResults)
    }

  let performAnalysis
    (args : ClientInterop.performAnalysisParams)
    : Task<ClientInterop.AnalysisEnvelope> =
    match args with
    | ClientInterop.AnalyzeHandler ah ->
      runAnalysis
        ah.handler.tlid
        ah.trace_id
        (ClientInterop.convert_trace_data ah.trace_data)
        ah.user_fns
        ah.user_tipes
        (List.map ClientInterop.convert_db ah.dbs)
        ah.handler.ast
        ah.secrets
    | ClientInterop.AnalyzeFunction af ->
      runAnalysis
        af.func.tlid
        af.trace_id
        (ClientInterop.convert_trace_data af.trace_data)
        af.user_fns
        af.user_tipes
        (List.map ClientInterop.convert_db af.dbs)
        af.func.ast
        af.secrets

type AnalysisResult = Result<ClientInterop.AnalysisEnvelope, string>

let init () =
  do Json.Vanilla.allow<AnalysisResult> ()
  do Json.Vanilla.allow<ClientInterop.ExecutionResult> ()
  do Json.Vanilla.allow<ClientInterop.performAnalysisParams> ()


