/// Builtin functions for building the CLI
/// (as opposed to functions needed by CLI programs, which are in StdLibCli)
module Builtins.CliHost.Libs.Cli

open System.Threading.Tasks
open FSharp.Control.Tasks


open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = LibExecution.RuntimeTypes.NameResolution
module VT = LibExecution.ValueType
module AT = LibExecution.AnalysisTypes
module Dval = LibExecution.Dval
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Exe = LibExecution.Execution
module PackageRefs = LibExecution.PackageRefs
module Json = Builtins.Pure.Libs.Json
module C2DT = LibExecution.CommonToDarkTypes
module D = LibExecution.DvalDecoder
module Utils = Builtins.CliHost.Utils
module Toplevels = LibCloud.Toplevels
module Tracing = LibDB.Tracing


/// Load all DBs from the global toplevel set.
let loadDBs () : Ply<Map<string, RT.DB.T>> =
  uply {
    let! tls = Toplevels.loadAllDBs ()
    let! program = Toplevels.toProgram tls
    return program.dbs
  }


type CliTraceSource =
  | RunScript of filename : string * code : string
  | EvalExpression of expression : string

module CliTraceSource =
  let toTraceParams (source : CliTraceSource) =
    match source with
    | RunScript(filename, code) -> ($"run {filename}", "code", RT.DString code)
    | EvalExpression expr -> ("eval", "expression", RT.DString expr)


// Create exception error from exn
let createExceptionError (e : exn) : RuntimeError.Error =
  RuntimeError.UncaughtException(
    Exception.getMessages e |> String.concat "\n",
    Exception.toMetadata e |> List.map (fun (k, v) -> (k, DString(string v)))
  )

// Parse CLI script code using the single-shot parseForCli function.
// This keeps PM creation and parsing in the same execution context,
// avoiding issues with lambda instructions crossing VM boundaries.
let parseCliScript
  (exeState : RT.ExecutionState)
  (branchId : System.Guid)
  (owner : string)
  (scriptName : string)
  (code : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, RuntimeError.Error>> =
  uply {
    let args =
      NEList.ofList
        (DUuid branchId)
        [ DString owner; DString scriptName; DString scriptName; DString code ]

    let parseForCliFnName =
      FQFnName.fqPackage (
        PackageRefs.Fn.LanguageTools.Parser.CliScript.parseForCli ()
      )

    let! execResult = Exe.executeFunction exeState parseForCliFnName [] args

    match execResult with
    | Ok dval ->
      match C2DT.Result.fromDT identity dval identity with
      | Ok parsedModuleAndUnresolvedNames ->
        return (Utils.CliScript.fromDT parsedModuleAndUnresolvedNames) |> Ok
      | Error(DString errMsg) ->
        return Error(RuntimeError.UncaughtException(errMsg, []))
      | Error _ ->
        return
          Exception.raiseInternal
            "Invalid error format from parseCliScript"
            [ "dval", dval ]
    | Error(rte, _cs) ->
      let! rteString = Exe.runtimeErrorToString exeState rte
      match rteString with
      | Ok errorDval ->
        return
          Exception.raiseInternal
            "Error executing parseCliScript function"
            [ "rte", errorDval ]
      | Error(nestedRte, _cs) ->
        return
          Exception.raiseInternal
            "Error running runtimeErrorToString"
            [ "original rte", rte; "nested rte", nestedRte ]
  }


module ExecutionError =
  let fqTypeName () = FQTypeName.fqPackage (PackageRefs.Type.Cli.executionError ())
  let typeRef () = TCustomType(NR.ok (fqTypeName ()), [])


let pmRT = LibDB.PackageManager.rt

// The `cliEvaluateExpression` and `cliParseAndExecuteScript` builtins
// build child execution states (a different branch, a fresh PM with
// the script's own fns/types grafted in, a tracer, etc.). Rather than
// re-`createState` from scratch — which would force us to know the
// full builtin set here and would cycle into `fns ()` below — we
// derive the child state from `parentState`. The parent already
// includes our own builtins (it was constructed by Cli/Cli.fs) so
// nested `eval` / `run` dispatches automatically.

let childState
  (parentState : RT.ExecutionState)
  (pm : RT.PackageManager)
  (tracing : RT.Tracing.Tracing)
  (branchId : System.Guid)
  (program : Program)
  : RT.ExecutionState =
  { parentState with
      tracing = tracing
      branchId = branchId
      program = program
      types = { package = pm.getType }
      values = { parentState.values with package = pm.getValue }
      fns =
        { parentState.fns with
            package = pm.getFn
            isHarmful = fun pkg -> pm.isHarmful branchId pkg }
      blobs = { get = pm.getBlob; persist = pm.persistBlob } }


let execute
  (parentState : RT.ExecutionState)
  (branchId : System.Guid)
  (mod' : Utils.CliScript.PTCliScriptModule)
  (_args : List<Dval>) // CLEANUP update to List<String>, and extract in builtin
  (dbs : Map<string, RT.DB.T>)
  (traceSource : CliTraceSource)
  : Ply<RT.ExecutionResult> =
  uply {
    let (program : Program) = { dbs = dbs }

    let types =
      List.concat
        [ mod'.types |> List.map PT2RT.PackageType.toRT
          mod'.submodules.types |> List.map PT2RT.PackageType.toRT ]

    let values =
      List.concat
        [ mod'.values
          |> List.map (PT2RT.PackageValue.toRT parentState.values.builtIn)
          mod'.submodules.values
          |> List.map (PT2RT.PackageValue.toRT parentState.values.builtIn) ]

    let fns =
      List.concat
        [ mod'.fns |> List.map PT2RT.PackageFn.toRT
          mod'.submodules.fns |> List.map PT2RT.PackageFn.toRT ]

    // TODO we should probably use LibPM's in-memory grafting thing instead of this
    // (no need for RT.PM.withExtras to exist, I think)
    let pm = pmRT |> PackageManager.withExtras types values fns

    let (traceDesc, inputName, inputValue) = CliTraceSource.toTraceParams traceSource
    let traceID = AT.TraceID.create ()
    let tracer = Tracing.createCliTracer traceID traceDesc inputName inputValue

    let state = childState parentState pm tracer.executionTracing branchId program

    match mod'.exprs with
    | [] ->
      return
        RuntimeError.CLIs.NoExpressionsToExecute
        |> RuntimeError.CLI
        |> raiseUntargetedRTE
    | exprs ->
      let exprInsrts = exprs |> List.map (PT2RT.Expr.toRT Map.empty 0 None)
      let results = exprInsrts |> List.map (Exe.executeExpr state)
      match List.tryLast results with
      | Some lastResult ->
        let! result = lastResult
        do! tracer.storeTraceResults state
        return result
      | None ->
        return
          Exception.raiseInternal
            "No results from executing expressions (which should be impossible..)"
            []
  }

/// Create a branch-specific execution state for parsing.
///
/// `allowHarmful` is passed in rather than inherited from `parentState` so
/// callers can turn on the escape hatch per-invocation (e.g. when Dark-side
/// `run --allow-harmful` reaches `cliParseAndExecuteScript`).
let createBranchState
  (parentState : RT.ExecutionState)
  (branchId : System.Guid)
  (allowHarmful : bool)
  =
  let program : Program = { dbs = Map.empty }
  let state =
    childState parentState pmRT Exe.noTracing branchId program
  { state with allowHarmful = allowHarmful }


let fns () : List<BuiltInFn> =
  [ { name = fn "cliParseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "accountID" (TypeReference.option TUuid) ""
          Param.make "branchId" TUuid ""
          Param.make "filename" TString ""
          Param.make "code" TString ""
          Param.make "args" (TList TString) ""
          Param.make
            "allowHarmful"
            TBool
            "Opt out of Harmful-deprecation halting (see docs/deprecation)" ]
      returnType = TypeReference.result TInt64 (ExecutionError.typeRef ())
      description =
        "Parses Dark code as a script, and and executes it, returning an exit code"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName (), [])
        let resultOk = Dval.resultOk KTInt64 errType
        let resultError = Dval.resultError KTInt64 errType
        (function
        | exeState,
          _,
          [],
          [ accountIDDval
            DUuid branchId
            DString filename
            DString code
            DList(_vtTODO, scriptArgs)
            DBool allowHarmful ] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`. None passes through
            // (anonymous runs, tests).
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            let exeState = { exeState with accountID = accountID }
            // Use branch-specific state for parsing so name resolution uses the right branch
            let branchState = createBranchState exeState branchId allowHarmful
            let! parsedScript =
              parseCliScript branchState branchId "CliScript" filename code

            try
              let! dbs = loadDBs ()

              match parsedScript with
              | Ok mod' ->
                match!
                  execute
                    exeState
                    branchId
                    mod'
                    scriptArgs
                    dbs
                    (RunScript(filename, code))
                with
                | Ok(DInt64 i) -> return resultOk (DInt64 i)
                | Ok DUnit -> return resultOk (DInt64 0L)
                | Ok result ->
                  return
                    RuntimeError.CLIs.NonIntReturned result
                    |> RuntimeError.CLI
                    |> RT2DT.RuntimeError.toDT
                    |> resultError
                | Error(e, callStack) ->
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing Script. Call-stack:\n{csString}\n"
                  return e |> RT2DT.RuntimeError.toDT |> resultError
              | Error e -> return e |> RT2DT.RuntimeError.toDT |> resultError
            with e ->
              return createExceptionError e |> RT2DT.RuntimeError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliEvaluateExpression" 0
      typeParams = []
      parameters =
        [ Param.make "accountID" (TypeReference.option TUuid) ""
          Param.make "branchId" TUuid ""
          Param.make "expression" TString ""
          Param.make
            "allowHarmful"
            TBool
            "Opt out of Harmful-deprecation halting (see docs/deprecation)" ]
      returnType = TypeReference.result TString (ExecutionError.typeRef ())
      description = "Evaluates a Dark expression and returns the result as a String"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName (), [])
        let resultOk = Dval.resultOk KTString errType
        let resultError = Dval.resultError KTString errType
        (function
        | exeState,
          _,
          [],
          [ accountIDDval; DUuid branchId; DString expression; DBool allowHarmful ] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`.
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            let exeState = { exeState with accountID = accountID }
            // Use branch-specific state for parsing so name resolution uses the right branch
            let branchState = createBranchState exeState branchId allowHarmful
            let! parsedScript =
              parseCliScript
                branchState
                branchId
                "CliScript"
                "exprWrapper"
                expression

            try
              let! dbs = loadDBs ()

              match parsedScript with
              | Ok mod' ->
                match!
                  execute exeState branchId mod' [] dbs (EvalExpression expression)
                with
                | Ok result ->
                  match result with
                  | DString s -> return resultOk (DString s)
                  | _ ->
                    let! asString = Exe.dvalToRepr exeState result
                    return resultOk (DString asString)
                | Error(e, callStack) ->
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing expression. Call-stack:\n{csString}\n"
                  return e |> RT2DT.RuntimeError.toDT |> resultError
              | Error e -> return e |> RT2DT.RuntimeError.toDT |> resultError
            with e ->
              return createExceptionError e |> RT2DT.RuntimeError.toDT |> resultError
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    ]


/// All builtins the outer CLI execution state needs: this module's own
/// fns (so nested `eval`/`run` dispatches recursively) plus every
/// `Builtins.*` library the CLI surface depends on.
///
/// `defaultConfig` has SSRF guards on (loopback / RFC1918 / metadata
/// blocked, scheme restricted). For local-dev cases that need to hit
/// private targets, swap in `Builtins.Http.Client.Libs.HttpClient.looseConfig`.
let builtinsToUse () : RT.Builtins =
  let ptPM = LibDB.PackageManager.pt
  LibExecution.Builtin.combine
    [ Builtins.Pure.Builtin.builtins ()
      Builtins.Http.Client.Builtin.builtins
        Builtins.Http.Client.Libs.HttpClient.defaultConfig
      Builtins.Language.Builtin.builtins ()
      Builtins.Cli.Builtin.builtins ()
      Builtins.Time.Builtin.builtins ()
      Builtins.Random.Builtin.builtins ()
      Builtins.Matter.Builtin.builtins ptPM
      Builtins.Http.Server.Builtin.builtins ()
      LibExecution.Builtin.make [] (fns ()) ]
    []


let builtins () = LibExecution.Builtin.make [] (fns ())
