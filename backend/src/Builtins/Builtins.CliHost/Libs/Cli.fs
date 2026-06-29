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


module ParseError =
  type Unparseable =
    { text : string; line : int64; column : int64; note : Option<string> }

  type ParseError =
    | Unparseable of Unparseable
    | Other of string

  let fqTypeName () =
    FQTypeName.fqPackage (
      PackageRefs.Type.LanguageTools.Parser.CliScript.parseError ()
    )

  let unparseableTypeName () =
    FQTypeName.fqPackage (
      PackageRefs.Type.LanguageTools.Parser.CliScript.unparseable ()
    )

  let unparseableToDT (u : Unparseable) : Dval =
    let typeName = unparseableTypeName ()
    let fields =
      [ "text", DString u.text
        "line", Dval.int (bigint u.line)
        "column", Dval.int (bigint u.column)
        "note", C2DT.Option.toDT DString KTString u.note ]
    DRecord(typeName, typeName, [], Map fields)

  let unparseableFromDT (d : Dval) : Unparseable =
    match d with
    | DRecord(_, _, _, fields) ->
      { text = fields |> D.field "text" |> D.string
        line = fields |> D.field "line" |> D.darkInt |> int64
        column = fields |> D.field "column" |> D.darkInt |> int64
        note = C2DT.Option.fromDT D.string (fields |> D.field "note") }
    | _ -> Exception.raiseInternal "Invalid Unparseable Dval" [ "dval", d ]

  let toDT (err : ParseError) : Dval =
    let typeName = fqTypeName ()
    let (caseName, fields) =
      match err with
      | Unparseable u -> "Unparseable", [ unparseableToDT u ]
      | Other msg -> "Other", [ DString msg ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : ParseError =
    match d with
    | DEnum(_, _, _, "Unparseable", [ uDval ]) ->
      Unparseable(unparseableFromDT uDval)
    | DEnum(_, _, _, "Other", [ DString msg ]) -> Other msg
    | _ -> Exception.raiseInternal "Invalid ParseError Dval" [ "dval", d ]

/// Parse CLI script code via the single-shot `parseForCli` Dark function.
/// Keeping PM creation and parsing in the same execution context avoids
/// lambda instructions crossing VM boundaries.
let parseCliScript
  (exeState : RT.ExecutionState)
  (branchId : System.Guid)
  (owner : string)
  (scriptName : string)
  (code : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, ParseError.ParseError>> =
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
      | Error parseErrorDval -> return Error(ParseError.fromDT parseErrorDval)
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

  let unhandledTypeName () = FQTypeName.fqPackage (PackageRefs.Type.Cli.unhandled ())

  type Unhandled = { message : string; metadata : List<string * string> }

  type ExecutionError =
    | Parse of ParseError.ParseError
    | Runtime of RT.RuntimeError.Error
    | Unhandled of Unhandled

  /// Capture an exception's message and metadata for the `Unhandled` case.
  /// `Exception.toMetadata` only ever produces string-stringified values,
  /// so `List<string * string>` faithfully represents what we have without
  /// the Dval-wrapping machinery.
  let unhandledFromExn (e : exn) : Unhandled =
    { message = Exception.getMessages e |> String.concat "\n"
      metadata = Exception.toMetadata e |> List.map (fun (k, v) -> (k, string v)) }

  let private unhandledToDT (u : Unhandled) : Dval =
    let typeName = unhandledTypeName ()
    let pairKT = KTTuple(VT.string, VT.string, [])
    let metadataDval =
      u.metadata
      |> List.map (fun (k, v) -> DTuple(DString k, DString v, []))
      |> fun items -> DList(VT.known pairKT, items)
    let fields = [ "message", DString u.message; "metadata", metadataDval ]
    DRecord(typeName, typeName, [], Map fields)

  let toDT (err : ExecutionError) : Dval =
    let typeName = fqTypeName ()
    let (caseName, fields) =
      match err with
      | Parse pe -> "Parse", [ ParseError.toDT pe ]
      | Runtime rte -> "Runtime", [ RT2DT.RuntimeError.toDT rte ]
      | Unhandled u -> "Unhandled", [ unhandledToDT u ]
    DEnum(typeName, typeName, [], caseName, fields)


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
  let state = childState parentState pmRT Exe.noTracing branchId program
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
            "Opt out of Harmful-deprecation halting (see docs/deprecation)"
          Param.make
            "sandbox"
            TBool
            "Run the script body with NO capabilities (a deny-all sandbox for untrusted scripts), instead of the host's configured grant" ]
      returnType = TypeReference.result TInt (ExecutionError.typeRef ())
      description =
        "Parses Dark code as a script, and and executes it, returning an exit code"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName (), [])
        let resultOk = Dval.resultOk KTInt errType
        let resultError = Dval.resultError KTInt errType
        (function
        | exeState,
          _,
          [],
          [ accountIDDval
            DUuid branchId
            DString filename
            DString code
            DList(_vtTODO, scriptArgs)
            DBool allowHarmful
            DBool sandbox ] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`. None passes through
            // (anonymous runs, tests).
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            let exeState = { exeState with accountID = accountID }
            // Use branch-specific state for parsing so name resolution uses the right branch.
            // Parsing keeps the host's caps — name resolution / package loading needs
            // cli-host effects to boot (the noCaps-breaks-bootstrap case). Only the script
            // *body* is sandboxed below (`runState`).
            let branchState = createBranchState exeState branchId allowHarmful

            try
              // parseCliScript itself can raise (e.g. deep VM failures); keep
              // it inside the try so its exceptions hit the Unhandled net.
              let! parsedScript =
                parseCliScript branchState branchId "CliScript" filename code

              let! dbs = loadDBs ()

              match parsedScript with
              | Ok mod' ->
                // `dark run` RESPECTS the host's configured grant by default (`hostCaps`: allCaps until
                // an instance grant is configured, then that grant) — the same posture as `eval`, so the
                // grant you set is the grant scripts obey. `--sandbox` drops to NO capabilities for
                // running untrusted scripts (any effectful builtin then raises).
                // TODO product decision, revisit: this favors "run my own script" over "run an untrusted
                // script" (sandbox is opt-IN). If `dark run <url>` / piping untrusted code becomes common,
                // a deny-all default + `--trust`/`--apply-host-caps` opt-in may be safer. See also the
                // trust-boundary TODO in `LanguageTools.Capabilities.all`.
                let runCaps =
                  if sandbox then
                    LibExecution.Capabilities.noCaps
                  else
                    LibDB.CapabilityGrants.hostCaps ()
                let exeState = { exeState with grantedCaps = runCaps }
                match!
                  execute
                    exeState
                    branchId
                    mod'
                    scriptArgs
                    dbs
                    (RunScript(filename, code))
                with
                | Ok(DInt i) -> return resultOk (DInt i)
                | Ok(DInt64 i) -> return resultOk (Dval.int (bigint i))
                | Ok DUnit -> return resultOk (Dval.int (bigint 0))
                | Ok result ->
                  let rte =
                    RuntimeError.CLIs.NonIntReturned result |> RuntimeError.CLI
                  return
                    resultError (ExecutionError.toDT (ExecutionError.Runtime rte))
                | Error(e, callStack) ->
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing Script. Call-stack:\n{csString}\n"
                  return resultError (ExecutionError.toDT (ExecutionError.Runtime e))
              | Error pe ->
                return resultError (ExecutionError.toDT (ExecutionError.Parse pe))
            // Runtime errors raised via `raiseUntargetedRTE` (e.g.
            // `NoExpressionsToExecute`) escape as `RuntimeErrorException`
            // rather than returning through the normal `Error(rte, _)`
            // channel. Catch them explicitly so they're classified as
            // `Runtime`, not `Unhandled`.
            with
            | RuntimeErrorException(_, rte) ->
              return resultError (ExecutionError.toDT (ExecutionError.Runtime rte))
            | e ->
              return
                resultError (
                  ExecutionError.toDT (
                    ExecutionError.Unhandled(ExecutionError.unhandledFromExn e)
                  )
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
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
      returnType =
        TypeReference.result
          (TypeReference.option TString)
          (ExecutionError.typeRef ())
      description =
        "Evaluates a Dark expression. Returns Some(reprString) for a value, "
        + "or None when the result is Unit (so callers can suppress empty echo)."
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName (), [])
        let okKT = KTCustomType(Dval.optionType (), [ VT.known KTString ])
        let resultOk = Dval.resultOk okKT errType
        let resultError = Dval.resultError okKT errType
        let okSome (s : string) = resultOk (Dval.optionSome KTString (DString s))
        let okNone () = resultOk (Dval.optionNone KTString)
        (function
        | exeState,
          _,
          [],
          [ accountIDDval; DUuid branchId; DString expression; DBool allowHarmful ] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`.
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            // `eval` runs the expression under the HOST's capabilities — `allCaps` until an instance
            // grant is configured, then whatever that grant allows (the gate denies uncovered builtins).
            let exeState =
              { exeState with
                  accountID = accountID
                  grantedCaps = LibDB.CapabilityGrants.hostCaps () }
            // Use branch-specific state for parsing so name resolution uses the right branch
            let branchState = createBranchState exeState branchId allowHarmful

            try
              // parseCliScript itself can raise (e.g. deep VM failures); keep
              // it inside the try so its exceptions hit the Unhandled net.
              let! parsedScript =
                parseCliScript
                  branchState
                  branchId
                  "CliScript"
                  "exprWrapper"
                  expression

              let! dbs = loadDBs ()

              match parsedScript with
              | Ok mod' ->
                match!
                  execute exeState branchId mod' [] dbs (EvalExpression expression)
                with
                | Ok result ->
                  match result with
                  | DUnit -> return okNone ()
                  | DString s -> return okSome s
                  | _ ->
                    let! asString = Exe.dvalToRepr exeState result
                    return okSome asString
                | Error(e, callStack) ->
                  let! csString = Exe.callStackString exeState callStack
                  print $"Error when executing expression. Call-stack:\n{csString}\n"
                  return resultError (ExecutionError.toDT (ExecutionError.Runtime e))
              | Error pe ->
                return resultError (ExecutionError.toDT (ExecutionError.Parse pe))
            with
            | RuntimeErrorException(_, rte) ->
              return resultError (ExecutionError.toDT (ExecutionError.Runtime rte))
            | e ->
              return
                resultError (
                  ExecutionError.toDT (
                    ExecutionError.Unhandled(ExecutionError.unhandledFromExn e)
                  )
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
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
