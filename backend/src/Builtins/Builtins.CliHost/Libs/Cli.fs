/// Builtin functions for building the CLI itself
/// (as opposed to functions CLI programs call, which live in packages)
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
module P = LibParser.Parser
module WT = LibParser.WrittenTypes
module WT2PT = LibParser.WrittenTypesToProgramTypes
module WTSourceFile = LibParser.SourceFile
module Validation = LibParser.Validation
module NRslv = LibParser.NameResolver
module Hashing = LibSerialization.Hashing.Hashing


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
  type ParseError = Message of string

  let fqTypeName () =
    FQTypeName.fqPackage (
      PackageRefs.Type.LanguageTools.Parser.CliScript.parseError ()
    )

  let toDT (err : ParseError) : Dval =
    let typeName = fqTypeName ()
    let (caseName, fields) =
      match err with
      | Message msg -> "Message", [ DString msg ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : ParseError =
    match d with
    | DEnum(_, _, _, "Message", [ DString msg ]) -> Message msg
    | _ -> Exception.raiseInternal "Invalid ParseError Dval" [ "dval", d ]

/// Lower a script's source items to a PTCliScriptModule. CLI execution identifies
/// the script by owner/scriptName; nested `module` blocks only affect the package
/// module path used when lowering declarations. `WTSourceFile.items` provides the
/// shared flat view over the parser's nested module tree.
///
/// Two passes: lower once against the base package manager, add the script's own
/// declarations to a package-manager overlay, then re-lower so declarations can
/// refer to siblings in the same script (for example, `main` calling `helper`).
/// The two passes are inherent: pass 1 cannot resolve those sibling references
/// because the declarations are not in the PM yet.
///
let private declarationsToModule
  (state : RT.ExecutionState)
  (branchId : Option<string>)
  (owner : string)
  (scriptName : string)
  (validated : Validation.ValidatedSourceFile)
  : Ply<Utils.CliScript.PTCliScriptModule> =
  uply {
    let sourceItems =
      validated
      |> Validation.ValidatedSourceFile.toWrittenTypes
      |> WTSourceFile.items
    let builtins : RT.Builtins =
      { values = state.values.builtIn; fns = state.fns.builtIn }
    let baseModules = if scriptName = "" then [] else [ scriptName ]

    // Build WT package declarations + top-level expressions.
    let wtFns = ResizeArray<WT.PackageFn.PackageFn>()
    let wtTypes = ResizeArray<WT.PackageType.PackageType>()
    let wtValues = ResizeArray<WT.PackageValue.PackageValue>()
    // Each trailing expr keeps its module path, so an expr inside `module M =`
    // can still resolve M's declarations by short name.
    let wtExprs = ResizeArray<List<string> * WT.Expr>()
    for item in sourceItems do
      match item with
      | WTSourceFile.Fn(declPath, fn) ->
        wtFns.Add(WT.packageFn owner (baseModules @ declPath) fn)
      | WTSourceFile.Type(declPath, t) ->
        wtTypes.Add(WT.packageType owner (baseModules @ declPath) t)
      | WTSourceFile.Value(declPath, v) ->
        wtValues.Add(WT.packageValue owner (baseModules @ declPath) v)
      | WTSourceFile.Expr(declPath, e) -> wtExprs.Add(baseModules @ declPath, e)
      // DB/test decls are produced only by test-mode parsing, not the CLI path.
      | WTSourceFile.TypeDB _
      | WTSourceFile.Test _ -> ()

    let onMissing = NRslv.OnMissing.Allow
    let fnList = List.ofSeq wtFns
    let typeList = List.ofSeq wtTypes
    let valueList = List.ofSeq wtValues

    // Graft locations are derived from names only, so they're identical across
    // both lowering passes (pass 2 keeps each decl's name, changing only body/hash).
    let fnLocations =
      fnList |> List.map (fun f -> WT2PT.PackageFn.Name.toLocation f.name)
    let typeLocations =
      typeList |> List.map (fun t -> WT2PT.PackageType.Name.toLocation t.name)
    let valueLocations =
      valueList |> List.map (fun v -> WT2PT.PackageValue.Name.toLocation v.name)

    let lowerFns pm =
      fnList
      |> Ply.List.mapSequentially (fun fn ->
        WT2PT.PackageFn.toPT
          builtins
          pm
          onMissing
          (WT2PT.PackageFn.Name.toModules fn.name)
          fn)
    let lowerTypes pm =
      typeList
      |> Ply.List.mapSequentially (fun t ->
        WT2PT.PackageType.toPT
          pm
          onMissing
          (WT2PT.PackageType.Name.toModules t.name)
          t)
    let lowerValues pm =
      valueList
      |> Ply.List.mapSequentially (fun v ->
        WT2PT.PackageValue.toPT
          builtins
          pm
          onMissing
          (WT2PT.PackageValue.Name.toModules v.name)
          v)

    // WT2PT gives each declaration an empty `Hash ""` placeholder. The graft
    // below keys declarations by hash, so without real content hashes a script's
    // types, values, or fns can collapse to one entry and only the last one
    // survives.
    //
    // Use the same `Hashing.compute*Hash` helpers package playback uses for
    // `Hash ""` declarations. SCC-aware hash stabilization is not needed here
    // because pass-1 bodies do not contain resolved sibling references.
    let hashType (t : PT.PackageType.PackageType) =
      { t with hash = Hashing.computeTypeHash Hashing.Normal t }
    let hashValue (v : PT.PackageValue.PackageValue) =
      { v with hash = Hashing.computeValueHash Hashing.Normal v }
    let hashFn (f : PT.PackageFn.PackageFn) =
      { f with hash = Hashing.computeFnHash Hashing.Normal f }

    // Pass 1: lower against the base pm (intra-script refs unresolved, allowed).
    // `ptForBranch` = core, plus that branch's delta ops overlaid (empty for `None`,
    // so this is exactly `pt` for the common main case). A branch fn referenced in
    // the script/expr thus resolves name->hash at parse time.
    //
    // The branch arrives as an argument rather than being read from process state
    // here, so that parsing against a branch you aren't sitting on is expressible at
    // all.
    let pm0 = LibDB.PackageManager.ptForBranch branchId
    let! fns1 = lowerFns pm0 |> Ply.map (List.map hashFn)
    let! types1 = lowerTypes pm0 |> Ply.map (List.map hashType)
    let! values1 = lowerValues pm0 |> Ply.map (List.map hashValue)

    // Graft the script's own declarations into the pm, keyed by location.
    let pm1 =
      pm0
      |> PT.PackageManager.withExtras
        (List.zip types1 typeLocations)
        (List.zip values1 valueLocations)
        (List.zip fns1 fnLocations)

    // Pass 2: re-lower with the grafted pm so intra-script references resolve.
    // Keep each declaration's pass-1 hash, because pass-2 bodies contain refs to
    // those hashes. Re-hashing the resolved bodies would make registered hashes
    // disagree with refs embedded in callers.
    let! fns = lowerFns pm1
    let fns =
      List.map2
        (fun (f1 : PT.PackageFn.PackageFn) (f2 : PT.PackageFn.PackageFn) ->
          { f2 with hash = f1.hash })
        fns1
        fns
    let! types = lowerTypes pm1
    let types =
      List.map2
        (fun (t1 : PT.PackageType.PackageType) (t2 : PT.PackageType.PackageType) ->
          { t2 with hash = t1.hash })
        types1
        types
    let! values = lowerValues pm1
    let values =
      List.map2
        (fun (v1 : PT.PackageValue.PackageValue) (v2 : PT.PackageValue.PackageValue) ->
          { v2 with hash = v1.hash })
        values1
        values

    // Graft the pass-2 declarations (resolved bodies, pass-1 hashes) for the
    // expressions' lowering — their refs then match the returned decls exactly.
    let pm2 =
      pm0
      |> PT.PackageManager.withExtras
        (List.zip types typeLocations)
        (List.zip values valueLocations)
        (List.zip fns fnLocations)

    let emptyContext =
      { WT2PT.Context.currentFnName = None
        WT2PT.Context.argMap = Map.empty
        WT2PT.Context.localBindings = Set.empty }
    let! exprs =
      wtExprs
      |> List.ofSeq
      |> Ply.List.mapSequentially (fun (modules, e) ->
        WT2PT.Expr.toPT builtins pm2 onMissing (owner :: modules) emptyContext e)

    let emptyDefs : Utils.CliScript.Definitions =
      { types = []; values = []; fns = [] }
    return
      { Utils.CliScript.PTCliScriptModule.types = types
        values = values
        fns = fns
        submodules = emptyDefs
        exprs = exprs }
  }

/// Parse a whole CLI script with the hand-written parser, lowering `WT → PT`. One
/// parser feeds both the editor (WrittenTypes directly) and execution (via this
/// lowering).
let parseCliScript
  (state : RT.ExecutionState)
  (branchId : Option<string>)
  (owner : string)
  (scriptName : string)
  (code : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, List<P.Diagnostic>>> =
  uply {
    match P.parseFor Validation.Script code with
    | Error diagnostics -> return Error diagnostics
    | Ok validated ->
      let! m = declarationsToModule state branchId owner scriptName validated
      return Ok m
  }

/// Parse a single expression (the `eval` path) with the hand-written parser,
/// lowering `WT → PT`. Reuses the script lowering so `let x = … in x` and short
/// statement sequences also work; a bare expression is just `exprs`.
let parseCliExpr
  (state : RT.ExecutionState)
  (branchId : Option<string>)
  (expression : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, List<P.Diagnostic>>> =
  uply {
    match P.parseFor Validation.Script expression with
    | Error diagnostics -> return Error diagnostics
    | Ok validated ->
      let! m = declarationsToModule state branchId "" "" validated
      return Ok m
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
  /// Metadata values are `obj`; we accept the lossy `string v` here (rather than
  /// the Dval-wrapping machinery) because this metadata is only ever displayed.
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
  (program : Program)
  : RT.ExecutionState =
  { parentState with
      tracing = tracing
      program = program
      types = { package = pm.getType }
      values = { parentState.values with package = pm.getValue }
      fns =
        { parentState.fns with
            package = pm.getFn
            isHarmful = fun pkg -> pm.isHarmful pkg }
      blobs = { get = pm.getBlob; persist = pm.persistBlob } }


let execute
  (parentState : RT.ExecutionState)
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

    // Graft the delta defs (compiled to RT) of the branch THIS RUN is on, alongside
    // the script's own, so a branch fn CALLED in the expr executes. Empty for main.
    //
    // From `parentState.branchId`, the same branch the parse resolved names against.
    // Reading process state here instead would let the two disagree -- names
    // resolved to hashes on one branch, executed against another branch's graft --
    // whenever a caller ran against a branch it wasn't sitting on.
    let branchOps =
      LibDB.PackageManager.opsForBranch (
        if parentState.branchId = "main" then None else Some parentState.branchId
      )
    let branchTypes =
      branchOps
      |> List.choose (function
        | PT.PackageOp.AddType t -> Some(PT2RT.PackageType.toRT t)
        | _ -> None)
    // Branch VALUES are deliberately NOT grafted here: the author path folds their
    // content into package_values + evaluates them (rt_dval), so getValue reads the
    // interpreter-computed Dval. The graft's PT2RT.PackageValue.toRT uses
    // evalConstantExpr (constants only) and would SHADOW that with an empty Dval for
    // a fn-call body. Name resolution stays branch-isolated via the SetName overlay.
    // scm-spec 11.
    let branchFns =
      branchOps
      |> List.choose (function
        | PT.PackageOp.AddFn f -> Some(PT2RT.PackageFn.toRT f)
        | _ -> None)

    // TODO we should probably use LibPM's in-memory grafting thing instead of this
    // (no need for RT.PM.withExtras to exist, I think)
    let pm =
      pmRT
      |> PackageManager.withExtras (branchTypes @ types) values (branchFns @ fns)

    let (traceDesc, inputName, inputValue) = CliTraceSource.toTraceParams traceSource
    let traceID = AT.TraceID.create ()
    let tracer = Tracing.createCliTracer traceID traceDesc inputName inputValue

    let state = childState parentState pm tracer.executionTracing program

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
let createBranchState (parentState : RT.ExecutionState) (allowHarmful : bool) =
  let program : Program = { dbs = Map.empty }
  let state = childState parentState pmRT Exe.noTracing program
  { state with allowHarmful = allowHarmful }


let fns () : List<BuiltInFn> =
  [ { name = fn "cliParseAndExecuteScript" 0
      typeParams = []
      parameters =
        [ Param.make "accountID" (TypeReference.option TUuid) ""
          Param.make
            "branchId"
            TString
            "the branch to resolve names against; \"\" is main. A parameter rather than ambient state so a caller can parse against a branch it isn't sitting on"
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
          [| accountIDDval
             DString branchId
             DString filename
             DString code
             DList(_vtTODO, scriptArgs)
             DBool allowHarmful
             DBool sandbox |] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`. None passes through
            // (anonymous runs, tests).
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            // `allowHarmful` belongs on the state the BODY runs under, not only on the parse state below:
            // the Harmful gate fires in the interpreter, so setting it on a state that never executes
            // leaves `--allow-harmful` parsed, threaded, and inert.
            let exeState =
              { exeState with
                  accountID = accountID
                  branchId = branchId
                  allowHarmful = allowHarmful }
            // Use branch-specific state for parsing so name resolution uses the right branch.
            // Parsing keeps the host's caps — name resolution / package loading needs
            // cli-host effects to boot (the noCaps-breaks-bootstrap case). Only the script
            // *body* is sandboxed below (`runCaps` on `exeState`).
            let branch = if branchId = "main" then None else Some branchId
            let branchState = createBranchState exeState allowHarmful

            try
              // A parse failure surfaces a precise diagnostic as a `ParseError`
              let! parseResult =
                parseCliScript branchState branch "CliScript" filename code
              let! parsedScript =
                match parseResult with
                | Ok m -> Ply(Ok m)
                | Error diags ->
                  let pe =
                    match diags with
                    | d :: _ -> ParseError.Message(P.renderDiagnostic code d)
                    | [] -> ParseError.Message "Parse error"
                  Ply(Error pe)

              let! dbs = loadDBs ()

              match parsedScript with
              | Ok mod' ->
                // `dark run` RESPECTS the host's configured grant by default
                // (`hostCaps`: allCaps until an instance grant is configured, then
                // that grant) -- the same posture as `eval`, so the grant you set is
                // the grant scripts obey. `--sandbox` drops to NO capabilities for
                // running untrusted scripts (any effectful builtin then raises).
                // TODO product decision, revisit: this favors "run my own script"
                // over "run an untrusted script" (sandbox is opt-IN). If `dark run
                // <url>` / piping untrusted code becomes common, a deny-all default
                // + `--trust`/`--apply-host-caps` opt-in may be safer. See also the
                // trust-boundary TODO in `LanguageTools.Capabilities.all`.
                let runCaps =
                  if sandbox then
                    LibExecution.Capabilities.noCaps
                  else
                    LibDB.CapabilityGrants.hostCaps ()
                let exeState = { exeState with grantedCaps = runCaps }
                match!
                  execute exeState mod' scriptArgs dbs (RunScript(filename, code))
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
          Param.make
            "branchId"
            TString
            "the branch to resolve names against; \"\" is main. A parameter rather than ambient state so a caller can parse against a branch it isn't sitting on"
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
          [| accountIDDval
             DString branchId
             DString expression
             DBool allowHarmful |] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`.
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            // `eval` runs the expression under the HOST's capabilities -- `allCaps`
            // until an instance grant is configured, then whatever that grant allows
            // (the gate denies uncovered builtins).
            let exeState =
              { exeState with
                  accountID = accountID
                  branchId = branchId
                  // See the note in `cliParseAndExecuteScript`: the gate fires where the expression runs.
                  allowHarmful = allowHarmful
                  grantedCaps = LibDB.CapabilityGrants.hostCaps () }
            // Use branch-specific state for parsing so name resolution uses the right branch
            let branch = if branchId = "main" then None else Some branchId
            let branchState = createBranchState exeState allowHarmful

            try
              // Parsing can raise (e.g. deep VM failures); keep it inside the try
              // so its exceptions hit the Unhandled net. `eval` is single-expression
              // only; parse failures surface a precise diagnostic (no fallback).
              let! parseResult = parseCliExpr branchState branch expression
              let! parsedScript =
                match parseResult with
                | Ok m -> Ply(Ok m)
                | Error diags ->
                  let pe =
                    match diags with
                    | d :: _ -> ParseError.Message(P.renderDiagnostic expression d)
                    | [] -> ParseError.Message "Parse error"
                  Ply(Error pe)

              let! dbs = loadDBs ()

              match parsedScript with
              | Ok mod' ->
                match! execute exeState mod' [] dbs (EvalExpression expression) with
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
