/// Builtin functions for building the CLI itself
/// (as opposed to functions CLI programs call, which live in packages)
module Builtins.CliHost.Libs.Cli

open System.Threading.Tasks
open FSharp.Control.Tasks


open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

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
module HashStabilization = LibDB.HashStabilization
module AstTransformer = LibDB.AstTransformer
module PackageLocation = LibDB.PackageLocation
module PolicyToDT = LibExecution.PermissionsToDarkTypes
module PolicyStore = LibDB.PolicyStore


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
  (owner : string)
  (scriptName : string)
  (validated : Validation.ValidatedSourceFile)
  : Ply<Utils.CliScript.PTCliScriptModule> =
  uply {
    let sourceItems =
      validated
      |> Validation.ValidatedSourceFile.toWrittenTypes
      |> WTSourceFile.items
    let builtins = state.builtins
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
          state.branchId
          (WT2PT.PackageFn.Name.toModules fn.name)
          fn)
    let lowerTypes pm =
      typeList
      |> Ply.List.mapSequentially (fun t ->
        WT2PT.PackageType.toPT
          pm
          onMissing
          state.branchId
          (WT2PT.PackageType.Name.toModules t.name)
          t)
    let lowerValues pm =
      valueList
      |> Ply.List.mapSequentially (fun v ->
        WT2PT.PackageValue.toPT
          builtins
          pm
          onMissing
          state.branchId
          (WT2PT.PackageValue.Name.toModules v.name)
          v)

    // WT2PT leaves each declaration with an empty `Hash ""`, and the graft below
    // keys declarations by hash, so each needs a distinct one before it can be
    // grafted. Use a location placeholder, not a content hash: a hash taken
    // before resolution is computed over unresolved references, which serialise
    // without their names, so two declarations differing only in which sibling
    // they mention would hash the same and the graft would keep one of them.
    let stampFn (f : PT.PackageFn.PackageFn) loc =
      { f with hash = PackageLocation.placeholderHash loc }
    let stampType (t : PT.PackageType.PackageType) loc =
      { t with hash = PackageLocation.placeholderHash loc }
    let stampValue (v : PT.PackageValue.PackageValue) loc =
      { v with hash = PackageLocation.placeholderHash loc }

    // Pass 1: lower against the base pm (intra-script refs unresolved, allowed).
    // The resolver looks up packages on `state.branchId` (threaded through WT2PT),
    // so WIP on this branch resolves without wrapping the pm.
    let pm0 = LibDB.PackageManager.ptForAccount state.accountID
    let! fns1 =
      lowerFns pm0 |> Ply.map (fun fns -> List.map2 stampFn fns fnLocations)
    let! types1 =
      lowerTypes pm0 |> Ply.map (fun ts -> List.map2 stampType ts typeLocations)
    let! values1 =
      lowerValues pm0 |> Ply.map (fun vs -> List.map2 stampValue vs valueLocations)

    // Graft the script's own declarations into the pm, keyed by location.
    let pm1 =
      pm0
      |> PT.PackageManager.withExtras
        (List.zip types1 typeLocations)
        (List.zip values1 valueLocations)
        (List.zip fns1 fnLocations)

    // Pass 2: re-lower with the grafted pm so intra-script references resolve.
    let! fns2 =
      lowerFns pm1 |> Ply.map (fun fns -> List.map2 stampFn fns fnLocations)
    let! types2 =
      lowerTypes pm1 |> Ply.map (fun ts -> List.map2 stampType ts typeLocations)
    let! values2 =
      lowerValues pm1 |> Ply.map (fun vs -> List.map2 stampValue vs valueLocations)

    // References are resolved now, so hash for real. That keeps script
    // declarations content-addressed: one structurally identical to a package
    // declaration lands on the same hash, and a rename changes nothing.
    //
    // Pass-2 bodies still reference siblings by placeholder, so the hashes have
    // to be computed in dependency order with each sibling's real hash
    // substituted in. Stabilization does exactly that, batching SCCs, so
    // mutually recursive script declarations work too.
    let stabilization =
      HashStabilization.stabilize
        AstTransformer.emptyMapping
        { types =
            List.map2
              (fun (t : PT.PackageType.PackageType) loc ->
                PackageLocation.toFQN loc, (t, t.hash, loc))
              types2
              typeLocations
            |> Map.ofList
          fns =
            List.map2
              (fun (f : PT.PackageFn.PackageFn) loc ->
                PackageLocation.toFQN loc, (f, f.hash, loc))
              fns2
              fnLocations
            |> Map.ofList
          values =
            List.map2
              (fun (v : PT.PackageValue.PackageValue) loc ->
                PackageLocation.toFQN loc, (v, v.hash, loc))
              values2
              valueLocations
            |> Map.ofList }

    let finalHash (current : PT.Hash) (loc : PT.PackageLocation) : PT.Hash =
      Map.tryFind (PackageLocation.toFQN loc) stabilization.fqnHashes
      |> Option.defaultValue current

    // Rewrite each body so its sibling references point at the final hashes.
    let fns =
      List.map2
        (fun (f : PT.PackageFn.PackageFn) loc ->
          { AstTransformer.transformFn stabilization.mapping f with
              hash = finalHash f.hash loc })
        fns2
        fnLocations
    let types =
      List.map2
        (fun (t : PT.PackageType.PackageType) loc ->
          { AstTransformer.transformType stabilization.mapping t with
              hash = finalHash t.hash loc })
        types2
        typeLocations
    let values =
      List.map2
        (fun (v : PT.PackageValue.PackageValue) loc ->
          { AstTransformer.transformValue stabilization.mapping v with
              hash = finalHash v.hash loc })
        values2
        valueLocations

    // Graft the final declarations for the expressions' lowering, so their refs
    // match the returned decls exactly.
    let pm2 =
      pm0
      |> PT.PackageManager.withExtras
        (List.zip types typeLocations)
        (List.zip values valueLocations)
        (List.zip fns fnLocations)

    // Register the same declarations so a runtime error can still name them. The
    // error outlives this graft: the CLI renders it once execution has returned,
    // and the pretty-printer turns hashes back into names by asking where a hash
    // is bound.
    LibDB.EphemeralPackages.register
      (List.map2
        (fun (t : PT.PackageType.PackageType) loc -> t.hash, loc)
        types
        typeLocations)
      (List.map2
        (fun (v : PT.PackageValue.PackageValue) loc -> v.hash, loc)
        values
        valueLocations)
      (List.map2
        (fun (f : PT.PackageFn.PackageFn) loc -> f.hash, loc)
        fns
        fnLocations)

    let emptyContext =
      { WT2PT.Context.currentFnName = None
        WT2PT.Context.argMap = Map.empty
        WT2PT.Context.localBindings = Set.empty }
    let! exprs =
      wtExprs
      |> List.ofSeq
      |> Ply.List.mapSequentially (fun (modules, e) ->
        WT2PT.Expr.toPT
          builtins
          pm2
          onMissing
          state.branchId
          (owner :: modules)
          emptyContext
          e)

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
  (owner : string)
  (scriptName : string)
  (code : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, List<P.Diagnostic>>> =
  uply {
    match P.parseFor Validation.Script code with
    | Error diagnostics -> return Error diagnostics
    | Ok validated ->
      let! m = declarationsToModule state owner scriptName validated
      return Ok m
  }

/// Parse a single expression (the `eval` path): the script lowering with no
/// module, so `let x = … in x` and short statement sequences also work; a
/// bare expression is just `exprs`.
let parseCliExpr
  (state : RT.ExecutionState)
  (expression : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, List<P.Diagnostic>>> =
  parseCliScript state "" "" expression


module ExecutionError =
  let fqTypeName () = FQTypeName.fqPackage (PackageRefs.Type.Cli.executionError ())
  let typeRef () = TCustomType(NR.ok (fqTypeName ()), [])

  let unhandledTypeName () = FQTypeName.fqPackage (PackageRefs.Type.Cli.unhandled ())

  type Unhandled = { message : string; metadata : List<string * string> }

  let permissionDeniedTypeName () =
    FQTypeName.fqPackage (PackageRefs.Type.Cli.permissionDenied ())

  /// Details for a run stopped by a policy denial: the runtime error and the
  /// resource, suggested rule, and call stack needed to show it or retry.
  type PermissionDenied =
    {
      error : RT.RuntimeError.Error
      /// What was attempted, e.g. "reading `/etc/hosts`".
      resource : string
      /// The exact `permissions allow` rule that would cover it, when one
      /// exists and the denying layer was the instance policy — the only
      /// layer that rule edits.
      rule : Option<string>
      /// The rendered call stack, for the CLI to print if the denial stands.
      callStack : string
    }

  type ExecutionError =
    | Parse of ParseError.ParseError
    | Runtime of RT.RuntimeError.Error
    | Denied of PermissionDenied
    | Unhandled of Unhandled

  /// A runtime error is a `Denied` when the run recorded an instance-layer
  /// denial: Dark cannot catch runtime errors, so the last denial recorded
  /// under the guest state is the one that ended the run. Any other layer's
  /// denial stays a plain `Runtime` error, since no `permissions allow` rule
  /// would change it.
  let classify
    (denied : ResizeArray<RT.PermissionDenialRecord>)
    (rte : RT.RuntimeError.Error)
    : ExecutionError =
    if denied.Count = 0 then
      Runtime rte
    else
      let last = denied[denied.Count - 1]
      match last.layer with
      | LibExecution.Permissions.Layer.Instance ->
        Denied
          { error = rte
            resource = last.resource
            rule = last.suggestion
            callStack = "" }
      | _ -> Runtime rte

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

  let private permissionDeniedToDT (d : PermissionDenied) : Dval =
    let typeName = permissionDeniedTypeName ()
    let fields =
      [ "error", RT2DT.RuntimeError.toDT d.error
        "resource", DString d.resource
        "rule", (d.rule |> Option.map DString |> Dval.option KTString)
        "callStack", DString d.callStack ]
    DRecord(typeName, typeName, [], Map fields)

  let toDT (err : ExecutionError) : Dval =
    let typeName = fqTypeName ()
    let (caseName, fields) =
      match err with
      | Parse pe -> "Parse", [ ParseError.toDT pe ]
      | Runtime rte -> "Runtime", [ RT2DT.RuntimeError.toDT rte ]
      | Denied d -> "Denied", [ permissionDeniedToDT d ]
      | Unhandled u -> "Unhandled", [ unhandledToDT u ]
    DEnum(typeName, typeName, [], caseName, fields)


/// Parse guest source (`run`/`eval`): the first diagnostic, rendered against
/// `source`, becomes the `ParseError` the caller reports.
let private parseGuest
  (parse : Ply<Result<Utils.CliScript.PTCliScriptModule, List<P.Diagnostic>>>)
  (source : string)
  : Ply<Result<Utils.CliScript.PTCliScriptModule, ParseError.ParseError>> =
  uply {
    match! parse with
    | Ok m -> return Ok m
    | Error diagnostics ->
      let message =
        match diagnostics with
        | d :: _ -> P.renderDiagnostic source d
        | [] -> "Parse error"
      return Error(ParseError.Message message)
  }

/// Run a guest entry point (`run`/`eval`), turning an escaping exception into
/// the builtin's error result. Runtime errors raised via `raiseUntargetedRTE`
/// (e.g. `NoExpressionsToExecute`) escape as `RuntimeErrorException` rather
/// than returning through the normal `Error(rte, _)` channel, so they are
/// classified `Runtime`, not `Unhandled`.
let private guestTry
  (resultError : Dval -> Dval)
  (toError : RT.RuntimeError.Error -> ExecutionError.ExecutionError)
  (body : unit -> Ply<Dval>)
  : Ply<Dval> =
  uply {
    try
      let! result = body ()
      return result
    with
    | RuntimeErrorException(_, rte) ->
      return resultError (ExecutionError.toDT (toError rte))
    | e ->
      return
        resultError (
          ExecutionError.toDT (
            ExecutionError.Unhandled(ExecutionError.unhandledFromExn e)
          )
        )
  }


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
      let exprInstrs = exprs |> List.map (PT2RT.Expr.toRT Map.empty 0 None)

      // Awaited in order, and the first error ends the script.
      let rec runInOrder (instrs : List<RT.Instructions>) : Ply<RT.ExecutionResult> =
        uply {
          match instrs with
          | [] -> return Ok DUnit
          | [ last ] -> return! Exe.executeExpr state last
          | instr :: rest ->
            match! Exe.executeExpr state instr with
            | Error _ as failed -> return failed
            | Ok _ -> return! runInOrder rest
        }

      let! result = runInOrder exprInstrs
      do! tracer.storeTraceResults state
      return result
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


/// The script's (or expression's) own compiled fns, by hash: the root the
/// invoker explicitly asked to run (see [PolicyStore.guestState]).
let private ownFns (mod' : Utils.CliScript.PTCliScriptModule) : List<RT.Hash> =
  List.append mod'.fns mod'.submodules.fns
  |> List.map (fun (fn : PT.PackageFn.PackageFn) -> PT2RT.Hash.toRT fn.hash)


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
            "Run the script body under a deny-all policy instead of the configured instance policy"
          Param.make
            "warnPermissions"
            TBool
            "Warn-only permission mode: record caller-policy violations instead of denying them, while the instance policy remains mandatory"
          Param.make
            "sessionAllow"
            (TList(TCustomType(NR.ok (PolicyToDT.Rule.typeName ()), [])))
            ("Rules the interactive user allowed for this run only; they widen "
             + "the instance layer in memory and are never saved") ]
      returnType =
        // Ok = (exitCode, violations); a violation is (resource, unmet aspect, via — the
        // package fn / lambda the access happened in, when known). Empty list = stayed
        // inside the grant (always empty unless warnPermissions).
        let violationT = TTuple(TString, TString, [ TypeReference.option TString ])
        TypeReference.result
          (TTuple(TInt, TList violationT, []))
          (ExecutionError.typeRef ())
      description =
        "Parses Dark code as a script, and and executes it, returning an exit code "
        + "and (in warn-permissions mode) caller-policy violations"
      fn =
        let errType = KTCustomType(ExecutionError.fqTypeName (), [])
        let optStrVT =
          VT.known (KTCustomType(Dval.optionType (), [ VT.known KTString ]))
        let violationKT = KTTuple(VT.string, VT.string, [ optStrVT ])
        let okKT =
          KTTuple(VT.known KTInt, VT.known (KTList(VT.known violationKT)), [])
        let resultOk = Dval.resultOk okKT errType
        let resultError = Dval.resultError okKT errType
        (function
        | exeState,
          _,
          [],
          [| accountIDDval
             DUuid branchId
             DString filename
             DString code
             DList(_vtTODO, scriptArgs)
             DBool allowHarmful
             DBool sandbox
             DBool warnPermissions
             DList(_, sessionAllowDvals) |] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`. None passes through
            // (anonymous runs, tests).
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            let exeState = { exeState with accountID = accountID }
            // Use branch-specific state for parsing so name resolution uses the right branch.
            // Parsing keeps the host's access (`run` and `eval` alike): it is
            // host-side name resolution over the package store, done by the
            // trusted CLI on the user's behalf before any guest code runs, so it
            // must not depend on what the operator granted the guest. With a
            // deny-all instance policy (a deleted policy file) the script must
            // still parse and then have its body denied with an actionable
            // message, not fail opaquely at parse. Only the script *body* runs
            // under the guest state built below.
            let branchState = createBranchState exeState branchId allowHarmful
            let sessionAllow = sessionAllowDvals |> List.map PolicyToDT.Rule.fromDT
            // Denials raised under the guest state land here; a run that ends
            // on one is reported as `Denied` so the CLI can offer the rule.
            let denied = ResizeArray<RT.PermissionDenialRecord>()

            return!
              guestTry resultError (ExecutionError.classify denied) (fun () ->
                uply {
                  // No module for the script's own declarations. A module named
                  // after the file put the whole path into every name the
                  // runtime prints back (`CliScript.rundir/tmp/x.dark.Celsius`),
                  // and it buys nothing: one script runs per process, and two
                  // that declare the same thing share a hash anyway. The
                  // filename reaches traces via `RunScript`.
                  let! parsedScript =
                    parseGuest (parseCliScript branchState "CliScript" "" code) code

                  let! dbs = loadDBs ()

                  match parsedScript with
                  | Ok mod' ->
                    // `dark run` and `eval` start from the host's configured
                    // deny-by-default policy. Missing or corrupt state remains
                    // locked down (see PolicyStore.instancePolicy).
                    // `--warn-permissions` may relax the run and package policies, but the
                    // instance policy remains a hard maximum.
                    let permissionSink =
                      if warnPermissions then
                        Some(ResizeArray<RT.PermissionViolation>())
                      else
                        None
                    let runPolicy =
                      if sandbox then
                        LibExecution.Permissions.Policy.denyAll
                      else
                        LibExecution.Permissions.Policy.allowAll
                    // Kept for diagnostics: the call-stack printer runs Dark
                    // code (name lookups) that the guest policy may deny.
                    let hostState = exeState
                    let guest =
                      PolicyStore.guestState
                        accountID
                        runPolicy
                        sessionAllow
                        (ownFns mod')
                        exeState
                    let exeState =
                      { guest with
                          permissionWarnings = permissionSink
                          deniedRequests = denied }
                    let! execResult =
                      execute
                        exeState
                        branchId
                        mod'
                        scriptArgs
                        dbs
                        (RunScript(filename, code))
                    let! violations =
                      match permissionSink with
                      | None -> Ply []
                      | Some sink ->
                        sink
                        |> List.ofSeq
                        |> Ply.List.mapSequentially
                          (fun (v : RT.PermissionViolation) ->
                            uply {
                              let! via =
                                match v.via with
                                | Some ep ->
                                  uply {
                                    let! s = Exe.executionPointToString exeState ep
                                    return Dval.optionSome KTString (DString s)
                                  }
                                | None -> Ply(Dval.optionNone KTString)
                              return
                                DTuple(
                                  DString v.resource,
                                  DString v.needed,
                                  [ via ]
                                )
                            })
                    let ok (exitCode : Dval) =
                      resultOk (
                        DTuple(
                          exitCode,
                          DList(VT.known violationKT, violations),
                          []
                        )
                      )
                    match execResult with
                    | Ok(DInt i) -> return ok (DInt i)
                    | Ok(DInt64 i) -> return ok (Dval.int (bigint i))
                    | Ok DUnit -> return ok (Dval.int (bigint 0))
                    | Ok result ->
                      let rte =
                        RuntimeError.CLIs.NonIntReturned result |> RuntimeError.CLI
                      return
                        resultError (
                          ExecutionError.toDT (ExecutionError.Runtime rte)
                        )
                    | Error(e, callStack) ->
                      let! csString = Exe.callStackString hostState callStack
                      match ExecutionError.classify denied e with
                      | ExecutionError.Denied d ->
                        // The CLI may offer to allow and retry; the stack is printed
                        // only if the denial stands.
                        return
                          resultError (
                            ExecutionError.toDT (
                              ExecutionError.Denied { d with callStack = csString }
                            )
                          )
                      | other ->
                        print
                          $"Error when executing Script. Call-stack:\n{csString}\n"
                        return resultError (ExecutionError.toDT other)
                  | Error pe ->
                    return
                      resultError (ExecutionError.toDT (ExecutionError.Parse pe))
                })
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }


    { name = fn "cliEvaluateExpression" 0
      typeParams = []
      parameters =
        [ Param.make "accountID" (TypeReference.option TUuid) ""
          Param.make "branchId" TUuid ""
          Param.make "expression" TString ""
          Param.make
            "currentModule"
            (TList TString)
            "Where the caller is standing, owner-first; names in the result are spelled relative to it"
          Param.make "width" TInt "Columns the result should be laid out for"
          Param.make "color" TBool "Whether the result may carry terminal color"
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
          vm,
          [],
          [| accountIDDval
             DUuid branchId
             DString expression
             DList(_, currentModule)
             DInt width
             DBool color
             DBool allowHarmful |] ->
          uply {
            // Attribute the run to the calling account so the trace
            // insert can stamp `traces.account_id`.
            let accountID = C2DT.Option.fromDT D.uuid accountIDDval
            let exeState = { exeState with accountID = accountID }
            // Branch-specific state for parsing, under the host's access — see
            // the note in `cliParseAndExecuteScript`.
            let branchState = createBranchState exeState branchId allowHarmful
            let denied = ResizeArray<RT.PermissionDenialRecord>()

            return!
              guestTry resultError (ExecutionError.classify denied) (fun () ->
                uply {
                  // Parsing can raise (e.g. deep VM failures); keep it inside
                  // guestTry so its exceptions hit the Unhandled net. `eval` is
                  // single-expression only; parse failures surface a precise
                  // diagnostic (no fallback).
                  let! parsedScript =
                    parseGuest (parseCliExpr branchState expression) expression

                  let! dbs = loadDBs ()

                  match parsedScript with
                  | Ok mod' ->
                    // The expression runs under the host's configured
                    // deny-by-default policy; missing or corrupt policy state
                    // remains locked down.
                    let hostState = exeState
                    let exeState =
                      PolicyStore.guestState
                        accountID
                        LibExecution.Permissions.Policy.allowAll
                        []
                        (ownFns mod')
                        exeState
                    let exeState = { exeState with deniedRequests = denied }
                    match!
                      execute
                        exeState
                        branchId
                        mod'
                        []
                        dbs
                        (EvalExpression expression)
                    with
                    | Ok result ->
                      match result with
                      | DUnit -> return okNone ()
                      | DString s -> return okSome s
                      | _ ->
                        // Width and color are the caller's to decide: they are
                        // facts about the process this output is headed for, and
                        // asking here would mean reaching into another builtin's
                        // terminal code. `Cli.Terminal` owns both and answers
                        // them in Dark.
                        let currentModule =
                          currentModule
                          |> List.choose (fun d ->
                            match d with
                            | DString s -> Some s
                            | _ -> None)
                        let! asString =
                          Exe.dvalToReprForTerminal
                            exeState
                            (intToInt32 vm width)
                            color
                            currentModule
                            result
                        return okSome asString
                    | Error(e, callStack) ->
                      let! csString = Exe.callStackString hostState callStack
                      match ExecutionError.classify denied e with
                      | ExecutionError.Denied d ->
                        // The CLI may offer to allow and retry; the stack is printed
                        // only if the denial stands.
                        return
                          resultError (
                            ExecutionError.toDT (
                              ExecutionError.Denied { d with callStack = csString }
                            )
                          )
                      | other ->
                        print
                          $"Error when executing expression. Call-stack:\n{csString}\n"
                        return resultError (ExecutionError.toDT other)
                  | Error pe ->
                    return
                      resultError (ExecutionError.toDT (ExecutionError.Parse pe))
                })
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }


    ]


/// All builtins the outer CLI execution state needs: this module's own
/// fns (so nested `eval`/`run` dispatches recursively) plus every
/// `Builtins.*` library the CLI surface depends on.
///
/// Guest HTTP runs under `HostHttp`'s default guest configuration: SSRF
/// guards on (loopback / RFC1918 / metadata blocked). A host that must reach
/// private targets replaces it with `LibExecution.HostHttp.setGuestConfig`.
let builtinsToUse () : RT.Builtins =
  let ptPM = LibDB.PackageManager.pt
  LibExecution.Builtin.combine
    [ Builtins.Pure.Builtin.builtins ()
      Builtins.Http.Client.Builtin.builtins ()
      Builtins.Language.Builtin.builtins ()
      Builtins.Cli.Builtin.builtins ()
      Builtins.Time.Builtin.builtins ()
      Builtins.Random.Builtin.builtins ()
      Builtins.Matter.Builtin.builtins ptPM
      Builtins.Http.Server.Builtin.builtins ()
      LibExecution.Builtin.make [] (fns ()) ]
    []


let builtins () = LibExecution.Builtin.make [] (fns ())
