/// Browser (WASM) host for the Darklang REPL.
///
/// The parser and runtime are the real ones (LibParser + LibExecution). This
/// file is the thin shell: it boots an execution state and exposes `Eval` (per
/// REPL line) plus `LoadPackagesFromUrl` (called once at startup to load the
/// package snapshot into an in-memory package manager, so `Stdlib.*` resolves).
///
/// The REPL is stateful across entries, in two ways:
/// - fn/type/`val` declarations become in-memory package items under the
///   `Repl` owner, so later entries call them unqualified
/// - a bare trailing `let` (no body) persists its bindings as session
///   variables, injected into later entries as pre-loaded VM registers
module Darklang.Wasm.Repl

open System.Threading.Tasks
open Microsoft.JSInterop

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module P = LibParser.Parser
module WT = LibParser.WrittenTypes
module WTSourceFile = LibParser.SourceFile
module WT2PT = LibParser.WrittenTypesToProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module NR = LibParser.NameResolver
module Exe = LibExecution.Execution
module BS = LibSerialization.Binary.Serialization
module HS = LibDB.HashStabilization
module PackageLocation = LibDB.PackageLocation

/// REPL declarations live under this owner; also the currentModule for
/// evaluation, so they resolve unqualified.
let private replOwner = "Repl"

// Starts empty; replaced with the loaded snapshot once LoadPackagesFromUrl runs.
let mutable private pm : PT.PackageManager = PT.PackageManager.empty
let mutable private stateOpt : RT.ExecutionState option = None
/// Session variables from bare `let` entries, in binding order.
let mutable private sessionVars : List<string * RT.Dval> = []

let private builtins : RT.Builtins =
  LibExecution.Builtin.combine
    [ Builtins.Pure.Builtin.builtins ()
      Builtins.Http.Client.Builtin.builtins
        Builtins.Http.Client.Libs.HttpClient.defaultConfig
      Output.builtins ()
      // live getter: lookups see snapshot + REPL-declared items
      PmLookup.builtins (fun () -> pm) ]
    []

let private buildState () : RT.ExecutionState =
  let pmRT = PT2RT.PackageManager.toRT builtins.values pm
  let program : RT.Program = { dbs = Map.empty }
  Exe.createState
    builtins
    pmRT
    Exe.noTracing
    RT.consoleReporter
    RT.consoleNotifier
    PT.mainBranchId
    program

let private getState () : RT.ExecutionState =
  match stateOpt with
  | Some s -> s
  | None ->
    let s = buildState ()
    stateOpt <- Some s
    s

/// Load the package snapshot (all PackageOps, framed as
/// [36-byte ascii uuid][4-byte LE length][op blob]) into an in-memory PM.
[<JSInvokable>]
let LoadPackagesFromUrl (snapshotUrl : string) : Task =
  task {
    use http = new System.Net.Http.HttpClient()
    let! bytes = http.GetByteArrayAsync(snapshotUrl)
    let ops = ResizeArray<PT.PackageOp>()
    let mutable i = 0
    while i < bytes.Length do
      if bytes.Length - i < 40 then
        invalidArg
          (nameof snapshotUrl)
          "package snapshot has a truncated frame header"

      let idStr = System.Text.Encoding.ASCII.GetString(bytes, i, 36)
      let len = System.BitConverter.ToInt32(bytes, i + 36)
      if len < 0 || bytes.Length - i - 40 < len then
        invalidArg (nameof snapshotUrl) "package snapshot has a truncated frame body"

      let blob = Array.sub bytes (i + 40) len
      ops.Add(BS.PT.PackageOp.deserialize (System.Guid.Parse idStr) blob)
      i <- i + 40 + len

    // CLEANUP: replace this snapshot seam with a hosted PT/RT PackageManager.
    // Resolve names against a versioned branch index, fetch immutable package
    // items by hash, and cache them in the browser. Until that service exists,
    // the generated snapshot keeps the browser runtime self-contained.
    pm <- LibDB.PackageManager.createInMemory (List.ofSeq ops)
    stateOpt <- Some(buildState ()) // rebuild the state with the loaded PM
  }
  :> Task

// ---------- declarations: REPL entries become Repl-owned package items ----------

/// Mirror of LibParser.Package's placeholder for Set*Name ops; the real hash
/// replaces it in HashStabilization.computeRealHashes.
let private nameBasedHash (loc : PT.PackageLocation) : PT.Hash =
  let nameKey = PackageLocation.toFQN loc
  let nameBytes =
    System.Security.Cryptography.SHA256.HashData(
      System.Text.Encoding.UTF8.GetBytes(nameKey)
    )
  PT.Hash(
    System.BitConverter.ToString(nameBytes).Replace("-", "").ToLowerInvariant()
  )

type private Classified =
  { fns : List<WT.PackageFn.PackageFn>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue>
    exprs : List<WT.Expr>
    errors : List<string> }

/// Split a parsed entry into Repl-owned declarations and expressions to
/// evaluate. `module M`-nested declarations become Repl.M.* items.
let private classify (sf : WT.SourceFile) : Classified =
  WTSourceFile.items sf
  |> List.fold
    (fun acc item ->
      match item with
      | WTSourceFile.Fn(path, fn) ->
        { acc with fns = acc.fns @ [ WT.packageFn replOwner path fn ] }
      | WTSourceFile.Type(path, t) ->
        { acc with types = acc.types @ [ WT.packageType replOwner path t ] }
      | WTSourceFile.Value(path, v) ->
        { acc with values = acc.values @ [ WT.packageValue replOwner path v ] }
      | WTSourceFile.Expr(_, e) -> { acc with exprs = acc.exprs @ [ e ] }
      | WTSourceFile.TypeDB _ ->
        { acc with
            errors =
              acc.errors @ [ "[<DB>] declarations are not supported in the REPL" ] }
      | WTSourceFile.Test _ ->
        { acc with
            errors =
              acc.errors @ [ "test assertions are not supported in the REPL" ] })
    { fns = []; types = []; values = []; exprs = []; errors = [] }

let private lowerOnce
  (pmX : PT.PackageManager)
  (c : Classified)
  : Ply<List<PT.PackageOp>> =
  uply {
    let! fns =
      c.fns
      |> Ply.List.mapSequentially (fun fn ->
        WT2PT.PackageFn.toPT
          builtins
          pmX
          NR.OnMissing.Allow
          PT.mainBranchId
          (WT2PT.PackageFn.Name.toModules fn.name)
          fn)
    let! types =
      c.types
      |> Ply.List.mapSequentially (fun typ ->
        WT2PT.PackageType.toPT
          pmX
          NR.OnMissing.Allow
          PT.mainBranchId
          (WT2PT.PackageType.Name.toModules typ.name)
          typ)
    let! values =
      c.values
      |> Ply.List.mapSequentially (fun value ->
        WT2PT.PackageValue.toPT
          builtins
          pmX
          NR.OnMissing.Allow
          PT.mainBranchId
          (WT2PT.PackageValue.Name.toModules value.name)
          value)
    return
      [ for (wtType, ptType) in List.zip c.types types do
          yield PT.PackageOp.AddType ptType
          let loc = WT2PT.PackageType.Name.toLocation wtType.name
          yield PT.PackageOp.SetName(loc, PT.PackageType(nameBasedHash loc))

        for (wtValue, ptValue) in List.zip c.values values do
          yield PT.PackageOp.AddValue ptValue
          let loc = WT2PT.PackageValue.Name.toLocation wtValue.name
          yield PT.PackageOp.SetName(loc, PT.PackageValue(nameBasedHash loc))

        for (wtFn, ptFn) in List.zip c.fns fns do
          yield PT.PackageOp.AddFn ptFn
          let loc = WT2PT.PackageFn.Name.toLocation wtFn.name
          yield PT.PackageOp.SetName(loc, PT.PackageFn(nameBasedHash loc)) ]
  }

/// Two lowering passes, like the package loader's convergence loop: pass 1
/// leaves intra-entry references unresolved, pass 2 re-lowers against a PM
/// holding pass 1's hashes so they resolve.
let private declsToOps (c : Classified) : Ply<List<PT.PackageOp>> =
  uply {
    let! raw1 = lowerOnce pm c
    let ops1 = HS.computeRealHashes raw1
    let pm1 = LibDB.PackageManager.withExtraOps pm ops1
    let! raw2 = lowerOnce pm1 c
    let remapped = HS.remapSetNames raw2 ops1
    return HS.computeRealHashes remapped
  }

// ---------- session variables: bare `let` entries persist their bindings ----------

let rec private letPatternNames (p : WT.LetPattern) : List<string> =
  match p with
  | WT.LPVariable(_, name) -> if name = "_" then [] else [ name ]
  | WT.LPUnit _
  | WT.LPWildcard _ -> []
  | WT.LPTuple(_, first, _, second, rest, _, _) ->
    letPatternNames first
    @ letPatternNames second
    @ (rest |> List.collect (snd >> letPatternNames))

/// Names bound by a `let` spine whose innermost body is the synthetic `()`
/// the caller appended. None if the expr isn't such a spine.
let rec private letSpineNames (e : WT.Expr) : Option<List<string>> =
  match e with
  | WT.ELet(_, pat, _, body, _, _) ->
    match body with
    | WT.EUnit _ -> Some(letPatternNames pat)
    | _ -> letSpineNames body |> Option.map (fun rest -> letPatternNames pat @ rest)
  | _ -> None

/// A bare-let entry (`let x = 5`) isn't a complete expression (no body). If
/// appending `()` makes it parse as a let spine, rewrite the entry to end with
/// the bound names instead, so evaluating yields the values to persist.
let private rewriteBareLet (source : string) : Option<string * List<string>> =
  let probe = P.parse (source + "\n()")
  match probe.parsed, probe.diagnostics with
  | Some(WT.SourceFile sf), [] ->
    match List.rev sf.exprsToEval with
    | last :: _ ->
      letSpineNames last
      |> Option.map (fun names ->
        let capture =
          match names with
          | [] -> "()"
          | [ n ] -> n
          | ns -> "(" + String.concat ", " ns + ")"
        (source + "\n" + capture, names))
    | [] -> None
  | _ -> None

/// Persist captured bindings. The capture expr was the tuple/var of the bound
/// names, so the result Dval unpacks positionally. Rebinding replaces.
let private storeBindings (names : List<string>) (dval : RT.Dval) : unit =
  let bound =
    match names with
    | [] -> []
    | [ n ] -> [ (n, dval) ]
    | _ ->
      match dval with
      | RT.DTuple(first, second, rest) -> List.zip names (first :: second :: rest)
      | _ -> []
  for (n, v) in bound do
    sessionVars <-
      (sessionVars |> List.filter (fun (existing, _) -> existing <> n)) @ [ (n, v) ]

// ---------- eval ----------

[<CLIMutable>]
type EvalResult = { result : string; error : bool; output : string }

let private success (result : string) : EvalResult =
  { result = result; error = false; output = Output.drain () }

let private failure (result : string) : EvalResult =
  { result = result; error = true; output = Output.drain () }

/// A side-effect-free dispatcher readiness probe for browser startup.
[<JSInvokable>]
let Ready () : bool = true

/// Render a runtime error via the Dark error pretty-printer — same message as
/// `dark eval`. Falls back to a debug repr if the pretty-printer itself fails.
let private renderError
  (s : RT.ExecutionState)
  (rte : RT.RuntimeError.Error)
  : Task<string> =
  task {
    match! Exe.runtimeErrorToString s rte with
    | Ok(RT.DString msg) -> return msg
    | _ -> return $"Runtime error: %A{rte}"
  }

/// Lower + execute one expression with session variables pre-loaded into the
/// VM's low registers (symbols maps each name to its register).
let private evalOne
  (s : RT.ExecutionState)
  (e : WT.Expr)
  : Task<RT.ExecutionResult> =
  task {
    let ctx : WT2PT.Context =
      { currentFnName = None
        argMap = Map.empty
        localBindings = Set.ofList (List.map fst sessionVars) }
    let! pt =
      WT2PT.Expr.toPT
        builtins
        pm
        NR.OnMissing.Allow
        PT.mainBranchId
        [ replOwner ]
        ctx
        e
      |> Ply.toTask
    let symbols =
      sessionVars |> List.mapi (fun i (name, _) -> (name, i)) |> Map.ofList
    let instrs = PT2RT.Expr.toRT symbols (List.length sessionVars) None pt
    let loads = sessionVars |> List.mapi (fun i (_, dv) -> RT.LoadVal(i, dv))
    let instrs = { instrs with instructions = loads @ instrs.instructions }
    return! Exe.executeExpr s instrs
  }

/// Parse → install declarations → execute expressions → capture bindings.
/// Called from JS per REPL entry.
[<JSInvokable>]
let Eval (source : string) : Task<EvalResult> =
  task {
    try
      // A bare trailing `let` isn't a complete expression; rewrite it to
      // return its bound values and remember the names to persist.
      let (source, capture) =
        match (P.parse source).diagnostics with
        | [] -> (source, [])
        | _ ->
          match rewriteBareLet source with
          | Some(rewritten, names) -> (rewritten, names)
          | None -> (source, [])

      let r = P.parse source
      match r.parsed, r.diagnostics with
      | Some(WT.SourceFile sf), [] ->
        let c = classify sf
        match c.errors with
        | e :: _ -> return failure e
        | [] ->
          let declNames =
            (c.types |> List.map _.name.name)
            @ (c.values |> List.map _.name.name)
            @ (c.fns |> List.map _.name.name)
          if not (List.isEmpty declNames) then
            let! ops = declsToOps c |> Ply.toTask
            pm <- LibDB.PackageManager.withExtraOps pm ops
            stateOpt <- Some(buildState ())

          let s = getState ()
          let results = ResizeArray<string>()
          let mutable lastDval : Option<RT.Dval> = None
          let mutable evalError : Option<RT.RuntimeError.Error> = None
          for e in c.exprs do
            if evalError.IsNone then
              match! evalOne s e with
              | Ok dval ->
                lastDval <- Some dval
                let! repr = Exe.dvalToRepr s dval
                results.Add repr
              | Error(rte, _callStack) -> evalError <- Some rte

          match evalError with
          | Some rte ->
            let! msg = renderError s rte
            return failure msg
          | None ->
            if not (List.isEmpty capture) then
              lastDval |> Option.iter (storeBindings capture)
            let header =
              if List.isEmpty declNames then
                []
              else
                [ "defined " + String.concat ", " declNames ]
            let text = String.concat "\n" (header @ List.ofSeq results)
            return success text
      | _, [] -> return failure "(nothing to evaluate)"
      | _, diags ->
        return
          diags
          |> List.map (P.renderDiagnostic source)
          |> String.concat "\n"
          |> failure
    with
    // Parsing/lowering can raise (e.g. deep VM failures) — the CLI's eval
    // builtin guards the same pipeline the same way. Render instead of letting
    // the exception reject the JS interop promise.
    | RT.RuntimeErrorException(_, rte) ->
      let! msg = renderError (getState ()) rte
      return failure msg
    | e -> return failure $"Internal error: {e.Message}"
  }
