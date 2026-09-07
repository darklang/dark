/// Package-value evaluation is bounded by the policy.
///
/// A `val` body is code, and storing it is not permission to RUN it with the
/// host's authority. Evaluation used to build its execution state with
/// `Policy.allowAll`, so a body could perform effects the very same expression
/// was denied at `eval` -- reading files, spawning processes -- on two separate
/// paths: authoring (`scmAddOps`) and CLI startup (`Seed.growIfNeeded`, which
/// evaluates whatever arrived by import or sync).
module Tests.PermissionEscape

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Inserts = LibDB.Inserts
module Seed = LibDB.Seed
module Effect = LibExecution.Effects
module Permission = LibExecution.Permissions
module BS = LibSerialization.Binary.Serialization


// ── Helpers ──────────────────────────────────────────────────────────────

let private loc (name : string) : PT.PackageLocation =
  { owner = "Test"; modules = [ "PermissionEscape" ]; name = name }

let private makeValue (body : PT.Expr) : PT.PackageValue.PackageValue =
  let uniqueHash =
    System.Guid.NewGuid().ToString("N") + System.Guid.NewGuid().ToString("N")
    |> PT.Hash
  { hash = uniqueHash; body = body; description = "" }

/// `Builtin.timeNowMs ()`: the cheapest host effect that still needs a
/// permission, so the test asserts the check and not the cost of the effect.
let private clockCall : PT.Expr = eApply (eBuiltinFn "timeNowMs" 0) [] [ eUnit () ]

/// Store a value as WIP so its row exists with `rt_dval` NULL -- exactly the
/// state `scmAddOps` and a synced-in op leave behind for evaluation to find.
let private storeUnevaluated (name : string) (body : PT.Expr) : Task<PT.Hash> =
  task {
    let value = makeValue body
    let! (_ : int64) =
      Inserts.insertAndApplyOpsAsWip
        PT.mainBranchId
        [ PT.PackageOp.AddValue value
          PT.PackageOp.SetName(loc name, PT.PackageValue value.hash) ]
    return value.hash
  }

let private isUnevaluated (PT.Hash hash) : Task<bool> =
  Sql.query "SELECT rt_dval IS NULL AS n FROM package_values WHERE hash = @hash"
  |> Sql.parameters [ "hash", Sql.string hash ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")
  |> Task.map (fun n -> n = 1L)

let private evaluateUnder
  (authority : Seed.EvaluationAuthority)
  : Task<Result<unit, List<Seed.ValueEvaluationError>>> =
  Seed.evaluateAllValues authority PT.mainBranchId (localBuiltIns pmPT) pmRT

/// Did evaluation report a failure for THIS value? Evaluation sweeps every
/// pending value in the store, so an unrelated one must not decide the test.
let private failedFor
  (hash : PT.Hash)
  (result : Result<unit, List<Seed.ValueEvaluationError>>)
  : bool =
  match result with
  | Ok() -> false
  | Error errors -> errors |> List.exists (fun e -> e.hash = Some hash)


// ── Tests ────────────────────────────────────────────────────────────────

let testDeniedEffectIsRefused =
  testTask "value evaluation: a body whose effect the policy denies is refused" {
    let! hash = storeUnevaluated "denied" clockCall

    let! result =
      evaluateUnder (
        Seed.Guest(fun state ->
          Task.FromResult
            { state with
                access = Permission.Access.start Permission.Policy.denyAll })
      )

    Expect.isTrue
      (failedFor hash result)
      "evaluating a denied effect must report a failure for that value"

    let! stillNull = isUnevaluated hash
    Expect.isTrue
      stillNull
      "a refused value stays unevaluated; it must not be stored as though it ran"
  }

let testAllowedEffectIsEvaluated =
  testTask "value evaluation: the same body evaluates when the policy allows it" {
    // The control for the test above: it pins that the refusal came from the
    // policy, not from a body that could never have evaluated anyway.
    let! hash = storeUnevaluated "allowed" clockCall

    let! result = evaluateUnder Seed.TrustedSeed

    Expect.isFalse
      (failedFor hash result)
      "the same body must evaluate when the authority allows the effect"

    let! stillNull = isUnevaluated hash
    Expect.isFalse stillNull "an evaluated value is stored"
  }

let testCallerAllowAllCannotWidenInstanceDenial =
  testTask "value evaluation: an allow-all caller cannot widen an instance denial" {
    // The escape as it actually presented. CLI control code runs allow-all on
    // purpose, so it can manage packages and the policy itself -- which means
    // inheriting the CALLER's access is not enough to bound `val`. What bounds
    // it is re-applying the instance policy on top, which is what
    // `EvaluationAuthority.underInstancePolicyAnd` does. Composed here by hand
    // rather than through that helper, because the helper reads the real
    // ~/.darklang/policy and a test must not depend on the developer's own.
    let! hash = storeUnevaluated "callerAllowAll" clockCall

    let callerAccess = Permission.Access.start Permission.Policy.allowAll
    let bounded =
      callerAccess
      |> Permission.Access.restrict
        Permission.Layer.Instance
        Permission.Policy.denyAll

    let! result =
      evaluateUnder (
        Seed.Guest(fun state -> Task.FromResult { state with access = bounded })
      )

    Expect.isTrue
      (failedFor hash result)
      "the instance policy is a hard maximum; an allow-all caller cannot lift it"

    let! stillNull = isUnevaluated hash
    Expect.isTrue stillNull "the value stays unevaluated"
  }


/// The owners that actually ship: one directory per owner under `packages/`,
/// read from disk so the list cannot drift from the tree.
///
/// The sweep below is scoped to these because the store it reads is shared with
/// every other test in the run, and test fixtures (`Test`, `Tests`, `SyncTest`,
/// `MyOrg`, and the three values THIS module inserts) are values nobody ships.
/// Sweeping them too made this test pass or fail on Expecto's scheduling.
let private shippedOwners : Set<string> =
  let rec walk (dir : string) : Option<string> =
    if System.String.IsNullOrEmpty dir then
      None
    elif
      System.IO.Directory.Exists(System.IO.Path.Combine(dir, "packages", "darklang"))
    then
      Some dir
    else
      walk (System.IO.Path.GetDirectoryName dir)

  match walk (System.IO.Directory.GetCurrentDirectory()) with
  | None ->
    Exception.raiseInternal
      "Couldn't find packages/ walking up from CWD"
      [ "cwd", System.IO.Directory.GetCurrentDirectory() ]
  | Some root ->
    System.IO.Directory.GetDirectories(System.IO.Path.Combine(root, "packages"))
    |> Array.map (fun dir -> (System.IO.DirectoryInfo dir).Name.ToLower())
    |> Set.ofArray

/// The guest state a pending value runs under, as production builds it, with
/// the instance layer replaced by allow-all so the assertions below are about
/// the PACKAGE layer regardless of this machine's policy file.
let private productionGuestWithOpenInstance
  (state : RT.ExecutionState)
  : Task<RT.ExecutionState> =
  task {
    let configure =
      match Seed.EvaluationAuthority.underInstancePolicy with
      | Seed.Guest configure -> configure
      | Seed.TrustedSeed -> failtest "expected the guest authority"
    let! configured = configure state
    return
      { configured with access = Permission.Access.start Permission.Policy.allowAll }
  }

let testGuestAuthorityInstallsApprovals =
  testTask "value evaluation: the guest authority installs package approvals" {
    // A state built by hand kept `createState`'s allow-all package lookup.
    let! (initial : RT.ExecutionState) =
      executionStateFor TestValues.pm false Map.empty
    let! (configured : RT.ExecutionState) = productionGuestWithOpenInstance initial
    Expect.isFalse
      (Permission.Policy.isAllowAll (
        configured.packagePolicy (RT.Hash "no-such-package")
      ))
      "an unknown, non-bundled package is denied until approved"
    let! (found : Option<PT.Hash>) =
      LibDB.ProgramTypes.Fn.find
        [ PT.mainBranchId ]
        { owner = "Darklang"; modules = [ "Stdlib"; "List" ]; name = "map" }
      |> Ply.toTask
    match found with
    | Some(PT.Hash h) ->
      Expect.isTrue
        (Permission.Policy.isAllowAll (configured.packagePolicy (RT.Hash h)))
        "a bundled function keeps its exemption"
    | None -> failtest "Stdlib.List.map not found"
  }

let testUnapprovedPackageFnInValueBodyIsRefused =
  testTask
    "value evaluation: a body calling an unapproved package fn is refused by the package layer" {
    // The end-to-end case: a Test-owned (non-bundled, unapproved) fn that
    // reads the clock, called from a pending value. Under an allow-all
    // instance the only layer that can deny is the consumer's approval.
    let clockHash =
      System.Guid.NewGuid().ToString("N") + System.Guid.NewGuid().ToString("N")
    let clockFn : PT.PackageFn.PackageFn =
      { hash = PT.Hash clockHash
        typeParams = []
        parameters =
          NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
        returnType = PT.TInt
        body = clockCall
        description = ""
        permissionCeiling = None }
    let! (_ : int64) =
      Inserts.insertAndApplyOpsAsWip
        PT.mainBranchId
        [ PT.PackageOp.AddFn clockFn
          PT.PackageOp.SetName(loc "unapprovedClock", PT.PackageFn clockFn.hash) ]
    let! hash =
      storeUnevaluated
        "callsUnapproved"
        (eApply (ePackageFn clockHash) [] [ eUnit () ])
    let! result = evaluateUnder (Seed.Guest productionGuestWithOpenInstance)
    Expect.isTrue (failedFor hash result) "the value's evaluation was refused"
    match result with
    | Error errors ->
      match
        errors
        |> List.tryFind (fun (e : Seed.ValueEvaluationError) -> e.hash = Some hash)
      with
      | Some own ->
        Expect.stringContains
          own.message
          "package policy"
          "refused by the consumer's approval layer"
      | None -> failtest "no failure recorded for the value"
    | Ok() -> failtest "expected a failure"
    let! stillNull = isUnevaluated hash
    Expect.isTrue stillNull "the value stays unevaluated"
  }

/// Permit package inspection during construction, but not mutation. Persisting
/// the evaluated result is host work outside the guest body and does not need
/// `PackageWrite`.
let private packageReadsOnly : Permission.Policy =
  Permission.Policy.create [ Permission.Rule.Effect Effect.Effect.PackageRead ] []

let testStoredValuesNeedNoHostEffects =
  testTask "package values: none performs a host effect when evaluated" {
    // Run each shipped value under the restricted policy. Static reachability
    // is intentionally unsuitable here: a value may hold an effectful callback
    // without executing it during construction. Environment-dependent work
    // belongs in a function so it is not frozen into the package seed.
    let notify _ _ _ _ = uply { return () }
    let sendException _ _ _ _ = uply { return () }

    let exeState =
      { LibExecution.Execution.createState
          (localBuiltIns pmPT)
          pmRT
          LibExecution.Execution.noTracing
          sendException
          notify
          PT.mainBranchId
          { dbs = Map.empty } with
          access = Permission.Access.start packageReadsOnly }

    let! stored =
      Sql.query
        """
        SELECT pv.hash, pv.pt_def, l.owner, l.modules, l.name
        FROM package_values pv
        JOIN locations l ON l.item_hash = pv.hash AND l.unlisted_at IS NULL
        """
      |> Sql.executeAsync (fun read ->
        let owner = read.string "owner"
        let modules = read.string "modules"
        let name = read.string "name"
        owner,
        $"{owner}.{modules}.{name}",
        PT.Hash(read.string "hash"),
        read.bytes "pt_def")
      |> Task.map (
        List.filter (fun (owner, _, _, _) ->
          Set.contains (owner.ToLower()) shippedOwners)
      )

    let offenders = ResizeArray<string>()

    for (_owner, named, hash, ptDef) in stored do
      let value = BS.PT.PackageValue.deserialize hash ptDef
      let instrs =
        LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT Map.empty 0 None value.body
      match! LibExecution.Execution.executeExpr exeState instrs with
      | Ok _ -> ()
      | Error(rte, _) ->
        let! rendered = LibExecution.Execution.runtimeErrorToString exeState rte
        let message =
          match rendered with
          | Ok(RT.DString message) -> message
          | Ok other -> string other
          | Error _ -> "(error could not be stringified)"
        offenders.Add $"  {named}: {message}"

    Expect.isEmpty
      offenders
      ("a package value must evaluate with no host effects, or it breaks on a "
       + "fresh install and freezes the build machine's answer into the store. "
       + "Make it a function instead:\n"
       + String.concat "\n" offenders)
  }


// testSequenced because these share one store. `testAllowedEffectIsEvaluated`
// evaluates under `TrustedSeed`, and evaluation sweeps EVERY pending value in
// the store -- including the ones the other two tests just inserted and are
// about to assert are still unevaluated. Run in parallel, it evaluates them out
// from under those assertions.
let tests =
  testSequenced
  <| testList
    "PermissionEscape"
    [ testDeniedEffectIsRefused
      testAllowedEffectIsEvaluated
      testCallerAllowAllCannotWidenInstanceDenial
      testGuestAuthorityInstallsApprovals
      testUnapprovedPackageFnInValueBodyIsRefused
      testStoredValuesNeedNoHostEffects ]
