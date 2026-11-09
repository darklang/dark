/// Trusted policy administration and package-effect inspection.
module Builtins.Matter.Libs.PM.Permissions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module Dval = LibExecution.Dval
module D = LibExecution.DvalDecoder
module VT = LibExecution.ValueType
module Builtin = LibExecution.Builtin
module PolicyToDT = LibExecution.PermissionsToDarkTypes
module EffectsToDT = LibExecution.EffectsToDarkTypes
module CommonToDT = LibExecution.CommonToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution
module PackagePermissions = LibDB.PackagePermissions
module PolicyStore = LibDB.PolicyStore

open Builtin.Shortcuts


let private policyAdminError () =
  RuntimeError.UncaughtException(
    "permission denied: policy changes require the trusted `dark permissions` command",
    []
  )
  |> raiseUntargetedRTE

let private accountIDOf (dval : Dval) : Option<System.Guid> =
  CommonToDT.Option.fromDT D.uuid dval

/// The running host's approval-analysis metadata, built once per builtin table:
/// `callEffectsFor` answers analysis by full builtin identity (name, version)
/// so different-effect versions never collapse. `fingerprint` includes those
/// effects and the analyzer version, so either kind of semantic change makes
/// existing approvals stale. Deterministic across runs of the same binary.
type private BuiltinEffects =
  { callEffectsFor : PackagePermissions.CallEffectsFor; fingerprint : string }

/// A table never changes once made. Building this on every call, which sorts and hashes every
/// builtin, was a seventh of `search`, which asks once for every function it lists.
let private builtinEffectsCache =
  System.Runtime.CompilerServices.ConditionalWeakTable<Dictionary<FQFnName.Builtin, BuiltInFn>, BuiltinEffects>()

let private builtinEffects (state : ExecutionState) : BuiltinEffects =
  builtinEffectsCache.GetValue(
    state.fns.builtIn,
    fun builtIn ->
      let sorted =
        builtIn
        |> Seq.map (fun (KeyValue(k, b)) -> k, b)
        |> List.ofSeq
        |> List.sortBy fst
      let byIdentity =
        sorted
        |> List.map (fun (k, b) -> (k.name, k.version), b.callEffects)
        |> Map.ofList
      let fingerprint = PolicyStore.builtinFingerprint builtIn
      { callEffectsFor = (fun key -> Map.tryFind key byIdentity)
        fingerprint = fingerprint }
  )

/// A policy builtin: impure, `Native`, never queryable. `impl` receives the
/// raw argument array and answers `incorrectArgs` itself for a bad shape.
let private policyFn
  (name : string)
  (parameters : List<BuiltInParam>)
  (returnType : TypeReference)
  (description : string)
  (impl : ExecutionState -> Dval[] -> Ply<Dval>)
  : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters = parameters
    returnType = returnType
    description = description
    fn =
      (function
      | state, _, _, args -> impl state args)
    previewable = Impure
    callEffects = set [ Effect.Native ]
    deprecated = NotDeprecated }

/// A [policyFn] only the trusted `dark permissions` command may call: guest
/// code (a `run`/`eval` state, `canManagePolicies = false`) is refused before
/// the arguments are looked at.
let private hostOnly
  (name : string)
  (parameters : List<BuiltInParam>)
  (returnType : TypeReference)
  (description : string)
  (impl : ExecutionState -> Dval[] -> Ply<Dval>)
  : BuiltInFn =
  policyFn name parameters returnType description (fun state args ->
    if not state.canManagePolicies then policyAdminError ()
    impl state args)

let private locationParam = Param.make "location" TString "Logical function name"

let fns () : List<BuiltInFn> =
  let policyType = TCustomType(NR.ok (PolicyToDT.Policy.typeName ()), [])
  let accountParam = Param.make "accountID" (TypeReference.option TUuid) ""

  [ policyFn
      "pmPolicyGetInstance"
      [ Param.make "unit" TUnit "" ]
      policyType
      ("Return the host-owned instance permission policy. Missing, malformed, "
       + "or unreadable policy state is represented as deny-all.")
      (fun _ args ->
        match args with
        | [| DUnit |] ->
          uply { return PolicyStore.instancePolicy () |> PolicyToDT.Policy.toDT }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyEditInstance"
      [ Param.make
          "edit"
          (TCustomType(NR.ok (PolicyToDT.PolicyEdit.typeName ()), []))
          "Replace the whole policy, or add and remove rules" ]
      TUnit
      ("Atomically change the host-owned instance permission policy: replace it "
       + "whole, or add allow and deny rules and remove rules (removals apply first). "
       + "Host-only.")
      (fun _ args ->
        match args with
        | [| edit |] ->
          uply {
            match PolicyToDT.PolicyEdit.fromDT edit with
            | PolicyToDT.PolicyEdit.Replace policy ->
              PolicyStore.setInstancePolicy policy
            | PolicyToDT.PolicyEdit.Change(allow, deny, remove) ->
              PolicyStore.editInstancePolicy allow deny remove
            return DUnit
          }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyListPackages"
      [ accountParam ]
      (TList TString)
      "List immutable package-function hashes with a locally stored consumer approval."
      (fun _ args ->
        match args with
        | [| accountIDDval |] ->
          uply {
            let hashes =
              PolicyStore.packagePolicies (accountIDOf accountIDDval)
              |> Map.toList
              |> List.map (fst >> DString)
            return DList(VT.string, hashes)
          }
        | _ -> incorrectArgs ())

    // Read-only policy metadata. This remains a Native effect, but does not
    // require the command-scoped authority reserved for policy mutations;
    // the Workbench needs it while inspecting a function. It reveals only what
    // guest code could already read: the names approved and the hashes they
    // point at, and, for a hash the caller names, its stored and explicit
    // policy. The full list of approved hashes stays host-only
    // (pmPolicyListPackages).
    policyFn
      "pmPolicyApprovals"
      [ accountParam
        Param.make
          "hash"
          (TypeReference.option TString)
          ("One immutable package-function hash to look up, or None for every "
           + "name with an approved version") ]
      (TList(TCustomType(NR.ok (PolicyToDT.Approval.typeName ()), [])))
      ("Approvals as this account sees them, one per (hash, name): the hash, the "
       + "logical name approved at it, its stored consumer policy, and the rules "
       + "written when it was approved as a root. With a hash, the entries for "
       + "that hash (one with no name when no name points at it); with None, one "
       + "entry per approved name, in name order. Hashes are host metadata, not "
       + "user-facing. Read-only.")
      (fun _ args ->
        match args with
        | [| accountIDDval; hashDval |] ->
          uply {
            let accountID = accountIDOf accountIDDval
            let policies = PolicyStore.packagePolicies accountID
            let entry (hash : string) (name : Option<string>) : Dval =
              PolicyToDT.Approval.toDT
                hash
                name
                (Map.tryFind hash policies)
                (PolicyStore.explicitApproval accountID hash)
            let named = PolicyStore.approvedVersions accountID |> Map.toList
            let entries =
              match CommonToDT.Option.fromDT D.string hashDval with
              | None ->
                named |> List.map (fun (name, hash) -> entry hash (Some name))
              | Some hash ->
                match
                  named |> List.filter (fun (_, approved) -> approved = hash)
                with
                | [] -> [ entry hash None ]
                | names ->
                  names |> List.map (fun (name, _) -> entry hash (Some name))
            return DList(VT.known (PolicyToDT.Approval.knownType ()), entries)
          }
        | _ -> incorrectArgs ())

    // Read-only; the Workbench uses it to explain effective access.
    policyFn
      "pmFnIsBundled"
      [ Param.make "hash" TString "The immutable package-function hash" ]
      TBool
      "Whether a package function is bundled first-party code. Read-only."
      (fun state args ->
        match args with
        | [| DString hash |] ->
          uply { return DBool(state.isBundledPackageFn (Hash hash)) }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyWithdraw"
      [ accountParam
        Param.make
          "target"
          (TCustomType(NR.ok (PolicyToDT.ApprovalTarget.typeName ()), []))
          "A name, to withdraw its approved version; or a hash, to revoke its approval" ]
      TUnit
      ("Take an approval back. By name: withdraw the version approved for that "
       + "logical function name; the hash's approval stays. By hash: remove the "
       + "local consumer approval of that package-function hash, and the "
       + "dependencies no other approved root still needs. Host-only.")
      (fun _ args ->
        match args with
        | [| accountIDDval; target |] ->
          uply {
            let accountID = accountIDOf accountIDDval
            match PolicyToDT.ApprovalTarget.fromDT target with
            | PolicyToDT.ApprovalTarget.Name location ->
              PolicyStore.unapproveVersion accountID location
            | PolicyToDT.ApprovalTarget.Hash hash ->
              // Dependency-aware revoke: drop only the policies no other approved
              // root still needs (see PolicyStore.revokePackageRoot).
              PolicyStore.revokePackageRoot accountID hash
            return DUnit
          }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyApprovalsStale"
      [ Param.make "unit" TUnit "" ]
      TBool
      "True when existing package approvals were recorded under different approval-analysis semantics than the running host's — a re-review signal (per-root, so re-approving one root does not clear it for others). A complete approval's stored allowlist still denies a newly-appeared effect; an acknowledged-incomplete approval is allow-all and bounded only by the instance/run policies, so for those the signal is the safeguard, not the package layer. Host-only."
      (fun state args ->
        match args with
        | [| DUnit |] ->
          uply {
            let fingerprint = (builtinEffects state).fingerprint
            return DBool(PolicyStore.approvalsAreStale fingerprint)
          }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyGrantsWithoutAPerson"
      [ Param.make "unit" TUnit "" ]
      TBool
      ("True when this process was started with DARK_GRANTS_WITHOUT_A_PERSON=1, the escape that "
       + "lets a scripted setup widen permissions with nobody at a terminal. Read once at "
       + "startup. Host-only.")
      (fun _ args ->
        match args with
        | [| DUnit |] ->
          uply { return DBool(GrantEscape.grantsWithoutAPerson.Force()) }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyApproveVersion"
      [ accountParam
        locationParam
        Param.make "hash" TString "Approved content hash"
        Param.make
          "rules"
          (TypeReference.option policyType)
          ("Resource rules to approve the closure under (e.g. one `http GET` "
           + "rule), or None to approve its analyzed effect set")
        Param.make
          "acknowledgeIncomplete"
          TBool
          "True only when a human accepted incomplete effect analysis"
        Param.make
          "acknowledgeContractChange"
          TBool
          "True only when a human has reviewed a changed permission contract" ]
      (TypeReference.result
        TUnit
        (TCustomType(NR.ok (PolicyToDT.ApprovalFailure.typeName ()), [])))
      ("Approve an immutable function closure and point its logical name at that version, as "
       + "one transaction. Contract comparison happens before any approval is stored. "
       + "`Ok ()` when approved; `Error (ContractChanged differences)` "
       + "when the contract changed and `acknowledgeContractChange` was false — "
       + "show them, obtain the review, and call again with the acknowledgment; "
       + "`Error (Refused message)` when the version cannot be approved as asked. "
       + "Host-only.")
      (fun state args ->
        match args with
        | [| accountIDDval
             DString location
             DString hash
             rulesDval
             DBool acknowledgeIncomplete
             DBool acknowledgeContractChange |] ->
          uply {
            let failureKT = PolicyToDT.ApprovalFailure.knownType ()
            let effects = builtinEffects state
            let rules =
              match rulesDval with
              | DEnum(_, _, _, "None", []) -> None
              | DEnum(_, _, _, "Some", [ policy ]) ->
                Some(PolicyToDT.Policy.fromDT policy)
              | _ -> incorrectArgs ()
            match!
              PackagePermissions.approveVersionForName
                PackagePermissions.Load.fromStore
                effects.callEffectsFor
                (accountIDOf accountIDDval)
                location
                hash
                rules
                acknowledgeIncomplete
                acknowledgeContractChange
                effects.fingerprint
            with
            | Ok PackagePermissions.ApprovalOutcome.Approved ->
              return Dval.resultOk KTUnit failureKT DUnit
            | Ok(PackagePermissions.ApprovalOutcome.ContractChanged differences) ->
              return
                Dval.resultError
                  KTUnit
                  failureKT
                  (PolicyToDT.ApprovalFailure.contractChanged differences)
            | Error message ->
              return
                Dval.resultError
                  KTUnit
                  failureKT
                  (PolicyToDT.ApprovalFailure.refused message)
          }
        | _ -> incorrectArgs ())

    policyFn
      "pmFnPermissionRequirements"
      [ Param.make "hash" TString "The package-fn hash to analyze" ]
      (TTuple(
        TList(TCustomType(NR.ok (EffectsToDT.Effect.typeName ()), [])),
        TBool,
        []
      ))
      "The transitive permission requirements of a package function (including code it can return for later use), and whether the analysis was complete. Incomplete analysis — a dynamic call the static walk can't follow, e.g. a callback in `List.map` — returns the partial effects with `false`; it is NOT an error, so callers display it as incomplete rather than crashing. Approval fails closed on incompleteness separately."
      (fun state args ->
        match args with
        | [| DString hashStr |] ->
          uply {
            let! result =
              PackagePermissions.permissionRequirements
                PackagePermissions.Load.fromStore
                (builtinEffects state).callEffectsFor
                hashStr
            return
              DTuple(
                EffectsToDT.toDT result.requiredEffects,
                DBool result.complete,
                []
              )
          }
        | _ -> incorrectArgs ()) ]


let builtins () = LibExecution.Builtin.make [] (fns ())
