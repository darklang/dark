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
  RuntimeError.TrustedCommandOnly RuntimeError.TrustedCommands.PolicyChanges
  |> raiseRTE

let private accountIDOf (dval : Dval) : Option<System.Guid> =
  CommonToDT.Option.fromDT D.uuid dval

/// The running host's approval-analysis metadata, built once per builtin table:
/// `callEffectsFor` answers analysis by full builtin identity (name, version)
/// so different-effect versions never collapse. `fingerprint` includes those
/// effects and the analyzer version, so either kind of semantic change makes
/// existing approvals stale. Deterministic across runs of the same binary.
type private BuiltinEffects =
  { callEffectsFor : PackagePermissions.CallEffectsFor; fingerprint : string }

/// A table never changes once made, and building this sorts and hashes every builtin, so it is
/// built once per table: `search` asks for every function it lists.
/// Weakly keyed on the table, so a dropped table takes its entry with it.
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
  (impl : ExecutionState -> Dval[] -> Dval)
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
  (impl : ExecutionState -> Dval[] -> Dval)
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
        | [| DUnit |] -> (PolicyStore.instancePolicy () |> PolicyToDT.Policy.toDT)
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyReviewToken"
      [ Param.make "unit" TUnit "" ]
      (TTuple(TString, policyType, []))
      "One consistent instance policy and the identity required to commit its edit."
      (fun _ args ->
        match args with
        | [| DUnit |] ->
          let snapshot = PolicyStore.get ()
          DTuple(
            DString(PolicyStore.reviewToken snapshot),
            PolicyToDT.Policy.toDT snapshot.instance,
            []
          )
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyNormalizeRule"
      [ Param.make
          "rule"
          (TCustomType(NR.ok (PolicyToDT.Rule.typeName ()), []))
          "A user-entered permission rule" ]
      (TCustomType(NR.ok (PolicyToDT.Rule.typeName ()), []))
      "Canonicalize a file rule using the same host path semantics as policy commits."
      (fun _ args ->
        match args with
        | [| rule |] -> rule |> PolicyToDT.Rule.fromDT |> PolicyToDT.Rule.toDT
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyEditInstance"
      [ Param.make "expected" TString "Identity of the reviewed snapshot"
        Param.make "policy" policyType "The policy calculated by Dark" ]
      TUnit
      "Commit the instance policy calculated by the trusted CLI, refusing a stale snapshot."
      (fun _ args ->
        match args with
        | [| DString expected; policy |] ->
          match
            PolicyStore.commitInstancePolicy
              expected
              (PolicyToDT.Policy.fromDT policy)
          with
          | Ok() -> DUnit
          | Error message -> Exception.raiseStoreCondition message []
        | _ -> incorrectArgs ())

    // Read this account's approvals once. Dark selects and formats the entries.
    policyFn
      "pmPolicyApprovals"
      [ accountParam ]
      (TTuple(
        TString,
        TList(TCustomType(NR.ok (PolicyToDT.Approval.typeName ()), [])),
        []
      ))
      "One account's named approvals and the snapshot identity required for an atomic edit. Read-only."
      (fun _ args ->
        match args with
        | [| accountIDDval |] ->
          let snapshot = PolicyStore.get ()
          let entries =
            PolicyStore.namedApprovalsInStore (accountIDOf accountIDDval) snapshot
            |> Map.toList
            |> List.map (fun (name, approval) ->
              PolicyToDT.Approval.toDT
                approval.hash
                (Some name)
                (Some approval.policy)
                (if approval.explicitRules then Some approval.policy else None))
          DTuple(
            DString(PolicyStore.reviewToken snapshot),
            DList(VT.known (PolicyToDT.Approval.knownType ()), Items entries),
            []
          )
        | _ -> incorrectArgs ())

    // Read-only; the Workbench uses it to explain effective access.
    policyFn
      "pmFnIsBundled"
      [ Param.make "hash" TString "The immutable package-function hash" ]
      TBool
      "Whether a package function is bundled first-party code. Read-only."
      (fun state args ->
        match args with
        | [| DString hash |] -> (DBool(state.isBundledPackageFn (Hash hash)))
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyRemove"
      [ accountParam
        Param.make
          "target"
          (TCustomType(NR.ok (PolicyToDT.ApprovalTarget.typeName ()), []))
          "Reviewed names to remove, or an edit to a saved policy" ]
      TUnit
      ("Remove the saved names selected by Dark, or commit the policy edit "
       + "calculated by the trusted CLI. Edits compare the reviewed store identity "
       + "atomically and preserve the approved code version and fingerprint. "
       + "Host-only.")
      (fun _ args ->
        match args with
        | [| accountIDDval; target |] ->
          let accountID = accountIDOf accountIDDval
          match PolicyToDT.ApprovalTarget.fromDT target with
          | PolicyToDT.ApprovalTarget.Names(expected, names) ->
            match PolicyStore.removeApprovals accountID expected names with
            | Ok() -> ()
            | Error message -> Exception.raiseStoreCondition message []
          | PolicyToDT.ApprovalTarget.EditPolicy(name, expected, policy) ->
            match
              PolicyStore.commitApprovalPolicy accountID name expected policy
            with
            | Ok() -> ()
            | Error message -> Exception.raiseStoreCondition message []
          DUnit
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyApprovalsStale"
      [ Param.make "unit" TUnit "" ]
      TBool
      "True when existing package approvals were recorded under different approval-analysis semantics than the running host's — a re-review signal (per-root, so re-approving one root does not clear it for others). A complete approval's stored allowlist still denies a newly-appeared effect; an acknowledged-incomplete approval is allow-all and bounded only by the instance/run policies, so for those the signal is the safeguard, not the package layer. Host-only."
      (fun state args ->
        match args with
        | [| DUnit |] ->
          let fingerprint = (builtinEffects state).fingerprint
          DBool(PolicyStore.approvalsAreStale fingerprint)
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyReviewVersion"
      [ accountParam
        locationParam
        Param.make "expected" TString "Policy identity from before review"
        Param.make "hash" TString "The code version to review"
        Param.make "rules" (TypeReference.option policyType) "Proposed rules"
        Param.make "acknowledgeIncomplete" TBool "Accept incomplete analysis" ]
      (TypeReference.result
        (TTuple(
          TBool,
          TList(TCustomType(NR.ok (PolicyToDT.ApprovalReviewReason.typeName ()), [])),
          []
        ))
        TString)
      "Return whether the approval needs saving and its review reasons, without writing."
      (fun state args ->
        match args with
        | [| accountIDDval
             DString location
             DString expected
             DString hash
             rulesDval
             DBool acknowledgeIncomplete |] ->
          let rules = CommonToDT.Option.fromDT PolicyToDT.Policy.fromDT rulesDval
          let effects = builtinEffects state
          let result =
            PackagePermissions.reviewVersionForName
              PackagePermissions.Load.fromStore
              effects.callEffectsFor
              (accountIDOf accountIDDval)
              location
              expected
              hash
              rules
              acknowledgeIncomplete
              effects.fingerprint
          let reasonKT = PolicyToDT.ApprovalReviewReason.knownType ()
          CommonToDT.Result.toDT
            (KTTuple(VT.bool, VT.known (KTList(VT.known reasonKT)), []))
            KTString
            result
            (fun (needsSave, reasons) ->
              DTuple(
                DBool needsSave,
                DList(
                  VT.known reasonKT,
                  Items(List.map PolicyToDT.ApprovalReviewReason.toDT reasons)
                ),
                []
              ))
            DString
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyApproveVersion"
      [ accountParam
        locationParam
        Param.make "expected" TString "Policy identity from before review"
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
      ("Remember access for this name and immutable function version, as "
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
             DString expected
             DString hash
             rulesDval
             DBool acknowledgeIncomplete
             DBool acknowledgeContractChange |] ->
          let failureKT = PolicyToDT.ApprovalFailure.knownType ()
          let effects = builtinEffects state
          let rules =
            match rulesDval with
            | DEnum(_, _, _, "None", []) -> None
            | DEnum(_, _, _, "Some", [ policy ]) ->
              Some(PolicyToDT.Policy.fromDT policy)
            | _ -> incorrectArgs ()
          match
            PackagePermissions.approveVersionForName
              PackagePermissions.Load.fromStore
              effects.callEffectsFor
              (accountIDOf accountIDDval)
              location
              expected
              hash
              rules
              acknowledgeIncomplete
              acknowledgeContractChange
              effects.fingerprint
          with
          | Ok PackagePermissions.ApprovalOutcome.Approved ->
            Dval.resultOk KTUnit failureKT DUnit
          | Ok(PackagePermissions.ApprovalOutcome.ContractChanged differences) ->
            Dval.resultError
              KTUnit
              failureKT
              (PolicyToDT.ApprovalFailure.contractChanged differences)
          | Error message ->
            Dval.resultError
              KTUnit
              failureKT
              (PolicyToDT.ApprovalFailure.refused message)
        | _ -> incorrectArgs ())

    policyFn
      "pmFnEffectReasons"
      [ Param.make "hash" TString "The function to inspect without executing it" ]
      (TTuple(
        TBool,
        TList(
          TTuple(
            TCustomType(NR.ok (EffectsToDT.Effect.typeName ()), []),
            TList TString,
            []
          )
        ),
        []
      ))
      "Effect-analysis completeness and a call path explaining each known effect."
      (fun state args ->
        match args with
        | [| DString hash |] ->
          let result, reasons =
            PackagePermissions.explain
              PackagePermissions.Load.fromStore
              (builtinEffects state).callEffectsFor
              hash
          let itemType = KTCustomType(EffectsToDT.Effect.typeName (), [])
          let values =
            reasons
            |> List.map (fun (effect, path) ->
              DTuple(
                EffectsToDT.Effect.toDT effect,
                DList(VT.string, Items(List.map DString path)),
                []
              ))
          DTuple(
            DBool result.complete,
            DList(
              VT.known (KTTuple(VT.known itemType, VT.known (KTList VT.string), [])),
              Items values
            ),
            []
          )
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
          let result =
            PackagePermissions.permissionRequirements
              PackagePermissions.Load.fromStore
              (builtinEffects state).callEffectsFor
              hashStr
          DTuple(EffectsToDT.toDT result.requiredEffects, DBool result.complete, [])
        | _ -> incorrectArgs ()) ]


let builtins () = LibExecution.Builtin.make [] (fns ())
