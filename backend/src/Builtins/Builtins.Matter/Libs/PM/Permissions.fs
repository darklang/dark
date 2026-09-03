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


let private policyType : TypeReference =
  TCustomType(NR.ok (PolicyToDT.Policy.typeName ()), [])

let private policyAdminError () =
  RuntimeError.UncaughtException(
    "permission denied: policy changes require the trusted `dark permissions` command",
    []
  )
  |> raiseUntargetedRTE

let private accountIDOf (dval : Dval) : Option<System.Guid> =
  CommonToDT.Option.fromDT D.uuid dval

/// The running host's approval-analysis metadata, read once per builtin call:
/// `callEffectsFor` answers analysis by full builtin identity (name, version)
/// so different-effect versions never collapse. `fingerprint` includes those
/// effects and the analyzer version, so either kind of semantic change makes
/// existing approvals stale. Deterministic across runs of the same binary.
type private BuiltinEffects =
  { callEffectsFor : PackagePermissions.CallEffectsFor; fingerprint : string }

let private builtinEffects (state : ExecutionState) : BuiltinEffects =
  let sorted = state.fns.builtIn |> Dictionary.toSortedList
  let byIdentity =
    sorted
    |> List.map (fun (k, b) -> (k.name, k.version), b.callEffects)
    |> Map.ofList
  let lines =
    $"analysis={LibExecution.CallGraph.analysisVersion}"
    :: (sorted
        |> List.map (fun (k, b) ->
          let effects =
            b.callEffects
            |> Set.toList
            |> List.map LibExecution.Effects.name
            |> List.sort
            |> String.concat ","
          $"{k.name}@{k.version}={effects}"))
  use sha = System.Security.Cryptography.SHA256.Create()
  let fingerprint =
    sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes(String.concat "\n" lines))
    |> Array.take 8
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""
  { callEffectsFor = (fun key -> Map.tryFind key byIdentity)
    fingerprint = fingerprint }

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
    sqlSpec = NotQueryable
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

let private accountParam = Param.make "accountID" (TypeReference.option TUuid) ""
let private locationParam = Param.make "location" TString "Logical function name"

let fns : List<BuiltInFn> =
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
      "pmPolicySetInstance"
      [ Param.make "policy" policyType "The complete replacement policy" ]
      TUnit
      "Replace the host-owned instance permission policy atomically. Host-only."
      (fun _ args ->
        match args with
        | [| policy |] ->
          uply {
            PolicyToDT.Policy.fromDT policy |> PolicyStore.setInstancePolicy
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
    // the Workbench needs it while inspecting a function.
    policyFn
      "pmPolicyPackageAccess"
      [ accountParam
        Param.make "hash" TString "The immutable package-function hash" ]
      (TTuple(TBool, TypeReference.option policyType, []))
      ("Return whether a function is bundled first-party code and its stored "
       + "consumer policy, if any. Used to explain effective access. Read-only.")
      (fun state args ->
        match args with
        | [| accountIDDval; DString hash |] ->
          uply {
            let policyKT = PolicyToDT.Policy.knownType ()
            let policy =
              PolicyStore.packagePolicies (accountIDOf accountIDDval)
              |> Map.tryFind hash
              |> Option.map PolicyToDT.Policy.toDT
              |> function
                | Some policy -> Dval.optionSome policyKT policy
                | None -> Dval.optionNone policyKT
            let bundled = state.isBundledPackageFn (Hash hash)
            return DTuple(DBool bundled, policy, [])
          }
        | _ -> incorrectArgs ())

    // Read-only policy metadata; see pmPolicyPackageAccess above.
    policyFn
      "pmPolicyPins"
      [ accountParam ]
      (TList(TTuple(TString, TString, [])))
      ("Every logical function name this account has pinned, with the pinned "
       + "content hash (host metadata; hashes are not user-facing). Read-only.")
      (fun _ args ->
        match args with
        | [| accountIDDval |] ->
          uply {
            let pins =
              PolicyStore.functionPins (accountIDOf accountIDDval)
              |> Map.toList
              |> List.map (fun (name, hash) ->
                DTuple(DString name, DString hash, []))
            return DList(VT.tuple VT.string VT.string [], pins)
          }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyRevokePackage"
      [ accountParam
        Param.make "hash" TString "The immutable package-function hash to revoke" ]
      TUnit
      "Remove the local consumer approval for a package-function hash. Host-only."
      (fun _ args ->
        match args with
        | [| accountIDDval; DString hashStr |] ->
          uply {
            // Dependency-aware revoke: drop only the policies no other approved
            // root still needs (see PolicyStore.revokePackageRoot).
            PolicyStore.revokePackageRoot (accountIDOf accountIDDval) hashStr
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

    policyFn
      "pmPolicyExplicitApproval"
      [ accountParam
        Param.make "hash" TString "The immutable package-function hash" ]
      (TypeReference.option policyType)
      ("The resource rules the consumer wrote when approving this version as a "
       + "root, or None for a derived approval. Read-only.")
      (fun _ args ->
        match args with
        | [| accountIDDval; DString hash |] ->
          uply {
            let policyKT = PolicyToDT.Policy.knownType ()
            return
              match
                PolicyStore.explicitApproval (accountIDOf accountIDDval) hash
              with
              | Some policy ->
                Dval.optionSome policyKT (PolicyToDT.Policy.toDT policy)
              | None -> Dval.optionNone policyKT
          }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyPinFunction"
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
        (TCustomType(NR.ok (PolicyToDT.PinFailure.typeName ()), [])))
      ("Approve an immutable function closure and pin its logical name as one "
       + "transaction. Contract comparison happens before any approval is stored. "
       + "`Ok ()` when approved and pinned; `Error (ContractChanged differences)` "
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
            let failureKT = PolicyToDT.PinFailure.knownType ()
            let effects = builtinEffects state
            let rules =
              match rulesDval with
              | DEnum(_, _, _, "None", []) -> None
              | DEnum(_, _, _, "Some", [ policy ]) ->
                Some(PolicyToDT.Policy.fromDT policy)
              | _ -> incorrectArgs ()
            match!
              PackagePermissions.approveAndPinFunctionVersion
                LibDB.ProgramTypes.Fn.get
                effects.callEffectsFor
                (accountIDOf accountIDDval)
                location
                hash
                rules
                acknowledgeIncomplete
                acknowledgeContractChange
                effects.fingerprint
            with
            | Ok PackagePermissions.PinOutcome.Pinned ->
              return Dval.resultOk KTUnit failureKT DUnit
            | Ok(PackagePermissions.PinOutcome.ContractChanged differences) ->
              return
                Dval.resultError
                  KTUnit
                  failureKT
                  (PolicyToDT.PinFailure.contractChanged differences)
            | Error message ->
              return
                Dval.resultError
                  KTUnit
                  failureKT
                  (PolicyToDT.PinFailure.refused message)
          }
        | _ -> incorrectArgs ())

    hostOnly
      "pmPolicyUnpinFunction"
      [ accountParam; locationParam ]
      TUnit
      "Remove a logical function version pin. Host-only."
      (fun _ args ->
        match args with
        | [| accountIDDval; DString location |] ->
          uply {
            PolicyStore.unpinFunction (accountIDOf accountIDDval) location
            return DUnit
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
                LibDB.ProgramTypes.Fn.get
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


let builtins = LibExecution.Builtin.make [] fns
