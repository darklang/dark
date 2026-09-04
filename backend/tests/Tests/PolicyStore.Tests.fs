/// The persisted policy store: format, approvals, pins, revocation, seeding.
module Tests.PolicyStore

open Expecto
open Prelude
open TestUtils.PTShortcuts

module Effect = LibExecution.Effects
module Permission = LibExecution.Permissions
module PT = LibExecution.ProgramTypes
module PackagePermissions = LibDB.PackagePermissions
module PolicyStore = LibDB.PolicyStore
module LocalFile = LibDB.LocalFile

let private only (item : 'a) : Permission.Scope<'a> = Permission.Scope.Only item

/// An https:443 rule for one method and host; `None` leaves the path (or the
/// query) unscoped.
let private httpRule
  (method : string)
  (host : string)
  (path : Option<string>)
  (query : Option<string>)
  : Permission.Rule =
  let scope value =
    match value with
    | Some v -> only v
    | None -> Permission.Scope.All
  Permission.Rule.Http
    { method = only method
      scheme = only "https"
      host = Permission.HostRule.Exact host
      port = only 443
      pathPrefix = scope path
      query = scope query }

let private scoped (hash : string) : PolicyStore.ScopedKey = None, hash

/// A `Unit -> Int` package fn with the given body.
let private unitFn (hash : string) (body : PT.Expr) : PT.PackageFn.PackageFn =
  { hash = PT.Hash hash
    body = body
    typeParams = []
    parameters = NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
    returnType = PT.TInt
    description = ""
    permissionCeiling = None }

/// Builtin effects for the review tests: `timeNowMs` is clock-only and
/// `fileRead` reads files; anything else is unknown (incomplete).
let private testEffects : PackagePermissions.CallEffectsFor =
  fun (name, _version) ->
    match name with
    | "timeNowMs" -> Some(Set.singleton Effect.Effect.Clock)
    | "fileRead" -> Some(Set.singleton Effect.Effect.FileRead)
    | _ -> None

let private callBuiltin (name : string) : PT.Expr =
  eApply (eBuiltinFn name 0) [] [ eUnit () ]

/// The review tests share one package universe: `v1` is the pinned version,
/// `v2` an identical-contract re-release, `wider` adds a file read, and
/// `dynamic` applies a callback the static walk cannot follow.
let private reviewUniverse =
  [ unitFn "v1" (callBuiltin "timeNowMs")
    unitFn "v2" (eLet (lpUnit ()) (eUnit ()) (callBuiltin "timeNowMs"))
    unitFn "wider" (eStatement (callBuiltin "fileRead") (callBuiltin "timeNowMs"))
    unitFn "dynamic" (eApply (eVar "callback") [] [ eUnit () ])
    // A root with a dependency: reads a file itself and calls `v1` for the clock.
    unitFn
      "caller"
      (eStatement (callBuiltin "fileRead") (eApply (ePackageFn "v1") [] [ eUnit () ])) ]
  |> List.map (fun fn -> fn.hash, fn)
  |> Map.ofList

let private loadFromUniverse : PackagePermissions.LoadFn =
  fun hash -> Ply(Map.tryFind hash reviewUniverse)

let private reviewUnder
  explicitPolicy
  currentPin
  hash
  acknowledgeIncomplete
  acknowledgeChange
  =
  PackagePermissions.reviewVersion
    loadFromUniverse
    testEffects
    currentPin
    hash
    explicitPolicy
    acknowledgeIncomplete
    acknowledgeChange
  |> Ply.toTask

let private review currentPin hash acknowledgeIncomplete acknowledgeChange =
  reviewUnder None currentPin hash acknowledgeIncomplete acknowledgeChange

let policyStoreRoundTripsVersionedPolicies =
  test "policy store round-trips instance and package approvals" {
    let stripeRule = httpRule "POST" "api.stripe.com" (Some "/v1") None
    let stripePolicy = Permission.Policy.create [ stripeRule ] []
    let store : PolicyStore.Store =
      { instance =
          Permission.Policy.create
            [ Permission.Rule.Effect Effect.Effect.Clock
              Permission.Rule.File(Permission.AccessKind.Read, only "/tmp") ]
            [ Permission.Rule.Effect Effect.Effect.Native ]
        packages = Map.ofList [ scoped "stripe@sha256:abc", stripePolicy ]
        functionPins = Map.ofList [ scoped "Feriel.Stripe.pay", "stripe@sha256:abc" ]
        approvedRoots = Map.empty }
    let decoded = PolicyStore.toBytes store |> PolicyStore.fromBytes
    Expect.equal
      (Permission.Policy.rules decoded.instance)
      (Permission.Policy.rules store.instance)
      "instance policy"
    Expect.equal
      (decoded.packages |> Map.tryFind (scoped "stripe@sha256:abc"))
      (Some stripePolicy)
      "package approval"
    Expect.equal
      (decoded.functionPins |> Map.tryFind (scoped "Feriel.Stripe.pay"))
      (Some "stripe@sha256:abc")
      "logical function pin"
  }

let policyStoreRejectsTrailingData =
  test "policy store rejects trailing bytes instead of guessing" {
    let bytes =
      PolicyStore.toBytes PolicyStore.empty
      |> fun data -> Array.append data [| 0xFFuy |]
    Expect.throws
      (fun () -> PolicyStore.fromBytes bytes |> ignore<PolicyStore.Store>)
      "trailing data is malformed"
  }

let policyStoreRejectsOlderFormat =
  test "policy store recognizes but rejects an older version of its own format" {
    let older = PolicyStore.formatVersion - 1
    let bytes =
      use stream = new System.IO.MemoryStream()
      use writer = new System.IO.BinaryWriter(stream)
      LibSerialization.Binary.Serializers.Common.String.write writer "DARK-POLICIES"
      writer.Write older
      writer.Flush()
      stream.ToArray()
    Expect.equal
      (PolicyStore.storedVersion bytes)
      (Some older)
      "an older file is recognized as ours, so seeding can back it up"
    Expect.throws
      (fun () -> PolicyStore.fromBytes bytes |> ignore<PolicyStore.Store>)
      "older bytes are never reinterpreted"
    Expect.isNone
      (PolicyStore.storedVersion [| 1uy; 2uy; 3uy |])
      "foreign bytes are not a version of ours"
    Expect.equal
      (PolicyStore.storedVersion (PolicyStore.toBytes PolicyStore.empty))
      (Some PolicyStore.formatVersion)
      "a current file reports the current version"
  }

let seedClassifiesTheFileFailClosed =
  test "startup seeding backs up an older format and never re-widens a deleted file" {
    let older = PolicyStore.formatVersion - 1
    let olderBytes =
      use stream = new System.IO.MemoryStream()
      use writer = new System.IO.BinaryWriter(stream)
      LibSerialization.Binary.Serializers.Common.String.write writer "DARK-POLICIES"
      writer.Write older
      writer.Flush()
      stream.ToArray()
    Expect.equal
      (PolicyStore.seedAction (LocalFile.Read olderBytes) true)
      (PolicyStore.SeedAction.BackUpAndReset older)
      "an older format is backed up and reset to deny-all, not re-seeded"
    Expect.equal
      (PolicyStore.seedAction LocalFile.Missing false)
      PolicyStore.SeedAction.WriteDefault
      "a first install gets the usable default"
    Expect.equal
      (PolicyStore.seedAction LocalFile.Missing true)
      PolicyStore.SeedAction.Leave
      "a deleted file stays missing (deny-all), never silently re-widened"
    Expect.equal
      (PolicyStore.seedAction
        (LocalFile.Read(PolicyStore.toBytes PolicyStore.empty))
        true)
      PolicyStore.SeedAction.Leave
      "a current file is left alone"
    Expect.equal
      (PolicyStore.seedAction (LocalFile.Read [| 1uy; 2uy; 3uy |]) true)
      PolicyStore.SeedAction.Leave
      "foreign or corrupt bytes are left alone (they already read as deny-all)"
    Expect.equal
      (PolicyStore.seedAction (LocalFile.Unreadable(exn "eacces")) true)
      PolicyStore.SeedAction.Leave
      "an unreadable file is left alone"
  }

/// The review tests share one package universe: `v1` is the pinned version,
/// `v2` an identical-contract re-release, `wider` adds a file read, and
/// `dynamic` applies a callback the static walk cannot follow.

let revokingOneRootKeepsSharedDependencies =
  test "revoking one approved root leaves a dependency another root still needs" {
    // Roots A and B both depend on shared C; A also has private dependency P.
    let accountID : Option<System.Guid> = None
    let allow = Permission.Policy.allowAll
    let store : PolicyStore.Store =
      { PolicyStore.empty with
          packages =
            [ "A"; "B"; "C"; "P" ]
            |> List.map (fun h -> scoped h, allow)
            |> Map.ofList
          approvedRoots =
            Map.ofList
              [ scoped "A",
                { closure = Set.ofList [ "A"; "C"; "P" ]
                  fingerprint = "f1"
                  explicitPolicy = None }
                scoped "B",
                { closure = Set.ofList [ "B"; "C" ]
                  fingerprint = "f1"
                  explicitPolicy = None } ] }
    let after = PolicyStore.revokeRootInStore accountID "A" store
    Expect.isNone (after.packages |> Map.tryFind (scoped "A")) "revoked root A gone"
    Expect.isNone
      (after.packages |> Map.tryFind (scoped "P"))
      "A's private dependency P is gone"
    Expect.isSome
      (after.packages |> Map.tryFind (scoped "C"))
      "shared dependency C survives because B still needs it"
    Expect.isSome
      (after.packages |> Map.tryFind (scoped "B"))
      "the other root B is untouched"
    Expect.isNone
      (after.approvedRoots |> Map.tryFind (scoped "A"))
      "A's root record is removed"
    // Revoking B afterwards now removes C, since nothing else needs it.
    let afterBoth = PolicyStore.revokeRootInStore accountID "B" after
    Expect.isNone
      (afterBoth.packages |> Map.tryFind (scoped "C"))
      "C is removed once its last root is revoked"
  }

let revokingARootDropsPinsToIt =
  test "revoking a root also drops the account's pins that point at it" {
    let accountID : Option<System.Guid> = None
    let other = Some(System.Guid.NewGuid())
    let store : PolicyStore.Store =
      { PolicyStore.empty with
          packages = Map.ofList [ scoped "A", Permission.Policy.allowAll ]
          functionPins =
            Map.ofList
              [ scoped "Acme.fetch", "A"
                scoped "Acme.other", "B"
                (other, "Acme.fetch"), "A" ]
          approvedRoots =
            Map.ofList
              [ scoped "A",
                { closure = Set.ofList [ "A" ]
                  fingerprint = "f1"
                  explicitPolicy = None } ] }
    let after = PolicyStore.revokeRootInStore accountID "A" store
    Expect.equal
      (after.functionPins |> Map.toList |> Set.ofList)
      (Set.ofList [ (other, "Acme.fetch"), "A"; scoped "Acme.other", "B" ])
      "this account's pin to A is gone; another account's pin and a pin to B stay"
  }

let reapprovingWithSmallerClosureDropsObsoleteDeps =
  test "re-approving a root with a smaller closure drops the deps it no longer needs" {
    let accountID : Option<System.Guid> = None
    let allow = Permission.Policy.allowAll
    let policiesFor hs = hs |> List.map (fun h -> h, allow)
    // First approval of A covers {A, B, C}.
    let store =
      PolicyStore.recordApprovalInStore
        accountID
        "A"
        (policiesFor [ "A"; "B"; "C" ])
        "f1"
        None
        PolicyStore.empty
    Expect.isSome
      (store.packages |> Map.tryFind (scoped "C"))
      "C installed initially"
    // Re-approve A covering only {A, B}; C is now obsolete and must be dropped.
    let store =
      PolicyStore.recordApprovalInStore
        accountID
        "A"
        (policiesFor [ "A"; "B" ])
        "f2"
        None
        store
    Expect.isNone
      (store.packages |> Map.tryFind (scoped "C"))
      "obsolete dependency C is removed on re-approval"
    Expect.isSome (store.packages |> Map.tryFind (scoped "A")) "A stays"
    Expect.isSome (store.packages |> Map.tryFind (scoped "B")) "B stays"
    // But a dep another root still needs is kept: approve D covering {D, C},
    // then re-approve A (already {A,B}) — C must survive because D needs it.
    let store =
      PolicyStore.recordApprovalInStore
        accountID
        "D"
        (policiesFor [ "D"; "C" ])
        "f2"
        None
        store
    let store =
      PolicyStore.recordApprovalInStore
        accountID
        "A"
        (policiesFor [ "A" ])
        "f2"
        None
        store
    Expect.isSome
      (store.packages |> Map.tryFind (scoped "C"))
      "C survives because root D still needs it"
  }

let approvingADependencyKeepsItsOwnRootApproval =
  test
    "a root's own approval is not replaced when another root lists it as a dependency" {
    let accountID : Option<System.Guid> = None
    let narrow = Permission.Policy.allowEffects (Set.singleton Effect.Effect.Clock)
    let wide = Permission.Policy.allowAll
    // C is approved on its own under narrow rules...
    let store =
      PolicyStore.recordApprovalInStore
        accountID
        "C"
        [ "C", narrow ]
        "f1"
        (Some narrow)
        PolicyStore.empty
    // ...then A, which calls C, is approved with a wide derived policy for C.
    let store =
      PolicyStore.recordApprovalInStore
        accountID
        "A"
        [ "A", wide; "C", wide ]
        "f1"
        None
        store
    Expect.equal
      (Map.tryFind (scoped "C") store.packages)
      (Some narrow)
      "C keeps the policy its own approval reviewed"
    Expect.equal
      (Map.tryFind (scoped "A") store.packages)
      (Some wide)
      "A's own policy is installed"
    Expect.equal
      ((PolicyStore.revokeRootInStore accountID "A" store).packages
       |> Map.tryFind (scoped "C"))
      (Some narrow)
      "revoking A leaves C's own approval"
    Expect.equal
      ((PolicyStore.revokeRootInStore accountID "A" store).approvedRoots
       |> Map.tryFind (scoped "C")
       |> Option.bind (fun r -> r.explicitPolicy))
      (Some narrow)
      "C's written rules are remembered as its provenance"
  }

let approvalAndPinAreOneTransaction =
  test "a stale pin review cannot leave the rejected version approved" {
    let accountID : Option<System.Guid> = None
    let location = "Acme.Tools.run"
    let key : PolicyStore.ScopedKey = accountID, location
    let store = { PolicyStore.empty with functionPins = Map.ofList [ key, "old" ] }
    let attempted =
      PolicyStore.recordApprovalAndMovePinInStore
        accountID
        "new"
        [ "new", Permission.Policy.allowAll; "dep", Permission.Policy.allowAll ]
        "effects-v1"
        None
        location
        (Some "someone-else")
        "new"
        store
    Expect.isError attempted "a concurrent/stale pin comparison is rejected"
    Expect.isEmpty store.packages "rejection installs no package policies"
    Expect.isEmpty store.approvedRoots "rejection records no approved root"
    Expect.equal store.functionPins[key] "old" "rejection leaves the pin unchanged"

    let accepted =
      PolicyStore.recordApprovalAndMovePinInStore
        accountID
        "new"
        [ "new", Permission.Policy.allowAll; "dep", Permission.Policy.allowAll ]
        "effects-v1"
        None
        location
        (Some "old")
        "new"
        store
    match accepted with
    | Error message -> failtest message
    | Ok accepted ->
      Expect.equal accepted.functionPins[key] "new" "the reviewed pin moves"
      Expect.isSome
        (accepted.packages |> Map.tryFind (scoped "new"))
        "the root approval is installed"
      Expect.isSome
        (accepted.packages |> Map.tryFind (scoped "dep"))
        "the closure approval is installed"
  }

let reapprovingOneRootLeavesOthersStale =
  test "re-approving one root under a new fingerprint leaves the others stale" {
    let approval fp hashes : PolicyStore.RootApproval =
      { closure = Set.ofList hashes; fingerprint = fp; explicitPolicy = None }
    // A and B both approved under f1; the runtime is now f2 (both stale).
    let store : PolicyStore.Store =
      { PolicyStore.empty with
          approvedRoots =
            Map.ofList
              [ scoped "A", approval "f1" [ "A" ]
                scoped "B", approval "f1" [ "B" ] ] }
    Expect.isTrue
      (PolicyStore.approvalsAreStaleInStore "f2" store)
      "both approvals predate f2"
    // Re-approve only A under f2. B must still read stale.
    let store =
      { store with
          approvedRoots =
            store.approvedRoots |> Map.add (scoped "A") (approval "f2" [ "A" ]) }
    Expect.isTrue
      (PolicyStore.approvalsAreStaleInStore "f2" store)
      "B was never re-reviewed, so approvals remain stale"
    // Re-approve B too; now nothing predates f2.
    let store =
      { store with
          approvedRoots =
            store.approvedRoots |> Map.add (scoped "B") (approval "f2" [ "B" ]) }
    Expect.isFalse
      (PolicyStore.approvalsAreStaleInStore "f2" store)
      "every root now reviewed under f2"
  }

let tests =
  testList
    "policyStore"
    [ policyStoreRoundTripsVersionedPolicies
      policyStoreRejectsTrailingData
      policyStoreRejectsOlderFormat
      seedClassifiesTheFileFailClosed
      revokingOneRootKeepsSharedDependencies
      revokingARootDropsPinsToIt
      reapprovingWithSmallerClosureDropsObsoleteDeps
      approvingADependencyKeepsItsOwnRootApproval
      approvalAndPinAreOneTransaction
      reapprovingOneRootLeavesOthersStale ]
