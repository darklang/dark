/// The persisted policy store: format, approvals, reviewed versions, revocation, seeding.
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
      path =
        match path with
        | Some path -> Permission.HttpPath.Subtree path
        | None -> Permission.HttpPath.All
      query = scope query }

let private scoped (hash : string) : PolicyStore.ScopedKey = None, hash

let private approval hash policy fingerprint : PolicyStore.Approval =
  { hash = hash; policy = policy; fingerprint = fingerprint; explicitRules = true }

/// A `Unit -> BigInt` package fn with the given body.
let private unitFn (hash : string) (body : PT.Expr) : PT.PackageFn.PackageFn =
  { hash = PT.Hash hash
    body = body
    typeParams = []
    parameters = NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
    returnType = PT.TBigInt
    description = ""
    bounds = [] }

/// Builtin effects for the review tests: `syscallGettimeofday` is clock-only and
/// `fileRead` reads files; anything else is unknown (incomplete).
let private testEffects : PackagePermissions.CallEffectsFor =
  fun (name, _version) ->
    match name with
    | "syscallGettimeofday" -> Some(Set.singleton Effect.Effect.Clock)
    | "fileRead" -> Some(Set.singleton Effect.Effect.FileRead)
    | _ -> None

let private callBuiltin (name : string) : PT.Expr =
  eApply (eBuiltinFn name 0) [] [ eUnit () ]

/// The review tests share one package universe: `v1` is the approved version,
/// `v2` an identical-contract re-release, `wider` adds a file read, and
/// `dynamic` applies a callback the static walk cannot follow.
let private reviewUniverse =
  [ unitFn "v1" (callBuiltin "syscallGettimeofday")
    unitFn "v2" (eLet (lpUnit ()) (eUnit ()) (callBuiltin "syscallGettimeofday"))
    unitFn
      "wider"
      (eStatement (callBuiltin "fileRead") (callBuiltin "syscallGettimeofday"))
    unitFn "dynamic" (eApply (eVar "callback") [] [ eUnit () ])
    // A root with a dependency: reads a file itself and calls `v1` for the clock.
    unitFn
      "caller"
      (eStatement (callBuiltin "fileRead") (eApply (ePackageFn "v1") [] [ eUnit () ])) ]
  |> List.map (fun fn -> fn.hash, fn)
  |> Map.ofList

let private loadFromUniverse : PackagePermissions.Load =
  { fn = (fun hash -> Map.tryFind hash reviewUniverse); value = (fun _ -> None) }

let private reviewUnder
  explicitPolicy
  previousApproval
  hash
  acknowledgeIncomplete
  acknowledgeChange
  =
  PackagePermissions.reviewVersion
    loadFromUniverse
    testEffects
    previousApproval
    hash
    explicitPolicy
    acknowledgeIncomplete
    acknowledgeChange

let private review previousApproval hash acknowledgeIncomplete acknowledgeChange =
  reviewUnder None previousApproval hash acknowledgeIncomplete acknowledgeChange

let policyStoreRoundTripsVersionedPolicies =
  test "policy store round-trips instance and package approvals" {
    let stripeRule = httpRule "POST" "api.stripe.com" (Some "/v1") None
    let exactRule =
      match Permission.Request.http "GET" "https://api.stripe.com/orders" with
      | Ok request -> Permission.Rule.forRequest request
      | Error message -> failtest message
    let stripePolicy = Permission.Policy.create [ stripeRule; exactRule ] []
    let store : PolicyStore.Store =
      { instance =
          Permission.Policy.create
            [ Permission.Rule.Effect Effect.Effect.Clock
              Permission.Rule.HttpServer(only "127.0.0.1", only 9090)
              Permission.Rule.HttpServer(Permission.Scope.All, Permission.Scope.All)
              Permission.Rule.File(Permission.AccessKind.Read, only "/tmp") ]
            [ Permission.Rule.Effect Effect.Effect.Native ]
        approvals =
          Map.ofList
            [ scoped "Feriel.Stripe.pay",
              approval "stripe@sha256:abc" stripePolicy "f1" ] }
    let decoded = PolicyStore.toBytes store |> PolicyStore.fromBytes
    Expect.equal
      (Permission.Policy.rules decoded.instance)
      (Permission.Policy.rules store.instance)
      "instance policy"
    Expect.equal
      (decoded.approvals
       |> Map.tryFind (scoped "Feriel.Stripe.pay")
       |> Option.map _.policy)
      (Some stripePolicy)
      "package approval"
    Expect.equal
      (decoded.approvals
       |> Map.tryFind (scoped "Feriel.Stripe.pay")
       |> Option.map _.hash)
      (Some "stripe@sha256:abc")
      "reviewed function version"
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
    // A complete payload with the old header must be rejected, not converted.
    let bytes = PolicyStore.toBytes PolicyStore.empty
    use stream = new System.IO.MemoryStream(bytes)
    use reader = new System.IO.BinaryReader(stream)
    LibSerialization.Binary.Serializers.Common.String.read reader |> ignore<string>
    use writer = new System.IO.BinaryWriter(stream)
    writer.Write older
    writer.Flush()
    Expect.equal
      (PolicyStore.storedVersion bytes)
      (Some older)
      "the version can be named in an unsupported-format error"
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
  test "startup seeds fresh installs and leaves existing policies untouched" {
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
      PolicyStore.SeedAction.Leave
      "an older format is not backed up, migrated, or reset"
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

let reapprovingASharedHashKeepsTheOtherNamesApproved =
  test "approving a second name for the same hash keeps the first name's approval" {
    // Content addressing makes one hash the target of several names. The
    // re-approval cleanup used to revoke every name pointing at the hash
    // and then restore only the one being
    // approved: after `Acme.a -> H` and `Acme.b -> H` only `Acme.b` was left.
    let accountID : Option<System.Guid> = None
    let approve location policy store =
      match
        PolicyStore.recordApprovalInStore
          accountID
          location
          (PolicyStore.reviewToken store)
          (approval "H" policy "effects-v1")
          store
      with
      | Ok store -> store
      | Error e -> failtest $"approval of {location} was rejected: {e}"
    let narrow = Permission.Policy.allowEffects (Set.singleton Effect.Effect.Clock)
    let store =
      PolicyStore.empty
      |> approve "Acme.a" narrow
      |> approve "Acme.b" Permission.Policy.allowAll
    Expect.equal
      store.approvals[scoped "Acme.a"].policy
      narrow
      "alias retains its scope"
    Expect.equal
      store.approvals[scoped "Acme.b"].policy
      Permission.Policy.allowAll
      "second scope"
    Expect.equal store.approvals.Count 2 "only named roots have policies"
    let approved name =
      Map.tryFind (accountID, name) store.approvals |> Option.map _.hash
    Expect.equal (approved "Acme.a") (Some "H") "the first name is still approved"
    Expect.equal (approved "Acme.b") (Some "H") "the second name is approved"
    // Explicit hash revocation removes every name approved for that hash.
    let revoked =
      PolicyStore.removeApprovalsInStore
        accountID
        (PolicyStore.reviewToken store)
        [ "Acme.a"; "Acme.b" ]
        store
      |> Result.defaultWith failtest
    Expect.isEmpty revoked.approvals "revoking the hash removes every named approval"
  }

let instancePolicyCommitsPreserveApprovals =
  test "instance policy commits preserve approvals and reject stale snapshots" {
    let named = approval "H" Permission.Policy.allowAll "fp"
    let store =
      { PolicyStore.empty with approvals = Map.ofList [ scoped "Acme.a", named ] }
    let token = PolicyStore.reviewToken store
    let edited =
      PolicyStore.commitInstancePolicyInStore token Permission.Policy.allowAll store
      |> Result.defaultWith failtest
    Expect.equal edited.approvals store.approvals "named grants are preserved"
    Expect.equal
      edited.instance
      Permission.Policy.allowAll
      "the calculated policy is stored"
    Expect.isError
      (PolicyStore.commitInstancePolicyInStore token Permission.Policy.denyAll edited)
      "another editor's committed policy cannot be overwritten"
  }

let selectedApprovalRemovalIsAtomicAndScoped =
  test "selected approval removals are atomic and account scoped" {
    let named = approval "H" Permission.Policy.allowAll "fp"
    let account = Some(System.Guid.NewGuid())
    let store =
      { PolicyStore.empty with
          instance = Permission.Policy.allowAll
          approvals =
            Map.ofList
              [ scoped "Acme.a", named
                scoped "Acme.b", named
                (account, "Acme.a"), named ] }
    let token = PolicyStore.reviewToken store
    let edited =
      PolicyStore.removeApprovalsInStore None token [ "Acme.a"; "Acme.b" ] store
      |> Result.defaultWith failtest
    Expect.equal
      edited.approvals
      (Map.ofList [ (account, "Acme.a"), named ])
      "only the selected account's names are removed"
    Expect.equal edited.instance store.instance "instance unchanged"
    Expect.equal
      (PolicyStore.namedApprovalsInStore account store)
      (Map.ofList [ "Acme.a", named ])
      "snapshot is account scoped"
    Expect.isError
      (PolicyStore.removeApprovalsInStore None token [ "Acme.a" ] edited)
      "a stale selection cannot erase a newer decision"
  }

/// Editing rules keeps the approved version and its permission-analysis fingerprint.
let committingSavedPoliciesPreservesReviewIdentity =
  test "committing saved policies preserves identity and rejects stale snapshots" {
    let clock = Permission.Rule.Effect Effect.Effect.Clock
    let random = Permission.Rule.Effect Effect.Effect.Random
    let native = Permission.Rule.Effect Effect.Effect.Native
    let key = scoped "Acme.Tools.run"
    let saved =
      { approval
          "old-code"
          (Permission.Policy.create [ clock; random ] [ native ])
          "old-fingerprint" with
          explicitRules = false }
    let otherAccount = Some(System.Guid.NewGuid()), "Acme.Tools.run"
    let otherName = scoped "Acme.Tools.alias"
    let store =
      { PolicyStore.empty with
          instance = Permission.Policy.allowAll
          approvals =
            Map.ofList [ key, saved; otherAccount, saved; otherName, saved ] }
    let expected = PolicyStore.reviewToken store
    let next = Permission.Policy.create [ random ] [ native ]
    let commit current policy =
      PolicyStore.commitApprovalPolicyInStore
        None
        "Acme.Tools.run"
        expected
        policy
        current
    let edited =
      match commit store next with
      | Ok edited -> edited
      | Error message -> failtest message
    let current = edited.approvals[key]
    Expect.equal current.policy next "stores the policy calculated by Dark"
    Expect.equal current.hash saved.hash "does not approve current code"
    Expect.equal
      current.fingerprint
      saved.fingerprint
      "does not refresh stale analysis"
    Expect.isTrue current.explicitRules "the edited policy stays explicit on update"
    Expect.equal edited.instance store.instance "instance unchanged"
    Expect.equal edited.approvals[otherAccount] saved "another account unchanged"
    Expect.equal edited.approvals[otherName] saved "an identical alias unchanged"
    let concurrent =
      [ edited
        { store with instance = Permission.Policy.denyAll }
        { store with approvals = Map.remove key store.approvals }
        { store with
            approvals = Map.add key { saved with hash = "new-code" } store.approvals }
        { store with
            approvals =
              Map.add key { saved with fingerprint = "new-analysis" } store.approvals } ]
    for changed in concurrent do
      Expect.isError
        (commit changed next)
        "a stale snapshot cannot overwrite a newer decision"
    Expect.isError
      (PolicyStore.commitApprovalPolicyInStore None "Missing.fn" expected next store)
      "an edit cannot create an approval for an unreviewed name"
    let empty =
      match commit store Permission.Policy.denyAll with
      | Ok edited -> edited.approvals[key]
      | Error message -> failtest message
    Expect.equal
      empty.policy
      Permission.Policy.denyAll
      "an empty edited policy can be committed"
    Expect.equal empty.hash saved.hash "empty approval keeps the reviewed version"
    Expect.isTrue empty.explicitRules "bare approve cannot resurrect inferred access"
  }

let approvalAndReviewedVersionAreOneTransaction =
  test "review rejects changed rules, revocation and instance edits" {
    let key = scoped "Acme.Tools.run"
    let broad = approval "same-code" Permission.Policy.allowAll "f1"
    let store = { PolicyStore.empty with approvals = Map.ofList [ key, broad ] }
    let expected = PolicyStore.reviewToken store
    let narrower = { broad with policy = Permission.Policy.denyAll }
    let changed =
      [ { store with approvals = Map.add key narrower store.approvals }
        { store with approvals = Map.remove key store.approvals }
        { store with instance = Permission.Policy.allowAll } ]
    for current in changed do
      Expect.isError
        (PolicyStore.recordApprovalInStore
          None
          "Acme.Tools.run"
          expected
          broad
          current)
        "an older review cannot replace a newer permission decision"
    let next = { broad with hash = "new-code" }
    match
      PolicyStore.recordApprovalInStore None "Acme.Tools.run" expected next store
    with
    | Error message -> failtest message
    | Ok saved ->
      Expect.equal saved.approvals[key] next "the whole approval changes atomically"
      Expect.equal saved.approvals.Count 1 "no separate dependency grants"
  }

let reapprovingOneRootLeavesOthersStale =
  test "re-approving one root under a new fingerprint leaves the others stale" {
    let root fp hash = approval hash Permission.Policy.allowAll fp
    // A and B both approved under f1; the runtime is now f2 (both stale).
    let store : PolicyStore.Store =
      { PolicyStore.empty with
          approvals =
            Map.ofList [ scoped "A", root "f1" "A"; scoped "B", root "f1" "B" ] }
    Expect.isTrue
      (PolicyStore.approvalsAreStaleInStore "f2" store)
      "both approvals predate f2"
    // Re-approve only A under f2. B must still read stale.
    let store =
      { store with
          approvals = store.approvals |> Map.add (scoped "A") (root "f2" "A") }
    Expect.isTrue
      (PolicyStore.approvalsAreStaleInStore "f2" store)
      "B was never re-reviewed, so approvals remain stale"
    // Re-approve B too; now nothing predates f2.
    let store =
      { store with
          approvals = store.approvals |> Map.add (scoped "B") (root "f2" "B") }
    Expect.isFalse
      (PolicyStore.approvalsAreStaleInStore "f2" store)
      "every root now reviewed under f2"
  }

/// A denial asks which approvals installed a member's policy. The answer is every root whose
/// closure holds it, named the way it was approved, and stale if any of them predates the
/// running binary's builtin declarations.
let linkedPolicyAncestorsNeedRoot =
  test "a linked policy ancestor is refused unless root placed it" {
    let temp =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-policy-links-{System.Guid.NewGuid()}"
      )
    let real = System.IO.Path.Combine(temp, "real")
    let link = System.IO.Path.Combine(temp, "link")
    System.IO.Directory.CreateDirectory real |> ignore<System.IO.DirectoryInfo>
    try
      System.IO.Directory.CreateSymbolicLink(link, real)
      |> ignore<System.IO.FileSystemInfo>
      Expect.equal
        (LocalFile.checkDirectoryChain (System.IO.Path.Combine(real, "policy")))
        (Ok())
        "a plain directory is accepted"
      Expect.isError
        (LocalFile.checkDirectoryChain link)
        "the policy directory itself is never a link"
      // As root, a link root made is one only root could have placed.
      if LibExecution.HostPosix.getuid () <> 0u then
        Expect.isError
          (LocalFile.checkDirectoryChain (System.IO.Path.Combine(link, "policy")))
          "an ancestor link the user made is refused"
      // Debian and Ubuntu ship `/bin` as a root-owned link, like macOS's `/var`.
      let systemLink = if System.OperatingSystem.IsMacOS() then "/var" else "/bin"
      if
        not (isNull (System.IO.DirectoryInfo systemLink).LinkTarget)
        && LibExecution.HostPosix.fileOwner systemLink = Ok "0"
      then
        Expect.equal
          (LocalFile.checkDirectoryChain (
            System.IO.Path.Combine(
              systemLink,
              $"dark-{System.Guid.NewGuid()}",
              "policy"
            )
          ))
          (Ok())
          "a root-owned system link is accepted"
    finally
      System.IO.Directory.Delete(temp, true)
  }

/// The suite runs on a policy of its own (`Tests.main`), not the clone's `~/.darklang/policy`. A
/// test that sets no directory of its own must not read or write the dev store's policy, where a
/// deny-all refuses the clone's own gates and printing.
let suiteNeverUsesTheClonesPolicy =
  test "the suite's policy is not the clone's own" {
    let home =
      System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)
    let clones =
      System.IO.Path.GetFullPath(System.IO.Path.Combine(home, ".darklang", "policy"))
    match LibExecution.HostSecurity.policyDirectory () with
    | Ok dir ->
      Expect.notEqual (System.IO.Path.GetFullPath dir) clones "a throwaway directory"
      Expect.isTrue
        (System.IO.File.Exists(System.IO.Path.Combine(dir, "policies.bin")))
        "seeded as a fresh install is"
    | Error e -> failtest e
  }

let tests =
  testList
    "policyStore"
    [ suiteNeverUsesTheClonesPolicy
      policyStoreRoundTripsVersionedPolicies
      policyStoreRejectsTrailingData
      reapprovingASharedHashKeepsTheOtherNamesApproved
      instancePolicyCommitsPreserveApprovals
      selectedApprovalRemovalIsAtomicAndScoped
      committingSavedPoliciesPreservesReviewIdentity
      policyStoreRejectsOlderFormat
      seedClassifiesTheFileFailClosed
      linkedPolicyAncestorsNeedRoot
      approvalAndReviewedVersionAreOneTransaction
      reapprovingOneRootLeavesOthersStale ]
