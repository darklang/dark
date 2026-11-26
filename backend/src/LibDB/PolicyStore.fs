/// Host-owned, versioned storage for instance policy and package approvals.
///
/// This file is deliberately outside the package database. Package code can
/// declare effects, but it cannot write the policy that authorizes them.
module LibDB.PolicyStore

open System
open System.IO
open Prelude
open LibSerialization.Binary.Serializers.Common

module P = LibExecution.Permissions
module RT = LibExecution.RuntimeTypes
module Execution = LibExecution.Execution
module PolicyBin = LibSerialization.Binary.Serializers.Permissions

/// One named code version and the access reviewed for it.
type Approval =
  {
    hash : string
    policy : P.Policy
    fingerprint : string
    /// Explicit rules carry forward on update; inferred effects are recomputed.
    explicitRules : bool
  }

type ScopedKey = Option<Guid> * string

type Store = { instance : P.Policy; approvals : Map<ScopedKey, Approval> }

let private scopedKey (accountID : Option<Guid>) (key : string) : ScopedKey =
  accountID, key

let private scopedEntries
  (accountID : Option<Guid>)
  (entries : Map<ScopedKey, 'a>)
  : Map<string, 'a> =
  entries
  |> Map.toList
  |> List.choose (fun ((account, key), value) ->
    if account = accountID then Some(key, value) else None)
  |> Map.ofList

let private magic = "DARK-POLICIES"

/// Only the current policy format is supported; other versions are rejected.
let formatVersion = 12

let private fileName = "policies.bin"

let empty : Store = { instance = P.Policy.denyAll; approvals = Map.empty }

// ── codec ─────────────────────────────────────────────────────────────────────

let private writeEntries
  (writer : BinaryWriter)
  (writeKey : BinaryWriter -> 'key -> unit)
  (writeValue : BinaryWriter -> 'a -> unit)
  (entries : Map<'key, 'a>)
  : unit =
  writer.Write entries.Count
  for KeyValue(key, value) in entries do
    writeKey writer key
    writeValue writer value

/// A string that identifies something (a hash, a name); never blank.
let private readIdentifier (reader : BinaryReader) (label : string) : string =
  let value = String.read reader
  if String.IsNullOrWhiteSpace value then
    raiseFormatError $"Invalid empty policy store {label}"
  value

/// A keyed section: count, then (key, value) pairs. Keys must be non-empty
/// and unique; `Map.ofList` alone would silently keep the last duplicate.
let private readEntries
  (reader : BinaryReader)
  (label : string)
  (readKey : BinaryReader -> 'key)
  (readValue : BinaryReader -> 'a)
  : Map<'key, 'a> =
  let count = reader.ReadInt32()
  if count < 0 then raiseFormatError $"Invalid policy store {label} count"
  let pairs =
    [ for _ in 1..count do
        let key = readKey reader
        yield key, readValue reader ]
  if (pairs |> List.map fst |> List.distinct |> List.length) <> count then
    raiseFormatError $"Duplicate policy store {label} key"
  Map.ofList pairs

let private writeScopedKey
  (writer : BinaryWriter)
  ((accountID, key) : ScopedKey)
  : unit =
  Option.write writer Guid.write accountID
  String.write writer key

let private readScopedKey (label : string) (reader : BinaryReader) : ScopedKey =
  Option.read reader Guid.read, readIdentifier reader $"{label} key"

let private writeApproval (writer : BinaryWriter) (approval : Approval) =
  String.write writer approval.hash
  PolicyBin.write writer approval.policy
  String.write writer approval.fingerprint
  writer.Write approval.explicitRules

let private readApproval (reader : BinaryReader) : Approval =
  { hash = readIdentifier reader "function hash"
    policy = PolicyBin.read reader
    fingerprint = readIdentifier reader "fingerprint"
    explicitRules = reader.ReadBoolean() }

let toBytes (store : Store) : byte[] =
  use stream = new MemoryStream()
  use writer = new BinaryWriter(stream)
  String.write writer magic
  writer.Write formatVersion
  PolicyBin.write writer store.instance
  writeEntries writer writeScopedKey writeApproval store.approvals
  writer.Flush()
  stream.ToArray()

let fromBytes (bytes : byte[]) : Store =
  use stream = new MemoryStream(bytes)
  use reader = new BinaryReader(stream)
  if String.read reader <> magic then
    raiseFormatError "Invalid policy store magic"
  let version = reader.ReadInt32()
  if version <> formatVersion then
    raiseFormatError $"Unsupported policy store version: {version}"
  let instance = PolicyBin.read reader
  let approvals =
    readEntries reader "approval" (readScopedKey "approval") readApproval
  if stream.Position <> stream.Length then
    raiseFormatError "Trailing bytes in policy store"
  { instance = instance; approvals = approvals }

/// Opaque identity of the entire reviewed policy, including grants and revocations.
let reviewToken (store : Store) : string =
  toBytes store
  |> System.Security.Cryptography.SHA256.HashData
  |> System.Convert.ToHexString

/// Identify unsupported policy formats for an actionable write error.
let storedVersion (bytes : byte[]) : Option<int> =
  try
    use stream = new MemoryStream(bytes)
    use reader = new BinaryReader(stream)
    if String.read reader = magic then Some(reader.ReadInt32()) else None
  with _ ->
    None

// ── file access ───────────────────────────────────────────────────────────────

let private unsupportedFormat (version : int) (where : string) : string =
  $"The policy file {where} is from an older Dark (format {version}; this build reads {formatVersion}) and can't be read. Run `dark permissions profile default --yes` to start again from the default permissions. The old file will be kept."

/// Why the policy file grants nothing, when it is in a format this build can't read. Such a
/// file reads as an empty policy, so without this every refusal looks like a chosen one.
let unsupportedFormatProblem () : Option<string> =
  match LocalFile.read fileName with
  | LocalFile.Read bytes ->
    match storedVersion bytes with
    | Some version when version <> formatVersion ->
      let where = LocalFile.path fileName |> Result.defaultValue fileName
      Some(unsupportedFormat version where)
    | _ -> None
  | LocalFile.Missing
  | LocalFile.Unreadable _ -> None

/// Parsed-store cache keyed on the file's (mtime, ctime, length) stamp.
/// Execution and review read policy repeatedly; without this each read parses the file. Same-process writes invalidate
/// eagerly; another process's write changes the stamp and is picked up on the
/// next read.
let private cacheLock = obj ()

let mutable private cache : Option<Option<struct (int64 * int64 * int64)> * Store> =
  None

/// Read the host policy. Missing, malformed, or unreadable state is
/// restrictive and never restores authority.
let get () : Store =
  // A missing file stamps as None and is cached too — the fresh-install case
  // must not pay a failed open per package-fn resolution.
  let stamp = LocalFile.stamp fileName
  lock cacheLock (fun () ->
    match cache with
    | Some(cachedStamp, store) when cachedStamp = stamp -> store
    | _ ->
      let store =
        match LocalFile.read fileName with
        | LocalFile.Read bytes ->
          try
            fromBytes bytes
          with _ ->
            empty
        // A present but inaccessible policy file must not restore authority.
        | LocalFile.Missing
        | LocalFile.Unreadable _ -> empty
      cache <- Some(stamp, store)
      store)

let private writeFile (store : Store) : unit =
  LocalFile.writeAtomic fileName (toBytes store)
  lock cacheLock (fun () -> cache <- None)

/// Read the store before mutation. A present but invalid file is an error so a
/// write cannot replace approvals and approved versions with an empty store.
let private getForWrite () : Store =
  // For the messages below: where the file actually is, which follows the store.
  let where = LocalFile.path fileName |> Result.defaultValue fileName
  match LocalFile.read fileName with
  | LocalFile.Missing -> empty
  | LocalFile.Read bytes ->
    try
      fromBytes bytes
    with _ ->
      match storedVersion bytes with
      // These three are store conditions, not internal errors: each is the state of a file the
      // person can deal with once told, so each prints as its sentence. The path is the real one;
      // the policy directory moves with the store, so `~/.darklang/policy` is not always it.
      | Some version when version <> formatVersion ->
        Exception.raiseStoreCondition (unsupportedFormat version where) []
      | _ ->
        Exception.raiseStoreCondition
          $"The policy file is damaged and will not be overwritten. Move {where} aside (the approvals in it are lost) and retry."
          []
  | LocalFile.Unreadable error ->
    // The reason in a few words, not the exception: interpolating it printed a .NET stack trace.
    let why =
      match error with
      | :? System.UnauthorizedAccessException -> "permission denied"
      | e -> e.Message
    Exception.raiseStoreCondition
      $"The policy file {where} exists but cannot be read ({why}); refusing to overwrite it."
      []

/// One locked read-modify-write. The change may refuse (returning `Error`),
/// in which case nothing is written.
let private update (change : Store -> Result<Store, 'e>) : Result<unit, 'e> =
  LocalFile.withExclusiveLock (fun () ->
    match change (getForWrite ()) with
    | Ok store ->
      writeFile store
      Ok()
    | Error e -> Error e)

/// [update] for a change that never refuses.
let private set (change : Store -> Store) : unit =
  update (fun store -> Ok(change store)) |> ignore<Result<unit, unit>>

let private nonBlank (label : string) (value : string) : unit =
  if String.IsNullOrWhiteSpace value then
    invalidArg label $"{label} cannot be empty"

/// Move a policy file this build can't read out of the way, so that a fresh policy can be
/// written. Returns where it went. A current or missing file is left alone.
let setAsideUnsupported () : Option<string> =
  LocalFile.withExclusiveLock (fun () ->
    match LocalFile.read fileName with
    | LocalFile.Read bytes ->
      match storedVersion bytes with
      | Some version when version <> formatVersion ->
        let moved = LocalFile.moveAside fileName $"{fileName}.format-{version}"
        lock cacheLock (fun () -> cache <- None)
        Some moved
      | _ -> None
    | LocalFile.Missing
    | LocalFile.Unreadable _ -> None)

// ── first-run seed ──────────────────────────────────────────────

/// A durable marker that this install has been seeded. It lives in the
/// (write-protected) policy directory and survives a deleted policy file, so a
/// genuine first install can be told apart from a deleted or reset one.
let private markerFileName = "initialized"

let private isInitialized () : bool =
  match LocalFile.read markerFileName with
  | LocalFile.Read _ -> true
  | _ -> false

let private stampInitialized () : unit =
  LocalFile.writeAtomic markerFileName (System.Text.Encoding.UTF8.GetBytes "1")

/// What startup does with the policy file it finds; see [seedInstanceIfMissing].
[<RequireQualifiedAccess>]
type SeedAction =
  /// Leave the file (or its absence) exactly as it is.
  | Leave
  /// Write the usable default instance policy and stamp the marker.
  | WriteDefault

/// Seed only a fresh installation. Existing files are never converted or
/// replaced; missing files in initialized installations grant nothing.
let seedAction (file : LocalFile.ReadResult) (initialized : bool) : SeedAction =
  match file with
  | LocalFile.Missing when not initialized -> SeedAction.WriteDefault
  | _ -> SeedAction.Leave

/// Apply [seedAction] to the policy file, once at startup. The common case,
/// a current file, is decided from a plain read so that a read-only home (a
/// sandbox, a shared account) can still run read-only commands; the lock,
/// which creates and secures the directory, is taken only when there is
/// something to write, and the classification is repeated under it.
let seedInstanceIfMissing (instance : P.Policy) : unit =
  match seedAction (LocalFile.read fileName) (isInitialized ()) with
  | SeedAction.Leave -> ()
  | _ ->
    LocalFile.withExclusiveLock (fun () ->
      let file = LocalFile.read fileName
      match seedAction file (isInitialized ()) with
      | SeedAction.Leave -> ()
      | SeedAction.WriteDefault ->
        writeFile { empty with instance = instance }
        stampInitialized ())

// ── approval metadata fingerprint ─────────────────────────────────────────────
// The fingerprint records the builtin-effect classification used for each
// approval. A changed fingerprint requires review before the named grant
// can be used again.

/// This binary's builtin declarations, hashed with the analyzer version. An approval records
/// it, and a different one marks the approval stale.
let builtinFingerprint
  (builtIn : System.Collections.Generic.Dictionary<RT.FQFnName.Builtin, RT.BuiltInFn>)
  : string =
  let lines =
    $"analysis={LibExecution.CallGraph.analysisVersion}"
    :: (builtIn
        |> Seq.map (fun (KeyValue(k, b)) -> k, b)
        |> List.ofSeq
        |> List.sortBy fst
        |> List.map (fun (k, b) ->
          let effects =
            LibExecution.Effects.permissionEffects b.callEffects
            |> Set.toList
            |> List.map LibExecution.Effects.name
            |> List.sort
            |> String.concat ","
          $"{k.name}@{k.version}={effects}"))
  use sha = System.Security.Cryptography.SHA256.Create()
  sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes(String.concat "\n" lines))
  |> Array.take 8
  |> Array.map (fun b -> b.ToString("x2"))
  |> String.concat ""

/// Pure staleness check behind [approvalsAreStale]. True when any approved
/// root's fingerprint differs from `current`. Per-root, so re-approving one
/// root leaves the others' staleness intact.
let approvalsAreStaleInStore (current : string) (store : Store) : bool =
  store.approvals
  |> Map.exists (fun _ approval -> approval.fingerprint <> current)

/// True when any approved root was reviewed under a different builtin-effect
/// fingerprint than `current` — a re-review signal.
let approvalsAreStale (current : string) : bool =
  approvalsAreStaleInStore current (get ())

// ── reads ─────────────────────────────────────────────────────────────────────

let instancePolicy () : P.Policy = (get ()).instance

/// Return only this account's approvals from the supplied store.
let namedApprovalsInStore accountID (store : Store) : Map<string, Approval> =
  scopedEntries accountID store.approvals

let namedApprovals accountID : Map<string, Approval> =
  namedApprovalsInStore accountID (get ())

let packagePolicies accountID : Map<string, P.Policy> =
  namedApprovals accountID |> Map.map (fun approval -> approval.policy)

let approvedVersion accountID location : Option<string> =
  namedApprovals accountID |> Map.tryFind location |> Option.map _.hash

// ── writes ────────────────────────────────────────────────────────────────────

let setInstancePolicy (policy : P.Policy) : unit =
  set (fun store -> { store with instance = policy })

/// Save the instance policy only if permissions have not changed since review.
let commitInstancePolicyInStore
  expected
  (policy : P.Policy)
  (store : Store)
  : Result<Store, string> =
  if reviewToken store <> expected then
    Error "Permissions changed during review. Review the edit again."
  else
    Ok { store with instance = policy }

let commitInstancePolicy expected policy : Result<unit, string> =
  update (commitInstancePolicyInStore expected policy)

/// Remove the selected approvals together, from this account only.
let removeApprovalsInStore
  accountID
  expected
  names
  (store : Store)
  : Result<Store, string> =
  if reviewToken store <> expected then
    Error "Permissions changed during review. Review the removal again."
  else
    let remaining =
      names
      |> List.fold
        (fun approvals name -> Map.remove (scopedKey accountID name) approvals)
        store.approvals
    Ok { store with approvals = remaining }

let removeApprovals accountID expected names : Result<unit, string> =
  update (removeApprovalsInStore accountID expected names)

/// Compare the complete policy reviewed before the prompt, then install one entry.
let recordApprovalInStore
  accountID
  location
  expected
  (approval : Approval)
  (store : Store)
  : Result<Store, string> =
  if reviewToken store <> expected then
    Error "Permissions changed during review. Review the approval again."
  else
    Ok
      { store with
          approvals = Map.add (scopedKey accountID location) approval store.approvals }

let recordApproval
  accountID
  location
  expected
  (approval : Approval)
  : Result<unit, string> =
  nonBlank "location" location
  nonBlank "hash" approval.hash
  update (recordApprovalInStore accountID location expected approval)

/// Change saved rules only if permissions have not changed since review.
/// Keep its approved code version and permission-analysis fingerprint.
let commitApprovalPolicyInStore
  accountID
  name
  expected
  (policy : P.Policy)
  (store : Store)
  : Result<Store, string> =
  if reviewToken store <> expected then
    Error "Permissions changed during review. Review the edit again."
  else
    let key = scopedKey accountID name
    match Map.tryFind key store.approvals with
    | None -> Error $"{name} has no remembered approval."
    | Some approval ->
      let edited = { approval with policy = policy; explicitRules = true }
      Ok { store with approvals = Map.add key edited store.approvals }

/// Save the reviewed edit without changing other approvals or instance rules.
let commitApprovalPolicy accountID name expected policy : Result<unit, string> =
  nonBlank "name" name
  update (commitApprovalPolicyInStore accountID name expected policy)

/// Remember exactly the reviewed requests, merging only grants for this same
/// entry version. The whole prompt is committed atomically or not at all.
let rememberReviewed
  (expected : Store)
  accountID
  name
  hash
  fingerprint
  (rules : List<P.Rule>)
  : unit =
  set (fun store ->
    if store <> expected then
      Exception.raiseInternal
        "Permissions changed during review; nothing was saved."
        []
    let key = scopedKey accountID name
    let oldRules, oldDenies =
      match Map.tryFind key store.approvals with
      | Some approval when
        approval.hash = hash && approval.fingerprint = fingerprint
        ->
        P.Policy.rules approval.policy
      | _ -> [], []
    let policy = P.Policy.create (List.distinct (oldRules @ rules)) oldDenies
    let approval =
      { hash = hash
        policy = policy
        fingerprint = fingerprint
        explicitRules = true }
    { store with approvals = Map.add key approval store.approvals })

// ── guest execution ───────────────────────────────────────────────────────────

/// Build a guest invocation from instance defaults and named approvals.
/// Explicit denies and the run policy bound every grant. Guest code cannot
/// administer policy.
let guestState
  (accountID : Option<Guid>)
  (runPolicy : P.Policy)
  (ownFns : List<RT.Hash>)
  (state : RT.ExecutionState)
  : RT.ExecutionState =
  let bundled = state.isBundledPackageFn
  let snapshot = get ()
  let fingerprint = lazy (builtinFingerprint state.fns.builtIn)
  let own = Set.ofList ownFns
  let lookup name (RT.Hash hash as fnHash) =
    // A user alias of bundled code still has its own named approval.
    let bundledName =
      name |> Option.forall (fun (n : string) -> n.StartsWith("Darklang."))
    if (bundledName && bundled fnHash) || Set.contains fnHash own then
      None
    else
      let policy =
        name
        |> Option.bind (fun name ->
          let key = scopedKey accountID name
          match Map.tryFind key snapshot.approvals with
          | Some approval when
            approval.hash = hash && approval.fingerprint = fingerprint.Value
            ->
            Some approval.policy
          | _ -> None)
      Some(policy |> Option.defaultValue P.Policy.denyAll)
  { state with
      accountID = accountID
      canManagePolicies = false
      packagePolicy = lookup
      permissionAccess = fun access -> access
      requestPermission = None }
  |> Execution.setInstancePolicy snapshot.instance
  |> Execution.restrictRun runPolicy

/// Test runs have an allow-all instance boundary without changing the saved
/// installation policy. Restrictions captured by a caller still apply.
let testState
  (accountID : Option<Guid>)
  (state : RT.ExecutionState)
  : RT.ExecutionState =
  guestState accountID P.Policy.allowAll [] state
  |> Execution.setInstancePolicy P.Policy.allowAll

/// Disposable test workers and any CLI children they start use the same test
/// instance policy. Only this serialized copy is widened; never write it back.
let testWorkerStore () : byte[] =
  { get () with instance = P.Policy.allowAll } |> toBytes

/// The hash a named applicable calls, for the approval root. A lambda or a builtin has none.
let rootOf (applicable : RT.Applicable) : List<RT.Hash> =
  match applicable with
  | RT.AppNamedFn { name = RT.FQFnName.Package hash } -> [ hash ]
  | _ -> []

/// The state a package function runs under when it is the approval root of its own run (a
/// handler a server calls, a view a host runs, a function the editor re-runs for live values):
/// explicit instance denies and `invokerAccess` remain binding. With no root (a
/// lambda) the state is the caller's.
let rootState
  (state : RT.ExecutionState)
  (invokerAccess : P.Access)
  (roots : List<RT.Hash>)
  : RT.ExecutionState =
  match roots with
  | [] -> state
  | _ ->
    let guest = guestState state.accountID P.Policy.allowAll [] state
    { guest with access = guest.access |> P.Access.constrainBy invokerAccess }
