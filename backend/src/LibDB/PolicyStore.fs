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

/// One approved root: the immutable dependency closure the approval
/// covered, and the builtin-effect fingerprint it was reviewed under. The
/// fingerprint is per-root, not global, so re-approving one root does not
/// silently mark every other root fresh (which would drop the re-review
/// signal for approvals that were never re-reviewed).
type RootApproval =
  {
    closure : Set<string>
    fingerprint : string
    /// The resource rules the consumer wrote for this root, when they did;
    /// `None` for an approval derived from the analyzed effect set. Kept so an
    /// `update` can carry a written policy forward and recompute a derived one,
    /// which cannot be told apart from the policy's shape alone.
    explicitPolicy : Option<P.Policy>
  }

type ScopedKey = Option<Guid> * string

type Store =
  { instance : P.Policy
    packages : Map<ScopedKey, P.Policy>
    // Account-scoped logical function name -> approved content hash. This is
    // the user's lock; hashes remain an implementation detail of the host
    // policy store.
    functionPins : Map<ScopedKey, string>
    // Account-scoped record of each approved *root* hash: the immutable
    // dependency closure the approval covered (revocation consults it so
    // dropping one root only removes dependencies no remaining root still
    // needs) and the per-root fingerprint (staleness consults it).
    approvedRoots : Map<ScopedKey, RootApproval> }

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

/// Versioned policy format. Older versions are rejected and handled by startup
/// migration, so bytes are never silently reinterpreted.
let formatVersion = 9

let private fileName = "policies.bin"

let empty : Store =
  { instance = P.Policy.denyAll
    packages = Map.empty
    functionPins = Map.empty
    approvedRoots = Map.empty }

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

let private writeRootApproval (writer : BinaryWriter) (approval : RootApproval) =
  String.write writer approval.fingerprint
  List.write writer String.write (Set.toList approval.closure)
  match approval.explicitPolicy with
  | None -> writer.Write false
  | Some policy ->
    writer.Write true
    PolicyBin.write writer policy

let private readRootApproval (reader : BinaryReader) : RootApproval =
  let fingerprint = String.read reader
  let closure =
    List.read reader (fun r -> readIdentifier r "approved-root closure hash")
  let explicitPolicy =
    if reader.ReadBoolean() then Some(PolicyBin.read reader) else None
  { closure = Set.ofList closure
    fingerprint = fingerprint
    explicitPolicy = explicitPolicy }

let toBytes (store : Store) : byte[] =
  use stream = new MemoryStream()
  use writer = new BinaryWriter(stream)
  String.write writer magic
  writer.Write formatVersion
  PolicyBin.write writer store.instance
  writeEntries writer writeScopedKey PolicyBin.write store.packages
  writeEntries writer writeScopedKey String.write store.functionPins
  writeEntries writer writeScopedKey writeRootApproval store.approvedRoots
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
  let packages =
    readEntries reader "package" (readScopedKey "package") PolicyBin.read
  let functionPins =
    readEntries reader "function pin" (readScopedKey "function pin") (fun r ->
      readIdentifier r "function hash")
  let approvedRoots =
    readEntries
      reader
      "approved-root"
      (readScopedKey "approved-root")
      readRootApproval
  if stream.Position <> stream.Length then
    raiseFormatError "Trailing bytes in policy store"
  { instance = instance
    packages = packages
    functionPins = functionPins
    approvedRoots = approvedRoots }

/// The format version stamped in `bytes`, if it carries our magic. Used to
/// distinguish "an older version of our own format" (migratable) from foreign
/// bytes or corruption (left untouched).
let storedVersion (bytes : byte[]) : Option<int> =
  try
    use stream = new MemoryStream(bytes)
    use reader = new BinaryReader(stream)
    if String.read reader = magic then Some(reader.ReadInt32()) else None
  with _ ->
    None

// ── file access ───────────────────────────────────────────────────────────────

/// Parsed-store cache keyed on the file's (mtime, ctime, length) stamp.
/// Function resolution consults pins on every package-fn lookup; without this
/// each lookup is a full file read + parse. Same-process writes invalidate
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
/// write cannot replace approvals and pins with an empty store.
let private getForWrite () : Store =
  match LocalFile.read fileName with
  | LocalFile.Missing -> empty
  | LocalFile.Read bytes ->
    try
      fromBytes bytes
    with _ ->
      match storedVersion bytes with
      | Some version when version > formatVersion ->
        Exception.raiseInternal
          $"The policy file was written by a newer Darklang (format {version}, this build reads {formatVersion}) and will not be overwritten. Upgrade, or move ~/.darklang/policy/policies.bin aside."
          []
      | _ ->
        Exception.raiseInternal
          "The policy file is damaged and will not be overwritten. Move ~/.darklang/policy/policies.bin aside (approvals and pins in it are lost) and retry."
          []
  | LocalFile.Unreadable message ->
    Exception.raiseInternal
      $"The policy file exists but cannot be read ({message}); refusing to overwrite it."
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

// ── first-run seed and migration ──────────────────────────────────────────────

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
  /// Back up this older-format file, write deny-all at the current version,
  /// and stamp the marker.
  | BackUpAndReset of version : int

/// Pure classification behind [seedInstanceIfMissing], fail-closed on
/// deletion and version mismatch:
///
/// - **First install** (missing, no marker): write the usable default so a
///   fresh install works without opening it wide.
/// - **Missing file, marker present** (deleted/reset): leave it. A missing
///   file reads as deny-all, so deletion fails closed instead of silently
///   restoring the default — which could *widen* an operator policy that was
///   tighter than the default.
/// - **Older format**: back up and reset to **deny-all** at the current
///   version (not the permissive default); the operator re-applies grants
///   from the backup.
/// - **Current-version, foreign/corrupt, or unreadable file**: leave it (an
///   unparseable file already reads as deny-all).
let seedAction (file : LocalFile.ReadResult) (initialized : bool) : SeedAction =
  match file with
  | LocalFile.Missing when initialized -> SeedAction.Leave
  | LocalFile.Missing -> SeedAction.WriteDefault
  | LocalFile.Read bytes ->
    match storedVersion bytes with
    | Some v when v < formatVersion -> SeedAction.BackUpAndReset v
    | _ -> SeedAction.Leave
  | LocalFile.Unreadable _ -> SeedAction.Leave

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
        stampInitialized ()
      | SeedAction.BackUpAndReset version ->
        match file with
        | LocalFile.Read bytes ->
          LocalFile.writeAtomic $"{fileName}.v{version}.bak" bytes
        | _ -> ()
        writeFile empty // deny-all at the current version, never re-widen
        stampInitialized ())

// ── approval metadata fingerprint ─────────────────────────────────────────────
// The fingerprint records the builtin-effect classification used for each
// approval. A changed fingerprint marks that root for re-review; it is a
// signal, not an additional enforcement layer.

/// Pure staleness check behind [approvalsAreStale]. True when any approved
/// root's fingerprint differs from `current`. Per-root, so re-approving one
/// root leaves the others' staleness intact.
let approvalsAreStaleInStore (current : string) (store : Store) : bool =
  store.approvedRoots
  |> Map.exists (fun _ approval -> approval.fingerprint <> current)

/// True when any approved root was reviewed under a different builtin-effect
/// fingerprint than `current` — a re-review signal.
let approvalsAreStale (current : string) : bool =
  approvalsAreStaleInStore current (get ())

// ── reads ─────────────────────────────────────────────────────────────────────

let instancePolicy () : P.Policy = (get ()).instance

let packagePolicies (accountID : Option<Guid>) : Map<string, P.Policy> =
  scopedEntries accountID (get ()).packages

/// Return the pinned hash for a logical function, if the consumer has chosen one.
let pinnedFunction (accountID : Option<Guid>) (location : string) : Option<string> =
  (get ()).functionPins |> Map.tryFind (scopedKey accountID location)

let functionPins (accountID : Option<Guid>) : Map<string, string> =
  scopedEntries accountID (get ()).functionPins

// ── writes ────────────────────────────────────────────────────────────────────

let setInstancePolicy (policy : P.Policy) : unit =
  set (fun store -> { store with instance = policy })

/// Every hash some approved root of `accountID` other than `except` still
/// covers.
let private closuresOfOtherRoots
  (accountID : Option<Guid>)
  (except : ScopedKey)
  (store : Store)
  : Set<string> =
  scopedEntries accountID (Map.remove except store.approvedRoots)
  |> Map.toSeq
  |> Seq.collect (fun (_, approval) -> Set.toSeq approval.closure)
  |> Set.ofSeq

/// Remove one root's approval, its unshared dependency policies, and pins to
/// that root. Shared dependencies remain available to other approved roots.
let revokeRootInStore
  (accountID : Option<Guid>)
  (rootHash : string)
  (store : Store)
  : Store =
  let rootKey = scopedKey accountID rootHash
  match Map.tryFind rootKey store.approvedRoots with
  | None -> store
  | Some revoked ->
    let stillNeeded = closuresOfOtherRoots accountID rootKey store
    let packages =
      Set.difference revoked.closure stillNeeded
      |> Set.fold
        (fun pkgs h -> Map.remove (scopedKey accountID h) pkgs)
        store.packages
    let functionPins =
      store.functionPins
      |> Map.filterWithIndex (fun (key : ScopedKey) pinned ->
        not (fst key = accountID && pinned = rootHash))
    { store with
        packages = packages
        functionPins = functionPins
        approvedRoots = Map.remove rootKey store.approvedRoots }

/// Revoke one approved root, dependency-aware. See [revokeRootInStore].
let revokePackageRoot (accountID : Option<Guid>) (rootHash : string) : unit =
  set (revokeRootInStore accountID rootHash)

/// Install the policies and approval record for a root's complete closure.
/// Replacing an approval first removes dependencies no longer needed.
let recordApprovalInStore
  (accountID : Option<Guid>)
  (rootHash : string)
  (policies : List<string * P.Policy>)
  (fingerprint : string)
  (explicitPolicy : Option<P.Policy>)
  (store : Store)
  : Store =
  let store = revokeRootInStore accountID rootHash store
  // A separately approved root keeps its own policy; another root's closure
  // must not replace or widen it.
  let ownedByAnotherRoot (hash : string) : bool =
    hash <> rootHash
    && Map.containsKey (scopedKey accountID hash) store.approvedRoots
  let packages =
    policies
    |> List.fold
      (fun packages (hash, policy) ->
        if ownedByAnotherRoot hash then
          packages
        else
          Map.add (scopedKey accountID hash) policy packages)
      store.packages
  let approval =
    { closure = policies |> List.map fst |> Set.ofList
      fingerprint = fingerprint
      explicitPolicy = explicitPolicy }
  { store with
      packages = packages
      approvedRoots =
        store.approvedRoots |> Map.add (scopedKey accountID rootHash) approval }

/// Install an approval and move its pin only if the pin still matches the
/// reviewed version. A stale comparison changes nothing.
let recordApprovalAndMovePinInStore
  (accountID : Option<Guid>)
  (rootHash : string)
  (policies : List<string * P.Policy>)
  (fingerprint : string)
  (explicitPolicy : Option<P.Policy>)
  (location : string)
  (expectedCurrent : Option<string>)
  (hash : string)
  (store : Store)
  : Result<Store, string> =
  let key = scopedKey accountID location
  if Map.tryFind key store.functionPins <> expectedCurrent then
    Error
      $"the pin for {location} changed while this update was being reviewed; re-run the update"
  else
    let store =
      recordApprovalInStore
        accountID
        rootHash
        policies
        fingerprint
        explicitPolicy
        store
    Ok { store with functionPins = store.functionPins |> Map.add key hash }

/// Atomically approve a closure and move its logical-name pin. Locking keeps
/// the policies and pin consistent, and stale pin moves are rejected.
let recordApprovalAndMovePin
  (accountID : Option<Guid>)
  (rootHash : string)
  (policies : List<string * P.Policy>)
  (fingerprint : string)
  (explicitPolicy : Option<P.Policy>)
  (location : string)
  (expectedCurrent : Option<string>)
  (hash : string)
  : Result<unit, string> =
  nonBlank "rootHash" rootHash
  nonBlank "location" location
  nonBlank "hash" hash
  policies |> List.iter (fun (h, _) -> nonBlank "package hash" h)
  update (
    recordApprovalAndMovePinInStore
      accountID
      rootHash
      policies
      fingerprint
      explicitPolicy
      location
      expectedCurrent
      hash
  )


/// The rules the consumer wrote when approving `hash` as a root, if any.
let explicitApproval (accountID : Option<Guid>) (hash : string) : Option<P.Policy> =
  Map.tryFind (scopedKey accountID hash) (get ()).approvedRoots
  |> Option.bind (fun approval -> approval.explicitPolicy)

let unpinFunction (accountID : Option<Guid>) (location : string) : unit =
  set (fun store ->
    { store with
        functionPins =
          store.functionPins |> Map.remove (scopedKey accountID location) })

// ── guest execution ───────────────────────────────────────────────────────────

/// Build the execution state for guest code. The persisted instance policy is
/// the hard maximum; run, package, bundled-function, and requested-function
/// policies are layered beneath it. Policy administration is disabled.
let guestState
  (accountID : Option<Guid>)
  (runPolicy : P.Policy)
  (sessionAllow : List<P.Rule>)
  (ownFns : List<RT.Hash>)
  (state : RT.ExecutionState)
  : RT.ExecutionState =
  // Session-only rules widen this process's instance layer; they are not saved.
  let instance =
    match sessionAllow with
    | [] -> instancePolicy ()
    | extra ->
      let allow, deny = P.Policy.rules (instancePolicy ())
      P.Policy.create (allow @ extra) deny
  // Bundled membership is supplied by the host. Explicit approvals and the
  // script's own functions are added as normal package policies.
  let bundled = state.isBundledPackageFn
  let approved =
    packagePolicies accountID
    |> Map.fold (fun acc hash policy -> Map.add (RT.Hash hash) policy acc) Map.empty
  let approved =
    ownFns
    |> List.fold (fun acc hash -> Map.add hash P.Policy.allowAll acc) approved
  let lookup (hash : RT.Hash) =
    match Map.tryFind hash approved with
    | Some policy -> Some policy
    | None -> if bundled hash then Some P.Policy.allowAll else None
  { state with
      accountID = accountID
      canManagePolicies = false
      canUsePrivateNetworkHttp = false }
  |> Execution.setInstancePolicy instance
  |> Execution.restrictRun runPolicy
  |> Execution.setPackagePolicies lookup
