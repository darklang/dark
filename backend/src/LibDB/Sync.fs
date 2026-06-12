/// The sync receiver/sender API — the cohesive module the Dark HTTP
/// handlers call, tying together the proven primitives (opsSince + insertAndApplyOps +
/// sync cursors) into the two wire operations.
///
/// A remote op and a local op are the same thing: the receiver replays through the existing
/// `insertAndApplyOps` path (idempotent — `INSERT OR IGNORE INTO package_ops`), then advances
/// this remote's cursor so the next poll resumes after the batch.
module LibDB.Sync

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module BS = LibSerialization.Binary.Serialization
module PMBlob = LibDB.RuntimeTypes.Blob

// ── Wire codec (the HTTP/Tailscale transport) ─────────────────────────────────────────
// The HTTP body for `GET /sync/events` is a batch of `(rowid, id, op_blob)` rows. `op_blob` is
// already the serialized `PackageOp`, so the wire format is just self-describing binary framing
// over the existing bytes — no new per-op serializer. Same bytes the file-based pull
// reads; only the carrier differs. Encode on the server, decode on the client, then apply
// through the identical `pull`/`insertAndApplyOps` path.

/// The sync wire-format version — the leading int32 of every batch. The op encoding is the
/// serialized `PackageOp` (BUMP this when that, or this framing, changes incompatibly). A peer on a
/// different version produces a different leading int32, so `decodeBatch` rejects it at decode
/// (fail-closed) rather than misreading — a cheap op-format-stability guard, in the
/// wire itself instead of a separate `/whoami` handshake. (Free to add now: no sync is deployed.)
// v2: each op now carries its `origin_ts` ADJACENT to the op (a wire field, never inside the op —
// that would change its content hash). v1 peers (no origin_ts) are rejected by the version gate.
// v3: MEANING-STABLE HASHING — content hashes are now over the alpha-normalized canonical form
// (bound-variable names no longer affect a hash). Every package item's hash changed, so a v2 peer and
// a v3 peer are incompatible; the Release gate pauses sync between them. Crossing v2→v3 is a clean
// break (pre-v3 data is disposable; the store rebuilds from source / re-pulls from a v3 peer) — see
// `Releases` Release 3.
let wireFormatVersion : int = 3

/// Encode an op batch to a single self-describing byte buffer: int32 version, int32 count, then per
/// op `rowid:int64 · id:16 bytes · origin_ts:utf8-string · len:int32 · op_blob:len bytes`.
let encodeBatch (ops : List<int64 * System.Guid * string * byte[]>) : byte[] =
  use ms = new System.IO.MemoryStream()
  use w = new System.IO.BinaryWriter(ms)
  w.Write(wireFormatVersion)
  w.Write(List.length ops)
  for (rowid, id, originTs, blob) in ops do
    w.Write(rowid)
    w.Write(id.ToByteArray())
    w.Write(originTs) // BinaryWriter.Write(string) is length-prefixed UTF-8
    w.Write(blob.Length)
    w.Write(blob)
  w.Flush()
  ms.ToArray()

/// Decode a wire buffer produced by `encodeBatch` back to the op batch (inverse of `encodeBatch`).
let decodeBatch (bytes : byte[]) : List<int64 * System.Guid * string * byte[]> =
  use ms = new System.IO.MemoryStream(bytes)
  use r = new System.IO.BinaryReader(ms)
  // version first — a mismatch means the peer speaks a different op encoding; reject rather than
  // misread (silent corruption). Fail-closed: the pull errors + the operator learns to upgrade.
  let version = r.ReadInt32()
  if version <> wireFormatVersion then
    Exception.raiseInternal
      $"decodeBatch: wire-format version mismatch — peer sent {version}, this instance speaks {wireFormatVersion}. A peer on a different Dark version; upgrade to sync."
      []
  let count = r.ReadInt32()
  [ for _ in 1..count do
      let rowid = r.ReadInt64()
      let id = System.Guid(r.ReadBytes 16)
      let originTs = r.ReadString() // inverse of BinaryWriter.Write(string)
      let len = r.ReadInt32()
      let blob = r.ReadBytes len
      // BinaryReader.ReadBytes returns FEWER bytes at EOF WITHOUT throwing — so a wire buffer
      // truncated mid-blob (a connection dropped mid-response over the tailnet) would silently
      // yield a partial, corrupt op. Guard it: fail loudly so the pull errors + retries on the
      // next poll instead of applying garbage. (Fixed-width fields above already throw at EOF.)
      if blob.Length <> len then
        Exception.raiseInternal
          $"decodeBatch: truncated wire buffer — expected {len}-byte op blob, got {blob.Length}"
          []
      yield (rowid, id, originTs, blob) ]

/// The sender read (`GET /sync/events?since=cursor`): the COMMITTED ops the puller hasn't seen, as
/// `(commitRowid, id, origin_ts, op_blob)` — `op_blob` + the commit-rowid cursor + the authoring
/// stamp the receiver needs for timestamp-LWW (carried adjacent, not inside the op).
///
/// **Sync ships committed work only** (the unit of sync is the commit; WIP stays local — see
/// `opsSinceCommitted`). A peer never receives your uncommitted mid-edit state; it converges on the
/// history you've chosen to publish by committing.
let opsToSend (cursor : int64) : Task<List<int64 * System.Guid * string * byte[]>> =
  Inserts.opsSinceCommitted cursor


// ── the RESOLUTION channel: synced decisions ride alongside the op log (their own rowid cursor) ──

/// Encode a resolution batch: int32 version, int32 count, then per resolution `rowid:int64` followed by
/// the fields as length-prefixed UTF-8 strings (id, owner, modules, name, item_type, chosen_hash,
/// resolved_by, branch_id, at). The resolution-channel analogue of `encodeBatch`.
let encodeResolutions (rs : List<int64 * Resolutions.Resolution>) : byte[] =
  use ms = new System.IO.MemoryStream()
  use w = new System.IO.BinaryWriter(ms)
  w.Write(wireFormatVersion)
  w.Write(List.length rs)
  for (rowid, r) in rs do
    w.Write(rowid)
    w.Write(r.id)
    w.Write(r.location.owner)
    w.Write(String.concat "." r.location.modules)
    w.Write(r.location.name)
    w.Write(r.itemKind.toString ())
    w.Write(r.chosenHash)
    w.Write(r.resolvedBy)
    w.Write(string r.branchId)
    w.Write(r.at)
  w.Flush()
  ms.ToArray()

/// Decode a resolution wire buffer (inverse of `encodeResolutions`).
let decodeResolutions (bytes : byte[]) : List<int64 * Resolutions.Resolution> =
  use ms = new System.IO.MemoryStream(bytes)
  use r = new System.IO.BinaryReader(ms)
  let version = r.ReadInt32()
  if version <> wireFormatVersion then
    Exception.raiseInternal
      $"decodeResolutions: wire-format version mismatch — peer sent {version}, this instance speaks {wireFormatVersion}. Upgrade to sync."
      []
  let count = r.ReadInt32()
  [ for _ in 1..count do
      let rowid = r.ReadInt64()
      let id = r.ReadString()
      let owner = r.ReadString()
      let modules = r.ReadString()
      let name = r.ReadString()
      let itemType = r.ReadString()
      let chosenHash = r.ReadString()
      let resolvedBy = r.ReadString()
      let branchId = r.ReadString()
      let at = r.ReadString()
      yield
        (rowid,
         ({ id = id
            location =
              { owner = owner
                modules = (if modules = "" then [] else String.split "." modules)
                name = name }
            itemKind = PT.ItemKind.fromString itemType
            chosenHash = chosenHash
            resolvedBy = resolvedBy
            branchId = System.Guid.Parse branchId
            at = at }
         : Resolutions.Resolution)) ]

/// Apply a peer's resolutions: record each (idempotent by id) + fold it into `locations` (the overlay,
/// LWW by `at`), then advance this peer's resolution cursor. Returns the new cursor (max rowid applied,
/// or the existing cursor if the batch is empty).
let applyRemoteResolutions
  (remote : string)
  (rs : List<int64 * Resolutions.Resolution>)
  : Task<int64> =
  task {
    match rs with
    | [] -> return! SyncCursors.resolutionCursorFor remote
    | _ ->
      for (_, r) in rs do
        do! Resolutions.recordAndApply r
      let maxRowid = rs |> List.map fst |> List.max
      do! SyncCursors.advanceResolutionCursor remote maxRowid
      return maxRowid
  }

/// Render a `PackageLocation` as the FQ "owner[.modules].name" string sync uses on the wire and in
/// the conflict store — the inverse of `parseLocation`.
let private formatLocation (loc : PT.PackageLocation) : string =
  let modulesStr = String.concat "." loc.modules
  if modulesStr = "" then
    $"{loc.owner}.{loc.name}"
  else
    $"{loc.owner}.{modulesStr}.{loc.name}"

/// Parse an FQ "owner[.modules].name" location back into a `PackageLocation` (owner = head,
/// name = last, modules = the middle) — the inverse of `formatLocation`.
let private parseLocation (location : string) : Option<PT.PackageLocation> =
  match location.Split('.') |> List.ofArray with
  | owner :: rest ->
    match List.rev rest with
    | name :: revModules ->
      Some { owner = owner; modules = List.rev revModules; name = name }
    | [] -> None
  | _ -> None

/// Detect sync divergences in a remote batch BEFORE applying it. For each incoming
/// `SetName`, if the location is already bound LOCALLY to a *different*, non-deprecated hash,
/// two peers gave the same name different content. Returns `(location, existingHash,
/// incomingHash)` per divergence — surfaced as **data** so the receiver never blocks; a higher
/// layer (`routeDivergences`) turns these into a `PT.SyncConflict.Divergence` for the sync policy.
/// `divergentBindings` is the core (it returns the structured location + hashes); `detectDivergences`
/// just renders the location to its FQ string.
let private divergentBindings
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageLocation * string * string>> =
  task {
    let result = ResizeArray<PT.PackageLocation * string * string>()
    for op in ops do
      match op with
      | PT.PackageOp.SetName(loc, target) ->
        let modulesStr = String.concat "." loc.modules
        let (PT.Hash incomingHash) = target.hash
        // the current (non-superseded) binding at this location, if any
        let! existing =
          Sql.query
            """
            SELECT item_hash FROM locations
            WHERE owner = @owner AND modules = @modules AND name = @name
              AND branch_id = @branch_id AND unlisted_at IS NULL
            LIMIT 1
            """
          |> Sql.parameters
            [ "owner", Sql.string loc.owner
              "modules", Sql.string modulesStr
              "name", Sql.string loc.name
              "branch_id", Sql.uuid branchId ]
          |> Sql.executeAsync (fun read -> read.string "item_hash")
        match existing with
        | existingHash :: _ when existingHash <> incomingHash ->
          result.Add((loc, existingHash, incomingHash))
        | _ -> ()
      | _ -> ()
    return List.ofSeq result
  }

let detectDivergences
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<string * string * string>> =
  task {
    let! triples = divergentBindings branchId ops
    return
      triples
      |> List.map (fun (loc, existingHash, incomingHash) ->
        (formatLocation loc, existingHash, incomingHash))
  }


/// The receiver apply (`POST /sync/events`): insert + apply a remote batch via the existing
/// playback path (idempotent), then advance this remote's cursor to the **max sender-rowid**
/// in the batch so the next poll resumes after it. Returns the new cursor. An empty batch is a
/// no-op (cursor unchanged). The `rowid` paired with each op is the *sender's* rowid (from its
/// `opsSince`), which is what the cursor tracks for resuming the poll.
/// Record each auto-resolved divergence in the conflict store so it's REVIEWABLE (`dark conflicts`)
/// rather than silently lost. The auto-resolution is last-writer-wins — the incoming bind has just
/// applied — so we log `(location, what-we-had, what-they-sent, "auto: last-writer-wins")`. The user
/// usually acknowledges; occasionally overrides (`dark conflicts resolve … mine`).
/// The live binding hash at an FQ "owner[.modules].name" location, if any. Parses the FQ string the
/// way `resolveConflict` does (owner = head, name = last, modules = the middle).
// NOTE: reads the live binding ACROSS branches (no `branch_id` filter). Correct while sync is
// main-only — the same FQN is live on at most one branch — but when cross-branch sync lands this must
// take the divergence's `branch_id` so it logs the right branch's outcome. `ORDER BY rowid DESC` keeps
// the `LIMIT 1` deterministic (newest row) in the meantime.
let private liveBindingHash (location : string) : Task<Option<string>> =
  task {
    match parseLocation location with
    | Some loc ->
      let! rows =
        Sql.query
          """
          SELECT item_hash FROM locations
          WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL
          ORDER BY rowid DESC
          LIMIT 1
          """
        |> Sql.parameters
          [ "o", Sql.string loc.owner
            "m", Sql.string (String.concat "." loc.modules)
            "n", Sql.string loc.name ]
        |> Sql.executeAsync (fun read -> read.string "item_hash")
      return List.tryHead rows
    | None -> return None
  }

let recordDivergences
  (remote : string)
  (divergences : List<string * string * string>)
  : Task<unit> =
  task {
    for (location, localHash, incomingHash) in divergences do
      // record the ACTUAL outcome after timestamp-LWW (the fold may have kept the local op if the
      // incoming was older-by-creation) — read the live binding to get the chosen winner.
      let! winner = liveBindingHash location
      let chosenHash = winner |> Option.defaultValue incomingHash
      do!
        Conflicts.record
          remote
          location
          localHash
          incomingHash
          chosenHash
          "auto:last-writer-wins"
  }


// ── daemon telemetry (local, not synced): one row per autosync poll cycle ──

/// Record one autosync cycle's outcome, then trim the table to the most recent 500 rows so this
/// telemetry stays bounded (the daemon polls every few seconds to a minute, forever).
let recordDaemonEvent
  (peersPolled : int)
  (changed : bool)
  (conflicts : int)
  (skews : int)
  : Task<unit> =
  task {
    do!
      Sql.query
        """
        INSERT INTO sync_daemon_events (peers_polled, changed, conflicts, skews)
        VALUES (@p, @c, @x, @s)
        """
      |> Sql.parameters
        [ "p", Sql.int peersPolled
          "c", Sql.int (if changed then 1 else 0)
          "x", Sql.int conflicts
          "s", Sql.int skews ]
      |> Sql.executeStatementAsync
    do!
      Sql.query
        """
        DELETE FROM sync_daemon_events
        WHERE id NOT IN (SELECT id FROM sync_daemon_events ORDER BY id DESC LIMIT 500)
        """
      |> Sql.executeStatementAsync
  }

/// The most recent `limit` daemon cycles, newest first — `(at, peersPolled, changed, conflicts, skews)`.
let recentDaemonEvents (limit : int) : Task<List<string * int * int * int * int>> =
  Sql.query
    """
    SELECT at, peers_polled, changed, conflicts, skews
    FROM sync_daemon_events ORDER BY id DESC LIMIT @n
    """
  |> Sql.parameters [ "n", Sql.int limit ]
  |> Sql.executeAsync (fun read ->
    (read.string "at",
     read.int "peers_polled",
     read.int "changed",
     read.int "conflicts",
     read.int "skews"))


/// The item kind of a content hash — needed to rebuild a `SetName` Reference for a keep-local
/// reconcile (the transport surfaced the divergence as data, so the original op isn't retained).
/// Reads `locations.item_type` first — that's the kind of the very binding we're restoring, present
/// even for a binding the incoming since superseded (its row is unlisted, not gone) — then falls
/// back to the projection tables. None if the hash is unknown to all of them.
let private kindOfHash (hash : string) : Task<Option<PT.ItemKind>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT item_type AS k FROM locations WHERE item_hash = @h
        UNION ALL SELECT 'fn'    FROM package_functions WHERE hash = @h
        UNION ALL SELECT 'type'  FROM package_types     WHERE hash = @h
        UNION ALL SELECT 'value' FROM package_values    WHERE hash = @h
        LIMIT 1
        """
      |> Sql.parameters [ "h", Sql.string hash ]
      |> Sql.executeAsync (fun read -> read.string "k")
    return rows |> List.tryHead |> Option.map PT.ItemKind.fromString
  }

/// Re-bind a location to OUR hash as a deliberate override ("keep mine"). Shared by the automatic
/// keep-local policy (`routeDivergences`) and the human 'mine' override (`resolveConflict`).
///
/// It records a `Resolution` — a synced decision that overrides the op-fold for this location — and
/// applies it immediately (re-binds locally now). The resolution's fresh `at` stamp is the newest, so it
/// wins timestamp-LWW; and because resolutions sync on their own channel (rowid-cursored, not gated by
/// commit), a re-pulling peer re-adopts our choice. This is what the old `OverrideName` op did, without
/// minting an op to dodge the content-hash collision — the override is a resolution, not new content.
let private overrideBinding
  (branchId : PT.BranchId)
  (loc : PT.PackageLocation)
  (target : PT.Reference)
  (resolvedBy : string)
  : Task<unit> =
  let at = System.DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
  Resolutions.recordAndApply (Resolutions.mk loc target resolvedBy branchId at)

/// A sync policy's verdict on a `SyncConflict`: accept the convergent last-writer-wins outcome the
/// fold already applied, or override the location to a specific reference. `OverrideTo` the LOCAL
/// reference is the "keep mine" move — it records a propagating `Resolution`; `OverrideTo` the
/// incoming (or anything already applied) is a no-op.
type SyncPolicyChoice =
  | AcceptLww
  | OverrideTo of PT.Reference

/// How sync conflicts are decided: given a surfaced conflict + the execution `CallContext`, pick an
/// outcome. Pure (the shipped LWW policy needs no IO); `CallContext` is passed for future policies
/// that branch on it (e.g. per-branch rules).
type SyncPolicy = PT.SyncConflict -> RT.CallContext -> SyncPolicyChoice

/// The shipped default: accept the last-writer-wins outcome the fold already applied — surface as
/// data, never block, pick no override. Swappable in tests / future config for keep-local or
/// ask-human policies.
let defaultSyncPolicy : SyncPolicy = fun _conflict _ctx -> AcceptLww

/// Route each detected divergence through the **sync policy**. This is the "higher layer" the
/// transport defers to: the receiver surfaces each `name → two hashes` divergence as data (never
/// blocks); HERE it becomes a first-class `PT.SyncConflict.Divergence` the policy resolves —
///   - `AcceptLww` (the default) → no reconciling op: the divergence stays surfaced and the
///     timestamp-LWW outcome the fold already applied stands. Behaviorally unchanged.
///   - `OverrideTo localRef` → KEEP LOCAL: record + apply a reconciling `Resolution` re-binding the
///     location to our hash (a synced decision that also propagates to peers, like a human
///     override), and mark the recorded conflict overridden.
///   - `OverrideTo` the incoming ref / anything else → no-op: that bind already applied.
/// `branchId` is the branch the reconcile op is written to (the receiver's current branch — sync
/// divergences are name bindings, applied on the branch the puller is on). Returns the number of
/// divergences the policy actively reconciled (0 under the default).
let routeDivergences
  (policy : SyncPolicy)
  (callCtx : RT.CallContext)
  (remote : string)
  (branchId : PT.BranchId)
  (divergences : List<string * string * string>)
  : Task<int> =
  task {
    let mutable reconciled = 0
    for (location, existingHash, incomingHash) in divergences do
      // Rebuild the contending references so the conflict is first-class data. Both contend for the
      // same location, hence the same kind; `kindOfHash` reads it from the binding we're restoring.
      match! kindOfHash existingHash with
      | Some kind ->
        match parseLocation location with
        | Some loc ->
          let localRef = PT.Reference.fromHashAndKind (PT.Hash existingHash, kind)
          let incomingRef = PT.Reference.fromHashAndKind (PT.Hash incomingHash, kind)
          let conflict = PT.SyncConflict.Divergence(loc, [ localRef; incomingRef ])
          match policy conflict callCtx with
          | OverrideTo target when target = localRef ->
            // keep local: re-bind the location to our hash via `overrideBinding` (the same move a
            // human 'mine' override makes in `resolveConflict`) — a synced `Resolution` that rides the
            // resolution channel so peers re-adopt our hash too — and mark the recorded conflict overridden.
            do! overrideBinding branchId loc target "auto:keep-local"
            do! Conflicts.markOverriddenByLocation remote location
            reconciled <- reconciled + 1
          | AcceptLww
          | OverrideTo _ -> () // accept LWW, or override-to-incoming (already applied) → no new op
        | None -> ()
      | None -> ()
    return reconciled
  }

let applyRemoteOps
  (remote : string)
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (opsWithRowids : List<int64 * string * PT.PackageOp>)
  : Task<int64 * List<string * string * string>> =
  task {
    match opsWithRowids with
    | [] ->
      let! cursor = SyncCursors.cursorFor remote
      return (cursor, [])
    | _ ->
      let ops = opsWithRowids |> List.map (fun (_, _, op) -> op)
      // detect divergences BEFORE applying (checks current local bindings) — same as `pull`;
      // surfaced as data, never blocks the apply.
      let! divergences = detectDivergences branchId ops
      // carry the peer's origin_ts (adjacent, from the wire) into the fold so playback orders the
      // binding by CREATION time, not arrival — the HTTP path now matches the file pull.
      let originTsMap =
        opsWithRowids
        |> List.map (fun (_, originTs, op) -> (Inserts.computeOpHash op, originTs))
        |> Map.ofList
      let! _ =
        Inserts.insertAndApplyOpsWithOrigin branchId commitHash ops originTsMap
      let maxRowid =
        opsWithRowids |> List.map (fun (rowid, _, _) -> rowid) |> List.max
      do! SyncCursors.advanceCursor remote maxRowid
      // record the auto-resolved divergences (recordDivergences reads the live binding so it logs
      // the actual timestamp-LWW outcome) — reviewable, not silently lost
      do! recordDivergences remote divergences
      return (maxRowid, divergences)
  }

/// The file-based pull: PULL another instance's ops into THIS instance by reading the peer's
/// op log directly (a second `data.db`, opened read-only) — no wire. Reads the source ops with
/// `rowid > sinceCursor` (ascending) and applies each through the **same `insertAndApplyOps`
/// the wire receiver uses** — so the local op LOG is written (making us a re-serving peer) AND
/// projections fold, idempotently (`INSERT OR IGNORE` by op-id). Returns the new cursor (max
/// source rowid applied, or `sinceCursor` if none).
///
/// Ops are grouped by `(branchId, commitHash)` — `insertAndApplyOps` takes one of each per
/// batch — preserving source-rowid order within each group. This is the engine of
/// `dark sync pull <other-data.db>`; the HTTP transport only swaps the source connection for a wire
/// body, applied through this same path.
let pull
  (sourceConnStr : string)
  (sinceCursor : int64)
  : Task<int64 * List<string * string * string>> =
  task {
    // read the source's ops above the cursor, into memory, then close the reader (the apply
    // path uses the local global connection, independent of this source connection).
    use source = new Microsoft.Data.Sqlite.SqliteConnection(sourceConnStr)
    source.Open()
    // The peer's authoring stamp travels ADJACENT to the op (a column), never inside it — putting
    // it in the op would change the content hash and break idempotent dedup. `origin_ts` is part of
    // the `package_ops` schema (the migration guarantees it), so we read it directly.
    // Committed ops only, cursored on the COMMIT's rowid (see `Inserts.opsSinceCommitted`): the unit
    // of sync is the commit, and the commit-rowid is monotonic at commit-creation so a later commit
    // never sorts before one already pulled. WIP (commit_hash IS NULL) stays local.
    use cmd = source.CreateCommand()
    cmd.CommandText <-
      "SELECT c.rowid AS crowid, po.id, po.op_blob, po.branch_id, po.commit_hash, po.origin_ts "
      + "FROM package_ops po JOIN commits c ON po.commit_hash = c.hash "
      + "WHERE c.rowid > $cursor ORDER BY c.rowid ASC, po.rowid ASC"
    cmd.Parameters.AddWithValue("$cursor", sinceCursor)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>

    // each row carries the peer's origin_ts so it travels with the op (timestamp-LWW: portable
    // authoring time, preserved on receive — not re-stamped to pull-time).
    let rows =
      ResizeArray<int64 *
      System.Guid *
      byte[] *
      System.Guid *
      Option<string> *
      string>()
    use reader = cmd.ExecuteReader()
    while reader.Read() do
      let commitHash = if reader.IsDBNull 4 then None else Some(reader.GetString 4)
      rows.Add(
        reader.GetInt64 0,
        System.Guid.Parse(reader.GetString 1),
        reader.GetFieldValue<byte[]> 2,
        System.Guid.Parse(reader.GetString 3),
        commitHash,
        reader.GetString 5
      )
    reader.Close()

    let rowList = List.ofSeq rows

    // apply per distinct (branchId, commitHash) batch through the existing receiver path —
    // writes the canonical op log + folds projections, idempotent by op-id. (`rowList` is in
    // rowid order, so each filtered batch preserves it.)
    let batches =
      rowList
      |> List.map (fun (_, _, _, branchId, commitHash, _) -> (branchId, commitHash))
      |> List.distinct
    let divergences = ResizeArray<string * string * string>()
    for (branchId, commitHash) in batches do
      let batchRows =
        rowList
        |> List.filter (fun (_, _, _, b, c, _) -> b = branchId && c = commitHash)
      let ops =
        batchRows
        |> List.map (fun (_, id, blob, _, _, _) ->
          BS.PT.PackageOp.deserialize id blob)
      // surface divergences BEFORE applying — `detectDivergences` checks the *current* local
      // binding, so it must run before `insertAndApplyOps` rebinds (last-writer). Surfaced as
      // data; never blocks the apply (divergence is data, resolved later, never blocking the apply).
      let! batchDivs = detectDivergences branchId ops
      divergences.AddRange batchDivs
      // insert + fold with the peer's ORIGIN timestamp per op (vs the local-insert default), so the
      // op is in package_ops with its TRUE creation time BEFORE playback folds it — that's what lets
      // `applySetName` order this binding by creation, not arrival, and skip a stale (older-created,
      // late-arriving) SetName. Every instance agrees on each op's origin_ts → all converge.
      let originTsMap =
        batchRows |> List.map (fun (_, id, _, _, _, ts) -> (id, ts)) |> Map.ofList
      let! _ =
        Inserts.insertAndApplyOpsWithOrigin branchId commitHash ops originTsMap
      ()

    let cursor =
      rowList
      |> List.fold (fun acc (rowid, _, _, _, _, _) -> max acc rowid) sinceCursor
    return (cursor, List.ofSeq divergences)
  }

/// Fetch the content blobs a peer has that we lack — the fetch-on-miss half of sync, since
/// `package_blobs` (large/binary content) don't ride the op stream. Reads the peer's blob
/// hashes, keeps the ones `Blob.missing` says we don't have, and copies their bytes over
/// (content-addressed `INSERT OR IGNORE`, so idempotent). Returns the count fetched.
let private pullBlobsFromStore (sourceConnStr : string) : Task<int> =
  task {
    use source = new Microsoft.Data.Sqlite.SqliteConnection(sourceConnStr)
    source.Open()

    use hashCmd = source.CreateCommand()
    hashCmd.CommandText <- "SELECT hash FROM package_blobs"
    let peerHashes = ResizeArray<string>()
    use reader = hashCmd.ExecuteReader()
    while reader.Read() do
      peerHashes.Add(reader.GetString 0)
    reader.Close()

    let! missing = PMBlob.missing (List.ofSeq peerHashes) |> Ply.toTask

    let mutable fetched = 0
    for h in missing do
      use bytesCmd = source.CreateCommand()
      bytesCmd.CommandText <- "SELECT bytes FROM package_blobs WHERE hash = $h"
      bytesCmd.Parameters.AddWithValue("$h", h)
      |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
      let result = bytesCmd.ExecuteScalar()
      if not (isNull result) then
        do! PMBlob.insert h (result :?> byte[]) |> Ply.toTask
        fetched <- fetched + 1
    return fetched
  }

/// Pull a file peer's RESOLUTIONS (the synced override decisions) into this instance, on the
/// resolution channel: read the rows above our resolution-cursor for this peer, record + fold each
/// (the overlay, LWW by `at`), and advance the cursor. Mirrors `pullBlobsFromStore` for resolutions.
/// Tolerates a peer with no `resolutions` table (an older store / a minimal test db) → nothing to pull.
let private pullResolutionsFromStore
  (sourcePath : string)
  (sourceConnStr : string)
  : Task<int> =
  task {
    use source = new Microsoft.Data.Sqlite.SqliteConnection(sourceConnStr)
    source.Open()

    use existsCmd = source.CreateCommand()
    existsCmd.CommandText <-
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'resolutions'"
    if isNull (existsCmd.ExecuteScalar()) then
      return 0
    else
      let! cursor = SyncCursors.resolutionCursorFor sourcePath
      use cmd = source.CreateCommand()
      cmd.CommandText <-
        "SELECT rowid, id, owner, modules, name, item_type, chosen_hash, resolved_by, branch_id, at "
        + "FROM resolutions WHERE rowid > $cursor ORDER BY rowid ASC"
      cmd.Parameters.AddWithValue("$cursor", cursor)
      |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
      let rows = ResizeArray<int64 * Resolutions.Resolution>()
      use reader = cmd.ExecuteReader()
      while reader.Read() do
        let modules = reader.GetString 3
        rows.Add(
          reader.GetInt64 0,
          { id = reader.GetString 1
            location =
              { owner = reader.GetString 2
                modules = (if modules = "" then [] else String.split "." modules)
                name = reader.GetString 4 }
            itemKind = PT.ItemKind.fromString (reader.GetString 5)
            chosenHash = reader.GetString 6
            resolvedBy = reader.GetString 7
            branchId = System.Guid.Parse(reader.GetString 8)
            at = reader.GetString 9 }
        )
      reader.Close()
      let rowList = List.ofSeq rows
      let! _ = applyRemoteResolutions sourcePath rowList
      return List.length rowList
  }

/// `dark sync pull <other-data.db>`, F# half: resume from the stored cursor for this peer,
/// `pull` its new ops into the local instance, fetch any content blobs we're missing, apply any
/// resolutions it has, then persist the advanced cursor — so the next pull resumes where this left
/// off. The peer key is the source path. Returns the new cursor.
let pullFromFile
  (sourcePath : string)
  : Task<int64 * List<string * string * string>> =
  task {
    let connStr = $"Data Source={sourcePath};Mode=ReadOnly"
    let! cursor = SyncCursors.cursorFor sourcePath
    let! (newCursor, divergences) = pull connStr cursor
    let! _blobsFetched = pullBlobsFromStore connStr
    let! _resolutionsApplied = pullResolutionsFromStore sourcePath connStr
    do! SyncCursors.advanceCursor sourcePath newCursor
    // auto-resolved (last-writer-wins) — record so it's reviewable, not silently lost
    do! recordDivergences sourcePath divergences
    return (newCursor, divergences)
  }

/// The display kinds of the ops a peer has above `sinceCursor` — feeds `dark sync pull`'s breakdown.
/// Reads the source (read-only) and, for each `SetName`, returns its target item kind
/// ("fn"/"type"/"value"). Counting the naming SetName — each item is named exactly once — gives the
/// per-kind item count with NO Add+SetName double-count and no pairing logic; non-naming ops are
/// omitted. File peers only (an HTTP wire batch isn't re-readable after the pull).
let opKindsSince (sourcePath : string) (sinceCursor : int64) : Task<List<string>> =
  task {
    let connStr = $"Data Source={sourcePath};Mode=ReadOnly"
    use source = new Microsoft.Data.Sqlite.SqliteConnection(connStr)
    source.Open()
    use cmd = source.CreateCommand()
    cmd.CommandText <-
      "SELECT id, op_blob FROM package_ops WHERE rowid > $cursor ORDER BY rowid ASC"
    cmd.Parameters.AddWithValue("$cursor", sinceCursor)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>

    let kinds = ResizeArray<string>()
    use reader = cmd.ExecuteReader()
    while reader.Read() do
      let id = System.Guid.Parse(reader.GetString 0)
      let blob = reader.GetFieldValue<byte[]> 1
      match BS.PT.PackageOp.deserialize id blob with
      | PT.PackageOp.SetName(_loc, target) -> kinds.Add(target.kind.toString ())
      | _ -> ()
    reader.Close()
    return List.ofSeq kinds
  }

/// The client apply-half of the HTTP transport: decode a wire batch (the body of a
/// `GET /sync/events` response, produced by `encodeBatch`) and apply it through the SAME receiver
/// path as the file pull and the wire POST — `applyRemoteOps` (op log + projections, idempotent),
/// advancing this peer's cursor. `branchId`/`commitHash` come from the request context (the
/// 3-field wire carries the ops; the protocol conveys branch + commit per request). Returns the
/// new cursor. So HTTP sync = `httpRequest` the body → `applyWireBatch` — the same fold, a wire
/// source instead of a file.
let applyWireBatch
  (remote : string)
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (wireBytes : byte[])
  : Task<int64 * List<string * string * string>> =
  task {
    // This F# receiver applies ops only — it does NOT fetch referenced content blobs. The blob
    // fetch is a SEPARATE step at the Dark layer: the CLI HTTP pull calls
    // `Darklang.Sync.fetchMissingBlobs` after this `applyWire` (GET /sync/blobs → pmBlobMissing →
    // GET /sync/blob?hash= + pmBlobInsert), the HTTP counterpart to the file pull's
    // `pullBlobsFromStore`. So the file/HTTP blob asymmetry is closed at the caller, not here.
    let opsWithRowids =
      decodeBatch wireBytes
      |> List.map (fun (rowid, id, originTs, blob) ->
        (rowid, originTs, BS.PT.PackageOp.deserialize id blob))
    return! applyRemoteOps remote branchId commitHash opsWithRowids
  }


/// Override a recorded conflict's auto-resolution. `keepMine` re-activates OUR hash at the location
/// by re-folding the `SetName` into projections (LOCAL — the op is already in the log, so this can't
/// create a new syncable op; propagating an override needs a distinguishing op, see the conflict
/// note); `theirs` keeps the incoming (which already won under last-writer-wins). Either way marks
/// the conflict overridden. Returns true if it was found + resolved.
let resolveConflict (conflictId : string) (keepMine : bool) : Task<bool> =
  task {
    match! Conflicts.getById conflictId with
    | None -> return false
    | Some c ->
      if not keepMine then
        // "theirs" — the incoming bind already applied (last-writer-wins); just record the choice
        do! Conflicts.markOverridden c.id
        return true
      else
        // "mine" — re-bind the location to our hash. Parse the FQ "owner[.modules].name", read the
        // binding's kind + branch from `locations`, then record a `Resolution` for our hash. A human
        // override is the LATEST decision, so `overrideBinding` makes it win timestamp-LWW
        // (last-resolver-wins) and — as a distinct op with a fresh rowid — rides sync so peers re-adopt it.
        match parseLocation c.location with
        | Some loc ->
          let modulesStr = String.concat "." loc.modules
          let! meta =
            Sql.query
              """
              SELECT item_type, branch_id FROM locations
              WHERE owner = @o AND modules = @m AND name = @n LIMIT 1
              """
            |> Sql.parameters
              [ "o", Sql.string loc.owner
                "m", Sql.string modulesStr
                "n", Sql.string loc.name ]
            |> Sql.executeAsync (fun read ->
              (read.string "item_type", (read.uuid "branch_id" : PT.BranchId)))
          match meta with
          | (itemType, branchId) :: _ ->
            let kind = PT.ItemKind.fromString itemType
            let target = PT.Reference.fromHashAndKind (PT.Hash c.localHash, kind)
            do! overrideBinding branchId loc target "human"
            do! Conflicts.markOverridden c.id
            return true
          | [] -> return false // the location no longer exists locally
        | None -> return false // unparseable location
  }
