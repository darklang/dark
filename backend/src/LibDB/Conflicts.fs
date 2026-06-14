/// Sync conflicts — the recorded, reviewable log of auto-resolved name-binding divergences.
///
/// A conflict is recorded at PULL TIME, where it's the accurate signal: `detectDivergences` knows a
/// pulled `SetName` rebinds a name we'd locally bound to a *different* hash (incoming-vs-local). The
/// pull auto-resolves by policy (default last-writer-wins — the incoming bind already applied) AND
/// records the conflict here, so nothing is *silently* lost: it's raised to the user, who usually
/// acknowledges ("the auto thing was right") and occasionally overrides.
///
/// What's stored is the STRUCTURED conflict, not prose: a serialized `PT.SyncConflict` (the
/// `conflict_blob` — the candidates), the `chosen_hash` (which content won) + `resolved_by` (the
/// policy that picked it, e.g. `auto:last-writer-wins`, or `human`), and a `status` lifecycle
/// (`auto-resolved` → `acknowledged` | `overridden`). The display reconstructs everything from these
/// fields — no string parsing.
///
/// Why a recorded log, not a pure op-log projection: everyone's "main" shares the constant
/// `PT.mainBranchId`, so two competing edits are SAME-branch — the log can't distinguish "a peer
/// overwrote me" from "I re-edited." The pull is the one place that knows it was a sync, so the
/// record is captured there. (A true projection would need a per-op source/peer marker.)
///
/// Local-only, NOT synced. The `sync_conflicts` table lives in `backend/migrations/schema.sql`.
module LibDB.Conflicts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module Serialize = LibSerialization.Binary.Serialization

/// One recorded conflict. `conflict` is the deserialized `SyncConflict` (its candidates); `chosenHash`
/// + `resolvedBy` are the resolution (which content won, and the policy/human that chose it); `status` is
/// the review lifecycle (`auto-resolved` | `acknowledged` | `overridden`).
type Conflict =
  { id : string
    kind : string
    location : string
    conflict : PT.SyncConflict
    chosenHash : string
    resolvedBy : string
    remote : string
    status : string }

  /// The contending content hashes, ordered [local; incoming], read off the divergence's candidates
  /// (so callers needn't re-deserialize). "" for a candidate a future kind doesn't carry.
  member this.candidateHashes : string * string =
    match this.conflict with
    | PT.SyncConflict.Divergence(_, candidates) ->
      let hashOf (r : PT.Reference) =
        match r.hash with
        | PT.Hash h -> h
      match candidates with
      | a :: b :: _ -> (hashOf a, hashOf b)
      | [ a ] -> (hashOf a, "")
      | [] -> ("", "")

  /// The local (loser-by-default) hash — candidate 0.
  member this.localHash = fst this.candidateHashes
  /// The incoming (auto-resolved winner by default) hash — candidate 1.
  member this.incomingHash = snd this.candidateHashes
  /// Has the user handled this? (`acknowledged` or `overridden` both count as reviewed.)
  member this.acknowledged =
    this.status = "acknowledged" || this.status = "overridden"
  /// Did a deliberate override replace the auto-resolution?
  member this.overridden = this.status = "overridden"

// ── location parsing + kind lookup (to rebuild the structured conflict from raw hashes) ──

/// Parse "owner[.modules].name" → PackageLocation via the shared `PackageLocation.fromFQN`, with a
/// lenient fallback (a degenerate location named after the raw string) for a malformed key.
let private parseLoc (location : string) : PT.PackageLocation =
  PackageLocation.fromFQN location
  |> Option.defaultValue { owner = ""; modules = []; name = location }

/// The item kind bound at a location — needed to rebuild the candidate `Reference`s. Falls back to
/// `Fn` when the location isn't (or isn't yet) in `locations` (e.g. a synthetic test record); the
/// candidate hashes are preserved regardless, so the fallback is harmless.
let private itemKindFor (location : string) : Task<PT.ItemKind> =
  task {
    let loc = parseLoc location
    let! rows =
      Sql.query
        "SELECT item_type FROM locations WHERE owner=@o AND modules=@m AND name=@n LIMIT 1"
      |> Sql.parameters
        [ "o", Sql.string loc.owner
          "m", Sql.string (String.concat "." loc.modules)
          "n", Sql.string loc.name ]
      |> Sql.executeAsync (fun read -> read.string "item_type")
    return
      rows
      |> List.tryHead
      |> Option.map PT.ItemKind.fromString
      |> Option.defaultValue PT.ItemKind.Fn
  }

// ── record / read / resolve ──

/// Record an auto-resolved divergence: build the structured `SyncConflict` from the contending hashes,
/// store its blob + the chosen winner + the policy that chose it. Idempotent on the live (remote,
/// location, exact conflict): the same divergence re-detected on a re-pull doesn't pile up a duplicate
/// row (unless the prior one was already overridden — then a fresh divergence is worth recording).
let record
  (remote : string)
  (location : string)
  (localHash : string)
  (incomingHash : string)
  (chosenHash : string)
  (resolvedBy : string)
  : Task<unit> =
  task {
    let! kind = itemKindFor location
    let loc = parseLoc location
    let localRef = PT.Reference.fromHashAndKind (PT.Hash localHash, kind)
    let incomingRef = PT.Reference.fromHashAndKind (PT.Hash incomingHash, kind)
    let conflict = PT.SyncConflict.Divergence(loc, [ localRef; incomingRef ])
    let id = System.Guid.NewGuid() |> string
    let blob = Serialize.PT.SyncConflict.serialize id conflict

    // dedup on the exact serialized conflict (encodes location + both hashes), still un-overridden
    let! existing =
      Sql.query
        """
        SELECT id FROM sync_conflicts
        WHERE remote = @remote AND location = @loc AND conflict_blob = @blob
          AND status <> 'overridden'
        LIMIT 1
        """
      |> Sql.parameters
        [ "remote", Sql.string remote
          "loc", Sql.string location
          "blob", Sql.bytes blob ]
      |> Sql.executeAsync (fun read -> read.string "id")
    match existing with
    | _ :: _ -> ()
    | [] ->
      do!
        Sql.query
          """
          INSERT INTO sync_conflicts
            (id, kind, location, conflict_blob, chosen_hash, resolved_by, remote)
          VALUES (@id, 'divergence', @loc, @blob, @chosen, @by, @remote)
          """
        |> Sql.parameters
          [ "id", Sql.string id
            "loc", Sql.string location
            "blob", Sql.bytes blob
            "chosen", Sql.string chosenHash
            "by", Sql.string resolvedBy
            "remote", Sql.string remote ]
        |> Sql.executeStatementAsync
  }

/// All recorded conflicts, newest first — the `dark conflicts` surface.
let list () : Task<List<Conflict>> =
  task {
    return!
      Sql.query
        """
        SELECT id, kind, location, conflict_blob, chosen_hash, resolved_by, remote, status
        FROM sync_conflicts ORDER BY detected_at DESC
        """
      |> Sql.executeAsync (fun read ->
        let id = read.string "id"
        let conflict =
          Serialize.PT.SyncConflict.deserialize id (read.bytes "conflict_blob")
        { id = id
          kind = read.string "kind"
          location = read.string "location"
          conflict = conflict
          chosenHash = read.string "chosen_hash"
          resolvedBy = read.string "resolved_by"
          remote = read.string "remote"
          status = read.string "status" })
  }

/// The user agrees with the auto-resolution — mark it reviewed (the common case). Only moves an
/// `auto-resolved` row; an `overridden` one stays overridden.
let acknowledge (id : string) : Task<unit> =
  Sql.query
    "UPDATE sync_conflicts SET status = 'acknowledged' WHERE id = @id AND status = 'auto-resolved'"
  |> Sql.parameters [ "id", Sql.string id ]
  |> Sql.executeStatementAsync

/// Acknowledge ALL currently-pending conflicts at once — the bulk "yeah, the auto thing was right"
/// path. Returns how many were newly acknowledged.
let acknowledgeAll () : Task<int> =
  task {
    let! pending =
      Sql.query
        "SELECT COUNT(*) AS n FROM sync_conflicts WHERE status = 'auto-resolved'"
      |> Sql.executeAsync (fun read -> read.int64 "n")
    do!
      Sql.query
        "UPDATE sync_conflicts SET status = 'acknowledged' WHERE status = 'auto-resolved'"
      |> Sql.executeStatementAsync
    return
      (match pending with
       | n :: _ -> int n
       | [] -> 0)
  }

/// The user chose a different resolution (a reconciling op was emitted) — mark it overridden.
let markOverridden (id : string) : Task<unit> =
  Sql.query "UPDATE sync_conflicts SET status = 'overridden' WHERE id = @id"
  |> Sql.parameters [ "id", Sql.string id ]
  |> Sql.executeStatementAsync

/// Mark the most recent un-overridden conflict at a location overridden — used when a resolution
/// POLICY (not a human) keeps local: `Sync.routeDivergences` emitted a reconciling op, so the
/// recorded auto-LWW outcome no longer reflects the live binding. Keyed by remote + location.
let markOverriddenByLocation (remote : string) (location : string) : Task<unit> =
  Sql.query
    """
    UPDATE sync_conflicts SET status = 'overridden'
    WHERE id = (SELECT id FROM sync_conflicts
                WHERE remote = @remote AND location = @loc AND status <> 'overridden'
                ORDER BY detected_at DESC LIMIT 1)
    """
  |> Sql.parameters [ "remote", Sql.string remote; "loc", Sql.string location ]
  |> Sql.executeStatementAsync

/// Look up one conflict by id (for the resolve flow).
let getById (id : string) : Task<Option<Conflict>> =
  task {
    let! all = list ()
    return all |> List.tryFind (fun c -> c.id = id)
  }
