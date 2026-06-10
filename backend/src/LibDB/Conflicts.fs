/// Sync conflicts — the recorded, reviewable log of auto-resolved name-binding divergences.
///
/// A conflict is recorded at PULL TIME, where it's the accurate signal: `detectDivergences` knows a
/// pulled `SetName` rebinds a name we'd locally bound to a *different* hash (incoming-vs-local). The
/// pull auto-resolves by policy (default last-writer-wins — the incoming bind already applied) AND
/// records the conflict here, so nothing is *silently* lost: it's raised to the user, who usually
/// acknowledges ("the auto thing was right") and occasionally overrides.
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

/// One recorded conflict: a name we'd bound to `localHash` that a pull from `remote` rebound to
/// `incomingHash`, auto-resolved by `resolution`. `acknowledged` = the user said "auto was right";
/// `overridden` = the user emitted a different resolution (a reconciling op).
type Conflict =
  { id : string
    location : string
    localHash : string
    incomingHash : string
    resolution : string
    remote : string
    acknowledged : bool
    overridden : bool }

/// Record an auto-resolved divergence. Idempotent on the live (location, remote, both hashes): the
/// same conflict re-detected on a re-pull doesn't pile up duplicate rows.
let record
  (remote : string)
  (location : string)
  (localHash : string)
  (incomingHash : string)
  (resolution : string)
  : Task<unit> =
  task {
    // dedup: skip if this exact unresolved conflict is already on record
    let! existing =
      Sql.query
        """
        SELECT id FROM sync_conflicts
        WHERE location = @loc AND remote = @remote
          AND local_hash = @local AND incoming_hash = @incoming
          AND overridden = 0
        LIMIT 1
        """
      |> Sql.parameters
        [ "loc", Sql.string location
          "remote", Sql.string remote
          "local", Sql.string localHash
          "incoming", Sql.string incomingHash ]
      |> Sql.executeAsync (fun read -> read.string "id")
    match existing with
    | _ :: _ -> ()
    | [] ->
      let id = System.Guid.NewGuid() |> string
      do!
        Sql.query
          """
          INSERT INTO sync_conflicts
            (id, location, local_hash, incoming_hash, resolution, remote)
          VALUES (@id, @loc, @local, @incoming, @resolution, @remote)
          """
        |> Sql.parameters
          [ "id", Sql.string id
            "loc", Sql.string location
            "local", Sql.string localHash
            "incoming", Sql.string incomingHash
            "resolution", Sql.string resolution
            "remote", Sql.string remote ]
        |> Sql.executeStatementAsync
  }

/// All recorded conflicts, newest first — the `dark conflicts` surface.
let list () : Task<List<Conflict>> =
  task {
    return!
      Sql.query
        """
        SELECT id, location, local_hash, incoming_hash, resolution, remote,
               acknowledged, overridden
        FROM sync_conflicts ORDER BY detected_at DESC
        """
      |> Sql.executeAsync (fun read ->
        { id = read.string "id"
          location = read.string "location"
          localHash = read.string "local_hash"
          incomingHash = read.string "incoming_hash"
          resolution = read.string "resolution"
          remote = read.string "remote"
          acknowledged = read.int64 "acknowledged" <> 0L
          overridden = read.int64 "overridden" <> 0L })
  }

/// The user agrees with the auto-resolution — stop surfacing it (the common case).
let acknowledge (id : string) : Task<unit> =
  task {
    do!
      Sql.query "UPDATE sync_conflicts SET acknowledged = 1 WHERE id = @id"
      |> Sql.parameters [ "id", Sql.string id ]
      |> Sql.executeStatementAsync
  }

/// Acknowledge ALL currently-unacknowledged conflicts at once — the bulk "yeah, the auto thing was
/// right" path. Returns how many were newly acknowledged.
let acknowledgeAll () : Task<int> =
  task {
    let! pending =
      Sql.query "SELECT COUNT(*) AS n FROM sync_conflicts WHERE acknowledged = 0"
      |> Sql.executeAsync (fun read -> read.int64 "n")
    do!
      Sql.query "UPDATE sync_conflicts SET acknowledged = 1 WHERE acknowledged = 0"
      |> Sql.executeStatementAsync
    return
      (match pending with
       | n :: _ -> int n
       | [] -> 0)
  }

/// The user chose a different resolution (a reconciling op was emitted) — mark it overridden.
let markOverridden (id : string) : Task<unit> =
  task {
    do!
      Sql.query
        "UPDATE sync_conflicts SET overridden = 1, acknowledged = 1 WHERE id = @id"
      |> Sql.parameters [ "id", Sql.string id ]
      |> Sql.executeStatementAsync
  }

/// Mark the most recent un-overridden conflict at a location overridden — used when a resolution
/// POLICY (not a human) keeps local: `Sync.routeDivergences` emitted a reconciling op, so the
/// recorded auto-LWW outcome no longer reflects the live binding. Keyed by remote + location.
let markOverriddenByLocation (remote : string) (location : string) : Task<unit> =
  task {
    do!
      Sql.query
        """
        UPDATE sync_conflicts SET overridden = 1, acknowledged = 1
        WHERE id = (SELECT id FROM sync_conflicts
                    WHERE remote = @remote AND location = @loc AND overridden = 0
                    ORDER BY detected_at DESC LIMIT 1)
        """
      |> Sql.parameters [ "remote", Sql.string remote; "loc", Sql.string location ]
      |> Sql.executeStatementAsync
  }

/// Look up one conflict by id (for the resolve flow).
let getById (id : string) : Task<Option<Conflict>> =
  task {
    let! all = list ()
    return all |> List.tryFind (fun c -> c.id = id)
  }
