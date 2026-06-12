/// Sync cursors — per-remote poll resume state.
///
/// Local-only, NOT synced: for each remote peer, how far this instance has folded that
/// remote's op stream. The cursor is a `package_ops` **rowid** — SQLite's implicit monotonic
/// insertion order (prework finding: no `seq` column needed). A poll resumes from the cursor
/// so a peer pulls only ops it hasn't seen, via `Inserts.opsSince cursor`.
///
/// The `sync_cursors` table lives in `backend/migrations/schema.sql` (created at startup).
module LibDB.SyncCursors

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

/// How far we've folded `remote`'s op stream (0 if never polled — pull from the start).
let cursorFor (remote : string) : Task<int64> =
  task {
    let! rows =
      Sql.query
        "SELECT folded_through_rowid AS r FROM sync_cursors WHERE remote = @remote"
      |> Sql.parameters [ "remote", Sql.string remote ]
      |> Sql.executeAsync (fun read -> read.int64 "r")
    return
      (match rows with
       | r :: _ -> r
       | [] -> 0L)
  }

/// Advance `remote`'s cursor to `rowid`. Idempotent upsert that NEVER moves backward — a
/// stale/duplicate advance can't rewind the cursor (so a re-applied batch won't cause a
/// re-fold of older ops). `MAX(existing, incoming)` enforces monotonicity.
let advanceCursor (remote : string) (rowid : int64) : Task<unit> =
  Sql.query
    """
    INSERT INTO sync_cursors (remote, folded_through_rowid)
    VALUES (@remote, @rowid)
    ON CONFLICT(remote) DO UPDATE SET
      folded_through_rowid =
        MAX(sync_cursors.folded_through_rowid, excluded.folded_through_rowid)
    """
  |> Sql.parameters [ "remote", Sql.string remote; "rowid", Sql.int64 rowid ]
  |> Sql.executeStatementAsync

/// How far we've applied `remote`'s RESOLUTIONS stream (0 if never). A separate cursor from the op
/// cursor — resolutions sync on their own channel (their own `resolutions` rowid).
let resolutionCursorFor (remote : string) : Task<int64> =
  task {
    let! rows =
      Sql.query
        "SELECT resolutions_through_rowid AS r FROM sync_cursors WHERE remote = @remote"
      |> Sql.parameters [ "remote", Sql.string remote ]
      |> Sql.executeAsync (fun read -> read.int64 "r")
    return
      (match rows with
       | r :: _ -> r
       | [] -> 0L)
  }

/// Advance `remote`'s RESOLUTIONS cursor to `rowid` (monotonic upsert; never rewinds), mirroring
/// `advanceCursor`. Coexists with the op cursor on the same `sync_cursors` row.
let advanceResolutionCursor (remote : string) (rowid : int64) : Task<unit> =
  Sql.query
    """
    INSERT INTO sync_cursors (remote, resolutions_through_rowid)
    VALUES (@remote, @rowid)
    ON CONFLICT(remote) DO UPDATE SET
      resolutions_through_rowid =
        MAX(sync_cursors.resolutions_through_rowid, excluded.resolutions_through_rowid)
    """
  |> Sql.parameters [ "remote", Sql.string remote; "rowid", Sql.int64 rowid ]
  |> Sql.executeStatementAsync

/// All known peers and how far we've synced each — the `dark sync status` surface. Empty if we've
/// never synced. Ordered by `remote` for a stable display.
let listCursors () : Task<List<string * int64>> =
  Sql.query
    "SELECT remote, folded_through_rowid AS r FROM sync_cursors ORDER BY remote"
  |> Sql.executeAsync (fun read -> (read.string "remote", read.int64 "r"))
