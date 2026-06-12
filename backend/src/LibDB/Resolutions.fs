/// Resolutions — synced decisions that OVERRIDE the op-fold for a contested name.
///
/// A sync conflict (a name diverged across instances) auto-resolves by policy (last-writer-wins). A
/// human — or a keep-local policy — can decide differently. That decision is NOT a new op: the op log
/// is authored content/structure; a resolution is a thin overlay that picks among EXISTING candidates.
/// The effective binding is `fold(ops)` [LWW] → then apply resolutions per location [last-resolver-wins
/// by `at`]. A resolution carries its own fresh `at` stamp, so it competes in the SAME timestamp-LWW
/// that orders bindings (`applySetName`) — which is what lets a "keep mine" decision propagate where
/// re-emitting the original `SetName` (same content hash → same op id → no new rowid) could not.
///
/// This REPLACES the old `OverrideName` op (an op invented only to dodge that hash collision). The
/// table is `resolutions` (schema.sql); a resolution's `id` is a uuid carried over the wire so peers
/// apply it idempotently (INSERT OR IGNORE).
module LibDB.Resolutions

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes

/// One resolution: a decision to bind `location` to `chosenHash` (a `chosen` content of `itemKind`),
/// made `by` (a policy name or "human") at time `at`. `id` is the wire-carried idempotency key.
type Resolution =
  { id : string
    location : PT.PackageLocation
    itemKind : PT.ItemKind
    chosenHash : string
    resolvedBy : string
    branchId : PT.BranchId
    at : string }

/// Mint a resolution for a "keep this candidate" decision. `at` is the resolver time (UTC, the same
/// stamp format the op log + locations use), which becomes the LWW stamp the binding competes on.
let mk
  (location : PT.PackageLocation)
  (chosen : PT.Reference)
  (resolvedBy : string)
  (branchId : PT.BranchId)
  (at : string)
  : Resolution =
  let (PT.Hash h) = chosen.hash
  { id = System.Guid.NewGuid() |> string
    location = location
    itemKind = chosen.kind
    chosenHash = h
    resolvedBy = resolvedBy
    branchId = branchId
    at = at }

/// Persist a resolution (idempotent on `id` — a re-pulled resolution from a peer doesn't duplicate).
let record (r : Resolution) : Task<unit> =
  Sql.query
    """
    INSERT OR IGNORE INTO resolutions
      (id, owner, modules, name, item_type, chosen_hash, resolved_by, branch_id, at)
    VALUES (@id, @o, @m, @n, @t, @hash, @by, @b, @at)
    """
  |> Sql.parameters
    [ "id", Sql.string r.id
      "o", Sql.string r.location.owner
      "m", Sql.string (String.concat "." r.location.modules)
      "n", Sql.string r.location.name
      "t", Sql.string (r.itemKind.toString ())
      "hash", Sql.string r.chosenHash
      "by", Sql.string r.resolvedBy
      "b", Sql.string (string r.branchId)
      "at", Sql.string r.at ]
  |> Sql.executeStatementAsync

/// Apply a resolution to the `locations` projection — the OVERLAY step. Re-binds the location to the
/// chosen content, gated by the SAME timestamp-LWW `applySetName` uses: a resolution whose `at` is
/// older than the current binding's `origin_ts` is stale and skipped (an exact tie breaks by the higher
/// content hash, portably). So every instance converges on the same winner regardless of arrival order.
let applyToLocations (r : Resolution) : Task<unit> =
  task {
    let modulesStr = String.concat "." r.location.modules
    let itemTypeStr = r.itemKind.toString ()

    let! cur =
      Sql.query
        """
        SELECT item_hash, origin_ts FROM locations
        WHERE owner = @o AND modules = @m AND name = @n AND item_type = @t
          AND branch_id = @b AND unlisted_at IS NULL
        LIMIT 1
        """
      |> Sql.parameters
        [ "o", Sql.string r.location.owner
          "m", Sql.string modulesStr
          "n", Sql.string r.location.name
          "t", Sql.string itemTypeStr
          "b", Sql.string (string r.branchId) ]
      |> Sql.executeAsync (fun read ->
        (read.string "item_hash", read.stringOrNone "origin_ts"))

    let skip =
      match cur with
      // already bound to the chosen content — idempotent no-op (so a re-pulled resolution doesn't churn)
      | (curHash, _) :: _ when curHash = r.chosenHash -> true
      // stale: this resolution is older-by-creation than the live binding (exact tie → higher hash wins)
      | (curHash, Some curTs) :: _ when curHash <> r.chosenHash ->
        r.at < curTs || (r.at = curTs && r.chosenHash < curHash)
      | _ -> false

    if skip then
      return ()
    else
      // supersede the existing binding at this path, then insert the resolved one with origin_ts = at
      do!
        Sql.query
          """
          UPDATE locations SET unlisted_at = datetime('now')
          WHERE owner = @o AND modules = @m AND name = @n AND item_type = @t
            AND branch_id = @b AND unlisted_at IS NULL
          """
        |> Sql.parameters
          [ "o", Sql.string r.location.owner
            "m", Sql.string modulesStr
            "n", Sql.string r.location.name
            "t", Sql.string itemTypeStr
            "b", Sql.string (string r.branchId) ]
        |> Sql.executeStatementAsync
      do!
        Sql.query
          """
          INSERT INTO locations
            (location_id, item_hash, owner, modules, name, item_type, branch_id, commit_hash, origin_ts)
          VALUES (@lid, @hash, @o, @m, @n, @t, @b, NULL, @at)
          """
        |> Sql.parameters
          [ "lid", Sql.string (System.Guid.NewGuid() |> string)
            "hash", Sql.string r.chosenHash
            "o", Sql.string r.location.owner
            "m", Sql.string modulesStr
            "n", Sql.string r.location.name
            "t", Sql.string itemTypeStr
            "b", Sql.string (string r.branchId)
            "at", Sql.string r.at ]
        |> Sql.executeStatementAsync
  }

/// Record + immediately apply (the local-authoring path: a human/keep-local decision takes effect now).
let recordAndApply (r : Resolution) : Task<unit> =
  task {
    do! record r
    do! applyToLocations r
  }

/// Read a `Resolution` off a `resolutions` row (shared by `list` + the sync read).
let ofRow (read : RowReader) : Resolution =
  { id = read.string "id"
    location =
      { owner = read.string "owner"
        modules = LibDB.PackageLocation.modulesOfString (read.string "modules")
        name = read.string "name" }
    itemKind = PT.ItemKind.fromString (read.string "item_type")
    chosenHash = read.string "chosen_hash"
    resolvedBy = read.string "resolved_by"
    branchId = read.uuid "branch_id"
    at = read.string "at" }

let private cols =
  "id, owner, modules, name, item_type, chosen_hash, resolved_by, branch_id, at"

/// All resolutions, oldest first (creation order) — for inspection.
let list () : Task<List<Resolution>> =
  Sql.query $"SELECT {cols} FROM resolutions ORDER BY rowid ASC"
  |> Sql.executeAsync ofRow

/// Resolutions authored with rowid > `cursor`, oldest first, paired with their rowid — the sender read
/// for a peer pull (the resolution channel's analogue of `Inserts.opsSince`).
let since (cursor : int64) : Task<List<int64 * Resolution>> =
  Sql.query
    $"SELECT rowid, {cols} FROM resolutions WHERE rowid > @cursor ORDER BY rowid ASC"
  |> Sql.parameters [ "cursor", Sql.int64 cursor ]
  |> Sql.executeAsync (fun r -> (r.int64 "rowid", ofRow r))

/// Re-apply every recorded resolution to `locations`, in creation (rowid) order — the overlay run after
/// a projection refold rebuilds `locations` from the op log alone, so overrides survive the rebuild.
let applyAll () : Task<unit> =
  task {
    let! all = list ()
    for r in all do
      do! applyToLocations r
  }
