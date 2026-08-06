module LibDB.Branches

open System.Threading.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing

// ————————————————————————————————————————————————————————————————————————————————
// Branch STORE (concurrency pivot). A branch = a stable ID + a name alias + a FRONTIER
// of ops. Ops live in the shared, content-addressed package_ops; a branch's authored ops
// are inserted effective=0 (in the log, NOT folded into main) and tagged in op_branches.
// A branch's overlay PM = LibDB.PackageManager.withExtraOps corePM (loadDeltaOps branchId).
// Merge up = flip those ops effective=1 + fold into main. Reuses the effective gate.
// ————————————————————————————————————————————————————————————————————————————————

type BranchRow = { id : string; name : string; createdAt : string }

/// The content-addressed row id for an op. One definition, shared with authoring and
/// the fold, so a branch tags the same op id those two mint.
let private opRowId (op : PT.PackageOp) : System.Guid = Hashing.computeOpRowId op

/// Create a branch off `parentId` (default "main" for a top-level branch). Records the parent
/// (branches off branches). The fork point is tracked PER NAME (branch_name_bases, recorded on
/// authoring), not a whole-branch watermark -- so no base column here. Idempotent on id.
// TODO(ephemeral-branch-GC): abandoned agent branches are only cleaned up by
// EXPLICIT archive today (decided: explicit for now, no TTL/auto-archive). When many
// concurrent agents spin up short-lived branches, revisit: a TTL, archive-on-merge,
// or an idle sweep. See QUESTIONS.md ("Ephemeral agent-branch GC").
let createBranch (id : string) (name : string) (parentId : string) : Task<unit> =
  // Upsert: new branch inserts; RE-USING an existing id REVIVES it (clears
  // archived_at/merged_at) so a review queue reused after approve/reject -- or any
  // archived branch re-created -- becomes active + visible again. Parent + name are
  // first-write-wins (not overwritten), keeping branch identity stable.
  Sql.query
    "INSERT INTO branches (id, name, parent_id) VALUES (@id, @name, @parent)
     ON CONFLICT(id) DO UPDATE SET archived_at = NULL, merged_at = NULL"
  |> Sql.parameters
    [ "id", Sql.string id; "name", Sql.string name; "parent", Sql.string parentId ]
  |> Sql.executeStatementAsync

/// All branches, oldest first.
let listBranches () : Task<List<BranchRow>> =
  Sql.query "SELECT id, name, created_at FROM branches ORDER BY created_at"
  |> Sql.executeAsync (fun read ->
    { id = read.string "id"
      name = read.string "name"
      createdAt = read.string "created_at" })

/// Does this branch exist at all?
///
/// Checks BOTH the registry row and the op tags, because either can exist without
/// the other: switching to a branch registers it before it has any ops, and a branch
/// imported from another machine arrives as tagged ops before anything registers it
/// locally. (Mirrors `SCM.PackageOps.branchExists` in Dark, which asks the same
/// question of the same two tables for the same reason.)
let exists (branchId : string) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches WHERE id = @b
         UNION ALL
         SELECT 1 AS n FROM op_branches WHERE branch_id = @b
         LIMIT 1"
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

/// Resolve a branch id by its name alias (first match), if any.
let branchIdForName (name : string) : Task<Option<string>> =
  Sql.query "SELECT id FROM branches WHERE name = @name LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "id")

/// The LIVE branch called `name`, if any.
///
/// Most recent wins, `rowid` breaking the tie: `created_at` is second-resolution and
/// concurrent agents starting one name inside a second is ordinary here.
///
/// Archived and merged are excluded: a name freed by archiving starts a NEW branch,
/// not the old one revived. `branchIdForName` does not filter, since a lookup of a
/// specific past branch still wants to find it.
let liveIdForName (name : string) : Task<Option<string>> =
  Sql.query
    "SELECT id FROM branches
     WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL
     ORDER BY created_at DESC, rowid DESC LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "id")

/// The branch a READ verb means by `name`: the most recent one still listed, merged or not.
///
/// Reads and writes want different answers, so this sits next to `liveIdForName`
/// rather than being one lookup with a flag. `dark diff feat` must still find a
/// just-merged `feat` -- it is listed, and a listing you cannot name is worse than
/// none. `dark switch feat` must not land on it: its work is already in the parent.
///
/// Archived is excluded from both, since archiving deletes the branch's unmerged ops.
let idForName (name : string) : Task<Option<string>> =
  Sql.query
    "SELECT id FROM branches
     WHERE name = @name AND archived_at IS NULL
     ORDER BY created_at DESC, rowid DESC LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "id")

/// Is this branch still somewhere you can stand: registered, not archived, not merged?
///
/// The id counterpart to `liveIdForName`. `exists` is a weaker question (it answers
/// yes for a branch whose work has already landed in its parent), and the difference
/// matters wherever a stored id is turned back into a place to author: a merged
/// branch is finished, and dropping you onto one is how an edit ends up somewhere it
/// can never reach.
let isLive (branchId : string) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches
         WHERE id = @id AND archived_at IS NULL AND merged_at IS NULL"
      |> Sql.parameters [ "id", Sql.string branchId ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

/// Is this branch's work already in its parent?
let isMerged (branchId : string) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches WHERE id = @id AND merged_at IS NOT NULL"
      |> Sql.parameters [ "id", Sql.string branchId ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

/// The name a branch goes by, for display. Falls back to the id, which is what an
/// imported branch that arrived as tagged ops with no registry row has to show.
let nameForId (branchId : string) : Task<string> =
  task {
    let! found =
      Sql.query "SELECT name FROM branches WHERE id = @id LIMIT 1"
      |> Sql.parameters [ "id", Sql.string branchId ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "name")
    return
      match found with
      | Some name when name <> "" -> name
      | _ -> branchId
  }

/// The branch a REF refers to, without creating anything: a name, a full id, or an
/// unambiguous id prefix.
///
/// Three spellings because all three get printed at people, and `dark branches`
/// abbreviates ids to 8 characters -- so the prefix is the form most likely to be
/// pasted. Ambiguity returns None rather than picking one. Mirrors
/// `Cli.Branch.lookupRef`, which the branch verbs go through.
let lookupRef (branchRef : string) : Task<Option<string>> =
  task {
    match! liveIdForName branchRef with
    | Some id -> return Some id
    | None ->
      let! exact = exists branchRef
      if exact then
        return Some branchRef
      elif String.length branchRef < 4 then
        // Too short to be a prefix worth guessing from; treat it as a name we don't have.
        return None
      else
        let! matches =
          Sql.query
            "SELECT id FROM branches
             WHERE id LIKE @prefix AND archived_at IS NULL
             LIMIT 2"
          |> Sql.parameters [ "prefix", Sql.string (branchRef + "%") ]
          |> Sql.executeAsync (fun read -> read.string "id")
        match matches with
        | [ only ] -> return Some only
        | _ -> return None
  }

/// Resolve a branch NAME to its id, starting the branch if that name has none.
/// Returns the id and whether it was just created (callers announce that; silently
/// running on a branch you didn't mean to start is indistinguishable from running on
/// the one you did).
///
/// A NAME is what a person types; it is reusable and renameable. An ID is what every
/// internal reference uses -- op tags, per-name bases, relay bundles, parent links --
/// and must survive a rename and never be joined to an unrelated branch that reused a
/// label. Two machines each starting a `fix-auth` have two branches.
///
/// Names at the CLI surface, ids everywhere below it. This function is that boundary.
let resolveOrCreate (name : string) (parentId : string) : Task<string * bool> =
  task {
    match! liveIdForName name with
    | Some id -> return (id, false)
    | None ->
      // Check-then-insert is a race, and concurrent agents sharing a `DARK_BRANCH` is
      // the normal case here.
      //
      // The guard is inside the INSERT rather than a UNIQUE index, because same-named
      // branches are legal: one started on another instance arrives by sync and is a
      // DIFFERENT branch sharing a label. This prevents only the local case, one
      // machine minting two branches for one name. One statement, so SQLite's write
      // lock makes it atomic; the loser writes nothing and re-reads the winner's id.
      let id = System.Guid.NewGuid().ToString()
      do!
        Sql.query
          "INSERT INTO branches (id, name, parent_id)
           SELECT @id, @name, @parent
           WHERE NOT EXISTS (
             SELECT 1 FROM branches
             WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL
           )"
        |> Sql.parameters
          [ "id", Sql.string id
            "name", Sql.string name
            "parent", Sql.string parentId ]
        |> Sql.executeStatementAsync

      match! liveIdForName name with
      | Some winner -> return (winner, winner = id)
      | None ->
        // Only reachable if the row we just wrote was archived or merged in between,
        // which means someone deliberately finished it. Report the id we minted
        // rather than inventing another.
        return (id, true)
  }

/// Fold a branch-scoped `Decide("propagation", ...)` into `propagation_policy`.
///
/// THIS RULE EXISTS TWICE. `PackageOpPlayback.applyDecide` folds the same op for MAIN
/// (branch_id = ""). Both must use the OP's stamp and both must guard on it, or the
/// same decision lands differently depending on which path it took and two machines
/// stop converging. Change one, change both.
///
/// A branch's DECISIONS fold immediately, scoped to the branch; its SetNames do not,
/// since those would leak into main's bindings. A decision is not a binding.
let private foldBranchDecide
  (branchId : string)
  (loc : PT.PackageLocation)
  (value : string)
  (reason : string)
  (originTs : string)
  : Task<unit> =
  let modules = String.concat "." loc.modules
  let key =
    [ "branch", Sql.string branchId
      "owner", Sql.string loc.owner
      "modules", Sql.string modules
      "name", Sql.string loc.name
      "origin_ts", Sql.string originTs ]
  if value = "unset" then
    // Clearing is a decision like any other, and guarded the same way so a stale
    // unset can't wipe a newer pin.
    Sql.query
      "DELETE FROM propagation_policy
       WHERE branch_id = @branch AND owner = @owner AND modules = @modules AND name = @name
         AND COALESCE(origin_ts, '') < @origin_ts"
    |> Sql.parameters key
    |> Sql.executeStatementAsync
  else
    Sql.query
      "INSERT INTO propagation_policy (branch_id, owner, modules, name, policy, note, origin_ts)
       VALUES (@branch, @owner, @modules, @name, @policy, @note, @origin_ts)
       ON CONFLICT(branch_id, owner, modules, name) DO UPDATE SET
         policy = excluded.policy, note = excluded.note, origin_ts = excluded.origin_ts
       WHERE excluded.origin_ts > COALESCE(propagation_policy.origin_ts, '')"
    |> Sql.parameters (
      key @ [ "policy", Sql.string value; "note", Sql.string reason ]
    )
    |> Sql.executeStatementAsync

/// Store a branch's authored ops with an EXPLICIT authoring stamp each: serialize + INSERT
/// effective=0 (in the shared log, NOT folded into main) + tag the frontier, in ONE
/// transaction, NO fold. Content-addressed id -> re-store dedups. Returns the number
/// of ops newly stored.
///
/// The stamp is a parameter because the two callers need different ones and getting it
/// wrong is silent. A locally-authored op takes a fresh stamp; an op arriving in a
/// branch BUNDLE keeps the stamp it was authored with, or cross-instance LWW resolves
/// by who imported last rather than who edited last.
let storeDeltaOpsStamped
  (branchId : string)
  (ops : List<PT.PackageOp * string>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      // A branch bundle carries its author's stamps, so it is a receive path like
      // any other: advance our clock past them or our own later edits on this branch
      // lose to them forever. See `OriginTs.observe`.
      ops |> List.iter (fun (_, ts) -> OriginTs.observe ts)

      let prepared =
        ops
        |> List.map (fun (op, ts) ->
          let opId = opRowId op
          (opId, BS.PT.PackageOp.serialize opId op, op, ts))

      let insertOps =
        "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
         VALUES (@id, @op_blob, 0, 0, @origin_ts)"
      let opRows =
        prepared
        |> List.map (fun (id, blob, _, ts) ->
          [ "id", Sql.uuid id
            "op_blob", Sql.bytes blob
            "origin_ts", Sql.string ts ])

      let insertTags =
        "INSERT OR IGNORE INTO op_branches (op_id, branch_id) VALUES (@op_id, @branch_id)"
      let tagRows =
        prepared
        |> List.map (fun (id, _, _, _) ->
          [ "op_id", Sql.uuid id; "branch_id", Sql.string branchId ])

      // one transaction; ops-insert counts come first, so truncate to the op rows.
      let affected =
        Sql.executeTransactionSync [ (insertOps, opRows); (insertTags, tagRows) ]

      for (_, _, op, ts) in prepared do
        match op with
        | PT.PackageOp.Decide("propagation", loc, value, reason, _) ->
          do! foldBranchDecide branchId loc value reason ts
        | _ -> ()

      return affected |> List.truncate (List.length opRows) |> List.sumBy int64
  }

/// Re-fold every BRANCH-scoped propagation decision straight from the log.
///
/// `propagation_policy` is listed in `Seed.projectionTables` as regenerable, which is
/// true for main: its rows come from `Decide` ops that `applyUnappliedOps` re-folds.
/// Branch ops are `effective = 0` and the fold skips them, so a rebuild that clears
/// the table and re-folds only effective ops deletes every branch pin without bringing
/// it back -- silently, on a flag documented as safe recovery. Hence this.
///
/// Oldest-first, so `foldBranchDecide`'s origin_ts guard sees the same sequence the
/// original authoring did.
let refoldBranchDecides () : Task<unit> =
  task {
    let! rows =
      Sql.query
        "SELECT p.id, p.op_blob, p.origin_ts, ob.branch_id
         FROM package_ops p JOIN op_branches ob ON ob.op_id = p.id
         ORDER BY p.origin_ts, p.rowid"
      |> Sql.executeAsync (fun read ->
        (read.uuid "id",
         read.bytes "op_blob",
         read.string "origin_ts",
         read.string "branch_id"))

    for (id, blob, ts, branchId) in rows do
      // A blob this build can't read is somebody else's newer op format; skip it
      // rather than fail the whole rebuild over one row we were never going to fold
      // anyway.
      let op =
        try
          Some(BS.PT.PackageOp.deserialize id blob)
        with _ ->
          None
      match op with
      | Some(PT.PackageOp.Decide("propagation", loc, value, reason, _)) ->
        do! foldBranchDecide branchId loc value reason ts
      | _ -> ()
  }

/// Store LOCALLY-AUTHORED branch ops, stamped from the process authoring clock.
///
/// The same monotonic clock main authoring uses. `strftime('now')` here would stamp
/// a branch op with wall-clock time while the counter may already be ahead of it, so
/// a branch edit made AFTER a burst of main authoring could take an earlier stamp
/// and lose the LWW it should win.
let storeDeltaOps (branchId : string) (ops : List<PT.PackageOp>) : Task<int64> =
  storeDeltaOpsStamped branchId (ops |> List.map (fun op -> (op, OriginTs.next ())))

/// MERGE up, half 1: flip a branch's frontier ops from effective=0 (branch-pending) to
/// effective=1 (part of main). Half 2 is the FOLD (Seed.applyUnappliedOps) which then folds
/// these now-effective, still-unapplied ops into main's projections -- kept separate because
/// the fold lives in Seed.fs (compiles after this file) and the caller runs it. Returns how
/// many ops were flipped. Deterministic replay + origin_ts LWW handle same-name collisions in
/// the fold; this is NOT a CRDT merge.
let markMergedEffective (branchId : string) : Task<int64> =
  task {
    // Count what is ABOUT to flip, not what is tagged. A branch can carry tags on
    // ops that are already effective (a whole-log branch import tags ops main
    // already has), and counting those made `MergeOutcome.merged` report work the
    // merge didn't do -- which is the one thing every command on this branch is
    // supposed to have stopped doing.
    let! toFlip =
      Sql.query
        "SELECT count(*) AS n FROM package_ops p
         JOIN op_branches b ON b.op_id = p.id
         WHERE b.branch_id = @b AND p.effective = 0"
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    let! _ =
      Sql.query
        "UPDATE package_ops SET effective = 1
         WHERE effective = 0
           AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeStatementAsync
    return toFlip
  }

/// Finish a merge: mark it merged AND drop the now-redundant frontier, in ONE transaction.
///
/// One transaction because the gap between them is the only unrecoverable interruption
/// point in a merge. Clearing the frontier first leaves a branch with no ops and no
/// merged flag, which the gate refuses as "nothing to merge" -- true of the branch and
/// false of the situation, since its changes are in main. It then lists as live forever.
///
/// Every other point in a merge is recoverable by running it again: the frontier is
/// still tagged, so the second run finishes what the first started.
let finishMerge (branchId : string) : unit =
  Sql.executeTransactionSync
    [ ("UPDATE branches SET merged_at = datetime('now') WHERE id = @b",
       [ [ "b", Sql.string branchId ] ])
      ("DELETE FROM op_branches WHERE branch_id = @b",
       [ [ "b", Sql.string branchId ] ]) ]
  |> ignore<List<int>>

// Per-name BASE model: a reload-stable fork marker.
//
// A branch forks off a PARENT. The base for each name it touches is the parent's
// content-hash for that name; a conflict is the parent having moved it since the fork.
// For parent=main the parent's state IS `locations`. A non-main parent is an overlay
// never materialized there, so its effective hashes come from folding the parent
// chain's SetName rebinds over main. Everything below routes through
// `parentNameHashes` so both cases share one path.

/// A branch's delta ops (deserialized), walking the parent chain (branch -> parent
/// -> ... until 'main'), ordered by origin_ts so same-name rebinds across the chain
/// resolve LWW. Shared by `loadDeltaOps` (the process overlay) and
/// `parentNameHashes` (the fork/merge base).
let chainOverlayOps (branchId : string) : Task<List<PT.PackageOp>> =
  Sql.query
    "WITH RECURSIVE chain(bid) AS (
       SELECT @start
       UNION
       SELECT b.parent_id FROM branches b JOIN chain c ON b.id = c.bid
       WHERE b.parent_id != 'main'
     )
     SELECT p.id, p.op_blob
     FROM package_ops p
     JOIN op_branches ob ON ob.op_id = p.id
     WHERE ob.branch_id IN (SELECT bid FROM chain)
     ORDER BY p.origin_ts, p.rowid"
  |> Sql.parameters [ "start", Sql.string branchId ]
  |> Sql.executeAsync (fun read ->
    BS.PT.PackageOp.deserialize (read.uuid "id") (read.bytes "op_blob"))

/// A branch's registered parent id ('main' if none recorded / unknown).
let parentOf (branchId : string) : Task<string> =
  task {
    let! p =
      Sql.query "SELECT parent_id FROM branches WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "parent_id")
    return p |> Option.defaultValue "main"
  }

/// A name key as stored in branch_name_bases: (owner, dotted-modules, name).
type private NameKey = string * string * string

/// main's current content-hash per name (the effective=1 fold, projected in `locations`).
let private mainNameHashes () : Task<Map<NameKey, string>> =
  task {
    let! rows =
      Sql.query
        "SELECT owner, modules, name, item_hash FROM locations WHERE unlisted_at IS NULL"
      |> Sql.executeAsync (fun read ->
        (read.string "owner", read.string "modules", read.string "name"),
        read.string "item_hash")
    return Map.ofList rows
  }

/// The PARENT's CURRENT effective content-hash per name -- the state a child forks
/// from and merges back into. parent=main -> `locations`. A non-main parent -> main
/// overridden by that parent chain's own SetName rebinds (latest by origin_ts wins),
/// since a non-main branch lives only as an overlay.
let parentNameHashes (parentId : string) : Task<Map<NameKey, string>> =
  task {
    let! baseMap = mainNameHashes ()
    if parentId = "main" then
      return baseMap
    else
      let! ops = chainOverlayOps parentId
      return
        ops
        |> List.fold
          (fun (m : Map<NameKey, string>) op ->
            match op with
            | PT.PackageOp.SetName(loc, target, _) ->
              let (Hash h) = target.hash
              Map.add (loc.owner, String.concat "." loc.modules, loc.name) h m
            | _ -> m)
          baseMap
  }

/// The branch chain's LIVE bindings, inverted: item-hash -> (kind, location).
///
/// Answers "where does this dependent live, as this branch sees it". A branch's items
/// have no `locations` row -- that is the name isolation -- so the main projection
/// cannot answer it and a branch-authored dependent would be invisible to discovery.
///
/// Folded latest-stamp-wins per NAME before inverting, so a hash the branch has moved
/// off does not come back as live.
let chainBindingsByHash
  (branchId : string)
  : Task<Map<string, List<PT.ItemKind * PT.PackageLocation>>> =
  task {
    let! ops = chainOverlayOps branchId

    let byLocation =
      ops
      |> List.fold
        (fun (m : Map<PT.PackageLocation, PT.ItemKind * string>) op ->
          match op with
          | PT.PackageOp.SetName(loc, target, _) ->
            let (Hash h) = target.hash
            Map.add loc (target.kind, h) m
          | _ -> m)
        Map.empty

    // LIST-valued: one hash can be bound at several names, since identical content
    // IS the same item.
    let pairs =
      byLocation
      |> Map.toList
      |> List.map (fun (loc, (kind, hash)) -> (hash, (kind, loc)))

    return
      pairs
      |> List.fold
        (fun (m : Map<string, List<PT.ItemKind * PT.PackageLocation>>) (h, entry) ->
          let existing = Map.tryFind h m |> Option.defaultValue []
          Map.add h (existing @ [ entry ]) m)
        Map.empty
  }


/// The locations a set of ops rebinds.
///
/// `Resolve` counts, for the same reason `SCM.PackageOps.bindingFromOp` says it does:
/// a resolution IS a rebind that also carries a decision id. Counting only `SetName`
/// leaves a name bound solely by resolving a conflict without a `branch_name_bases`
/// row -- and without a base the detector cannot prove both sides moved, so that name
/// can never conflict again.
let private rebindKeys (ops : List<PT.PackageOp>) : List<PT.PackageLocation> =
  ops
  |> List.choose (fun op ->
    match op with
    | PT.PackageOp.SetName(loc, _, _) -> Some loc
    | PT.PackageOp.Resolve(_, loc, _) -> Some loc
    | _ -> None)

/// Record the per-name BASE for a branch: for each name these ops rebind, capture
/// the PARENT's CURRENT content-hash (or '' if the name is new to the parent), ONCE
/// -- first touch wins (INSERT OR IGNORE). Content hashes are stable across reload,
/// so this is a reliable fork marker. Call after storeDeltaOps.
let recordNameBases
  (branchId : string)
  (parentId : string)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    let! parentHashes = parentNameHashes parentId
    for loc in rebindKeys ops do
      let key = (loc.owner, String.concat "." loc.modules, loc.name)
      let baseHash = parentHashes |> Map.tryFind key |> Option.defaultValue ""
      do!
        Sql.query
          "INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
           VALUES (@b, @owner, @modules, @name, @base)"
        |> Sql.parameters
          [ "b", Sql.string branchId
            "owner", Sql.string loc.owner
            "modules", Sql.string (String.concat "." loc.modules)
            "name", Sql.string loc.name
            "base", Sql.string baseHash ]
        |> Sql.executeStatementAsync
  }

/// This branch's recorded per-name bases: the parent's hash for each name at the
/// branch's first touch.
///
/// The one place that reads the table, so callers (the merge gate below, tests, and
/// eventually the view's "how far has this drifted" line) don't each hand-roll the
/// same SELECT and drift from the schema.
let nameBasesFor (branchId : string) : Task<List<NameKey * string>> =
  Sql.query
    "SELECT owner, modules, name, base_hash FROM branch_name_bases WHERE branch_id = @b"
  |> Sql.parameters [ "b", Sql.string branchId ]
  |> Sql.executeAsync (fun read ->
    (read.string "owner", read.string "modules", read.string "name"),
    read.string "base_hash")

/// Names where the PARENT's CURRENT content-hash differs from this branch's recorded
/// base -- i.e. the parent changed the name since the branch forked off it. These
/// are the merge conflicts.
let nameConflicts (branchId : string) : Task<List<string>> =
  task {
    let! parentId = parentOf branchId
    let! parentHashes = parentNameHashes parentId
    let! bases = nameBasesFor branchId
    return
      bases
      |> List.choose (fun ((o, m, n), baseHash) ->
        let cur = parentHashes |> Map.tryFind (o, m, n) |> Option.defaultValue ""
        if cur <> baseHash then
          Some(if m = "" then $"{o}.{n}" else $"{o}.{m}.{n}")
        else
          None)
  }

// Conflict PRESENTATION lives in Dark now (`SCM.Conflicts`): one base-agnostic detector over
// branch_name_bases OR sync_bases, returning STRUCTURED conflicts that the CLI
// renders and the `conflicts` table records. F# keeps no display formatting for it,
// and main-to-main sync gets the same detector for free. `nameConflicts` above stays
// -- it's the merge GATE, not a view.

/// REBASE: accept the parent's current state as this branch's new base. Returns the
/// names the parent had changed since the fork (so the user sees what moved); after
/// this the branch's own ops layer on top (LWW by origin_ts) and merge is unblocked.
/// Per-name accept/override is a follow-up (scm-spec 5, 7).
let rebase (branchId : string) : Task<List<string>> =
  task {
    let! parentId = parentOf branchId
    let! changed = nameConflicts branchId
    let! parentHashes = parentNameHashes parentId
    // Rewrite each base to the parent's current hash for that name.
    let! bases =
      Sql.query
        "SELECT owner, modules, name FROM branch_name_bases WHERE branch_id = @b"
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeAsync (fun read ->
        read.string "owner", read.string "modules", read.string "name")
    for (o, m, n) in bases do
      let cur = parentHashes |> Map.tryFind (o, m, n) |> Option.defaultValue ""
      do!
        Sql.query
          "UPDATE branch_name_bases SET base_hash = @h
           WHERE branch_id = @b AND owner = @owner AND modules = @modules AND name = @name"
        |> Sql.parameters
          [ "h", Sql.string cur
            "b", Sql.string branchId
            "owner", Sql.string o
            "modules", Sql.string m
            "name", Sql.string n ]
        |> Sql.executeStatementAsync
    return changed
  }

// Per-name RESOLUTION: keep-mine / take-theirs for a conflicted name.
//
// On the op log and LWW, with no "source" column: take-theirs UNTAGS the branch's
// SetName so its overlay falls back to the parent's binding; keep-mine RE-STAMPS it to
// the newest origin_ts so it wins LWW. Both then set the branch's base for that name to
// the parent's current hash, clearing the conflict.

/// Parse "owner.Mod.Sub.name" -> (owner, dotted-modules, name). Mirrors the
/// detector's fqn format.
let private parseFqn (fqn : string) : Option<string * string * string> =
  match fqn.Split('.') |> Array.toList with
  | owner :: rest when not (List.isEmpty rest) ->
    // rest = [Mod; Sub; ...; name]; the last segment is the name, the rest are modules.
    match List.rev rest with
    | name :: revModules ->
      Some(owner, revModules |> List.rev |> String.concat ".", name)
    | [] -> None
  | _ -> None

/// The branch's OWN frontier ops (deserialized, with ids) -- just this branch's tag,
/// not the chain.
let private ownFrontierOps
  (branchId : string)
  : Task<List<System.Guid * PT.PackageOp>> =
  Sql.query
    "SELECT p.id, p.op_blob FROM package_ops p
     JOIN op_branches ob ON ob.op_id = p.id
     WHERE ob.branch_id = @b"
  |> Sql.parameters [ "b", Sql.string branchId ]
  |> Sql.executeAsync (fun read ->
    let id = read.uuid "id"
    id, BS.PT.PackageOp.deserialize id (read.bytes "op_blob"))

/// ids of the branch's frontier SetName ops that bind a given (owner, dotted-modules, name).
let private setNameIdsFor
  (ops : List<System.Guid * PT.PackageOp>)
  (owner : string, modules : string, name : string)
  : List<System.Guid> =
  ops
  |> List.choose (fun (id, op) ->
    match op with
    | PT.PackageOp.SetName(loc, _, _) when
      loc.owner = owner
      && String.concat "." loc.modules = modules
      && loc.name = name
      ->
      Some id
    | _ -> None)

/// Set the branch's recorded base for one name to the parent's CURRENT hash (clears
/// the conflict).
let private setBaseToParentFor
  (branchId : string)
  (owner : string, modules : string, name : string)
  : Task<unit> =
  task {
    let! parentId = parentOf branchId
    let! parentHashes = parentNameHashes parentId
    let cur =
      parentHashes |> Map.tryFind (owner, modules, name) |> Option.defaultValue ""
    do!
      Sql.query
        "UPDATE branch_name_bases SET base_hash = @h
         WHERE branch_id = @b AND owner = @owner AND modules = @modules AND name = @name"
      |> Sql.parameters
        [ "h", Sql.string cur
          "b", Sql.string branchId
          "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name ]
      |> Sql.executeStatementAsync
  }

/// RESOLVE take-theirs: drop the branch's binding for `fqn` (untag its SetName ops)
/// so the overlay falls back to the parent's version, then set the base to the
/// parent's current. Returns Ok or why not.
let resolveTakeTheirs
  (branchId : string)
  (fqn : string)
  : Task<Result<unit, string>> =
  task {
    match parseFqn fqn with
    | None -> return Error $"can't parse a name from \"{fqn}\""
    | Some key ->
      let! ops = ownFrontierOps branchId
      let ids = setNameIdsFor ops key
      if List.isEmpty ids then
        return
          Error $"branch \"{branchId}\" doesn't bind {fqn} -- nothing to resolve"
      else
        for id in ids do
          do!
            Sql.query "DELETE FROM op_branches WHERE op_id = @id AND branch_id = @b"
            |> Sql.parameters [ "id", Sql.uuid id; "b", Sql.string branchId ]
            |> Sql.executeStatementAsync
        do! setBaseToParentFor branchId key
        return Ok()
  }

/// RESOLVE keep-mine: re-stamp the branch's SetName for `fqn` to newest origin_ts so it wins LWW even
/// if the parent's binding is newer, then set the base to the parent's current. Returns Ok or why not.
let resolveKeepMine (branchId : string) (fqn : string) : Task<Result<unit, string>> =
  task {
    match parseFqn fqn with
    | None -> return Error $"can't parse a name from \"{fqn}\""
    | Some key ->
      let! ops = ownFrontierOps branchId
      let ids = setNameIdsFor ops key
      if List.isEmpty ids then
        return
          Error $"branch \"{branchId}\" doesn't bind {fqn} -- nothing to resolve"
      else
        // Re-stamp strictly AFTER every existing op, not just "now": main authoring
        // stamps origin_ts from a monotonic counter that can run ahead of
        // wall-clock, so `strftime('now')` wouldn't reliably beat the parent's
        // binding. Global MAX(origin_ts) + 1s guarantees this SetName wins LWW.
        for id in ids do
          do!
            Sql.query
              "UPDATE package_ops
               SET origin_ts =
                 (SELECT strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second') FROM package_ops)
               WHERE id = @id"
            |> Sql.parameters [ "id", Sql.uuid id ]
            |> Sql.executeStatementAsync
        do! setBaseToParentFor branchId key
        return Ok()
  }

/// Load a branch's delta ops (deserialized) -- the delta to overlay on core:
///   `LibDB.PackageManager.withExtraOps corePM (loadDeltaOps branchId)`.
/// BRANCHES OFF BRANCHES: walks the parent chain (branch -> parent -> ... until
/// 'main'), so B off A off main sees A's frontier AND its own. Ordered by origin_ts
/// (LWW). Same query as the merge base (`chainOverlayOps`) so overlay and base can
/// never drift.
let loadDeltaOps (branchId : string) : Task<List<PT.PackageOp>> =
  chainOverlayOps branchId

/// The branch's OWN frontier ops (deserialized), ordered oldest-first -- its
/// authoring history, for `dark log <branch>` (audit/review the SEQUENCE,
/// complementary to `diff`'s net effect). Just this branch's tag, NOT the parent
/// chain.
let frontierOps (branchId : string) : Task<List<PT.PackageOp>> =
  Sql.query
    "SELECT p.id, p.op_blob FROM package_ops p
     JOIN op_branches ob ON ob.op_id = p.id
     WHERE ob.branch_id = @b
     ORDER BY p.origin_ts, p.rowid"
  |> Sql.parameters [ "b", Sql.string branchId ]
  |> Sql.executeAsync (fun read ->
    BS.PT.PackageOp.deserialize (read.uuid "id") (read.bytes "op_blob"))

/// MERGE into a NON-MAIN parent: a non-main parent is an overlay, never materialized in main's
/// projections, so we do NOT flip effective / fold (that would leak the child into
/// main). Instead RETAG the child's frontier ops onto the parent -- the parent's
/// overlay then folds them when a process runs `--branch <parent>`. INSERT-OR-IGNORE
/// + DELETE handles the (rare) shared-op case.
///
/// Retag and mark-merged go in ONE transaction, as in `finishMerge`: the child stops
/// owning its ops the instant the DELETE lands, so an interruption before the merged
/// flag is set leaves a branch the gate refuses to merge.
let retagFrontierToParent (branchId : string) (parentId : string) : Task<int64> =
  task {
    let! n =
      Sql.query "SELECT count(*) AS n FROM op_branches WHERE branch_id = @b"
      |> Sql.parameters [ "b", Sql.string branchId ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Sql.executeTransactionSync
      [ ("INSERT OR IGNORE INTO op_branches (op_id, branch_id)
          SELECT op_id, @parent FROM op_branches WHERE branch_id = @b",
         [ [ "parent", Sql.string parentId; "b", Sql.string branchId ] ])
        ("DELETE FROM op_branches WHERE branch_id = @b",
         [ [ "b", Sql.string branchId ] ])
        ("UPDATE branches SET merged_at = datetime('now') WHERE id = @b",
         [ [ "b", Sql.string branchId ] ]) ]
    |> ignore<List<int>>
    return n
  }
