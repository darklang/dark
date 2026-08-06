module LibDB.Branches

open System.Threading.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing

/// The branch STORE. A branch is a stable id, a name alias, and a FRONTIER of ops.
///
/// Ops live in the shared content-addressed `package_ops`; a branch's own are inserted
/// effective=0 (in the log, not folded into main) and tagged in `op_branches`. Its overlay PM is
/// `withExtraOps corePM (loadDeltaOps branchId)`. Merging up flips those ops effective=1 and folds
/// them, reusing the same gate.

type BranchRow = { id : string; name : string; createdAt : string }

/// The content-addressed row id for an op. One definition, shared with authoring and
/// the fold, so a branch tags the same op id those two mint.
let private opRowId (op : PT.PackageOp) : System.Guid = Hashing.computeOpRowId op

/// Create a branch off `parentId` ("main" for a top-level branch). The fork point is tracked PER
/// NAME (branch_name_bases, recorded on authoring), not a whole-branch watermark, so there's no
/// base column here. Idempotent on id.
// TODO(ephemeral-branch-GC): abandoned agent branches are cleaned up only by EXPLICIT archive
// (decided: no TTL/auto-archive for now). Revisit if short-lived agent branches pile up.
let createBranch
  (id : PT.BranchId)
  (name : string)
  (parentId : PT.BranchId)
  : Task<unit> =
  // Upsert: re-using an existing id REVIVES it (clears archived_at/merged_at), so an archived
  // branch re-created is active and visible again. Parent and name are first-write-wins, which
  // keeps branch identity stable.
  Sql.query
    "INSERT INTO branches (id, name, parent_id) VALUES (@id, @name, @parent)
     ON CONFLICT(id) DO UPDATE SET archived_at = NULL, merged_at = NULL"
  |> Sql.parameters
    [ "id", Sql.string (string id)
      "name", Sql.string name
      "parent", Sql.string (string parentId) ]
  |> Sql.executeStatementAsync

/// All branches, oldest first.
let listBranches () : Task<List<BranchRow>> =
  Sql.query "SELECT id, name, created_at FROM branches ORDER BY created_at"
  |> Sql.executeAsync (fun read ->
    { id = read.string "id"
      name = read.string "name"
      createdAt = read.string "created_at" })

// Three questions, deliberately distinct: EXISTS (known here at all), LIVE (still somewhere you
// can author), LISTED (still worth naming, merged or not). Reads and writes want different
// answers, which is why the name lookups come as a pair rather than one lookup with a flag.

/// Does this branch exist at all? Registry row OR op tags, since either can exist without the
/// other: switching registers a branch before it has ops, and an imported branch arrives as
/// tagged ops with nothing registering it locally. (Mirrors `SCM.PackageOps.branchExists`.)
let exists (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches WHERE id = @b
         UNION ALL
         SELECT 1 AS n FROM op_branches WHERE branch_id = @b
         LIMIT 1"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

/// Resolve a branch id by its name alias (first match), if any. Unfiltered: a lookup of a
/// specific past branch still wants to find it.
let branchIdForName (name : string) : Task<Option<PT.BranchId>> =
  Sql.query "SELECT id FROM branches WHERE name = @name LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    PT.BranchId.ParseUnsafe(read.string "id"))

/// The LIVE branch called `name`, if any: not archived, not merged. Most recent wins, `rowid`
/// breaking the tie because `created_at` is second-resolution and concurrent agents starting one
/// name inside a second is ordinary here. A name freed by archiving starts a NEW branch.
let liveIdForName (name : string) : Task<Option<PT.BranchId>> =
  Sql.query
    "SELECT id FROM branches
     WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL
     ORDER BY created_at DESC, rowid DESC LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    PT.BranchId.ParseUnsafe(read.string "id"))

/// The branch a READ verb means by `name`: the most recent one still listed, merged or not, so
/// `dark diff feat` still finds a just-merged `feat` while `dark switch feat` (which uses
/// `liveIdForName`) won't land on it. Archived is excluded from both, since archiving deletes the
/// branch's unmerged ops.
let idForName (name : string) : Task<Option<PT.BranchId>> =
  Sql.query
    "SELECT id FROM branches
     WHERE name = @name AND archived_at IS NULL
     ORDER BY created_at DESC, rowid DESC LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    PT.BranchId.ParseUnsafe(read.string "id"))

/// Is this branch still somewhere you can stand: registered, not archived, not merged? The id
/// counterpart to `liveIdForName`, and the check to use wherever a stored id becomes a place to
/// author: a merged branch is finished, and an edit landing on one can never reach anywhere.
let isLive (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches
         WHERE id = @id AND archived_at IS NULL AND merged_at IS NULL"
      |> Sql.parameters [ "id", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

/// Is this branch's work already in its parent?
let isMerged (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches WHERE id = @id AND merged_at IS NOT NULL"
      |> Sql.parameters [ "id", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

/// The name a branch goes by, for display. Falls back to the id, which is what an
/// imported branch that arrived as tagged ops with no registry row has to show.
let nameForId (branchId : PT.BranchId) : Task<string> =
  task {
    // Main has no `branches` row, so the fallback below would show its raw id.
    if branchId.IsMain then
      return PT.BranchId.MainName
    else
      let! found =
        Sql.query "SELECT name FROM branches WHERE id = @id LIMIT 1"
        |> Sql.parameters [ "id", Sql.string (string branchId) ]
        |> Sql.executeRowOptionAsync (fun read -> read.string "name")

      return
        match found with
        | Some name when name <> "" -> name
        | _ -> string branchId
  }

/// The branch a REF refers to, without creating anything: a name, a full id, or an unambiguous id
/// prefix. All three get printed at people, and `dark branches` abbreviates ids to 8 characters,
/// so the prefix is the form most likely to be pasted. Ambiguity returns None rather than picking
/// one. Mirrors `Cli.Branch.lookupRef`, which the branch verbs go through.
let lookupRef (branchRef : string) : Task<Option<PT.BranchId>> =
  task {
    // Main is resolvable by name and by id like any other branch, and has to be: it has no
    // `branches` row, so neither the name lookup nor the id-prefix search below can find it.
    if
      branchRef = PT.BranchId.MainName
      || PT.BranchId.Parse branchRef = Some PT.BranchId.Main
    then
      return Some PT.BranchId.Main
    else
      match! liveIdForName branchRef with
      | Some id -> return Some id
      | None ->
        match PT.BranchId.Parse branchRef with
        | Some id ->
          let! exact = exists id
          return (if exact then Some id else None)
        | None ->
          if String.length branchRef < 4 then
            // Too short to be a prefix worth guessing from; treat it as a name we don't have.
            return None
          else
            let! matches =
              Sql.query
                "SELECT id FROM branches
                 WHERE id LIKE @prefix AND archived_at IS NULL
                 LIMIT 2"
              |> Sql.parameters [ "prefix", Sql.string (branchRef + "%") ]
              |> Sql.executeAsync (fun read ->
                PT.BranchId.ParseUnsafe(read.string "id"))

            match matches with
            | [ only ] -> return Some only
            | _ -> return None
  }

/// Mint a branch for <param name>, and report whether this call is the one that created it.
///
/// Its own function because F# cannot compile awaits inside a `match` ARM into a static state machine
/// (FS3511, an error under Release). Each task here awaits at its own top level.
let private mintBranch
  (name : string)
  (parentId : PT.BranchId)
  : Task<PT.BranchId * bool> =
  task {
    // Check-then-insert is a race, and concurrent agents sharing a `DARK_BRANCH` is normal
    // here. The guard is inside the INSERT rather than a UNIQUE index, because same-named
    // branches are legal: one started on another instance is a DIFFERENT branch sharing a
    // label, so only the local case (one machine minting two) is prevented. One statement, so
    // SQLite's write lock makes it atomic; the loser writes nothing and re-reads the winner.
    let id = PT.BranchId.Id(System.Guid.NewGuid())

    let insert =
      Sql.query
        "INSERT INTO branches (id, name, parent_id)
         SELECT @id, @name, @parent
         WHERE NOT EXISTS (
           SELECT 1 FROM branches
           WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL
         )"
      |> Sql.parameters
        [ "id", Sql.string (string id)
          "name", Sql.string name
          "parent", Sql.string (string parentId) ]
      |> Sql.executeStatementAsync

    do! insert
    let! winner = liveIdForName name

    return
      match winner with
      | Some w -> (w, w = id)
      // Only reachable if the row we just wrote was archived or merged in between, which means
      // someone deliberately finished it. Report the id we minted rather than inventing one.
      | None -> (id, true)
  }

/// Resolve a branch NAME to its id, starting the branch if that name has none. Returns the id and
/// whether it was just created (callers announce that; silently running on a branch you didn't
/// mean to start is indistinguishable from running on the one you did).
///
/// A NAME is what a person types: reusable, renameable, and two machines each starting a
/// `fix-auth` have two branches. An ID is what every internal reference uses (op tags, per-name
/// bases, relay bundles, parent links) and must survive a rename. This function is that boundary.
let resolveOrCreate
  (name : string)
  (parentId : PT.BranchId)
  : Task<PT.BranchId * bool> =
  task {
    let! existing = liveIdForName name

    match existing with
    | Some id -> return (id, false)
    | None -> return! mintBranch name parentId
  }

/// Fold a branch-scoped propagation `Decision` into `propagation_policy`.
///
/// THIS RULE EXISTS TWICE: `PackageOpPlayback.applyDecision` folds the same op for MAIN, under
/// `BranchId.Main`. Both must use the OP's stamp and both must guard on it, or the same decision
/// lands differently depending on which path it took and two machines stop converging.
///
/// A branch's DECISIONS fold immediately, scoped to the branch; its SetNames do not, since those
/// would leak into main's bindings. A decision is not a binding.
let private foldBranchDecide
  (branchId : PT.BranchId)
  (loc : PT.PackageLocation)
  (policy : PT.PropagationPolicy)
  (reason : string)
  (originTs : string)
  : Task<unit> =
  let modules = String.concat "." loc.modules
  let key =
    [ "branch", Sql.string (string branchId)
      "owner", Sql.string loc.owner
      "modules", Sql.string modules
      "name", Sql.string loc.name
      "origin_ts", Sql.string originTs ]
  if policy = PT.PropagationPolicy.Unset then
    // Clearing is a decision like any other, guarded the same way so a stale unset can't wipe a
    // newer pin.
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
      key @ [ "policy", Sql.string policy.ToText; "note", Sql.string reason ]
    )
    |> Sql.executeStatementAsync

/// Store a branch's authored ops with an EXPLICIT authoring stamp each: serialize + INSERT
/// effective=0 (in the shared log, NOT folded into main) + tag the frontier, in ONE transaction,
/// NO fold. Content-addressed id -> re-store dedups. Returns the number of ops newly stored.
///
/// The stamp is a parameter because the two callers need different ones: a locally-authored op
/// takes a fresh stamp, while an op arriving in a branch BUNDLE keeps the stamp it was authored
/// with, or cross-instance LWW resolves by who imported last rather than who edited last.
let storeDeltaOpsStamped
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp * string>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      // A branch bundle carries its author's stamps, so it is a receive path like any other:
      // advance our clock past them or our own later edits on this branch lose to them forever.
      // See `OriginTs.observe`.
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
          [ "op_id", Sql.uuid id; "branch_id", Sql.string (string branchId) ])

      // one transaction; ops-insert counts come first, so truncate to the op rows.
      let affected =
        Sql.executeTransactionSync [ (insertOps, opRows); (insertTags, tagRows) ]

      for (_, _, op, ts) in prepared do
        match op with
        | PT.PackageOp.Decision(_, loc, reason, PT.DecisionKind.Propagation policy) ->
          do! foldBranchDecide branchId loc policy reason ts
        | _ -> ()

      return affected |> List.truncate (List.length opRows) |> List.sumBy int64
  }

/// Re-fold every BRANCH-scoped propagation decision straight from the log.
///
/// `propagation_policy` is listed in `Seed.projectionTables` as regenerable, which is true only
/// for main: branch ops are `effective = 0` and the fold skips them, so a rebuild that clears the
/// table and re-folds only effective ops would delete every branch pin without bringing it back.
///
/// Oldest-first, so `foldBranchDecide`'s origin_ts guard sees the same sequence authoring did.
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
         PT.BranchId.ParseUnsafe(read.string "branch_id")))

    for (id, blob, ts, branchId) in rows do
      // A blob this build can't read is somebody else's newer op format; skip it rather than fail
      // the whole rebuild over one row we were never going to fold anyway.
      let op =
        try
          Some(BS.PT.PackageOp.deserialize id blob)
        with _ ->
          None
      match op with
      | Some(PT.PackageOp.Decision(_, loc, reason, PT.DecisionKind.Propagation policy)) ->
        do! foldBranchDecide branchId loc policy reason ts
      | _ -> ()
  }

/// Store LOCALLY-AUTHORED branch ops, stamped from the process authoring clock -- the same
/// monotonic clock main authoring uses, never `strftime('now')`. See `LibDB.OriginTs`.
let storeDeltaOps (branchId : PT.BranchId) (ops : List<PT.PackageOp>) : Task<int64> =
  storeDeltaOpsStamped branchId (ops |> List.map (fun op -> (op, OriginTs.next ())))

/// MERGE up, half 1: flip a branch's frontier ops from effective=0 (branch-pending) to
/// effective=1 (part of main). Half 2 is the FOLD (`Seed.applyUnappliedOps`), kept separate
/// because it lives in Seed.fs, which compiles after this file. Returns how many ops flipped.
/// Deterministic replay + origin_ts LWW handle same-name collisions; this is NOT a CRDT merge.
let markMergedEffective (branchId : PT.BranchId) : Task<int64> =
  task {
    // Count what is ABOUT to flip, not what is tagged: a branch can carry tags on ops main
    // already has (a whole-log import does), and counting those reports work the merge didn't do.
    let! toFlip =
      Sql.query
        "SELECT count(*) AS n FROM package_ops p
         JOIN op_branches b ON b.op_id = p.id
         WHERE b.branch_id = @b AND p.effective = 0"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    let! _ =
      Sql.query
        "UPDATE package_ops SET effective = 1
         WHERE effective = 0
           AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeStatementAsync
    return toFlip
  }

/// Finish a merge: mark it merged AND drop the now-redundant frontier, in ONE transaction.
///
/// The gap between them is the only unrecoverable interruption point in a merge: clearing the
/// frontier first leaves a branch with no ops and no merged flag, which the gate refuses as
/// "nothing to merge" while its changes are already in main, and it lists as live forever. Every
/// other point is recoverable by running the merge again, since the frontier is still tagged.
let finishMerge (branchId : PT.BranchId) : unit =
  Sql.executeTransactionSync
    [ ("UPDATE branches SET merged_at = datetime('now') WHERE id = @b",
       [ [ "b", Sql.string (string branchId) ] ])
      ("DELETE FROM op_branches WHERE branch_id = @b",
       [ [ "b", Sql.string (string branchId) ] ]) ]
  |> ignore<List<int>>

// Per-name BASE model: a reload-stable fork marker.
//
// A branch forks off a PARENT. The base for each name it touches is the parent's content-hash for
// that name; a conflict is the parent having moved it since the fork. For parent=main the parent's
// state IS `locations`; a non-main parent is an overlay never materialized there, so its effective
// hashes come from folding the parent chain's SetName rebinds over main. Everything below routes
// through `parentNameHashes` so both cases share one path.

/// A branch's delta ops (deserialized), walking the parent chain (branch -> parent -> ... until
/// 'main'), ordered by origin_ts so same-name rebinds across the chain resolve LWW. Shared by
/// `loadDeltaOps` (the process overlay) and `parentNameHashes` (the fork/merge base).
let chainOverlayOps (branchId : PT.BranchId) : Task<List<PT.PackageOp>> =
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
  |> Sql.parameters [ "start", Sql.string (string branchId) ]
  |> Sql.executeAsync (fun read ->
    BS.PT.PackageOp.deserialize (read.uuid "id") (read.bytes "op_blob"))

/// A branch's registered parent id ('main' if none recorded / unknown).
let parentOf (branchId : PT.BranchId) : Task<PT.BranchId> =
  task {
    let! p =
      Sql.query "SELECT parent_id FROM branches WHERE id = @b"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "parent_id")
    return
      p |> Option.map PT.BranchId.ParseUnsafe |> Option.defaultValue PT.BranchId.Main
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

/// The PARENT's CURRENT effective content-hash per name -- the state a child forks from and merges
/// back into. parent=main -> `locations`; a non-main parent -> main overridden by that chain's own
/// SetName rebinds (latest by origin_ts wins), since a non-main branch lives only as an overlay.
let parentNameHashes (parentId : PT.BranchId) : Task<Map<NameKey, string>> =
  task {
    let! baseMap = mainNameHashes ()
    if parentId.IsMain then
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
/// Answers "where does this dependent live, as this branch sees it". A branch's items have no
/// `locations` row -- that is the name isolation -- so the main projection cannot answer it and a
/// branch-authored dependent would be invisible to discovery. Folded latest-stamp-wins per NAME
/// before inverting, so a hash the branch has moved off does not come back as live.
let chainBindingsByHash
  (branchId : PT.BranchId)
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

    // LIST-valued: one hash can be bound at several names, since identical content IS one item.
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
/// An `Override` counts, same as in `SCM.PackageOps.bindingFromOp`: it IS a rebind that also carries a
/// decision id. Counting only `SetName` leaves a name bound solely by resolving a
/// conflict with no `branch_name_bases` row, and without a base the detector cannot prove both
/// sides moved, so that name can never conflict again.
let private rebindKeys (ops : List<PT.PackageOp>) : List<PT.PackageLocation> =
  ops
  |> List.choose (fun op ->
    match op with
    | PT.PackageOp.SetName(loc, _, _) -> Some loc
    | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override _) -> Some loc
    | _ -> None)

/// Record the per-name BASE for a branch: for each name these ops rebind, capture the PARENT's
/// CURRENT content-hash (or '' if the name is new to the parent) ONCE, first touch wins (INSERT OR
/// IGNORE). Content hashes are stable across reload, so this is a reliable fork marker. Call after
/// storeDeltaOps.
let recordNameBases
  (branchId : PT.BranchId)
  (parentId : PT.BranchId)
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
          [ "b", Sql.string (string branchId)
            "owner", Sql.string loc.owner
            "modules", Sql.string (String.concat "." loc.modules)
            "name", Sql.string loc.name
            "base", Sql.string baseHash ]
        |> Sql.executeStatementAsync
  }

/// This branch's recorded per-name bases: the parent's hash for each name at the branch's first
/// touch. The one place that reads the table, so callers don't each hand-roll the same SELECT.
let nameBasesFor (branchId : PT.BranchId) : Task<List<NameKey * string>> =
  Sql.query
    "SELECT owner, modules, name, base_hash FROM branch_name_bases WHERE branch_id = @b"
  |> Sql.parameters [ "b", Sql.string (string branchId) ]
  |> Sql.executeAsync (fun read ->
    (read.string "owner", read.string "modules", read.string "name"),
    read.string "base_hash")

/// Names where the PARENT's CURRENT content-hash differs from this branch's recorded base, i.e.
/// the parent changed the name since the branch forked off it. These are the merge conflicts.
let nameConflicts (branchId : PT.BranchId) : Task<List<string>> =
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

// Conflict PRESENTATION lives in Dark (`SCM.Conflicts`): one base-agnostic detector over
// branch_name_bases OR sync_bases, returning STRUCTURED conflicts the CLI renders and the
// `conflicts` table records. `nameConflicts` above stays -- it's the merge GATE, not a view.

/// REBASE: accept the parent's current state as this branch's new base. Returns the names the
/// parent had changed since the fork; after this the branch's own ops layer on top (LWW by
/// origin_ts) and merge is unblocked.
let rebase (branchId : PT.BranchId) : Task<List<string>> =
  task {
    let! parentId = parentOf branchId
    let! changed = nameConflicts branchId
    let! parentHashes = parentNameHashes parentId
    // Rewrite each base to the parent's current hash for that name.
    let! bases =
      Sql.query
        "SELECT owner, modules, name FROM branch_name_bases WHERE branch_id = @b"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
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
            "b", Sql.string (string branchId)
            "owner", Sql.string o
            "modules", Sql.string m
            "name", Sql.string n ]
        |> Sql.executeStatementAsync
    return changed
  }

// Per-name RESOLUTION: keep-mine / take-theirs for a conflicted name, done on the op log and LWW
// with no "source" column. Both set the branch's base for that name to the parent's current hash,
// which is what clears the conflict.

/// Parse "owner.Mod.Sub.name" -> (owner, dotted-modules, name). Mirrors the detector's fqn format.
let private parseFqn (fqn : string) : Option<string * string * string> =
  match fqn.Split('.') |> Array.toList with
  | owner :: rest when not (List.isEmpty rest) ->
    // rest = [Mod; Sub; ...; name]; the last segment is the name, the rest are modules.
    match List.rev rest with
    | name :: revModules ->
      Some(owner, revModules |> List.rev |> String.concat ".", name)
    | [] -> None
  | _ -> None

/// The branch's OWN frontier ops (deserialized, with ids) -- just this branch's tag, not the chain.
let private ownFrontierOps
  (branchId : PT.BranchId)
  : Task<List<System.Guid * PT.PackageOp>> =
  Sql.query
    "SELECT p.id, p.op_blob FROM package_ops p
     JOIN op_branches ob ON ob.op_id = p.id
     WHERE ob.branch_id = @b"
  |> Sql.parameters [ "b", Sql.string (string branchId) ]
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

/// Set the branch's recorded base for one name to the parent's CURRENT hash (clears the conflict).
let private setBaseToParentFor
  (branchId : PT.BranchId)
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
          "b", Sql.string (string branchId)
          "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name ]
      |> Sql.executeStatementAsync
  }

/// RESOLVE take-theirs: drop the branch's binding for `fqn` (untag its SetName ops) so the overlay
/// falls back to the parent's version, then set the base to the parent's current.
let resolveTakeTheirs
  (branchId : PT.BranchId)
  (fqn : string)
  : Task<Result<unit, string>> =
  task {
    match parseFqn fqn with
    | None -> return Error $"can't parse a name from \"{fqn}\""
    | Some key ->
      let! ops = ownFrontierOps branchId
      let ids = setNameIdsFor ops key
      if List.isEmpty ids then
        // The name: this reaches a person through `dark resolve`, and they typed a name.
        let! branchName = nameForId branchId
        return
          Error $"branch \"{branchName}\" doesn't bind {fqn} -- nothing to resolve"
      else
        for id in ids do
          do!
            Sql.query "DELETE FROM op_branches WHERE op_id = @id AND branch_id = @b"
            |> Sql.parameters
              [ "id", Sql.uuid id; "b", Sql.string (string branchId) ]
            |> Sql.executeStatementAsync
        do! setBaseToParentFor branchId key
        return Ok()
  }

/// RESOLVE keep-mine: re-stamp the branch's SetName for `fqn` to the newest origin_ts so it wins
/// LWW even if the parent's binding is newer, then set the base to the parent's current.
let resolveKeepMine
  (branchId : PT.BranchId)
  (fqn : string)
  : Task<Result<unit, string>> =
  task {
    match parseFqn fqn with
    | None -> return Error $"can't parse a name from \"{fqn}\""
    | Some key ->
      let! ops = ownFrontierOps branchId
      let ids = setNameIdsFor ops key
      if List.isEmpty ids then
        // The name: this reaches a person through `dark resolve`, and they typed a name.
        let! branchName = nameForId branchId
        return
          Error $"branch \"{branchName}\" doesn't bind {fqn} -- nothing to resolve"
      else
        // Re-stamp strictly AFTER every existing op, not just "now": authoring stamps can run
        // ahead of wall clock (see `LibDB.OriginTs`), so global MAX(origin_ts) + 1s is what
        // guarantees this SetName wins LWW.
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
/// Same query as the merge base (`chainOverlayOps`), so overlay and base can never drift, which
/// also means B off A off main sees A's frontier AND its own.
let loadDeltaOps (branchId : PT.BranchId) : Task<List<PT.PackageOp>> =
  chainOverlayOps branchId

/// The branch's OWN frontier ops (deserialized), oldest-first -- its authoring history, for
/// `dark log <branch>` (the SEQUENCE, complementary to `diff`'s net effect). Just this branch's
/// tag, NOT the parent chain.
let frontierOps (branchId : PT.BranchId) : Task<List<PT.PackageOp>> =
  Sql.query
    "SELECT p.id, p.op_blob FROM package_ops p
     JOIN op_branches ob ON ob.op_id = p.id
     WHERE ob.branch_id = @b
     ORDER BY p.origin_ts, p.rowid"
  |> Sql.parameters [ "b", Sql.string (string branchId) ]
  |> Sql.executeAsync (fun read ->
    BS.PT.PackageOp.deserialize (read.uuid "id") (read.bytes "op_blob"))

/// MERGE into a NON-MAIN parent: the parent is an overlay, never materialized in main's
/// projections, so we do NOT flip effective / fold (that would leak the child into main). Instead
/// RETAG the child's frontier ops onto the parent, whose overlay folds them when a process runs
/// `--branch <parent>`. INSERT-OR-IGNORE + DELETE handles the (rare) shared-op case.
///
/// Retag and mark-merged go in ONE transaction, as in `finishMerge`: the child stops owning its
/// ops the instant the DELETE lands, so an interruption before the merged flag is set leaves a
/// branch the gate refuses to merge.
let retagFrontierToParent
  (branchId : PT.BranchId)
  (parentId : PT.BranchId)
  : Task<int64> =
  task {
    let! n =
      Sql.query "SELECT count(*) AS n FROM op_branches WHERE branch_id = @b"
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Sql.executeTransactionSync
      [ ("INSERT OR IGNORE INTO op_branches (op_id, branch_id)
          SELECT op_id, @parent FROM op_branches WHERE branch_id = @b",
         [ [ "parent", Sql.string (string parentId)
             "b", Sql.string (string branchId) ] ])
        ("DELETE FROM op_branches WHERE branch_id = @b",
         [ [ "b", Sql.string (string branchId) ] ])
        ("UPDATE branches SET merged_at = datetime('now') WHERE id = @b",
         [ [ "b", Sql.string (string branchId) ] ]) ]
    |> ignore<List<int>>
    return n
  }
