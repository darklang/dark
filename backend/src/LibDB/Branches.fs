/// The branch STORE. A branch is a stable id, a name alias, and a FRONTIER of ops.
///
/// Ops live in the shared content-addressed `package_ops`; a branch's own are inserted
/// effective=0 (in the log, not folded into main) and tagged in `op_branches`. Its overlay PM is
/// `withExtraOps corePM (loadDeltaOps branchId)`. Merging up flips those ops effective=1 and folds
/// them, reusing the same gate.
module LibDB.Branches

open System.Threading.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing

/// The content-addressed row id for an op. One definition, shared with authoring and
/// the fold, so a branch tags the same op id those two mint.
/// An op's row id: its content hash. Public because the branch authoring path asks whether the log
/// already holds an op before deciding it needs restating (see `PT.restatingBinding`).
let opRowId (op : PT.PackageOp) : System.Guid = Hashing.computeOpRowId op

/// Create a branch off `parentId` (main for a top-level branch). The fork point is tracked PER
/// NAME (branch_name_bases, recorded on authoring), not a whole-branch watermark, so there's no
/// base column here. Idempotent on id.
///
/// Branches are reclaimed only by an EXPLICIT archive. There is no TTL and no auto-archive, so an
/// abandoned agent branch stays live until someone says otherwise.
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

/// Register a branch if it is new, and leave an existing row alone. Never revives: that is
/// `createBranch`'s job, for the paths that mean it (a bundle arriving says the branch is alive
/// somewhere). Authoring goes through this one, so an edit cannot un-merge a branch by landing on it.
let registerIfNew
  (id : PT.BranchId)
  (name : string)
  (parentId : PT.BranchId)
  : Task<unit> =
  Sql.query
    "INSERT OR IGNORE INTO branches (id, name, parent_id) VALUES (@id, @name, @parent)"
  |> Sql.parameters
    [ "id", Sql.string (string id)
      "name", Sql.string name
      "parent", Sql.string (string parentId) ]
  |> Sql.executeStatementAsync

/// Merged or archived: finished, and not somewhere you can author. An op tagged to a finished branch
/// is stranded, with nothing left to merge it.
let isFinished (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches
         WHERE id = @id AND (archived_at IS NOT NULL OR merged_at IS NOT NULL)"
      |> Sql.parameters [ "id", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }

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

let liveIdForName (name : string) : Task<Option<PT.BranchId>> =
  Sql.query
    "SELECT id FROM branches
     WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL
     ORDER BY created_at DESC, rowid DESC LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    PT.BranchId.ParseUnsafe(read.string "id"))

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

/// The branch a REF refers to, without creating anything: a name, a full id, or an unambiguous id
/// prefix. All three get printed at people, and `dark branches` abbreviates ids to 8 characters,
/// so the prefix is the form most likely to be pasted. Mirrors `Cli.Branch.lookupRef`, which the
/// branch verbs go through.
///
/// A miss says WHY, because only one kind of miss should create a branch. A bare `None` would stand
/// for four different things at once, and the caller would silently start a branch named after a
/// peer's uuid, or after an ambiguous prefix.
type RefLookup =
  | Found of PT.BranchId
  /// A full id, and no branch here has it: a peer's, or a mispaste.
  | UnknownId of PT.BranchId
  /// An id prefix matching more than one branch.
  | Ambiguous of string
  /// Not a live name, not an id, not a prefix: a name this store has never had.
  | NoSuchName of string

let lookupRef (branchRef : string) : Task<RefLookup> =
  task {
    // Main is resolvable by name and by id like any other branch, and has to be: it has no
    // `branches` row, so neither the name lookup nor the id-prefix search below can find it.
    if
      branchRef = PT.BranchId.MainName
      || PT.BranchId.Parse branchRef = Some PT.BranchId.Main
    then
      return Found PT.BranchId.Main
    else
      match! liveIdForName branchRef with
      | Some id -> return Found id
      | None ->
        match PT.BranchId.Parse branchRef with
        | Some id ->
          let! exact = exists id
          return (if exact then Found id else UnknownId id)
        | None ->
          if String.length branchRef < 4 then
            // Too short to be a prefix worth guessing from; treat it as a name we don't have.
            return NoSuchName branchRef
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
            | [ only ] -> return Found only
            | [] -> return NoSuchName branchRef
            | _ -> return Ambiguous branchRef
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

/// INSERT (id, blob, ts) rows into `package_ops` INERT (applied = 0, effective = 0: in
/// the shared log, NOT folded into main) and tag them onto the branch, in ONE
/// transaction. Content-addressed id -> re-store dedups; returns how many ops were
/// newly stored. Per call site: the prepared rows and <param source>, the
/// `op_branches.source` vocabulary ('op' for authored or undecodable-bundle ops).
///
/// Never tag an op main already runs. The row dedups by content, so a branch author (or a bundle)
/// can hit an id that is effective = 1; a tag on it hid main's own op from main's draft and commit,
/// since every draft query excludes tagged ids. An effective op is never tagged, and the other
/// side of that invariant is `Inserts.insertAndApplyOpsWith`, which untags what it makes effective.
let private storeInertTagged
  (branchId : PT.BranchId)
  (source : string)
  (records : List<System.Guid * byte[] * string>)
  : int64 =
  let insertOps =
    "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
     VALUES (@id, @op_blob, 0, 0, @origin_ts)"
  let opRows =
    records
    |> List.map (fun (id, blob, ts) ->
      [ "id", Sql.uuid id; "op_blob", Sql.bytes blob; "origin_ts", Sql.string ts ])
  let insertTags =
    "INSERT OR IGNORE INTO op_branches (op_id, branch_id, source)
     SELECT @op_id, @branch_id, @source
     WHERE NOT EXISTS (SELECT 1 FROM package_ops WHERE id = @op_id AND effective = 1)"
  let tagRows =
    records
    |> List.map (fun (id, _, _) ->
      [ "op_id", Sql.uuid id
        "branch_id", Sql.string (string branchId)
        "source", Sql.string source ])
  // one transaction; ops-insert counts come first, so truncate to the op rows.
  Sql.executeTransactionSync [ (insertOps, opRows); (insertTags, tagRows) ]
  |> List.truncate (List.length opRows)
  |> List.sumBy int64


/// Store a branch's authored ops with an EXPLICIT authoring stamp each: serialize + INSERT
/// effective=0 (in the shared log, NOT folded into main) + tag the frontier, in ONE transaction,
/// NO fold. Content-addressed id -> re-store dedups. Returns the number of ops newly stored.
///
/// The stamp is a parameter because the two callers need different ones: a locally-authored op
/// takes a fresh stamp, while an op arriving in a branch BUNDLE keeps the stamp it was authored
/// with, or cross-instance LWW resolves by who imported last rather than who edited last.
/// <param source> says what put these ops on the branch: 'op', 'propagation' or 'resolution', the same
/// vocabulary as `locations.source`. Authoring passes 'op'; the two other kinds are recorded at the one
/// point each is known, `pmPropagate` and `pmSetName`.
/// Which of <param ids> the op log already holds. A restatement is only needed for an op that would
/// dedupe, and asking is cheaper than discovering it from an insert that affected no rows.
let heldOpIds (ids : List<System.Guid>) : Task<Set<System.Guid>> =
  task {
    if List.isEmpty ids then
      return Set.empty
    else
      let clause = ids |> List.mapi (fun i _ -> $"@id_{i}") |> String.concat ", "
      let! rows =
        Sql.query $"SELECT id FROM package_ops WHERE id IN ({clause})"
        |> Sql.parameters (ids |> List.mapi (fun i id -> ($"id_{i}", Sql.uuid id)))
        |> Sql.executeAsync (fun read -> read.uuid "id")
      return Set.ofList rows
  }


let storeDeltaOpsStampedFrom
  (source : string)
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

      let stored =
        prepared
        |> List.map (fun (id, blob, _, ts) -> (id, blob, ts))
        |> storeInertTagged branchId source

      for (_, _, op, ts) in prepared do
        match op with
        | PT.PackageOp.Decision(_, loc, reason, PT.DecisionKind.Propagation policy) ->
          do! foldBranchDecide branchId loc policy reason ts
        | _ -> ()

      return stored
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
      match BS.PT.PackageOp.tryDeserialize id blob with
      | Some(PT.PackageOp.Decision(_, loc, reason, PT.DecisionKind.Propagation policy)) ->
        do! foldBranchDecide branchId loc policy reason ts
      | _ -> ()
  }

/// Store LOCALLY-AUTHORED branch ops, stamped from the process authoring clock -- the same
/// monotonic clock main authoring uses, never `strftime('now')`. See `LibDB.OriginTs`.
let storeDeltaOpsStamped
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp * string>)
  : Task<int64> =
  storeDeltaOpsStampedFrom "op" branchId ops

/// The raw sibling of `storeDeltaOpsStamped`, for a bundle's ops this build cannot DECODE: the
/// sender's id and blob verbatim, effective = 0, tagged, with the guard the decoded path has. No
/// Decision folding, since nothing here can read them. Present-but-inert beats absent: a branch
/// three ops short resolves differently than on the sender, and the next build that can read them
/// applies them, which is what main sync already does with such ops.
let storeDeltaBlobsStamped
  (branchId : PT.BranchId)
  (records : List<System.Guid * byte[] * string>)
  : Task<int64> =
  task {
    if List.isEmpty records then
      return 0L
    else
      records |> List.iter (fun (_, _, ts) -> OriginTs.observe ts)
      return storeInertTagged branchId "op" records
  }

/// Authored ops, fresh stamps.
let storeDeltaOps (branchId : PT.BranchId) (ops : List<PT.PackageOp>) : Task<int64> =
  storeDeltaOpsStamped branchId (ops |> List.map (fun op -> (op, OriginTs.next ())))

/// `storeDeltaOps` for ops with a known provenance: 'propagation' or 'resolution'.
let storeDeltaOpsFrom
  (source : string)
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  storeDeltaOpsStampedFrom
    source
    branchId
    (ops |> List.map (fun op -> (op, OriginTs.next ())))

// Flipping a branch's frontier effective, and closing the branch afterwards, are in Dark
// (`SCM.Branches.markMergedEffective` / `.finishMerge`): both are SQL, and the second is one
// transaction because the gap inside it is a merge's only unrecoverable interruption point. What
// runs between them is the fold, which is `scmApplyMergedOps`.

// Per-name BASE model: a reload-stable fork marker.
//
// A branch forks off a PARENT. The base for each name it touches is the parent's content-hash for
// that name; a conflict is the parent having moved it since the fork. For parent=main the parent's
// state IS `locations`; a non-main parent is an overlay never materialized there, so its effective
// hashes come from folding the parent chain's SetName rebinds over main. Everything below routes
// through `parentNameHashes` so both cases share one path.

/// A branch's delta ops (deserialized), walking the parent chain (branch -> parent -> ... until main),
/// ordered by origin_ts so same-name rebinds across the chain resolve LWW. Shared by
/// `loadDeltaOps` (the process overlay) and `parentNameHashes` (the fork/merge base).
/// Skips what this build cannot decode, like every other reader of the log. This one matters most: it
/// is the overlay a process RESOLVES through, loaded at boot for whatever branch you are standing on, so
/// raising here does not fail one command, it fails the CLI.
let chainOverlayOps (branchId : PT.BranchId) : Task<List<PT.PackageOp>> =
  task {
    let! decoded =
      Sql.query
        "WITH RECURSIVE chain(bid) AS (
           SELECT @start
           UNION
           SELECT b.parent_id FROM branches b JOIN chain c ON b.id = c.bid
           WHERE b.parent_id <> @mainId
         )
         SELECT p.id, p.op_blob
         FROM package_ops p
         JOIN op_branches ob ON ob.op_id = p.id
         WHERE ob.branch_id IN (SELECT bid FROM chain)
           -- An ancestor's WIP stays its own: only the branch itself contributes uncommitted
           -- ops to the view; parents contribute what they committed.
           AND (ob.branch_id = @start OR p.commit_hash IS NOT NULL)
         -- `p.id`, not `p.rowid`: rowid is ARRIVAL order, which is a fact about this machine, so
         -- two stores holding the same ops applied them in different orders whenever two stamps
         -- tied exactly, and showed different code. An op id is a content hash, so this order is
         -- the same everywhere.
         --
         -- Not yet the same tie-break as the FOLD, which compares the bound item's hash
         -- (`Lww.isStale`): a branch and main can still disagree on an exact tie until the overlay
         -- learns that rule. Same-stamp ties need two authorings in the same instant, so this is
         -- the ordering half of the fix, not the whole of it.
         ORDER BY p.origin_ts, p.id"
      // `@mainId`, never the literal 'main': `parent_id` holds main's UUID, so comparing against the
      // NAME is true of every row, and the walk then terminates only because main has no `branches`
      // row. The same drift `Branching.BranchId` exists to stop, in SQL, where no type checker reads
      // it.
      |> Sql.parameters
        [ "start", Sql.string (string branchId)
          "mainId", Sql.string (string PT.BranchId.Main) ]
      |> Sql.executeAsync (fun read ->
        BS.PT.PackageOp.tryDeserialize (read.uuid "id") (read.bytes "op_blob"))
    return decoded |> List.choose (fun o -> o)
  }

/// Re-arm every branch event <param branchId>'s bundle may have been waiting on, so
/// the next fold looks again.
///
/// Two populations: events parked as `applied = 2` (see
/// `PackageOpPlayback.applyBranchEvent`), and applied `BranchEvent`s naming THIS
/// branch -- an event for a then-unknown branch folded to nothing and marked itself
/// done, which is wrong once its bundle turns up. Re-folding is idempotent (flips
/// only inert ops, stamps only uncommitted ones), and events are few.
///
/// Found by tag byte rather than decoding each blob: after the 8-byte header the
/// first payload byte is the case tag, and `BranchEvent` is 10.
let undeferBranchEvents (branchId : PT.BranchId) : Task<unit> =
  task {
    do!
      Sql.query "UPDATE package_ops SET applied = 0 WHERE applied = 2"
      |> Sql.executeStatementAsync

    let! events =
      Sql.query
        "SELECT id, op_blob FROM package_ops
         WHERE applied = 1 AND effective = 1 AND substr(op_blob, 9, 1) = X'0A'"
      |> Sql.executeAsync (fun read ->
        (read.uuid "id",
         BS.PT.PackageOp.tryDeserialize (read.uuid "id") (read.bytes "op_blob")))

    let mine =
      events
      |> List.choose (fun (id, op) ->
        match op with
        | Some(PT.PackageOp.BranchEvent(b, _, _)) when b = branchId -> Some id
        | _ -> None)

    for id in mine do
      do!
        Sql.query "UPDATE package_ops SET applied = 0 WHERE id = @id"
        |> Sql.parameters [ "id", Sql.string (string id) ]
        |> Sql.executeStatementAsync
  }


/// <param branchId> and its ancestors, nearest first, ending at main.
///
/// The order IS the precedence: everything scoped to a branch chain resolves most-specific-first, so
/// a caller can take the first row it finds. Main is always last and always present, since every
/// chain ends there.
let branchChain (branchId : PT.BranchId) : Task<List<PT.BranchId>> =
  task {
    // The whole body is one shape, not an `if` around two `return`s: a `let!` inside one arm of a
    // conditional makes the task state machine dynamically compiled (FS3511), which is a warning in
    // Debug and an error in Release.
    let! rows =
      if branchId.IsMain then
        Task.FromResult []
      else
        // Depth carried so the walk comes back nearest-first; a plain UNION loses the order.
        Sql.query
          "WITH RECURSIVE chain(bid, depth) AS (
             SELECT @start, 0
             UNION ALL
             SELECT b.parent_id, c.depth + 1 FROM branches b JOIN chain c ON b.id = c.bid
             WHERE b.parent_id <> @mainId AND c.depth < 64
           )
           SELECT bid FROM chain ORDER BY depth"
        |> Sql.parameters
          [ "start", Sql.string (string branchId)
            "mainId", Sql.string (string PT.BranchId.Main) ]
        |> Sql.executeAsync (fun read -> read.string "bid")

    let ids = rows |> List.choose PT.BranchId.Parse
    return ids @ [ PT.BranchId.Main ]
  }

/// A branch's registered parent id; main when none is recorded, or the branch is unknown here.
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

/// What an op does to a NAME: binds it to some content, ends it, or neither.
///
/// One reading of "which ops bind" for every overlay fold below. An `Override` counts as a bind:
/// it IS a rebind that also carries a decision id (`resolve mine` writes one), and every fold that
/// forgot it produced a different wrong answer -- an invisible dependent, a missing name base, a
/// stale fork base. Answering it in one place is what stops the next fold from forgetting it too.
type NameEffect =
  | Binds of PT.PackageLocation * PT.ItemKind * string
  | Ends of PT.PackageLocation

let nameEffect (op : PT.PackageOp) : Option<NameEffect> =
  match op with
  | PT.PackageOp.SetName(loc, target, _)
  | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target) ->
    let (Hash h) = target.hash
    Some(Binds(loc, target.kind, h))
  | PT.PackageOp.Unbind(loc, _) -> Some(Ends loc)
  | _ -> None


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
            let key (loc : PT.PackageLocation) =
              (loc.owner, String.concat "." loc.modules, loc.name)
            match nameEffect op with
            | Some(Binds(loc, _, h)) -> Map.add (key loc) h m
            | Some(Ends loc) -> Map.remove (key loc) m
            | None -> m)
          baseMap
  }


/// <param ops> with any REVERT re-authored as the decision it is (`PT.restatingBinding`).
///
/// A `SetName` the log already holds, for a name this branch currently binds to something else.
/// Authoring only: an incoming duplicate really is a duplicate, so the receive paths that share
/// `storeDeltaOps` never come through here. Liveness is the branch's own -- its chain folded over
/// main, since its items have no `locations` row.
let restateReverts
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let bindings =
      ops |> List.filter (fun op -> Option.isSome (PT.restatingBinding "" op))

    if List.isEmpty bindings then
      return ops
    else
      let! liveHere = parentNameHashes branchId
      let! held = heldOpIds (bindings |> List.map opRowId)

      let saidAgain (op : PT.PackageOp) : bool =
        match op with
        | PT.PackageOp.SetName(location, target, _) ->
          let (Hash h) = target.hash
          let key =
            (location.owner, String.concat "." location.modules, location.name)
          Map.tryFind key liveHere <> Some h
        | _ -> false

      return
        ops
        |> List.map (fun op ->
          if saidAgain op && Set.contains (opRowId op) held then
            PT.restatingBinding (OriginTs.next ()) op |> Option.defaultValue op
          else
            op)
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
          match nameEffect op with
          | Some(Binds(loc, kind, h)) -> Map.add loc (kind, h) m
          | Some(Ends loc) -> Map.remove loc m
          | None -> m)
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
/// A name bound solely by resolving a conflict still needs its `branch_name_bases` row: without a
/// base the detector cannot prove both sides moved, so that name can never conflict again. Hence
/// `nameEffect`, which counts an `Override` as the rebind it is.
let private rebindKeys (ops : List<PT.PackageOp>) : List<PT.PackageLocation> =
  ops
  |> List.choose (fun op ->
    match nameEffect op with
    | Some(Binds(loc, _, _)) -> Some loc
    // Unbinding moves a name too, to nothing; the base is what tells that apart from the parent
    // editing it meanwhile.
    | Some(Ends loc) -> Some loc
    | None -> None)

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

// READING the bases, comparing them against the parent (the merge GATE), and moving them (REBASE) are
// all in Dark: `SCM.Branches.nameBases`, `.nameConflicts`, `.rebase`. Conflict PRESENTATION is Dark's
// too (`SCM.Conflicts`), over the same rows. What is left here is WRITING them, which happens mid-author
// inside `scmAddOps` and so cannot cross back into Dark, and `parentNameHashes`, which that write needs.

// Per-name RESOLUTION -- keep-mine and take-theirs -- is in Dark (`SCM.Branches`), along with the fqn
// parsing it needs. Both are op-log surgery plus a base update, and take-theirs never leaves Dark at
// all. Keep-mine comes back through `scmStoreBranchOpStamped`, for the one thing it cannot do: turn an
// op into bytes under a stamp it chose.

/// Load a branch's delta ops (deserialized) -- the delta to overlay on core:
///   `LibDB.PackageManager.withExtraOps corePM (loadDeltaOps branchId)`.
/// Same query as the merge base (`chainOverlayOps`), so overlay and base can never drift, which
/// also means B off A off main sees A's frontier AND its own.
let loadDeltaOps (branchId : PT.BranchId) : Task<List<PT.PackageOp>> =
  chainOverlayOps branchId
