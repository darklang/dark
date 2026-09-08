/// Applies PackageOps to the DB projection tables.
/// These tables (package_types, package_values, package_functions, locations) are projections
/// of the source-of-truth package_ops table.
///
/// All writes go through a caller-provided SqliteConnection so the entire op
/// batch can run inside one outer transaction. On cold-start grow (9000+ ops)
/// that turns ~20k tiny WAL commits into one, which is the difference between
/// a 5-second startup and a sub-second one. Inserts.fs (the other caller) opens
/// a one-shot connection per call.
module LibDB.PackageOpPlayback


open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BS = LibSerialization.Binary.Serialization
module DE = LibDB.DependencyExtractor
open LibSerialization.Hashing

// `Ctx` and the `exec` / `execRows` / scalar readers / `p*` binders: one connection and one
// prepared-command cache for the whole batch. See LibDB.PreparedBatch for why.
open LibDB.PreparedBatch


/// Bind the location key nearly every projection statement filters on: $owner,
/// $modules (dot-joined) and $name. Per call site, only the location varies.
let private pLoc (cmd : SqliteCommand) (location : PT.PackageLocation) : unit =
  p cmd "$owner" location.owner
  p cmd "$modules" (String.concat "." location.modules)
  p cmd "$name" location.name


// ------------------------------------------------------------------
// Dependency table maintenance.
// ------------------------------------------------------------------

/// Record what an item's body calls: one row per callee, by hash AND by the name this parse resolved
/// it through.
///
/// ADDS, never replaces: content is immutable so a hash's callees never change, but
/// two names can hold one body, and each name it was resolved through must survive.
/// Readers join `locations` for what is live.
let updateDependencies
  (ctx : Ctx)
  (itemHash : string)
  (deps : List<DE.Dependency>)
  : Task<unit> =
  task {
    if List.isEmpty deps then
      ()
    else
      // Each dep contributes 6 placeholders to the VALUES list.
      let placeholders =
        deps
        |> List.mapi (fun i _ ->
          $"($item_hash, $hash_{i}, $kind_{i}, $owner_{i}, $modules_{i}, $name_{i})")
        |> String.concat ", "

      let sql =
        "INSERT OR IGNORE INTO package_dependencies "
        + "(item_hash, depends_on_hash, depends_on_item_type, depends_on_owner, depends_on_modules, depends_on_name) "
        + "VALUES "
        + placeholders

      do!
        exec ctx sql (fun cmd ->
          p cmd "$item_hash" itemHash
          deps
          |> List.iteri (fun i dep ->
            let (Hash hashStr) = dep.hash
            p cmd $"$hash_{i}" hashStr
            p cmd $"$kind_{i}" (dep.itemKind.toString ())
            match dep.location with
            | Some loc ->
              p cmd $"$owner_{i}" loc.owner
              p cmd $"$modules_{i}" (String.concat "." loc.modules)
              p cmd $"$name_{i}" loc.name
            | None ->
              p cmd $"$owner_{i}" System.DBNull.Value
              p cmd $"$modules_{i}" System.DBNull.Value
              p cmd $"$name_{i}" System.DBNull.Value))
  }


// ------------------------------------------------------------------
// Individual op handlers.
// ------------------------------------------------------------------

/// Prove that an already-stored row holds the same body we are about to write over.
///
/// A hash is a content identity, not merely a database key, so two different bodies
/// under one hash is a bug somewhere upstream, not a row to overwrite. Raise rather than
/// let the second body silently become the first.
let private ensureExistingBodyMatches
  (ctx : Ctx)
  (kind : string)
  (table : string)
  (hash : Hash)
  (incomingFingerprint : Hash)
  (storedFingerprint : byte[] -> Hash)
  : Task<unit> =
  task {
    let (Hash hashStr) = hash
    let! existing =
      bytesOption ctx $"SELECT pt_def FROM {table} WHERE hash = $hash" (fun cmd ->
        p cmd "$hash" hashStr)

    match existing with
    | Some bytes when storedFingerprint bytes = incomingFingerprint -> return ()
    | Some _ ->
      return
        raise (
          System.InvalidOperationException(
            $"{kind} hash {hashStr} is already stored with different content"
          )
        )
    | None ->
      return
        Exception.raiseInternal
          "Package projection insert conflicted, but its row was not found"
          [ "kind", kind; "hash", hashStr; "table", table ]
  }


/// Write one content-addressed projection row: insert it, or, if the hash is already
/// present, check the stored body matches before refreshing its metadata.
///
/// <param columns> carries the body and metadata, and is written on both paths.
/// <param insertOnlyNulls> names columns the insert must mention but this path never
/// fills: `package_values` leaves `rt_dval` and `value_type` for
/// `Seed.evaluateAllValues`, which runs once every op in the batch has been applied and
/// cross-package references can resolve.
/// <param mayRewriteExisting> is what a BRANCH does not get.
///
/// An existing row and an incoming item with the same hash agree about behaviour by construction --
/// that is what the hash is -- so the only thing that can differ is the doc comment, and the row is
/// main's. A branch folds its own content here so its bodies are runnable, and rewriting the row
/// would publish the branch's wording to everyone. A branch says what it thinks an item is with a
/// `Describe`, which stays on the branch like its names do.
let private upsertContentAddressed
  (ctx : Ctx)
  (kind : string)
  (table : string)
  (hash : Hash)
  (columns : List<string * obj>)
  (insertOnlyNulls : List<string>)
  (mayRewriteExisting : bool)
  (incomingFingerprint : Hash)
  (storedFingerprint : byte[] -> Hash)
  : Task<unit> =
  task {
    let (Hash hashStr) = hash
    let names = columns |> List.map fst

    let bind (cmd : SqliteCommand) : unit =
      p cmd "$hash" hashStr
      columns |> List.iter (fun (name, value) -> p cmd $"${name}" value)

    let insertColumns = ("hash" :: names) @ insertOnlyNulls |> String.concat ", "
    let insertValues =
      ("$hash" :: (names |> List.map (fun name -> $"${name}")))
      @ (insertOnlyNulls |> List.map (fun _ -> "NULL"))
      |> String.concat ", "

    let! inserted =
      execRows
        ctx
        $"""
        INSERT INTO {table} ({insertColumns})
        VALUES ({insertValues})
        ON CONFLICT(hash) DO NOTHING
        """
        bind

    if inserted = 0 then
      do!
        ensureExistingBodyMatches
          ctx
          kind
          table
          hash
          incomingFingerprint
          storedFingerprint

      if mayRewriteExisting then
        let assignments =
          names |> List.map (fun name -> $"{name} = ${name}") |> String.concat ", "

        do! exec ctx $"UPDATE {table} SET {assignments} WHERE hash = $hash" bind
  }


/// Apply a single AddType op to the package_types table.
let private applyAddType
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (typ : PT.PackageType.PackageType)
  : Task<unit> =
  task {
    // Use the hash already set on the item (computed by LoadPackagesFromDisk
    // or Propagation with SCC awareness). Only recompute if hash is empty.
    let hash =
      match typ.hash with
      | Hash "" -> Hashing.computeTypeHash Hashing.Normal typ
      | h -> h
    let typ = { typ with hash = hash }
    let (Hash hashStr) = hash

    do!
      upsertContentAddressed
        ctx
        "type"
        "package_types"
        hash
        [ "pt_def", box (BS.PT.PackageType.serialize hashStr typ)
          "rt_def",
          box (typ |> PT2RT.PackageType.toRT |> BS.RT.PackageType.serialize hashStr)
          "description", box typ.description ]
        []
        mayRewriteExisting
        (Hashing.computeTypeHash Hashing.Normal typ)
        (fun bytes ->
          BS.PT.PackageType.deserialize hash bytes
          |> Hashing.computeTypeHash Hashing.Normal)

    let refs = DE.extractFromType typ
    do! updateDependencies ctx hashStr refs
  }

/// Apply a single AddValue op to the package_values table.
let private applyAddValue
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (value : PT.PackageValue.PackageValue)
  : Task<unit> =
  task {
    let hash =
      match value.hash with
      | Hash "" -> Hashing.computeValueHash Hashing.Normal value
      | h -> h
    let value = { value with hash = hash }
    let (Hash hashStr) = hash

    do!
      upsertContentAddressed
        ctx
        "value"
        "package_values"
        hash
        [ "pt_def", box (BS.PT.PackageValue.serialize hashStr value)
          "description", box value.description ]
        [ "rt_dval"; "value_type" ]
        mayRewriteExisting
        (Hashing.computeValueHash Hashing.Normal value)
        (fun bytes ->
          BS.PT.PackageValue.deserialize hash bytes
          |> Hashing.computeValueHash Hashing.Normal)

    let refs = DE.extractFromValue value
    do! updateDependencies ctx hashStr refs
  }

/// Apply a single AddFn op to the package_functions table.
let private applyAddFn
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (fn : PT.PackageFn.PackageFn)
  : Task<unit> =
  task {
    let hash =
      match fn.hash with
      | Hash "" -> Hashing.computeFnHash Hashing.Normal fn
      | h -> h
    let fn = { fn with hash = hash }
    let (Hash hashStr) = hash

    do!
      upsertContentAddressed
        ctx
        "fn"
        "package_functions"
        hash
        [ "pt_def", box (BS.PT.PackageFn.serialize hashStr fn)
          "rt_instrs",
          box (fn |> PT2RT.PackageFn.toRT |> BS.RT.PackageFn.serialize hashStr)
          "description", box fn.description ]
        []
        mayRewriteExisting
        (Hashing.computeFnHash Hashing.Normal fn)
        (fun bytes ->
          BS.PT.PackageFn.deserialize hash bytes
          |> Hashing.computeFnHash Hashing.Normal)

    let refs = DE.extractFromFn fn
    do! updateDependencies ctx hashStr refs
  }

/// The `origin_ts` the log stamped on <param opId>, or None when the log does not hold it.
///
/// Id alone is the whole key: `package_ops` has one row per op; branch claims live
/// in `op_branches`.
let private originTsOf (ctx : Ctx) (opId : System.Guid) : Task<Option<string>> =
  textOption ctx "SELECT origin_ts FROM package_ops WHERE id = $id" (fun cmd ->
    p cmd "$id" (string opId))

/// The newest `Unbind` folded at a name, by the stamp of the op that made it. An `Unbind` leaves a
/// TOMBSTONE in `locations`: a row unlisted the moment it is written, stamped with the unbind's own
/// origin_ts, so a SetName for the name that arrives after the unbind but was authored before it can
/// find out and stay stale. Without one, two stores converge on different answers depending on which
/// of the two ops arrived first.
let private latestUnbindTs
  (ctx : Ctx)
  (location : PT.PackageLocation)
  : Task<Option<string>> =
  textOption
    ctx
    ("SELECT origin_ts FROM locations "
     + "WHERE owner = $owner AND modules = $modules AND name = $name "
     + "AND source = 'unbind' AND origin_ts IS NOT NULL "
     + "ORDER BY origin_ts DESC LIMIT 1")
    (fun cmd -> pLoc cmd location)

/// Apply a Set*Name op to the locations table.
/// <param source> is what put the binding there: "op" for a normal fold, "resolution" for a human's answer
/// to a conflict. `discard` deletes op-fold bindings but skips resolutions, so the tag is what stops a
/// routine discard from silently undoing a decision someone made on purpose.
let private applySetNameFrom
  (ctx : Ctx)
  (source : string)
  (opForStamp : PT.PackageOp)
  (itemHash : Hash)
  (location : PT.PackageLocation)
  (itemKind : PT.ItemKind)
  : Task<unit> =
  task {
    let itemTypeStr = itemKind.toString ()
    let locationId = System.Guid.NewGuid()
    let (Hash itemHashStr) = itemHash

    // Read off the OP, not inferred from what is currently live. An `Override` names no predecessor,
    // and saying it replaced whatever happened to be there would put lineage on a row that has none.
    let previousHash =
      match opForStamp with
      | PT.PackageOp.SetName(_, _, Some(Hash h)) -> Some h
      | _ -> None

    // Timestamp-LWW: order by the op's CREATION time (`origin_ts`), not arrival, so
    // every instance converges on the same binding. Unknown stamps fall through to
    // last-writer, leaving non-sync playback unchanged. Reads run on ctx.conn so
    // they see writes from earlier ops in this same transaction.
    //
    // THIS op is handed in rather than rebuilt from (location, hash, kind): a
    // resolution and the SetName it resembles hash differently, so a reconstruction
    // would read as unknown and silently degrade to last-writer-wins.
    let thisOpId = Hashing.computeOpRowId opForStamp
    let! thisTs = originTsOf ctx thisOpId

    // Keyed by NAME, not (name, kind): a location's identity is (owner, modules, name) -- `item_type` is
    // only a lookup hint (item_hash + kind -> find the thing), never part of what a name IS. So the
    // binding this op supersedes is whatever is live at the name, whatever kind it holds.
    let! curBinding =
      pairOption
        ctx
        ("SELECT item_hash, origin_ts FROM locations "
         + "WHERE owner = $owner AND modules = $modules AND name = $name "
         + "AND unlisted_at IS NULL LIMIT 1")
        (fun cmd -> pLoc cmd location)

    let isStale =
      match curBinding, thisTs with
      // Different hash, both stamped: the LWW rule (incl. the portable higher-hash
      // tie-break) lives in `LibDB.Lww`, shared with `SCM.Conflicts.incomingWins`;
      // `Tests.Lww` asserts they agree.
      | Some(curHash, Some curTs), Some t when curHash <> itemHashStr ->
        Lww.isStale t itemHashStr curTs curHash
      // Same content re-applied: keep the EARLIEST origin_ts, so the binding's stamp is identical on every
      // instance regardless of arrival order. Re-stamping with a later equal-hash op would let a
      // different-hash op stamped between the two win on one instance and lose on another.
      | Some(curHash, Some curTs), Some t when curHash = itemHashStr -> t >= curTs
      | _ -> false

    // Authored before the name was unbound, arriving after: the unbind is the later word.
    let! unboundSince = latestUnbindTs ctx location
    let unboundAfter =
      match unboundSince, thisTs with
      // The mirror of applyUnbind's check: ties go to the binding, so only a strictly later
      // tombstone blocks the SetName. One rule, `Lww.unbindBeatsBinding`, both directions.
      | Some u, Some t -> Lww.unbindBeatsBinding u t
      | _ -> false

    if isStale || unboundAfter then
      return ()
    else
      // 1. Unlist whatever is live at the target name (handles updates). One name holds ONE item: this
      //    does NOT filter on item_type, so binding a fn over a name that held a value replaces it rather
      //    than leaving both live.
      do!
        exec ctx """
          UPDATE locations
          SET unlisted_at = datetime('now')
          WHERE owner = $owner
            AND modules = $modules
            AND name = $name
            AND unlisted_at IS NULL
          """ (fun cmd -> pLoc cmd location)

      // 2. Nothing else is touched: a SetName binds its own name only. A hash is
      //    routinely live at several names, and retiring the others needs an op that
      //    names them.

      // 3. Insert new location entry (with origin_ts for cross-instance timestamp-LWW).
      do!
        exec ctx """
          INSERT INTO locations
            (location_id, item_hash, owner, modules, name, item_type, origin_ts, source, op_id,
             previous)
          VALUES ($location_id, $item_hash, $owner, $modules, $name, $item_type, $origin_ts, $source,
                  $op_id, $previous)
          """ (fun cmd ->
          pUuid cmd "$location_id" locationId
          p cmd "$item_hash" itemHashStr
          pLoc cmd location
          p cmd "$item_type" itemTypeStr
          pOpt cmd "$origin_ts" thisTs
          p cmd "$source" source
          // The op that wrote this row, so a later reader can find it exactly rather than by its stamp.
          p cmd "$op_id" (string thisOpId)
          // What this binding replaced, taken from the op rather than inferred. Conflict detection
          // compares it against the incoming side's, so it has to mean the same thing on both.
          pOpt cmd "$previous" previousHash)
  }

/// Serialize a DeprecationKind + message for the annotation_blob column.
/// Keeps the on-disk representation close to the binary op serializer so one
/// reader can surface both op-log history and current projected state.
let private serializeAnnotation
  (kind : PT.DeprecationKind)
  (message : string)
  : byte array =
  use ms = new System.IO.MemoryStream()
  use w = new System.IO.BinaryWriter(ms)
  LibSerialization.Binary.Serializers.PT.PackageOp.DeprecationKind.write w kind
  LibSerialization.Binary.Serializers.Common.String.write w message
  ms.ToArray()


/// Record a deprecation-state change: supersede any prior un-superseded `deprecations`
/// row for (item_hash, item_kind), then insert the new row. Per call site: the state
/// ('deprecated' or 'undeprecated') and the annotation blob (a serialized kind+message,
/// or None for an undeprecate, stored as NULL).
///
/// Identity is hash-keyed: `Reference` carries only a Hash, so two FQNs sharing a hash
/// deprecate together. Not branch-scoped: a deprecation is keyed on content, and a
/// branch's `Deprecate` never folds at all.
let private writeDeprecationState
  (ctx : Ctx)
  (target : PT.Reference)
  (state : string)
  (blob : Option<byte[]>)
  : Task<unit> =
  task {
    let (Hash itemHashStr) = target.hash
    let itemKindStr = target.kind.toString ()

    do!
      exec ctx """
        UPDATE deprecations
        SET unlisted_at = datetime('now')
        WHERE item_hash = $item_hash
          AND item_kind = $item_kind
          AND unlisted_at IS NULL
        """ (fun cmd ->
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr)

    do!
      exec ctx """
        INSERT INTO deprecations
          (deprecation_id, item_hash, item_kind, state, annotation_blob)
        VALUES
          ($deprecation_id, $item_hash, $item_kind, $state, $blob)
        """ (fun cmd ->
        pUuid cmd "$deprecation_id" (System.Guid.NewGuid())
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr
        p cmd "$state" state
        match blob with
        | Some b -> p cmd "$blob" b
        | None -> p cmd "$blob" System.DBNull.Value)
  }


/// Apply a Deprecate op: a `deprecated` row carrying the serialized kind + message.
let private applyDeprecate
  (ctx : Ctx)
  (target : PT.Reference)
  (kind : PT.DeprecationKind)
  (message : string)
  : Task<unit> =
  writeDeprecationState
    ctx
    target
    "deprecated"
    (Some(serializeAnnotation kind message))


/// Apply an Undeprecate op: an `undeprecated` row with no annotation.
let private applyUndeprecate (ctx : Ctx) (target : PT.Reference) : Task<unit> =
  writeDeprecationState ctx target "undeprecated" None


/// Apply a Describe op: the item's stored text becomes <param text>.
///
/// A rewrite in place rather than a row in a projection table, and that is only sound because the
/// doc is not in the identity hash: the blob for hash H carries H's behaviour plus whatever H is
/// currently said to be, and only the first half is what H means. So there is nothing to reconcile
/// -- last writer wins by `origin_ts`, like every other fold here.
///
/// Both the blob and the `description` column, because both are read: the column by the listings
/// and search, the blob by anything that loads the item.
let private applyDescribe
  (ctx : Ctx)
  (target : PT.Reference)
  (text : string)
  : Task<unit> =
  task {
    let (Hash hashStr) = target.hash

    let table, reserialize =
      match target.kind with
      | PT.ItemKind.Fn ->
        "package_functions",
        (fun (bytes : byte[]) ->
          let fn = BS.PT.PackageFn.deserialize target.hash bytes
          BS.PT.PackageFn.serialize hashStr { fn with description = text })
      | PT.ItemKind.Type ->
        "package_types",
        (fun bytes ->
          let t = BS.PT.PackageType.deserialize target.hash bytes
          BS.PT.PackageType.serialize hashStr { t with description = text })
      | PT.ItemKind.Value ->
        "package_values",
        (fun bytes ->
          let v = BS.PT.PackageValue.deserialize target.hash bytes
          BS.PT.PackageValue.serialize hashStr { v with description = text })

    let! stored =
      bytesOption ctx $"SELECT pt_def FROM {table} WHERE hash = $hash" (fun cmd ->
        p cmd "$hash" hashStr)

    // Nothing here to describe: the item has not arrived (a Describe can travel ahead of the
    // AddFn that carries its subject). Dropped rather than stored, the same as every other op
    // whose target this store does not hold.
    match stored with
    | None -> return ()
    | Some bytes ->
      do!
        exec
          ctx
          $"UPDATE {table} SET pt_def = $pt_def, description = $description WHERE hash = $hash"
          (fun cmd ->
            p cmd "$hash" hashStr
            p cmd "$pt_def" (reserialize bytes)
            p cmd "$description" text)
  }


// ------------------------------------------------------------------
// Op dispatch.
// ------------------------------------------------------------------

/// Fold the non-binding half of a `Decision` into whichever projection owns it.
///
/// This is what makes `propagation_policy` and the Constraint acks in `conflicts` DERIVED tables rather
/// than a second source of truth. `Override` is not here: it binds a name, so it folds through
/// `applySetNameFrom` alongside the other binding ops.
///
/// The stamp comes from the OP's own `origin_ts`, not from now, so a decision keeps the time it was made
/// when it syncs -- which is what lets LWW agree across instances instead of "whoever imported last wins".
let private applyDecision
  (ctx : Ctx)
  (branchId : PT.BranchId)
  (op : PT.PackageOp)
  (loc : PT.PackageLocation)
  (reason : string)
  (kind : PT.DecisionKind)
  : Task<unit> =
  task {
    let opId = Hashing.computeOpRowId op
    // "" for an op the log does not hold: it loses every `origin_ts` guard below, which is the right
    // answer for a decision nothing recorded.
    let! ts = originTsOf ctx opId |> Task.map (Option.defaultValue "")

    match kind with
    | PT.DecisionKind.Override _ ->
      // Folded as a binding, not here. Kept explicit so adding a case to `DecisionKind` is a compile
      // error in both places rather than a silent no-op in one of them.
      ()

    | PT.DecisionKind.Propagation PT.PropagationPolicy.Unset ->
      // Clearing is a decision like any other, so it's an op -- but it's the one that removes the row rather
      // than writing it. Still guarded by origin_ts, so a stale unset can't wipe a newer pin.
      do!
        exec ctx "DELETE FROM propagation_policy
           WHERE branch_id = $branch AND owner = $owner AND modules = $modules AND name = $name
             AND COALESCE(origin_ts, '') < $ts" (fun cmd ->
          p cmd "$branch" (string branchId)
          pLoc cmd loc
          p cmd "$ts" ts)

    | PT.DecisionKind.Propagation policy ->
      // Guarded by origin_ts so an older op arriving late can't undo a newer decision.
      do!
        exec
          ctx
          "INSERT INTO propagation_policy (branch_id, owner, modules, name, policy, note, origin_ts)
           VALUES ($branch, $owner, $modules, $name, $policy, $note, $ts)
           ON CONFLICT(branch_id, owner, modules, name) DO UPDATE SET
             policy = excluded.policy, note = excluded.note, origin_ts = excluded.origin_ts
           WHERE excluded.origin_ts > COALESCE(propagation_policy.origin_ts, '')"
          (fun cmd ->
            p cmd "$branch" (string branchId)
            pLoc cmd loc
            p cmd "$policy" policy.ToText
            p cmd "$note" reason
            p cmd "$ts" ts)

    | PT.DecisionKind.Ack findingId ->
      // A finding nobody has answered isn't stored at all -- only answers are rows, because detection
      // re-derives the findings whenever anyone asks.
      do!
        exec
          ctx
          "INSERT INTO conflicts
             (id, owner, modules, name, item_type, kind, candidates, auto_resolved_to, reason, status, origin_ts)
           -- item_type left empty, not guessed: the op doesn't carry the usage's
           -- kind, and nothing reads an ack row's kind (`ackedIds` selects on
           -- id + status).
           VALUES ($id, $owner, $modules, $name, '', 'stale-usage', '[]', '', $reason, 'acked', $ts)
           ON CONFLICT(id) DO UPDATE SET
             status = 'acked', reason = excluded.reason, origin_ts = excluded.origin_ts"
          (fun cmd ->
            p cmd "$id" findingId
            pLoc cmd loc
            p cmd "$reason" reason
            p cmd "$ts" ts)
  }


/// Apply a branch event: what happened to a BRANCH, as opposed to what happened to a name.
///
/// Only the MONOTONIC events: "merged" and "archived" can be applied twice in any
/// order and land in the same place, so they need no stamp column. Rename is LWW and
/// would need one; it isn't here.
///
/// An event naming a branch this store does not have updates nothing, which is the right answer rather than
/// an error: branch ids travel with a bundle, so the branches you share match, and the ones you never
/// shared are none of this store's business.
let private applyBranchEvent
  (ctx : Ctx)
  (eventOpId : System.Guid)
  (branchId : PT.BranchId)
  (event : PT.BranchEventKind)
  (at : string)
  : Task<unit> =
  task {
    match event with
    | PT.Merged mergedOps ->
      // The push that carried this merge could not deliver the branch's ops
      // (content-addressed, already present here whenever the branch was shared), so
      // setting merged_at alone leaves them effective=0 and main without the
      // branch's code. So do what a local merge does, for EXACTLY the ops the event
      // names (this store may hold more -- its own unpushed edits stay on the
      // branch): into main they flip effective; into another parent they retag onto
      // it with the child's name bases. Ids bind once as a JSON array (built by
      // hand: the reflection serializer is disabled under AOT).
      let ids =
        "["
        + (mergedOps
           |> List.map (fun g -> "\"" + string g + "\"")
           |> String.concat ",")
        + "]"
      let b = string branchId
      let bindB (cmd : SqliteCommand) =
        p cmd "$b" b
        p cmd "$ids" ids

      let! parent =
        textOption ctx "SELECT parent_id FROM branches WHERE id = $b" (fun cmd ->
          p cmd "$b" b)
      let parentIsMain =
        match parent with
        | None -> true
        | Some pid -> PT.BranchId.Parse pid = Some PT.BranchId.Main || pid = ""

      // A merge event routinely arrives BEFORE the bundle whose ops it names
      // (`dark pull` before `dark branch pull`); folded then it would flip nothing
      // and never be re-examined. So an event whose branch has no ops yet stays
      // deferred and the next fold takes it. Only when the branch EXISTS here: an
      // event for a never-shared branch folds to nothing, correctly.
      let! tagged =
        scalarInt
          ctx
          "SELECT count(*) FROM op_branches WHERE branch_id = $b"
          (fun cmd -> p cmd "$b" b)
      let! branchKnownHere =
        scalarInt ctx "SELECT count(*) FROM branches WHERE id = $b" (fun cmd ->
          p cmd "$b" b)

      if tagged = 0L && branchKnownHere > 0L then
        // `applied = 2`, DEFERRED: folded, did nothing, and waiting for ops it names. A third state
        // rather than back to 0, because `readPending` selects `applied = 0` and `applyUnappliedOps`
        // loops until nothing is pending: an op that keeps re-appearing never settles and the loop
        // raises. 2 is skipped by that select, and `Branches.undeferBranchEvents` puts it back to 0
        // when a bundle lands.
        do!
          exec ctx "UPDATE package_ops SET applied = 2 WHERE id = $e" (fun cmd ->
            p cmd "$e" (string eventOpId))
      elif parentIsMain then
        do!
          exec
            ctx
            "UPDATE package_ops SET effective = 1
             WHERE effective = 0
               AND id IN (SELECT value FROM json_each($ids))
               AND id IN (SELECT op_id FROM op_branches WHERE branch_id = $b)"
            bindB
        // Stamp the flipped ops with the event's own commit: the branch bundle
        // carried no commits, and uncommitted they'd show up in main's draft. (On
        // the merger's own store the event is itself uncommitted at this point; the
        // Dark merge commits both together right after.)
        do!
          exec
            ctx
            // The op_branches predicate matches the flip above: ids are content-addressed, so
            // without it an event for a branch this store never held could stamp a same-content
            // op sitting in main's draft with the merger's commit. Tag rows still exist here;
            // the DELETE runs after both UPDATEs.
            "UPDATE package_ops SET commit_hash = (SELECT commit_hash FROM package_ops WHERE id = $e)
             WHERE commit_hash IS NULL
               AND id IN (SELECT value FROM json_each($ids))
               AND id IN (SELECT op_id FROM op_branches WHERE branch_id = $b)
               AND (SELECT commit_hash FROM package_ops WHERE id = $e) IS NOT NULL"
            (fun cmd ->
              bindB cmd
              p cmd "$e" (string eventOpId))
      else
        let parentId = Option.defaultValue "" parent
        let bindP (cmd : SqliteCommand) =
          bindB cmd
          p cmd "$p" parentId
        do!
          exec
            ctx
            "INSERT OR IGNORE INTO op_branches (op_id, branch_id, source)
             SELECT op_id, $p, source FROM op_branches
             WHERE branch_id = $b AND op_id IN (SELECT value FROM json_each($ids))"
            bindP
        do!
          exec
            ctx
            "INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
             SELECT $p, owner, modules, name, base_hash FROM branch_name_bases WHERE branch_id = $b"
            bindP

      // The branch's PINS and FOLLOWS go where its ops go. A pin is a decision about a name --
      // "this caller does not follow that dependency" -- and it is stored per branch, so a merge
      // that moved the ops and left the decisions behind left the parent unable to see them: its
      // next edit repointed a caller the child had deliberately pinned, and the pin was still
      // there, under an id nothing consults any more.
      //
      // OR IGNORE, not REPLACE: the parent's own decision about a name is the parent's, and a
      // child cannot overrule it by merging. Same rule the name bases above follow.
      let mergeTarget =
        match parent with
        | Some pid when not parentIsMain -> pid
        | _ -> string PT.BranchId.Main
      do!
        exec ctx "INSERT OR IGNORE INTO propagation_policy
             (branch_id, owner, modules, name, policy, note, origin_ts)
           SELECT $target, owner, modules, name, policy, note, origin_ts
             FROM propagation_policy WHERE branch_id = $b" (fun cmd ->
          p cmd "$b" b
          p cmd "$target" mergeTarget)

      do!
        exec
          ctx
          "DELETE FROM op_branches
           WHERE branch_id = $b AND op_id IN (SELECT value FROM json_each($ids))"
          bindB

      // Last, and only when nothing of the branch is left here. A branch holding ops the merger never
      // saw stays live, holding exactly those; merging it locally later names them in a second event.
      do!
        exec
          ctx
          "UPDATE branches SET merged_at = $at
           WHERE id = $b AND merged_at IS NULL
             AND NOT EXISTS (SELECT 1 FROM op_branches WHERE branch_id = $b)"
          (fun cmd ->
            p cmd "$b" b
            p cmd "$at" at)
    | PT.Archived ->
      do!
        exec
          ctx
          "UPDATE branches SET archived_at = $at WHERE id = $b AND archived_at IS NULL"
          (fun cmd ->
            p cmd "$b" (string branchId)
            p cmd "$at" at)
  }


/// Apply an `Unbind`: the name stops existing. Unlists whatever is live at the location and writes the
/// tombstone `latestUnbindTs` reads; content is untouched. Same LWW as a binding: an unbind stamped
/// before the live binding is an old op arriving late, and the name it would take out was bound after
/// it, so it stays.
let private applyUnbind
  (ctx : Ctx)
  (op : PT.PackageOp)
  (location : PT.PackageLocation)
  (previous : Option<Hash>)
  : Task<unit> =
  task {
    let thisOpId = Hashing.computeOpRowId op
    let! thisTs = originTsOf ctx thisOpId

    let! live =
      pairOption
        ctx
        ("SELECT item_type, origin_ts FROM locations "
         + "WHERE owner = $owner AND modules = $modules AND name = $name "
         + "AND unlisted_at IS NULL LIMIT 1")
        (fun cmd -> pLoc cmd location)

    let isStale =
      match live, thisTs with
      // Ties go to the binding: `Lww.unbindBeatsBinding` is the one statement of the rule.
      | Some(_, Some curTs), Some t -> not (Lww.unbindBeatsBinding t curTs)
      | _ -> false

    if isStale then
      return ()
    else
      do!
        exec ctx """
          UPDATE locations
          SET unlisted_at = datetime('now')
          WHERE owner = $owner
            AND modules = $modules
            AND name = $name
            AND unlisted_at IS NULL
          """ (fun cmd -> pLoc cmd location)

      // The tombstone. `item_hash` is what it unbound (or nothing, when the op named no predecessor)
      // and `item_type` is what was live, or 'fn' when nothing was: both are lookup hints on a row no
      // live-binding read ever sees, since it is unlisted from birth.
      let previousHash = previous |> Option.map (fun (Hash h) -> h)
      let kind =
        match live with
        | Some(k, _) -> k
        | None -> PT.ItemKind.Fn.toString ()
      do!
        exec ctx """
          INSERT INTO locations
            (location_id, item_hash, owner, modules, name, item_type, origin_ts, source, op_id,
             previous, unlisted_at)
          VALUES ($location_id, $item_hash, $owner, $modules, $name, $item_type, $origin_ts, 'unbind',
                  $op_id, $previous, datetime('now'))
          """ (fun cmd ->
          pUuid cmd "$location_id" (System.Guid.NewGuid())
          p cmd "$item_hash" (previousHash |> Option.defaultValue "")
          pLoc cmd location
          p cmd "$item_type" kind
          pOpt cmd "$origin_ts" thisTs
          p cmd "$op_id" (string thisOpId)
          pOpt cmd "$previous" previousHash)
  }


let private applyOp
  (ctx : Ctx)
  (source : string)
  (mayRewriteExisting : bool)
  (op : PT.PackageOp)
  : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType ctx mayRewriteExisting typ
    | PT.PackageOp.AddValue value -> do! applyAddValue ctx mayRewriteExisting value
    | PT.PackageOp.AddFn fn -> do! applyAddFn ctx mayRewriteExisting fn
    | PT.PackageOp.SetName(loc, target, _) ->
      do! applySetNameFrom ctx source op target.hash loc target.kind
    | PT.PackageOp.Unbind(loc, previous) -> do! applyUnbind ctx op loc previous
    | PT.PackageOp.Deprecate(target, kind, message) ->
      do! applyDeprecate ctx target kind message
    | PT.PackageOp.Undeprecate target -> do! applyUndeprecate ctx target
    | PT.PackageOp.Describe(target, text) -> do! applyDescribe ctx target text
    | PT.PackageOp.Decision(id, loc, reason, kind) ->
      match kind with
      | PT.DecisionKind.Override target ->
        // An override answers ONE name, so it binds one name. Unlisting every other location that
        // happens to share the hash would be collateral damage.
        do! applySetNameFrom ctx "resolution" op target.hash loc target.kind
        // Close the local conflict record too: the op converges the BINDING
        // everywhere, but without this the non-choosing machine keeps listing the
        // conflict as pending.
        do!
          exec
            ctx
            // Scoped to item KIND as well as name: one location can hold a fn AND a
            // value, and overriding the fn must not close the value's
            // still-contested conflict.
            "UPDATE conflicts SET status = 'overridden', resolved_by = $op
             WHERE owner = $owner AND modules = $modules AND name = $name
               AND item_type = $kind AND status = 'pending'"
            (fun cmd ->
              p cmd "$op" id
              p cmd "$kind" (target.kind.toString ())
              pLoc cmd loc)
      | _ ->
        // This fold is main's, so the row is main's. A branch's decision is folded by the branch path
        // with that branch's id; the two must spell main the same way or a policy set on main is written
        // under an id nothing reads.
        do! applyDecision ctx PT.BranchId.Main op loc reason kind
    | PT.PackageOp.BranchEvent(branchId, event, at) ->
      // An event for a branch this store has never heard of folds to nothing. That is not a failure:
      // branch ids travel with a bundle, so the ones you share match.
      do! applyBranchEvent ctx (Hashing.computeOpRowId op) branchId event at
  }


// ------------------------------------------------------------------
// Public API.
// ------------------------------------------------------------------

/// Apply a list of PackageOps using a caller-provided open SqliteConnection.
/// The caller controls transaction boundaries: wrap the call in BEGIN/COMMIT
/// for a bulk-replay, or use auto-commit for a small commit-time batch. A
/// fresh prepared-statement cache (Ctx) is created and disposed per call,
/// so the cache lifetime matches a single `applyOpsOnConnection` invocation.
let applyOpsOnConnectionFrom
  (conn : SqliteConnection)
  (source : string)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    let ctx = newCtx conn
    try
      for op in ops do
        do! applyOp ctx source true op
    finally
      disposeCtx ctx

    // Names just changed meaning; long-lived processes hold cached answers that
    // never expire on their own -- see `Caching.invalidateAll`.
    Caching.invalidateAll ()
  }

let applyOpsOnConnection
  (conn : SqliteConnection)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  applyOpsOnConnectionFrom conn "op" ops


/// Convenience wrapper for callers that don't have a shared connection (e.g.
/// Inserts.fs at commit time, where the op batch is small). Opens a fresh
/// connection per call and wraps the whole batch in a single transaction:
/// faster than auto-commit, and it makes the apply atomic with respect to
/// other readers.
let applyOpsFrom (source : string) (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    use tx = conn.BeginTransaction()
    do! applyOpsOnConnectionFrom conn source ops
    tx.Commit()
  }

let applyOps (ops : List<PT.PackageOp>) : Task<unit> = applyOpsFrom "op" ops


/// A BRANCH's content ops: folded so the branch's own bodies are runnable and its dependency edges
/// are visible, but never rewriting a row that is already there. See `upsertContentAddressed`.
let applyBranchContentOps (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    use tx = conn.BeginTransaction()
    let ctx = newCtx conn
    try
      for op in ops do
        do! applyOp ctx "op" false op
    finally
      disposeCtx ctx
    tx.Commit()
  }


/// Record the callees of these `Add*` ops' items without folding anything else. For an op the log
/// already holds: the content is there, but this parse may have reached its callees through names the
/// first fold never saw (two names, one body), and `updateDependencies` adds those. Every other kind of
/// op is skipped; there is nothing to record for it.
let recordDependenciesOnly (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    let adds =
      ops
      |> List.choose (fun op ->
        match op with
        | PT.PackageOp.AddFn f when f.hash <> Hash "" ->
          let (Hash h) = f.hash
          Some(h, DE.extractFromFn f)
        | PT.PackageOp.AddType t when t.hash <> Hash "" ->
          let (Hash h) = t.hash
          Some(h, DE.extractFromType t)
        | PT.PackageOp.AddValue v when v.hash <> Hash "" ->
          let (Hash h) = v.hash
          Some(h, DE.extractFromValue v)
        | _ -> None)

    if not (List.isEmpty adds) then
      use conn = new SqliteConnection(LibDB.Sqlite.connString)
      do! conn.OpenAsync()
      use tx = conn.BeginTransaction()
      let ctx = newCtx conn
      try
        for (hash, deps) in adds do
          do! updateDependencies ctx hash deps
      finally
        disposeCtx ctx
      tx.Commit()
      Caching.invalidateAll ()
  }
