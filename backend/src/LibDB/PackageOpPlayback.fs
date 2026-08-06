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

// `Ctx`, `exec`, `execRows`, `bytesOption`, `p`, `pUuid`, `pOpt`: one connection and one
// prepared-command cache for the whole batch. See LibDB.PreparedBatch for why.
open LibDB.PreparedBatch


// ------------------------------------------------------------------
// Dependency table maintenance.
// ------------------------------------------------------------------

/// Update dependencies for an item atomically.
/// Clears existing dependencies and stores new ones in a single statement
/// (a multi-statement script — SQLite runs them in order on the same command).
let private updateDependencies
  (ctx : Ctx)
  (itemHash : string)
  (deps : List<DE.Dependency>)
  : Task<unit> =
  task {
    if List.isEmpty deps then
      do!
        exec
          ctx
          "DELETE FROM package_dependencies WHERE item_hash = $item_hash"
          (fun cmd -> p cmd "$item_hash" itemHash)
    else
      // Each dep contributes 6 placeholders to the VALUES list.
      let placeholders =
        deps
        |> List.mapi (fun i _ ->
          $"($item_hash, $hash_{i}, $kind_{i}, $owner_{i}, $modules_{i}, $name_{i})")
        |> String.concat ", "

      let sql =
        "DELETE FROM package_dependencies WHERE item_hash = $item_hash; "
        + "INSERT OR IGNORE INTO package_dependencies "
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
/// The three item kinds differ only in which table and columns they use and how their
/// canonical fingerprint is computed, so they share this and keep only their own
/// serialization.
///
/// <param columns> carries the body and metadata, and is written on both paths.
/// <param insertOnlyNulls> names columns the insert must mention but this path never
/// fills: `package_values` leaves `rt_dval` and `value_type` for
/// `Seed.evaluateAllValues`, which runs once every op in the batch has been applied and
/// cross-package references can resolve.
let private upsertContentAddressed
  (ctx : Ctx)
  (kind : string)
  (table : string)
  (hash : Hash)
  (columns : List<string * obj>)
  (insertOnlyNulls : List<string>)
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

      let assignments =
        names |> List.map (fun name -> $"{name} = ${name}") |> String.concat ", "

      do! exec ctx $"UPDATE {table} SET {assignments} WHERE hash = $hash" bind
  }


/// Apply a single AddType op to the package_types table.
let private applyAddType
  (ctx : Ctx)
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
        (Hashing.computeTypeHash Hashing.Normal typ)
        (fun bytes ->
          BS.PT.PackageType.deserialize hash bytes
          |> Hashing.computeTypeHash Hashing.Normal)

    // Extract and store dependency references atomically. Each
    // Dependency carries its own location (populated by the resolver).
    let refs = DE.extractFromType typ
    do! updateDependencies ctx hashStr refs
  }

/// Apply a single AddValue op to the package_values table.
let private applyAddValue
  (ctx : Ctx)
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
        (Hashing.computeValueHash Hashing.Normal value)
        (fun bytes ->
          BS.PT.PackageValue.deserialize hash bytes
          |> Hashing.computeValueHash Hashing.Normal)

    let refs = DE.extractFromValue value
    do! updateDependencies ctx hashStr refs
  }

/// Apply a single AddFn op to the package_functions table.
let private applyAddFn (ctx : Ctx) (fn : PT.PackageFn.PackageFn) : Task<unit> =
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
        (Hashing.computeFnHash Hashing.Normal fn)
        (fun bytes ->
          BS.PT.PackageFn.deserialize hash bytes
          |> Hashing.computeFnHash Hashing.Normal)

    let refs = DE.extractFromFn fn
    do! updateDependencies ctx hashStr refs
  }

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
    let modulesStr = String.concat "." location.modules
    let itemTypeStr = itemKind.toString ()
    let locationId = System.Guid.NewGuid()
    let (Hash itemHashStr) = itemHash

    // Read off the OP, not inferred from what is currently live. A `Resolve` names no predecessor, and
    // saying it replaced whatever happened to be there would put lineage on a row that has none.
    let previousHash =
      match opForStamp with
      | PT.PackageOp.SetName(_, _, Some(Hash h)) -> Some h
      | _ -> None

    // Timestamp-LWW: order this binding by the op's CREATION time, not its arrival. Compare this op's
    // `origin_ts` against the current binding's. An op created BEFORE the current binding's is stale --
    // an old op arriving late by sync -- so the existing binding stays and this one lives in the log
    // without being the active name. Computed identically everywhere, so every instance converges on the
    // same hash regardless of arrival order.
    //
    // Unknown stamps fall through to last-writer, leaving non-sync playback unchanged. Reads run on
    // ctx.conn so they see writes from earlier ops in this same transaction.
    //
    // THIS op is handed in rather than rebuilt from (location, hash, kind). A resolution and the SetName
    // it resembles are different ops with different content hashes, so a reconstruction hashes to an op
    // that is not in the log, the stamp reads as unknown, and the staleness check silently degrades to
    // last-writer-wins for every binding.
    let thisOpId = LibSerialization.Hashing.Hashing.computeOpRowId opForStamp

    let! thisTs =
      task {
        use cmd = ctx.conn.CreateCommand()
        // By id alone, which is the whole key: `package_ops` holds one row per op, and a branch's claim on
        // it lives in `op_branches`. There is no other branch's stamp to read by mistake.
        cmd.CommandText <- "SELECT origin_ts FROM package_ops WHERE id = $id"
        cmd.Parameters.AddWithValue("$id", string thisOpId)
        |> ignore<SqliteParameter>
        use! reader = cmd.ExecuteReaderAsync()
        let! hasRow = reader.ReadAsync()
        if hasRow && not (reader.IsDBNull 0) then
          return Some(reader.GetString 0)
        else
          return None
      }

    let! curBinding =
      task {
        use cmd = ctx.conn.CreateCommand()
        // Keyed by NAME, not (name, kind): a location's identity is (owner, modules, name) — `item_type` is
        // only a lookup hint (item_hash + kind -> find the thing), never part of what a name IS. So the
        // binding this op supersedes is whatever is live at the name, whatever kind it holds.
        cmd.CommandText <-
          "SELECT item_hash, origin_ts FROM locations "
          + "WHERE owner = $owner AND modules = $modules AND name = $name "
          + "AND unlisted_at IS NULL LIMIT 1"
        cmd.Parameters.AddWithValue("$owner", location.owner)
        |> ignore<SqliteParameter>
        cmd.Parameters.AddWithValue("$modules", modulesStr)
        |> ignore<SqliteParameter>
        cmd.Parameters.AddWithValue("$name", location.name)
        |> ignore<SqliteParameter>
        use! reader = cmd.ExecuteReaderAsync()
        let! hasRow = reader.ReadAsync()
        if hasRow then
          let h = reader.GetString 0
          let ts = if reader.IsDBNull 1 then None else Some(reader.GetString 1)
          return Some(h, ts)
        else
          return None
      }

    let isStale =
      match curBinding, thisTs with
      // On an EXACT TIE (two DIFFERENT ops for one name stamped the same millisecond — a genuine
      // cross-instance race), break by item hash: the higher wins. That tie-break is PORTABLE (content,
      // not arrival/rowid), so every instance converges on the same winner. Local sequential authoring
      // never ties — `Inserts` self-stamps each op with a strictly-increasing origin_ts.
      | Some(curHash, Some curTs), Some t when curHash <> itemHashStr ->
        // THE rule lives in `LibDB.Lww`, so this cannot drift from the copy that decides the same
        // question when a conflict is recorded (`SCM.Conflicts.incomingWins`, in Dark). If those two
        // disagree, two machines converge on different winners. `Tests.Lww` asserts they agree.
        Lww.isStale t itemHashStr curTs curHash
      // Same content re-applied: keep the EARLIEST origin_ts, so the binding's stamp is identical on every
      // instance regardless of arrival order. Re-stamping with a later equal-hash op would let a
      // different-hash op stamped between the two win on one instance and lose on another.
      | Some(curHash, Some curTs), Some t when curHash = itemHashStr -> t >= curTs
      | _ -> false

    if isStale then
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
          """ (fun cmd ->
          p cmd "$owner" location.owner
          p cmd "$modules" modulesStr
          p cmd "$name" location.name)

      // 2. Nothing else is touched. A `SetName` binds ITS OWN name and says nothing about any other
      //    name on the same hash, however it arrived. Identical content is one item, so a hash is
      //    routinely live at several names, and deprecating the others would take out a colleague's
      //    name because you renamed yours. No op in the model says "this is a rename"; a rename that
      //    needs to retire the old name wants an op that NAMES the old location.

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
          p cmd "$owner" location.owner
          p cmd "$modules" modulesStr
          p cmd "$name" location.name
          p cmd "$item_type" itemTypeStr
          pOpt cmd "$origin_ts" thisTs
          p cmd "$source" source
          // The op that wrote this row, so a later reader can find it exactly rather than by its stamp.
          // Already computed above, for the LWW stamp lookup.
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


/// Apply a Deprecate op — supersede any prior un-superseded row for (branch, item_hash, item_kind).
/// CLEANUP: identity is hash-keyed (`Reference` carries only a Hash), so two unrelated FQNs sharing a hash
/// deprecate together. Fix: carry location on `Reference` + the `deprecations` table and filter by it.
let private applyDeprecate
  (ctx : Ctx)
  (target : PT.Reference)
  (kind : PT.DeprecationKind)
  (message : string)
  : Task<unit> =
  task {
    let (Hash itemHashStr) = target.hash
    let itemKindStr = target.kind.toString ()
    let deprecationId = System.Guid.NewGuid()
    let blob = serializeAnnotation kind message

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
          ($deprecation_id, $item_hash, $item_kind, 'deprecated', $blob)
        """ (fun cmd ->
        pUuid cmd "$deprecation_id" deprecationId
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr
        p cmd "$blob" blob)
  }


/// Apply an Undeprecate op to the deprecations projection table.
/// Records an `undeprecated`-state row that supersedes any prior row for the same
/// (item_hash, item_kind). Not branch-scoped: a deprecation is keyed on content, and a branch's
/// `Deprecate` never folds at all.
let private applyUndeprecate (ctx : Ctx) (target : PT.Reference) : Task<unit> =
  task {
    let (Hash itemHashStr) = target.hash
    let itemKindStr = target.kind.toString ()
    let deprecationId = System.Guid.NewGuid()

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
          ($deprecation_id, $item_hash, $item_kind, 'undeprecated', NULL)
        """ (fun cmd ->
        pUuid cmd "$deprecation_id" deprecationId
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr)
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
    let opId = LibSerialization.Hashing.Hashing.computeOpRowId op

    let! ts =
      task {
        use cmd = ctx.conn.CreateCommand()
        cmd.CommandText <- "SELECT origin_ts FROM package_ops WHERE id = $id"
        cmd.Parameters.AddWithValue("$id", string opId) |> ignore<SqliteParameter>
        use! reader = cmd.ExecuteReaderAsync()
        let! hasRow = reader.ReadAsync()
        if hasRow && not (reader.IsDBNull 0) then
          return reader.GetString 0
        else
          return ""
      }

    let modules = String.concat "." loc.modules

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
          cmd.Parameters.AddWithValue("$branch", string branchId)
          |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$owner", loc.owner)
          |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$modules", modules)
          |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$name", loc.name) |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$ts", ts) |> ignore<SqliteParameter>)

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
            cmd.Parameters.AddWithValue("$branch", string branchId)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$owner", loc.owner)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$modules", modules)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$name", loc.name)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$policy", policy.ToText)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$note", reason) |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$ts", ts) |> ignore<SqliteParameter>)

    | PT.DecisionKind.Ack findingId ->
      // A finding nobody has answered isn't stored at all -- only answers are rows, because detection
      // re-derives the findings whenever anyone asks.
      do!
        exec
          ctx
          "INSERT INTO conflicts
             (id, owner, modules, name, item_type, kind, candidates, auto_resolved_to, reason, status, origin_ts)
           -- item_type is left EMPTY rather than guessed. The op doesn't carry the usage's kind, and
           -- an ack row's kind isn't read by anything (`ackedIds` selects on id + status); writing 'fn' for
           -- what might be a type or a value would be asserting something false into a column someone will
           -- eventually trust.
           VALUES ($id, $owner, $modules, $name, '', 'stale-usage', '[]', '', $reason, 'acked', $ts)
           ON CONFLICT(id) DO UPDATE SET
             status = 'acked', reason = excluded.reason, origin_ts = excluded.origin_ts"
          (fun cmd ->
            cmd.Parameters.AddWithValue("$id", findingId) |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$owner", loc.owner)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$modules", modules)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$name", loc.name)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$reason", reason)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$ts", ts) |> ignore<SqliteParameter>)
  }


/// Apply a branch event: what happened to a BRANCH, as opposed to what happened to a name.
///
/// Only the MONOTONIC events. "merged" and "archived" can be applied twice, in either order, by any number
/// of instances, and land in the same place -- which is what lets them travel with no stamp column on
/// `branches` to guard them. A rename is last-writer-wins and would need one; `branches` has no `origin_ts`
/// and adding one is a shape change to a canonical table, so rename is deliberately not here yet.
///
/// An event naming a branch this store does not have updates nothing, which is the right answer rather than
/// an error: branch ids travel with a bundle, so the branches you share match, and the ones you never
/// shared are none of this store's business.
let private applyBranchEvent
  (ctx : Ctx)
  (branchId : PT.BranchId)
  (event : PT.BranchEventKind)
  (at : string)
  : Task<unit> =
  task {
    match event with
    | PT.Merged ->
      do!
        exec
          ctx
          "UPDATE branches SET merged_at = $at WHERE id = $b AND merged_at IS NULL"
          (fun cmd ->
            cmd.Parameters.AddWithValue("$b", string branchId)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$at", at) |> ignore<SqliteParameter>)

      // Marking it merged is not enough on its own. If this store already HOLDS the branch -- which it
      // does whenever the two of you shared it -- its ops are sitting here effective=0, inert. The push
      // that carried the merge could not deliver them, because they are content-addressed and already
      // present, so the only thing that crossed was this event. Setting the flag and stopping leaves a
      // branch that reads `[merged]` next to a main that does not have its code.
      //
      // So do here what a local merge does: flip the frontier effective and drop the tags. The fold picks
      // them up on its next pass, and both machines land on identical hashes because the ops are the same
      // ops.
      do!
        exec
          ctx
          "UPDATE package_ops SET effective = 1
           WHERE effective = 0
             AND id IN (SELECT op_id FROM op_branches WHERE branch_id = $b)"
          (fun cmd ->
            cmd.Parameters.AddWithValue("$b", string branchId)
            |> ignore<SqliteParameter>)

      do!
        exec ctx "DELETE FROM op_branches WHERE branch_id = $b" (fun cmd ->
          cmd.Parameters.AddWithValue("$b", string branchId)
          |> ignore<SqliteParameter>)
    | PT.Archived ->
      do!
        exec
          ctx
          "UPDATE branches SET archived_at = $at WHERE id = $b AND archived_at IS NULL"
          (fun cmd ->
            cmd.Parameters.AddWithValue("$b", string branchId)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$at", at) |> ignore<SqliteParameter>)
  }


let private applyOp (ctx : Ctx) (source : string) (op : PT.PackageOp) : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType ctx typ
    | PT.PackageOp.AddValue value -> do! applyAddValue ctx value
    | PT.PackageOp.AddFn fn -> do! applyAddFn ctx fn
    | PT.PackageOp.SetName(loc, target, _) ->
      do! applySetNameFrom ctx source op target.hash loc target.kind
    | PT.PackageOp.Deprecate(target, kind, message) ->
      do! applyDeprecate ctx target kind message
    | PT.PackageOp.Undeprecate target -> do! applyUndeprecate ctx target
    | PT.PackageOp.Decision(id, loc, reason, kind) ->
      match kind with
      | PT.DecisionKind.Override target ->
        // An override answers ONE name, so it binds one name. Unlisting every other location that
        // happens to share the hash would be collateral damage.
        do! applySetNameFrom ctx "resolution" op target.hash loc target.kind
        // Close the local record for that name too. The op converges the BINDING on every machine, and
        // without this the machine that didn't make the choice keeps listing the conflict as pending and
        // `show` keeps reporting an auto-pick that is no longer what's live -- an answered question that
        // still looks open, on the side that didn't answer it.
        do!
          exec
            ctx
            // Scoped to the item KIND as well as the name. One location can hold a fn AND a value at
            // once, so matching on the name alone closes a conflict nobody answered: overriding the fn
            // marks the value's conflict overridden too, and it disappears from `dark conflicts` with
            // its binding still contested.
            "UPDATE conflicts SET status = 'overridden', resolved_by = $op
             WHERE owner = $owner AND modules = $modules AND name = $name
               AND item_type = $kind AND status = 'pending'"
            (fun cmd ->
              cmd.Parameters.AddWithValue("$op", id) |> ignore<SqliteParameter>
              cmd.Parameters.AddWithValue("$kind", target.kind.toString ())
              |> ignore<SqliteParameter>
              cmd.Parameters.AddWithValue("$owner", loc.owner)
              |> ignore<SqliteParameter>
              cmd.Parameters.AddWithValue("$modules", String.concat "." loc.modules)
              |> ignore<SqliteParameter>
              cmd.Parameters.AddWithValue("$name", loc.name)
              |> ignore<SqliteParameter>)
      | _ ->
        // This fold is main's, so the row is main's. A branch's decision is folded by the branch path
        // with that branch's id; the two must spell main the same way or a policy set on main is written
        // under an id nothing reads.
        do! applyDecision ctx PT.BranchId.Main op loc reason kind
    | PT.PackageOp.BranchEvent(branchId, event, at) ->
      // An event for a branch this store has never heard of folds to nothing. That is not a failure:
      // branch ids travel with a bundle, so the ones you share match.
      do! applyBranchEvent ctx branchId event at
  }


// ------------------------------------------------------------------
// Public API.
// ------------------------------------------------------------------

/// Apply a list of PackageOps using a caller-provided open SqliteConnection.
/// The caller controls transaction boundaries — wrap the call in BEGIN/COMMIT
/// for a bulk-replay or use auto-commit for a small commit-time batch. A
/// fresh prepared-statement cache (Ctx) is created and disposed per call,
/// so the cache lifetime matches a single `applyOpsOnConnection` invocation.
///
/// Dep-edge location columns come straight from each `Dependency`'s `location` (stashed on `NameResolution`
/// at resolve time) — no post-hoc backfill.
let applyOpsOnConnectionFrom
  (conn : SqliteConnection)
  (source : string)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    let ctx = newCtx conn
    try
      for op in ops do
        do! applyOp ctx source op
    finally
      disposeCtx ctx

    // The fold just changed what names mean. Anything holding a cached answer from before now holds a
    // wrong one, and in a long-lived process (the REPL, the LSP, a daemon) that answer never expires on
    // its own. See `Caching.invalidateAll`.
    Caching.invalidateAll ()
  }

let applyOpsOnConnection
  (conn : SqliteConnection)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  applyOpsOnConnectionFrom conn "op" ops


/// Convenience wrapper for callers that don't have a shared connection (e.g.
/// Inserts.fs at commit time, where the op batch is small). Opens a fresh
/// connection per call and wraps the whole batch in a single transaction —
/// faster than auto-commit and makes the apply atomic with respect to other
/// readers.
let applyOpsFrom (source : string) (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    use tx = conn.BeginTransaction()
    do! applyOpsOnConnectionFrom conn source ops
    tx.Commit()
  }

let applyOps (ops : List<PT.PackageOp>) : Task<unit> = applyOpsFrom "op" ops
