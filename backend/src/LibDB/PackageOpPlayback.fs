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


// ------------------------------------------------------------------
// Low-level helpers — raw Microsoft.Data.Sqlite on a shared connection
// with per-SQL-template SqliteCommand caching.
// ------------------------------------------------------------------

/// Per-batch context: holds the open connection plus a cache of
/// SqliteCommand objects keyed by SQL text. First time we see a given
/// SQL, we build + Prepare() the command; subsequent calls clear the
/// parameter collection and reuse the same prepared command. Avoids
/// re-allocating SqliteCommand and re-parsing SQL on each of the ~20k
/// statements that fly during a 9845-op grow.
type private Ctx =
  { conn : SqliteConnection
    cmds : System.Collections.Generic.Dictionary<string, SqliteCommand> }

let private newCtx (conn : SqliteConnection) : Ctx =
  { conn = conn
    cmds = System.Collections.Generic.Dictionary<string, SqliteCommand>() }

let private disposeCtx (ctx : Ctx) : unit =
  for KeyValue(_, cmd) in ctx.cmds do
    cmd.Dispose()
  ctx.cmds.Clear()

/// Run a non-query SQL statement on the shared connection.
/// `setParams` populates the SqliteCommand's parameters (named with `$name`).
/// On first call for a given `sql`, the command is built and Prepare()'d;
/// later calls reuse the same SqliteCommand (clearing + re-adding params).
let private exec
  (ctx : Ctx)
  (sql : string)
  (setParams : SqliteCommand -> unit)
  : Task<unit> =
  task {
    let cmd =
      match ctx.cmds.TryGetValue(sql) with
      | true, c -> c
      | false, _ ->
        let c = ctx.conn.CreateCommand()
        c.CommandText <- sql
        c.Prepare()
        ctx.cmds[sql] <- c
        c
    cmd.Parameters.Clear()
    setParams cmd
    let! _ = cmd.ExecuteNonQueryAsync()
    return ()
  }

/// Helper for `cmd.Parameters.AddWithValue` that always returns unit.
let inline private p (cmd : SqliteCommand) (name : string) (value : obj) =
  cmd.Parameters.AddWithValue(name, value) |> ignore<SqliteParameter>

/// Bind a `Guid` as its canonical text representation. Without this, the
/// default Microsoft.Data.Sqlite type mapping is `BLOB(16)`, which does
/// not match the TEXT columns we store branch_id / location_id / etc. as
/// — so foreign-key checks fail with "constraint violated" even though
/// the parent row exists. (Fumble's `Sql.uuid` did this implicitly; we
/// replicate it.)
let inline private pUuid
  (cmd : SqliteCommand)
  (name : string)
  (value : System.Guid)
  =
  p cmd name (string value)

/// Bind a `string option` as either the string or DBNull.
let inline private pOpt
  (cmd : SqliteCommand)
  (name : string)
  (value : string option)
  =
  match value with
  | Some s -> p cmd name (box s)
  | None -> p cmd name (box System.DBNull.Value)


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

    let ptDef = BS.PT.PackageType.serialize hashStr typ
    let rtDef = typ |> PT2RT.PackageType.toRT |> BS.RT.PackageType.serialize hashStr

    do!
      exec ctx """
        INSERT OR REPLACE INTO package_types (hash, pt_def, rt_def, description)
        VALUES ($hash, $pt_def, $rt_def, $description)
        """ (fun cmd ->
        p cmd "$hash" hashStr
        p cmd "$pt_def" ptDef
        p cmd "$rt_def" rtDef
        p cmd "$description" typ.description)

    // Extract and store dependency references atomically. Each
    // Dependency carries its own location (populated by the resolver).
    let refs = DE.extractFromType typ
    do! updateDependencies ctx hashStr refs
  }

/// Apply a single AddValue op to the package_values table.
/// Note: rt_dval and value_type are stored as NULL here. They are populated
/// by Seed.evaluateAllValues after all ops are applied, so cross-
/// package references resolve correctly.
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

    let ptDef = BS.PT.PackageValue.serialize hashStr value

    // ON CONFLICT(hash) since values are content-addressed; we may re-encounter
    // the same hash via re-applied or duplicated ops.
    do!
      exec ctx """
        INSERT INTO package_values (hash, pt_def, rt_dval, value_type, description)
        VALUES ($hash, $pt_def, NULL, NULL, $description)
        ON CONFLICT(hash) DO UPDATE SET
          pt_def = excluded.pt_def,
          description = excluded.description
        """ (fun cmd ->
        p cmd "$hash" hashStr
        p cmd "$pt_def" ptDef
        p cmd "$description" value.description)

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

    let ptDef = BS.PT.PackageFn.serialize hashStr fn
    let rtInstrs = fn |> PT2RT.PackageFn.toRT |> BS.RT.PackageFn.serialize hashStr

    do!
      exec ctx """
        INSERT OR REPLACE INTO package_functions (hash, pt_def, rt_instrs, description)
        VALUES ($hash, $pt_def, $rt_instrs, $description)
        """ (fun cmd ->
        p cmd "$hash" hashStr
        p cmd "$pt_def" ptDef
        p cmd "$rt_instrs" rtInstrs
        p cmd "$description" fn.description)

    let refs = DE.extractFromFn fn
    do! updateDependencies ctx hashStr refs
  }

/// Apply a Set*Name op to the locations table.
/// isRename = true when this SetName is a standalone rename (not paired with Add*),
///   meaning old locations for the same hash should be deprecated.
/// <param source> is what put the binding there: "op" for a normal fold, "resolution" for a human's answer
/// to a conflict. `discard` deletes op-fold bindings but skips resolutions, so the tag is what stops a
/// routine discard from silently undoing a decision someone made on purpose.
let private applySetNameFrom
  (ctx : Ctx)
  (source : string)
  (opForStamp : PT.PackageOp)
  (isRename : bool)
  (itemHash : Hash)
  (location : PT.PackageLocation)
  (itemKind : PT.ItemKind)
  : Task<unit> =
  task {
    let modulesStr = String.concat "." location.modules
    let itemTypeStr = itemKind.toString ()
    let locationId = System.Guid.NewGuid()
    let (Hash itemHashStr) = itemHash

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
        // Timestamp LWW, tie-break by HIGHER hash. This reads `isStale`, so the op is SKIPPED when its
        // hash is the lower one. Stamps are `yyyy-MM-ddTHH:mm:ss.fffZ`, so lexical `<` is chronological.
        //
        // THIS RULE EXISTS TWICE. `SCM.Conflicts.incomingWins` in Dark decides the same question when
        // recording a conflict, and the two must agree or one machine records a conflict the other doesn't,
        // or worse, they converge on different winners. Change one, change both. They agree today: later
        // stamp wins, exact tie goes to the higher hash. The Dark twin has no equal-hash case because
        // `detect` filters those out before asking; this one needs it, since the fold sees every op.
        t < curTs || (t = curTs && itemHashStr < curHash)
      // Same content re-applied (a re-pull, or two instances that independently authored identical bytes for
      // this name): keep the EARLIEST origin_ts so the binding's stamp is identical on every instance
      // regardless of fold/arrival order. Skip unless this op is strictly earlier (then re-bind to lower it).
      // Without this the fold would re-stamp the binding with a LATER equal-hash op, and a different-hash op
      // stamped between the two could then win on one instance and lose on another → divergence. (receiveOps'
      // MIN-reconcile already keeps ops from arriving with a raised stamp; this makes the fold correct on its
      // own, for any caller.)
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

      // 2. If this is a rename (standalone SetName, not paired with Add*), also deprecate old locations
      //    pointing to the same hash. We do NOT do this for Add+SetName pairs because multiple items can
      //    legitimately share the same hash (e.g. Int8.ParseError and Int16.ParseError).
      if isRename then
        do!
          exec ctx """
            UPDATE locations
            SET unlisted_at = datetime('now')
            WHERE item_hash = $item_hash
              AND unlisted_at IS NULL
            """ (fun cmd -> p cmd "$item_hash" itemHashStr)

      // 3. Insert new location entry (with origin_ts for cross-instance timestamp-LWW).
      do!
        exec ctx """
          INSERT INTO locations
            (location_id, item_hash, owner, modules, name, item_type, origin_ts, source, op_id)
          VALUES ($location_id, $item_hash, $owner, $modules, $name, $item_type, $origin_ts, $source, $op_id)
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
          p cmd "$op_id" (string thisOpId))
  }

/// The ordinary name binding. <param source> records WHAT put it here -- see `locations.source`.
let private applySetName
  (ctx : Ctx)
  (source : string)
  (opForStamp : PT.PackageOp)
  (isRename : bool)
  (itemHash : Hash)
  (location : PT.PackageLocation)
  (itemKind : PT.ItemKind)
  : Task<unit> =
  applySetNameFrom ctx source opForStamp isRename itemHash location itemKind


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

/// Fold a Decide op into whichever projection owns that kind.
///
/// This is what makes `propagation_policy` and the Constraint acks in `conflicts` DERIVED tables rather
/// than a second source of truth.
///
/// The stamp comes from the OP's own `origin_ts`, not from now, so a decision keeps the time it was made
/// when it syncs -- which is what lets LWW agree across instances instead of "whoever imported last wins".
///
/// Unknown kinds are IGNORED rather than an error: a newer instance will invent kinds this build does not
/// know, and refusing to fold one turns "you're behind" into "your log is broken".
let private applyDecide
  (ctx : Ctx)
  (branchId : string)
  (op : PT.PackageOp)
  (kind : string)
  (loc : PT.PackageLocation)
  (value : string)
  (reason : string)
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
    | "propagation" when value = "unset" ->
      // Clearing is a decision like any other, so it's an op -- but it's the one that removes the row rather
      // than writing it. Still guarded by origin_ts, so a stale unset can't wipe a newer pin.
      do!
        exec ctx "DELETE FROM propagation_policy
           WHERE branch_id = $branch AND owner = $owner AND modules = $modules AND name = $name
             AND COALESCE(origin_ts, '') < $ts" (fun cmd ->
          cmd.Parameters.AddWithValue("$branch", branchId)
          |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$owner", loc.owner)
          |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$modules", modules)
          |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$name", loc.name) |> ignore<SqliteParameter>
          cmd.Parameters.AddWithValue("$ts", ts) |> ignore<SqliteParameter>)

    | "propagation" ->
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
            cmd.Parameters.AddWithValue("$branch", branchId)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$owner", loc.owner)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$modules", modules)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$name", loc.name)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$policy", value) |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$note", reason) |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$ts", ts) |> ignore<SqliteParameter>)

    | "constraint-ack" ->
      // `value` is the finding id. A finding nobody has answered isn't stored at all -- only answers are
      // rows, because detection re-derives the findings whenever anyone asks.
      do!
        exec
          ctx
          "INSERT INTO conflicts
             (id, owner, modules, name, item_type, kind, candidates, auto_resolved_to, reason, status, origin_ts)
           -- item_type is left EMPTY rather than guessed. The Decide op doesn't carry the usage's kind, and
           -- an ack row's kind isn't read by anything (`ackedIds` selects on id + status); writing 'fn' for
           -- what might be a type or a value would be asserting something false into a column someone will
           -- eventually trust.
           VALUES ($id, $owner, $modules, $name, '', 'stale-usage', '[]', '', $reason, 'acked', $ts)
           ON CONFLICT(id) DO UPDATE SET
             status = 'acked', reason = excluded.reason, origin_ts = excluded.origin_ts"
          (fun cmd ->
            cmd.Parameters.AddWithValue("$id", value) |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$owner", loc.owner)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$modules", modules)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$name", loc.name)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$reason", reason)
            |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$ts", ts) |> ignore<SqliteParameter>)

    | _ -> ()
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
  (branchId : string)
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
            cmd.Parameters.AddWithValue("$b", branchId) |> ignore<SqliteParameter>
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
            cmd.Parameters.AddWithValue("$b", branchId) |> ignore<SqliteParameter>)

      do!
        exec ctx "DELETE FROM op_branches WHERE branch_id = $b" (fun cmd ->
          cmd.Parameters.AddWithValue("$b", branchId) |> ignore<SqliteParameter>)
    | PT.Archived ->
      do!
        exec
          ctx
          "UPDATE branches SET archived_at = $at WHERE id = $b AND archived_at IS NULL"
          (fun cmd ->
            cmd.Parameters.AddWithValue("$b", branchId) |> ignore<SqliteParameter>
            cmd.Parameters.AddWithValue("$at", at) |> ignore<SqliteParameter>)
  }


let private applyOp
  (ctx : Ctx)
  (source : string)
  (addedHashes : Set<Hash>)
  (op : PT.PackageOp)
  : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType ctx typ
    | PT.PackageOp.AddValue value -> do! applyAddValue ctx value
    | PT.PackageOp.AddFn fn -> do! applyAddFn ctx fn
    | PT.PackageOp.SetName(loc, target, _) ->
      let isRename = not (Set.contains target.hash addedHashes)
      do! applySetName ctx source op isRename target.hash loc target.kind
    | PT.PackageOp.Deprecate(target, kind, message) ->
      do! applyDeprecate ctx target kind message
    | PT.PackageOp.Undeprecate target -> do! applyUndeprecate ctx target
    | PT.PackageOp.Resolve(_, loc, target) ->
      // Never a rename: a resolution answers ONE name, and unlisting every other location that happens to
      // share the hash (which identical content does, routinely) would be collateral damage.
      do! applySetNameFrom ctx "resolution" op false target.hash loc target.kind
    | PT.PackageOp.Decide(kind, loc, value, reason, _) ->
      // main: branch_id = ""
      do! applyDecide ctx "" op kind loc value reason
    | PT.PackageOp.BranchEvent(branchId, event, at) ->
      do! applyBranchEvent ctx branchId event at
  }


/// Collect hashes from Add* ops to distinguish "add + name" from "rename".
/// When SetName references a hash that was added in the same batch, it's
/// giving a name to a new item. Otherwise it's a rename (move).
let private collectAddedHashes (ops : List<PT.PackageOp>) : Set<Hash> =
  ops
  |> List.choose (fun op ->
    match op with
    | PT.PackageOp.AddType t -> Some t.hash
    | PT.PackageOp.AddValue v -> Some v.hash
    | PT.PackageOp.AddFn f -> Some f.hash
    | _ -> None)
  |> Set.ofList


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
      let addedHashes = collectAddedHashes ops
      for op in ops do
        do! applyOp ctx source addedHashes op
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
