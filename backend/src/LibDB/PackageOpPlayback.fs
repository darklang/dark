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
/// branchId = branch context, commitHash = None means WIP, Some id means committed.
/// isRename = true when this SetName is a standalone rename (not paired with Add*),
///   meaning old locations for the same hash should be deprecated.
let private applySetName
  (ctx : Ctx)
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
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

    // ── timestamp-LWW: order this binding by the op's CREATION time,
    // not arrival. Read this op's `origin_ts` (the authoring stamp, already in package_ops) and the
    // CURRENT binding's `origin_ts` (the name→authoring-time mapping in `locations`). If this op was
    // created BEFORE the current binding's op — an old op arriving late via sync — it's stale: keep the
    // existing, newer-by-creation binding (the op still lives in the log; it's just not the active name).
    // Computed identically on every instance, so all converge to the SAME hash regardless of arrival
    // order. Unknown stamps (op not in package_ops / pre-origin_ts data) → no skip = prior last-writer
    // behavior, so non-sync playback (seed grow, local authoring) is unchanged. Reads run on ctx.conn so
    // they see writes from earlier ops in this same applyOps transaction.
    let thisOp =
      PT.PackageOp.SetName(
        location,
        PT.Reference.fromHashAndKind (itemHash, itemKind)
      )
    let (Hash thisOpHashStr) = LibSerialization.Hashing.Hashing.computeOpHash thisOp
    let thisOpId = System.Guid(System.Convert.FromHexString(thisOpHashStr)[0..15])

    let! thisTs =
      task {
        use cmd = ctx.conn.CreateCommand()
        // Scope by (id, branch_id) — the composite PK. The same content op can exist on two branches with
        // different origin_ts, so an `id`-only lookup could read the wrong branch's stamp and pick a different
        // LWW winner across instances.
        cmd.CommandText <-
          "SELECT origin_ts FROM package_ops WHERE id = $id AND branch_id = $branch_id"
        cmd.Parameters.AddWithValue("$id", string thisOpId)
        |> ignore<SqliteParameter>
        cmd.Parameters.AddWithValue("$branch_id", string branchId)
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
          + "AND branch_id = $branch_id AND unlisted_at IS NULL LIMIT 1"
        cmd.Parameters.AddWithValue("$owner", location.owner)
        |> ignore<SqliteParameter>
        cmd.Parameters.AddWithValue("$modules", modulesStr)
        |> ignore<SqliteParameter>
        cmd.Parameters.AddWithValue("$name", location.name)
        |> ignore<SqliteParameter>
        cmd.Parameters.AddWithValue("$branch_id", string branchId)
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
        Lww.isStale t itemHashStr curTs curHash
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
            AND branch_id = $branch_id
          """ (fun cmd ->
          p cmd "$owner" location.owner
          p cmd "$modules" modulesStr
          p cmd "$name" location.name
          pUuid cmd "$branch_id" branchId)

      // 2. If this is a rename (standalone SetName, not paired with Add*), also deprecate old locations
      //    pointing to the same hash. We do NOT do this for Add+SetName pairs because multiple items can
      //    legitimately share the same hash (e.g. Int8.ParseError and Int16.ParseError).
      if isRename then
        do!
          exec ctx """
            UPDATE locations
            SET unlisted_at = datetime('now')
            WHERE item_hash = $item_hash
              AND branch_id = $branch_id
              AND unlisted_at IS NULL
            """ (fun cmd ->
            p cmd "$item_hash" itemHashStr
            pUuid cmd "$branch_id" branchId)

      // 3. Insert new location entry (with origin_ts for cross-instance timestamp-LWW).
      do!
        exec ctx """
          INSERT INTO locations (location_id, item_hash, owner, modules, name, item_type, branch_id, commit_hash, origin_ts)
          VALUES ($location_id, $item_hash, $owner, $modules, $name, $item_type, $branch_id, $commit_hash, $origin_ts)
          """ (fun cmd ->
          pUuid cmd "$location_id" locationId
          p cmd "$item_hash" itemHashStr
          p cmd "$owner" location.owner
          p cmd "$modules" modulesStr
          p cmd "$name" location.name
          p cmd "$item_type" itemTypeStr
          pUuid cmd "$branch_id" branchId
          pOpt cmd "$commit_hash" commitHash
          pOpt cmd "$origin_ts" thisTs)
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
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
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
        WHERE branch_id = $branch_id
          AND item_hash = $item_hash
          AND item_kind = $item_kind
          AND unlisted_at IS NULL
        """ (fun cmd ->
        pUuid cmd "$branch_id" branchId
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr)

    do!
      exec ctx """
        INSERT INTO deprecations
          (deprecation_id, branch_id, commit_hash, item_hash, item_kind, state, annotation_blob)
        VALUES
          ($deprecation_id, $branch_id, $commit_hash, $item_hash, $item_kind, 'deprecated', $blob)
        """ (fun cmd ->
        pUuid cmd "$deprecation_id" deprecationId
        pUuid cmd "$branch_id" branchId
        pOpt cmd "$commit_hash" commitHash
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr
        p cmd "$blob" blob)
  }


/// Apply an Undeprecate op to the deprecations projection table.
/// Records an `undeprecated`-state row that supersedes any prior row for the
/// same (branch, item_hash, item_kind). This is how child branches override
/// ancestor-branch deprecations.
let private applyUndeprecate
  (ctx : Ctx)
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (target : PT.Reference)
  : Task<unit> =
  task {
    let (Hash itemHashStr) = target.hash
    let itemKindStr = target.kind.toString ()
    let deprecationId = System.Guid.NewGuid()

    do!
      exec ctx """
        UPDATE deprecations
        SET unlisted_at = datetime('now')
        WHERE branch_id = $branch_id
          AND item_hash = $item_hash
          AND item_kind = $item_kind
          AND unlisted_at IS NULL
        """ (fun cmd ->
        pUuid cmd "$branch_id" branchId
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr)

    do!
      exec ctx """
        INSERT INTO deprecations
          (deprecation_id, branch_id, commit_hash, item_hash, item_kind, state, annotation_blob)
        VALUES
          ($deprecation_id, $branch_id, $commit_hash, $item_hash, $item_kind, 'undeprecated', NULL)
        """ (fun cmd ->
        pUuid cmd "$deprecation_id" deprecationId
        pUuid cmd "$branch_id" branchId
        pOpt cmd "$commit_hash" commitHash
        p cmd "$item_hash" itemHashStr
        p cmd "$item_kind" itemKindStr)
  }


/// Apply a RevertPropagation op — undoes the source's WIP location and the
/// dependents' repointed locations, restoring the previous state.
let private applyRevertPropagation
  (ctx : Ctx)
  (branchId : PT.BranchId)
  (sourceLocation : PT.PackageLocation)
  (restoredSourceRef : PT.Reference)
  (revertedRepoints : List<PT.PropagateRepoint>)
  : Task<unit> =
  task {
    let sourceItemKind = restoredSourceRef.kind

    // For each reverted repoint: unlist toRef, un-unlist fromRef.
    // Skip repoints for the source item — those are handled by the
    // dedicated source-handling block below (avoids redundant
    // double-toggle in mutual recursion).
    let dependentRepoints =
      revertedRepoints
      |> List.filter (fun r ->
        r.location <> sourceLocation || r.toRef.kind <> sourceItemKind)

    for repoint in dependentRepoints do
      let (Hash toHashStr) = repoint.toRef.hash
      let (Hash fromHashStr) = repoint.fromRef.hash

      do!
        exec ctx """
          UPDATE locations
          SET unlisted_at = datetime('now')
          WHERE item_hash = $item_hash
            AND branch_id = $branch_id
            AND unlisted_at IS NULL
          """ (fun cmd ->
          p cmd "$item_hash" toHashStr
          pUuid cmd "$branch_id" branchId)

      do!
        exec ctx """
          UPDATE locations
          SET unlisted_at = NULL
          WHERE location_id = (
            SELECT location_id FROM locations
            WHERE item_hash = $item_hash
              AND branch_id = $branch_id
              AND unlisted_at IS NOT NULL
            -- rowid tiebreak: unlisted_at is second-resolution, so a tie would pick an arbitrary row and a
            -- re-fold could restore a different version. Highest rowid = last-unlisted among ties = the true latest.
            ORDER BY unlisted_at DESC, rowid DESC
            LIMIT 1
          )
          """ (fun cmd ->
          p cmd "$item_hash" fromHashStr
          pUuid cmd "$branch_id" branchId)

    // Undo source: unlist WIP location, un-unlist committed location.
    let modulesStr = String.concat "." sourceLocation.modules
    let itemTypeStr = sourceItemKind.toString ()
    let (Hash restoredSourceHashStr) = restoredSourceRef.hash

    do!
      exec ctx """
        UPDATE locations
        SET unlisted_at = datetime('now')
        WHERE owner = $owner
          AND modules = $modules
          AND name = $name
          AND item_type = $item_type
          AND branch_id = $branch_id
          AND unlisted_at IS NULL
          AND commit_hash IS NULL
        """ (fun cmd ->
        p cmd "$owner" sourceLocation.owner
        p cmd "$modules" modulesStr
        p cmd "$name" sourceLocation.name
        p cmd "$item_type" itemTypeStr
        pUuid cmd "$branch_id" branchId)

    do!
      exec ctx """
        UPDATE locations
        SET unlisted_at = NULL
        WHERE location_id = (
          SELECT location_id FROM locations
          WHERE item_hash = $item_hash
            AND branch_id = $branch_id
            AND unlisted_at IS NOT NULL
          -- rowid tiebreak (see the other restore query): deterministic 'latest' across a re-fold.
          ORDER BY unlisted_at DESC, rowid DESC
          LIMIT 1
        )
        """ (fun cmd ->
        p cmd "$item_hash" restoredSourceHashStr
        pUuid cmd "$branch_id" branchId)
  }


// ------------------------------------------------------------------
// Op dispatch.
// ------------------------------------------------------------------

/// Apply a single PackageOp to the projection tables.
/// addedHashes = hashes of items added by Add* ops earlier in this batch
///   (used to distinguish "add + name" from "rename").
let private applyOp
  (ctx : Ctx)
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (addedHashes : Set<Hash>)
  (op : PT.PackageOp)
  : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType ctx typ
    | PT.PackageOp.AddValue value -> do! applyAddValue ctx value
    | PT.PackageOp.AddFn fn -> do! applyAddFn ctx fn
    | PT.PackageOp.SetName(loc, target) ->
      let isRename = not (Set.contains target.hash addedHashes)
      do! applySetName ctx branchId commitHash isRename target.hash loc target.kind
    | PT.PackageOp.Deprecate(target, kind, message) ->
      do! applyDeprecate ctx branchId commitHash target kind message
    | PT.PackageOp.Undeprecate target ->
      do! applyUndeprecate ctx branchId commitHash target
    | PT.PackageOp.PropagateUpdate _ ->
      // Location changes are already handled by the individual SetName ops that
      // accompany this op in the propagation batch. Applying them here too would
      // create duplicate location entries.
      ()
    | PT.PackageOp.RevertPropagation(_,
                                     _,
                                     sourceLocation,
                                     restoredSourceRef,
                                     revertedRepoints) ->
      do!
        applyRevertPropagation
          ctx
          branchId
          sourceLocation
          restoredSourceRef
          revertedRepoints
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
let applyOpsOnConnection
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    let ctx = newCtx conn
    try
      let addedHashes = collectAddedHashes ops
      for op in ops do
        do! applyOp ctx branchId commitHash addedHashes op
    finally
      disposeCtx ctx
  }


/// Convenience wrapper for callers that don't have a shared connection (e.g.
/// Inserts.fs at commit time, where the op batch is small). Opens a fresh
/// connection per call and wraps the whole batch in a single transaction —
/// faster than auto-commit and makes the apply atomic with respect to other
/// readers.
let applyOps
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    use tx = conn.BeginTransaction()
    do! applyOpsOnConnection conn branchId commitHash ops
    tx.Commit()
  }
