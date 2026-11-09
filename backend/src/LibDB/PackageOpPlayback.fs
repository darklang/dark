/// THE FOLD: applies PackageOps to the projection tables. The content tables (package_types,
/// package_values, package_functions, package_tests, package_traits, package_trait_impls) are shared
/// by every branch; names, deprecations and docs are written into the op's layer (`LibDB.Layers`).
///
/// All writes go through a caller-provided SqliteConnection so the entire op
/// batch can run inside one outer transaction. On cold-start grow (9000+ ops)
/// that turns ~20k tiny WAL commits into one, which is the difference between
/// a 5-second startup and a sub-second one.
module LibDB.PackageOpPlayback


open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Lww = LibExecution.Lww
module BS = LibSerialization.Binary.Serialization
module DE = LibDB.DependencyExtractor
open LibSerialization.Hashing

// `Ctx` and the `exec` / `execRows` / scalar readers / `p*` binders: one connection and one
// prepared-command cache for the whole batch. See LibDB.PreparedBatch for why.
open LibDB.PreparedBatch


// ------------------------------------------------------------------
// Dependency table maintenance.
// ------------------------------------------------------------------

/// One row of `package_dependencies`: an item, one callee by hash and kind, and the name this parse
/// resolved the callee through. The fields are the table's columns in order, which are also its key,
/// so sorting these sorts them the way the table stores them.
type DependencyRow =
  { itemHash : string
    hash : string
    kind : string
    owner : string
    modules : string
    name : string }

/// What an item's body calls: one row per callee, by hash AND by the name this parse resolved it
/// through.
let private dependencyRows
  (itemHash : string)
  (deps : List<DE.Dependency>)
  : List<DependencyRow> =
  deps
  |> List.map (fun dep ->
    let (Hash hashStr) = dep.hash
    let kind = dep.itemKind.toString ()
    match dep.location with
    | Some loc ->
      { itemHash = itemHash
        hash = hashStr
        kind = kind
        owner = loc.owner
        modules = String.concat "." loc.modules
        name = loc.name }
    | None ->
      // '' rather than NULL: the location columns are part of the table's key.
      { itemHash = itemHash
        hash = hashStr
        kind = kind
        owner = ""
        modules = ""
        name = "" })

let private dependencyInsert =
  "INSERT OR IGNORE INTO package_dependencies "
  + "(item_hash, depends_on_hash, depends_on_item_type, depends_on_owner, depends_on_modules, depends_on_name) "
  + "VALUES (?1, ?2, ?3, ?4, ?5, ?6)"

/// Record dependency rows, on the caller's connection and in its transaction.
///
/// ADDS, never replaces: content is immutable so a hash's callees never change, but
/// two names can hold one body, and each name it was resolved through must survive.
/// Readers join `locations` for what is live.
///
/// In key order, through one statement bound by position: a cold start writes about 40,000 of
/// these, and writing them in the order the ops came, through a command that binds each column by
/// name, was about a fifth of its work.
let writeDependencies (conn : SqliteConnection) (rows : List<DependencyRow>) : unit =
  rows
  |> List.sort
  |> insertRows conn dependencyInsert (fun r ->
    [| r.itemHash; r.hash; r.kind; r.owner; r.modules; r.name |])


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
/// <param incomingFingerprint> is computed only when the hash is already stored, which on a cold
/// fold is almost never: it rehashes the whole item.
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
  (incomingFingerprint : unit -> Hash)
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
          (incomingFingerprint ())
          storedFingerprint

      if mayRewriteExisting then
        // Reference-name snapshots use the first stored body as their baseline.
        // Promoting branch content to main must not change that baseline.
        let metadata = names |> List.filter (fun name -> name <> "pt_def")
        if not (List.isEmpty metadata) then
          let assignments =
            metadata
            |> List.map (fun name -> $"{name} = ${name}")
            |> String.concat ", "
          do! exec ctx $"UPDATE {table} SET {assignments} WHERE hash = $hash" bind
  }


/// Apply a single AddType op to the package_types table.
let private applyAddType
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (typ : PT.PackageType.PackageType)
  : Task<List<DependencyRow>> =
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
        (fun () -> Hashing.computeTypeHash Hashing.Normal typ)
        (fun bytes ->
          BS.PT.PackageType.deserialize hash bytes
          |> Hashing.computeTypeHash Hashing.Normal)

    return dependencyRows hashStr (DE.extractFromType typ)
  }

/// Apply a single AddValue op to the package_values table.
let private applyAddValue
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (value : PT.PackageValue.PackageValue)
  : Task<List<DependencyRow>> =
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
        (fun () -> Hashing.computeValueHash Hashing.Normal value)
        (fun bytes ->
          BS.PT.PackageValue.deserialize hash bytes
          |> Hashing.computeValueHash Hashing.Normal)

    return dependencyRows hashStr (DE.extractFromValue value)
  }

/// Apply a single AddTrait op to the package_traits table.
let private applyAddTrait
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (t : PT.Trait.Trait)
  : Task<List<DependencyRow>> =
  task {
    let hash =
      match t.hash with
      | Hash "" -> Hashing.computeTraitHash Hashing.Normal t
      | h -> h
    let t = { t with hash = hash }
    let (Hash hashStr) = hash

    do!
      upsertContentAddressed
        ctx
        "trait"
        "package_traits"
        hash
        [ "pt_def", box (BS.PT.Trait.serialize hashStr t)
          "description", box t.description ]
        []
        mayRewriteExisting
        (fun () -> Hashing.computeTraitHash Hashing.Normal t)
        (fun bytes ->
          BS.PT.Trait.deserialize hash bytes
          |> Hashing.computeTraitHash Hashing.Normal)

    return dependencyRows hashStr (DE.extractFromTrait t)
  }

/// Apply a single AddTraitImpl op to the package_trait_impls table.
///
/// `originTs` is the op's own stamp, kept on the row because selection needs to order two impls of
/// one trait for one type the same way on every instance (`LibExecution.Lww`). Empty when the op has
/// no stamp: then the impls have no order and a call that finds both says so.
let private applyAddImpl
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (originTs : string)
  (i : PT.TraitImpl.TraitImpl)
  : Task<List<DependencyRow>> =
  task {
    let hash =
      match i.hash with
      | Hash "" -> Hashing.computeImplHash Hashing.Normal i
      | h -> h
    let i = { i with hash = hash }
    let (Hash hashStr) = hash
    let traitHash =
      match i.trait_.resolved with
      | Ok { name = PT.FQTraitName.Package(Hash t) } -> t
      | Error _ -> ""

    do!
      upsertContentAddressed
        ctx
        "impl"
        "package_trait_impls"
        hash
        [ "pt_def", box (BS.PT.TraitImpl.serialize hashStr i)
          "trait_hash", box traitHash
          "description", box i.description
          "origin_ts", box originTs ]
        []
        mayRewriteExisting
        (fun () -> Hashing.computeImplHash Hashing.Normal i)
        (fun bytes ->
          BS.PT.TraitImpl.deserialize hash bytes
          |> Hashing.computeImplHash Hashing.Normal)

    return dependencyRows hashStr (DE.extractFromImpl i)
  }

/// Apply a single AddFn op to the package_functions table.
let private applyAddFn
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (fn : PT.PackageFn.PackageFn)
  : Task<List<DependencyRow>> =
  task {
    let hash =
      match fn.hash with
      | Hash "" -> Hashing.computeFnHash Hashing.Normal fn
      | h -> h
    let fn = { fn with hash = hash }
    let (Hash hashStr) = hash
    // Compiled once. Both columns come out of the same compilation, and doing it twice was
    // doing the whole of PT2RT twice for every function written.
    let rt = PT2RT.PackageFn.toRT fn

    do!
      upsertContentAddressed
        ctx
        "fn"
        "package_functions"
        hash
        [ "pt_def", box (BS.PT.PackageFn.serialize hashStr fn)
          "rt_instrs", box (BS.RT.PackageFn.serialize hashStr rt)
          // The other half of what the compiler makes: which instruction produced which
          // expression's value. Its own column, because running the code never reads it.
          "debug_symbols",
          box (BS.RT.PackageFn.serializeDebugSymbols hashStr rt.symbols.Value)
          "description", box fn.description ]
        []
        mayRewriteExisting
        (fun () -> Hashing.computeFnHash Hashing.Normal fn)
        (fun bytes ->
          BS.PT.PackageFn.deserialize hash bytes
          |> Hashing.computeFnHash Hashing.Normal)

    return dependencyRows hashStr (DE.extractFromFn fn)
  }

/// Apply a single AddTest op. Unlike a function, a test is stored as source
/// only: it is compiled when `dark test run` runs it, and the runtime package
/// manager has no way to look one up, so nothing can call a test.
let private applyAddTest
  (ctx : Ctx)
  (mayRewriteExisting : bool)
  (test : PT.PackageTest.PackageTest)
  : Task<List<DependencyRow>> =
  task {
    let hash =
      match test.hash with
      | Hash "" -> Hashing.computeTestHash Hashing.Normal test
      | h -> h
    let test = { test with hash = hash }
    let (Hash hashStr) = hash

    do!
      upsertContentAddressed
        ctx
        "test"
        "package_tests"
        hash
        [ "pt_def", box (BS.PT.PackageTest.serialize hashStr test)
          "description", box test.description ]
        []
        mayRewriteExisting
        (fun () -> Hashing.computeTestHash Hashing.Normal test)
        (fun bytes ->
          BS.PT.PackageTest.deserialize hash bytes
          |> Hashing.computeTestHash Hashing.Normal)

    return dependencyRows hashStr (DE.extractFromTest test)
  }

/// What the log says about the op being folded: its stamp, whether it is still in the draft, and what
/// wrote it. Read from its row on the branch it is folding into, which every caller has stored first.
type OpRow = { ts : Option<string>; draft : bool; source : string }

module OpRow =
  /// An op the log does not hold: nothing stamped it, and it is nobody's commit.
  let notInLog : OpRow = { ts = None; draft = true; source = "op" }

/// An op as `Seed.foldRead` has it: read from the log along with its row id and its row, so the fold
/// neither hashes it again for its id nor reads its row back one op at a time.
type LoggedOp = { op : PT.PackageOp; id : System.Guid; row : OpRow }

let private opRowOf
  (ctx : Ctx)
  (branch : PT.BranchId)
  (id : System.Guid)
  : Task<OpRow> =
  task {
    let cmd =
      command
        ctx
        "SELECT origin_ts, commit_hash IS NULL, source FROM package_ops
         WHERE id = $id AND branch_id = $branch"
    cmd.Parameters.Clear()
    pUuid cmd "$id" id
    p cmd "$branch" (string branch)
    use! reader = cmd.ExecuteReaderAsync()
    let! found = reader.ReadAsync()
    if found then
      return
        { ts = (if reader.IsDBNull 0 then None else Some(reader.GetString 0))
          draft = reader.GetInt64 1 <> 0L
          source = reader.GetString 2 }
    else
      return OpRow.notInLog
  }

/// The kind of item <param branch>'s view binds at <param location>, if it binds one: what a removal
/// records on its tombstone.
let private kindInView
  (ctx : Ctx)
  (branch : PT.BranchId)
  (location : PT.PackageLocation)
  : Task<Option<string>> =
  task {
    let cmd =
      command
        ctx
        "SELECT branch_id, draft, item_type FROM locations
         WHERE owner = $owner AND modules = $modules AND name = $name AND live = 1"
    cmd.Parameters.Clear()
    pLoc cmd location
    use! reader = cmd.ExecuteReaderAsync()
    let rows =
      reader
      |> Sqlite.readRows (fun r ->
        (r.GetString 0, r.GetInt64 1 <> 0L, r.GetString 2))
    return Layers.pick (Layers.chainOf branch) rows
  }

let private layerParams (cmd : SqliteCommand) (layer : Layers.Layer) =
  p cmd "$branch" (string layer.branch)
  p cmd "$draft" (if layer.draft then 1L else 0L)

/// The live row of <param layer> at a name: (item_hash, origin_ts). An `item_hash` of "" is a
/// removal.
let private liveInLayer
  (ctx : Ctx)
  (layer : Layers.Layer)
  (location : PT.PackageLocation)
  : Task<Option<string * Option<string>>> =
  pairOption ctx "SELECT item_hash, origin_ts FROM locations
     WHERE owner = $owner AND modules = $modules AND name = $name
       AND branch_id = $branch AND draft = $draft AND live = 1" (fun cmd ->
    pLoc cmd location
    layerParams cmd layer)

/// Write a row into <param layer> at <param location>: it becomes the layer's live row there.
/// <param itemHash> "" for a removal.
let private writeName
  (ctx : Ctx)
  (layer : Layers.Layer)
  (location : PT.PackageLocation)
  (itemHash : string)
  (itemType : string)
  (ts : Option<string>)
  (source : string)
  (opId : System.Guid)
  (previous : Option<string>)
  : Task<unit> =
  task {
    do! Layers.supersede ctx Layers.locations layer (fun cmd -> pLoc cmd location)
    do!
      exec ctx """
        INSERT INTO locations
          (branch_id, draft, owner, modules, name, item_hash, item_type, origin_ts, source, op_id,
           previous)
        VALUES ($branch, $draft, $owner, $modules, $name, $item_hash, $item_type, $origin_ts, $source,
                $op_id, $previous)
        """ (fun cmd ->
        layerParams cmd layer
        pLoc cmd location
        p cmd "$item_hash" itemHash
        p cmd "$item_type" itemType
        pOpt cmd "$origin_ts" ts
        p cmd "$source" source
        p cmd "$op_id" (string opId)
        pOpt cmd "$previous" previous)
  }

/// Fold a binding: <param location> holds <param itemHash> in <param layer>, unless what the layer
/// holds there is newer.
///
/// Last-writer-wins by the ops' own stamps (`LibExecution.Lww`, shared with conflict detection), so
/// a merge folding a branch's older edit over the parent's newer one keeps the newer one. A removal
/// in the layer beats a binding only when it is strictly later.
let private applySetName
  (ctx : Ctx)
  (layer : Layers.Layer)
  (row : OpRow)
  (source : string)
  (op : PT.PackageOp)
  (opId : System.Guid)
  (itemHash : Hash)
  (location : PT.PackageLocation)
  (itemKind : PT.ItemKind)
  : Task<unit> =
  task {
    let (Hash itemHashStr) = itemHash
    let previousHash =
      match op with
      | PT.PackageOp.SetName(_, _, Some(Hash h)) -> Some h
      | _ -> None

    let! current = liveInLayer ctx layer location
    let isStale =
      match current, row.ts with
      | Some("", Some removedTs), Some t -> Lww.unbindBeatsBinding removedTs t
      | Some(curHash, Some curTs), Some t when curHash <> itemHashStr ->
        Lww.isStale t itemHashStr curTs curHash
      // Same content again: keep the earliest stamp, so the binding's stamp does not depend on
      // the order its ops arrived in.
      | Some(curHash, Some curTs), Some t when curHash = itemHashStr -> t >= curTs
      | _ -> false

    if not isStale then
      do!
        writeName
          ctx
          layer
          location
          itemHashStr
          (itemKind.toString ())
          row.ts
          source
          opId
          previousHash
  }

/// Fold an `Unbind`: the name stops existing in <param layer>, which hides it in every layer below.
/// Content is untouched. Ties go to the binding (`Lww.unbindBeatsBinding`).
let private applyUnbind
  (ctx : Ctx)
  (layer : Layers.Layer)
  (row : OpRow)
  (opId : System.Guid)
  (location : PT.PackageLocation)
  (previous : Option<Hash>)
  : Task<unit> =
  task {
    let! current = liveInLayer ctx layer location
    let isStale =
      match current, row.ts with
      | Some(h, Some curTs), Some t when h <> "" ->
        not (Lww.unbindBeatsBinding t curTs)
      | _ -> false

    if not isStale then
      let! kind = kindInView ctx layer.branch location
      do!
        writeName
          ctx
          layer
          location
          ""
          (kind |> Option.defaultValue (PT.ItemKind.Fn.toString ()))
          row.ts
          "op"
          opId
          (previous |> Option.map (fun (Hash h) -> h))
  }

/// Record a deprecation-state change in <param layer>: the newest statement stands, by the op's own
/// stamp, whatever order the ops arrived in. Keyed on content: two names sharing a hash deprecate
/// together.
let private writeDeprecationState
  (ctx : Ctx)
  (layer : Layers.Layer)
  (row : OpRow)
  (opId : System.Guid)
  (target : PT.Reference)
  (state : string)
  (blob : Option<byte[]>)
  : Task<unit> =
  task {
    let (Hash itemHashStr) = target.hash
    let itemKindStr = target.kind.toString ()
    let bindKey (cmd : SqliteCommand) =
      p cmd "$item_hash" itemHashStr
      p cmd "$item_kind" itemKindStr

    let! standingTs =
      textOption ctx "SELECT origin_ts FROM deprecations
         WHERE item_hash = $item_hash AND item_kind = $item_kind
           AND branch_id = $branch AND draft = $draft AND live = 1" (fun cmd ->
        bindKey cmd
        layerParams cmd layer)

    let ts = row.ts |> Option.defaultValue ""
    let stale =
      match standingTs with
      | Some standing when standing <> "" -> ts < standing
      | _ -> false

    if not stale then
      do! Layers.supersede ctx Layers.deprecations layer bindKey
      do!
        exec ctx """
          INSERT INTO deprecations
            (branch_id, draft, item_hash, item_kind, state, annotation_blob, origin_ts, op_id)
          VALUES ($branch, $draft, $item_hash, $item_kind, $state, $blob, $origin_ts, $op_id)
          """ (fun cmd ->
          layerParams cmd layer
          bindKey cmd
          p cmd "$state" state
          p cmd "$origin_ts" ts
          p cmd "$op_id" (string opId)
          match blob with
          | Some b -> p cmd "$blob" b
          | None -> p cmd "$blob" System.DBNull.Value)
  }


// ------------------------------------------------------------------
// Op dispatch.
// ------------------------------------------------------------------

/// Fold one op into <param branch>'s projections: content into the shared content tables, and
/// names, deprecations and docs into the layer its row is in (the draft until it is committed).
/// Returns the dependency rows of the item it adds, for the caller to write (`writeDependencies`).
///
/// <param logged> is the op's row id and its row in the log. A function, because most content ops
/// need neither, and finding them costs a hash and a read.
let private applyOp
  (ctx : Ctx)
  (branch : PT.BranchId)
  (logged : unit -> Task<System.Guid * OpRow>)
  (op : PT.PackageOp)
  : Task<List<DependencyRow>> =
  task {
    // Content rows are shared by every branch, and identical by hash; the only thing that can
    // differ is the doc comment, and only main's work rewrites it, or a branch's wording would be
    // published to everyone. A branch says what it thinks an item is with an `UpdateDoc`.
    let mayRewriteExisting = branch.IsMain
    match op with
    | PT.PackageOp.AddType typ -> return! applyAddType ctx mayRewriteExisting typ
    | PT.PackageOp.AddValue value ->
      return! applyAddValue ctx mayRewriteExisting value
    | PT.PackageOp.AddFn fn -> return! applyAddFn ctx mayRewriteExisting fn
    | PT.PackageOp.AddTrait t -> return! applyAddTrait ctx mayRewriteExisting t
    | PT.PackageOp.AddTraitImpl i ->
      // The op's own time, so two rival impls order the same way wherever they land.
      let! (_, row) = logged ()
      return! applyAddImpl ctx mayRewriteExisting (Option.defaultValue "" row.ts) i
    | PT.PackageOp.AddTest test -> return! applyAddTest ctx mayRewriteExisting test
    | _ ->
      let! (opId, row) = logged ()
      let layer : Layers.Layer = { branch = branch; draft = row.draft }
      match op with
      | PT.PackageOp.SetName(loc, target, _) ->
        do! applySetName ctx layer row row.source op opId target.hash loc target.kind
      | PT.PackageOp.Unbind(loc, previous) ->
        do! applyUnbind ctx layer row opId loc previous
      // `restating` is part of the op's identity and nothing else: what it SAYS is the kind and
      // the message.
      | PT.PackageOp.Deprecate(target, kind, message, _) ->
        do!
          writeDeprecationState
            ctx
            layer
            row
            opId
            target
            "deprecated"
            (Some(Deprecations.serializeAnnotation kind message))
      | PT.PackageOp.Undeprecate(target, _) ->
        do! writeDeprecationState ctx layer row opId target "undeprecated" None
      // A rename is its two binding effects; the names its callers use are read from the op itself
      // (`ReferenceNames`), as `SetReferenceNames` is.
      | PT.PackageOp.Rename(oldLocation, newLocation, target, _, _) ->
        do! applyUnbind ctx layer row opId oldLocation (Some target.hash)
        do!
          applySetName
            ctx
            layer
            row
            row.source
            op
            opId
            target.hash
            newLocation
            target.kind
      | PT.PackageOp.SetReferenceNames _ -> ()
      | PT.PackageOp.UpdateDoc(location, part, text, previous, _) ->
        do!
          Docs.applyUpdateDoc
            ctx
            layer
            opId
            (Option.defaultValue "" row.ts)
            location
            part
            text
            previous
      | PT.PackageOp.Decision(id, loc, reason, kind) ->
        match kind with
        | PT.DecisionKind.Restore(_, target, _) ->
          // Undo putting an earlier version back: a binding, like an override.
          do!
            applySetName
              ctx
              layer
              row
              "resolution"
              op
              opId
              target.hash
              loc
              target.kind
        | PT.DecisionKind.Override target ->
          // An override answers ONE name, so it binds one name.
          do!
            applySetName
              ctx
              layer
              row
              "resolution"
              op
              opId
              target.hash
              loc
              target.kind
          // And closes the record it settles, on the branch it is folded on. Scoped to item KIND as
          // well as name: one location can hold a fn AND a value, and overriding the fn must not
          // close the value's conflict.
          do!
            exec
              ctx
              "UPDATE conflicts SET status = 'overridden', resolved_by = $op
               WHERE owner = $owner AND modules = $modules AND name = $name
                 AND item_type = $kind AND status = 'pending' AND branch_id = $branch"
              (fun cmd ->
                p cmd "$op" id
                p cmd "$kind" (target.kind.toString ())
                p cmd "$branch" (string layer.branch)
                pLoc cmd loc)
        | PT.DecisionKind.Ack findingId ->
          // A finding nobody has answered isn't stored at all -- only answers are rows, because
          // detection re-derives the findings whenever anyone asks.
          do!
            exec
              ctx
              "INSERT INTO conflicts
                 (id, owner, modules, name, item_type, kind, candidates, auto_resolved_to, reason,
                  status, origin_ts, branch_id)
               VALUES ($id, $owner, $modules, $name, '', 'stale-usage', '[]', '', $reason, 'acked',
                       $ts, $branch)
               ON CONFLICT(id) DO UPDATE SET
                 status = 'acked', reason = excluded.reason, origin_ts = excluded.origin_ts"
              (fun cmd ->
                p cmd "$id" findingId
                pLoc cmd loc
                p cmd "$reason" reason
                p cmd "$ts" (Option.defaultValue "" row.ts)
                p cmd "$branch" (string branch))
      | _ -> ()
      return []
  }


// ------------------------------------------------------------------
// Public API.
// ------------------------------------------------------------------

/// Fold <param ops> into <param branch>'s projections on <param ctx>, and return their dependency rows.
let private foldOps
  (ctx : Ctx)
  (branch : PT.BranchId)
  (ops : List<(unit -> Task<System.Guid * OpRow>) * PT.PackageOp>)
  : Task<List<DependencyRow>> =
  task {
    let! rows =
      ops |> Task.mapSequentially (fun (logged, op) -> applyOp ctx branch logged op)

    // Names just changed meaning; long-lived processes hold cached answers that
    // never expire on their own -- see `Caching.invalidateAll`.
    Caching.invalidateAll ()
    return List.concat rows
  }

/// Fold <param ops> into <param branch>'s projections, on a caller-provided connection, so the caller
/// controls the transaction: one outer transaction for a bulk fold, or the action's own. Each op must
/// already be stored on the branch; its row says its stamp and whether it is still in the draft.
let applyOpsOnConnection
  (conn : SqliteConnection)
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    let ctx = newCtx conn
    try
      let lookedUp (op : PT.PackageOp) () =
        task {
          let id = Hashing.computeOpRowId op
          let! row = opRowOf ctx branch id
          return (id, row)
        }
      let! rows = foldOps ctx branch (ops |> List.map (fun op -> (lookedUp op, op)))
      writeDependencies conn rows
    finally
      disposeCtx ctx
  }

/// Fold ops the caller read from the log into <param branch>'s projections, on its connection and
/// in its transaction, and return their dependency rows rather than writing them: a bulk fold writes
/// them all at once, in key order (`writeDependencies`).
let foldLoggedOnConnection
  (conn : SqliteConnection)
  (branch : PT.BranchId)
  (ops : List<LoggedOp>)
  : Task<List<DependencyRow>> =
  task {
    let ctx = newCtx conn
    try
      return!
        foldOps
          ctx
          branch
          (ops
           |> List.map (fun logged ->
             ((fun () -> Task.FromResult((logged.id, logged.row))), logged.op)))
    finally
      disposeCtx ctx
  }

/// `applyOpsOnConnection` on a connection of its own, in one transaction.
let applyOps (branch : PT.BranchId) (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    use tx = conn.BeginTransaction()
    do! applyOpsOnConnection conn branch ops
    tx.Commit()
  }


/// Record the callees of these `Add*` ops' items without folding anything else, on the caller's
/// connection. For an op the log already holds: the content is there, but this parse may have reached
/// its callees through names the first fold never saw (two names, one body), and
/// `writeDependencies` adds those. Every other kind of op is skipped.
let recordDependenciesOnConnection
  (conn : SqliteConnection)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
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
      | PT.PackageOp.AddTrait t when t.hash <> Hash "" ->
        let (Hash h) = t.hash
        Some(h, DE.extractFromTrait t)
      | PT.PackageOp.AddTraitImpl i when i.hash <> Hash "" ->
        let (Hash h) = i.hash
        Some(h, DE.extractFromImpl i)
      | PT.PackageOp.AddTest test when test.hash <> Hash "" ->
        let (Hash h) = test.hash
        Some(h, DE.extractFromTest test)
      | _ -> None)

  adds
  |> List.collect (fun (hash, deps) -> dependencyRows hash deps)
  |> writeDependencies conn
  Task.FromResult(())
