/// Applies PackageOps to the DB projection tables.
/// These tables (package_types, package_values, package_functions, locations) are projections
/// of the source-of-truth package_ops table.
module LibPackageManager.PackageOpPlayback


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BS = LibSerialization.Binary.Serialization
module DE = LibPackageManager.DependencyExtractor
open LibSerialization.Hashing


/// Update dependencies for an item atomically.
/// Clears existing dependencies and stores new ones in a single operation.
let private updateDependencies
  (itemHash : string)
  (deps : List<DE.Dependency>)
  : Task<unit> =
  task {
    if List.isEmpty deps then
      // Just delete, no inserts needed
      do!
        Sql.query "DELETE FROM package_dependencies WHERE item_hash = @item_hash"
        |> Sql.parameters [ "item_hash", Sql.string itemHash ]
        |> Sql.executeStatementAsync
    else
      // Build a single SQL script: DELETE + batched INSERTs
      // SQLite executes multi-statement scripts atomically within one call
      let insertValues =
        deps
        |> List.mapi (fun i _ -> $"(@item_hash, @depends_on_{i})")
        |> String.concat ", "

      let insertParams =
        deps |> List.mapi (fun i (Hash dep) -> $"depends_on_{i}", Sql.string dep)

      let sql =
        $"DELETE FROM package_dependencies WHERE item_hash = @item_hash; "
        + $"INSERT OR IGNORE INTO package_dependencies (item_hash, depends_on_hash) VALUES {insertValues}"

      do!
        Sql.query sql
        |> Sql.parameters ([ "item_hash", Sql.string itemHash ] @ insertParams)
        |> Sql.executeStatementAsync
  }


/// Apply a single AddType op to the package_types table
let private applyAddType (typ : PT.PackageType.PackageType) : Task<unit> =
  task {
    // Use the hash already set on the item (computed by LoadPackagesFromDisk
    // or Propagation with SCC awareness). Only recompute if hash is empty.
    let hash =
      match typ.hash with
      | Hash "" -> Hashing.computeTypeHash Hashing.Normal typ
      | h -> h
    let typ = { typ with hash = hash }
    let (Hash hash) = hash

    let ptDef = BS.PT.PackageType.serialize hash typ
    let rtDef = typ |> PT2RT.PackageType.toRT |> BS.RT.PackageType.serialize hash

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_types (hash, pt_def, rt_def)
        VALUES (@hash, @pt_def, @rt_def)
        """
      |> Sql.parameters
        [ "hash", Sql.string hash
          "pt_def", Sql.bytes ptDef
          "rt_def", Sql.bytes rtDef ]
      |> Sql.executeStatementAsync

    // Extract and store dependency references atomically
    let refs = DE.extractFromType typ
    do! updateDependencies hash refs
  }

/// Apply a single AddValue op to the package_values table
/// Note: rt_dval and value_type are stored as NULL here.
/// They are evaluated in Phase 3 by LocalExec.evaluateAllValues after all ops are applied.
let private applyAddValue (value : PT.PackageValue.PackageValue) : Task<unit> =
  task {
    let hash =
      match value.hash with
      | Hash "" -> Hashing.computeValueHash Hashing.Normal value
      | h -> h
    let value = { value with hash = hash }
    let (Hash hash) = hash

    let ptDef = BS.PT.PackageValue.serialize hash value

    // Store NULL for rt_dval and value_type - they're populated by evaluateAllValues
    // after all packages are loaded (so cross-package references resolve correctly).
    // Use ON CONFLICT(hash) since values are content-addressed.
    do!
      Sql.query
        """
        INSERT INTO package_values (hash, pt_def, rt_dval, value_type)
        VALUES (@hash, @pt_def, NULL, NULL)
        ON CONFLICT(hash) DO UPDATE SET
          pt_def = excluded.pt_def
        """
      |> Sql.parameters [ "hash", Sql.string hash; "pt_def", Sql.bytes ptDef ]
      |> Sql.executeStatementAsync

    // Extract and store dependency references atomically
    // Use hash string for consistency with locations.item_hash
    let refs = DE.extractFromValue value
    do! updateDependencies hash refs
  }

/// Apply a single AddFn op to the package_functions table
let private applyAddFn (fn : PT.PackageFn.PackageFn) : Task<unit> =
  task {
    let hash =
      match fn.hash with
      | Hash "" -> Hashing.computeFnHash Hashing.Normal fn
      | h -> h
    let fn = { fn with hash = hash }
    let (Hash hash) = hash

    let ptDef = BS.PT.PackageFn.serialize hash fn
    let rtInstrs = fn |> PT2RT.PackageFn.toRT |> BS.RT.PackageFn.serialize hash

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_functions (hash, pt_def, rt_instrs)
        VALUES (@hash, @pt_def, @rt_instrs)
        """
      |> Sql.parameters
        [ "hash", Sql.string hash
          "pt_def", Sql.bytes ptDef
          "rt_instrs", Sql.bytes rtInstrs ]
      |> Sql.executeStatementAsync

    // Extract and store dependency references atomically
    let refs = DE.extractFromFn fn
    do! updateDependencies hash refs
  }

/// Apply a Set*Name op to the locations table atomically.
/// branchId = branch context, commitHash = None means WIP, Some id means committed
/// isRename = true when this SetName is a standalone rename (not paired with Add*),
///   meaning old locations for the same hash should be deprecated.
let private applySetName
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

    let commitHashParam =
      match commitHash with
      | Some s -> Sql.string s
      | None -> Sql.dbnull

    // 1. Deprecate any existing location at the target path (handles updates)
    let mutable statements =
      [ ("""
         UPDATE locations
         SET unlisted_at = datetime('now')
         WHERE owner = @owner
           AND modules = @modules
           AND name = @name
           AND item_type = @item_type
           AND unlisted_at IS NULL
           AND branch_id = @branch_id
         """,
         [ [ "owner", Sql.string location.owner
             "modules", Sql.string modulesStr
             "name", Sql.string location.name
             "item_type", Sql.string itemTypeStr
             "branch_id", Sql.uuid branchId ] ]) ]

    // 2. If this is a rename (standalone SetName, not paired with Add*),
    //    also deprecate old locations pointing to the same hash.
    //    We do NOT do this for Add+SetName pairs because multiple items can
    //    legitimately share the same hash (e.g. Int8.ParseError and
    //    Int16.ParseError have identical definitions).
    if isRename then
      statements <-
        statements
        @ [ ("""
             UPDATE locations
             SET unlisted_at = datetime('now')
             WHERE item_hash = @item_hash
               AND branch_id = @branch_id
               AND unlisted_at IS NULL
             """,
             [ [ "item_hash", Sql.string itemHashStr
                 "branch_id", Sql.uuid branchId ] ]) ]

    // 3. Insert new location entry
    statements <-
      statements
      @ [ ("""
           INSERT INTO locations (location_id, item_hash, owner, modules, name, item_type, branch_id, commit_hash)
           VALUES (@location_id, @item_hash, @owner, @modules, @name, @item_type, @branch_id, @commit_hash)
           """,
           [ [ "location_id", Sql.uuid locationId
               "item_hash", Sql.string itemHashStr
               "owner", Sql.string location.owner
               "modules", Sql.string modulesStr
               "name", Sql.string location.name
               "item_type", Sql.string itemTypeStr
               "branch_id", Sql.uuid branchId
               "commit_hash", commitHashParam ] ]) ]

    let _ = Sql.executeTransactionSync statements
    ()
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


/// Apply a Deprecate op to the deprecations projection table.
/// Supersedes any prior un-superseded row for (branch, item_hash, item_kind).
let private applyDeprecate
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

    let commitHashParam =
      match commitHash with
      | Some s -> Sql.string s
      | None -> Sql.dbnull

    let statements =
      [ ("""
         UPDATE deprecations
         SET unlisted_at = datetime('now')
         WHERE branch_id = @branch_id
           AND item_hash = @item_hash
           AND item_kind = @item_kind
           AND unlisted_at IS NULL
         """,
         [ [ "branch_id", Sql.uuid branchId
             "item_hash", Sql.string itemHashStr
             "item_kind", Sql.string itemKindStr ] ])
        ("""
         INSERT INTO deprecations
           (deprecation_id, branch_id, commit_hash, item_hash, item_kind, state, annotation_blob)
         VALUES
           (@deprecation_id, @branch_id, @commit_hash, @item_hash, @item_kind, 'deprecated', @blob)
         """,
         [ [ "deprecation_id", Sql.uuid deprecationId
             "branch_id", Sql.uuid branchId
             "commit_hash", commitHashParam
             "item_hash", Sql.string itemHashStr
             "item_kind", Sql.string itemKindStr
             "blob", Sql.bytes blob ] ]) ]

    let _ = Sql.executeTransactionSync statements
    ()
  }


/// Apply an Undeprecate op to the deprecations projection table.
/// Records an `undeprecated`-state row that supersedes any prior row for the
/// same (branch, item_hash, item_kind). This is how child branches override
/// ancestor-branch deprecations.
let private applyUndeprecate
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (target : PT.Reference)
  : Task<unit> =
  task {
    let (Hash itemHashStr) = target.hash
    let itemKindStr = target.kind.toString ()
    let deprecationId = System.Guid.NewGuid()

    let commitHashParam =
      match commitHash with
      | Some s -> Sql.string s
      | None -> Sql.dbnull

    let statements =
      [ ("""
         UPDATE deprecations
         SET unlisted_at = datetime('now')
         WHERE branch_id = @branch_id
           AND item_hash = @item_hash
           AND item_kind = @item_kind
           AND unlisted_at IS NULL
         """,
         [ [ "branch_id", Sql.uuid branchId
             "item_hash", Sql.string itemHashStr
             "item_kind", Sql.string itemKindStr ] ])
        ("""
         INSERT INTO deprecations
           (deprecation_id, branch_id, commit_hash, item_hash, item_kind, state, annotation_blob)
         VALUES
           (@deprecation_id, @branch_id, @commit_hash, @item_hash, @item_kind, 'undeprecated', NULL)
         """,
         [ [ "deprecation_id", Sql.uuid deprecationId
             "branch_id", Sql.uuid branchId
             "commit_hash", commitHashParam
             "item_hash", Sql.string itemHashStr
             "item_kind", Sql.string itemKindStr ] ]) ]

    let _ = Sql.executeTransactionSync statements
    ()
  }


/// Apply a single PackageOp to the projection tables
/// branchId = branch context, commitHash = None means WIP, Some id means committed
/// addedHashes = hashes of items added by Add* ops earlier in this batch
///   (used to distinguish "add + name" from "rename")
let private applyOp
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (addedHashes : Set<Hash>)
  (op : PT.PackageOp)
  : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType typ
    | PT.PackageOp.AddValue value -> do! applyAddValue value
    | PT.PackageOp.AddFn fn -> do! applyAddFn fn
    | PT.PackageOp.SetName(loc, target) ->
      let isRename = not (Set.contains target.hash addedHashes)
      do! applySetName branchId commitHash isRename target.hash loc target.kind
    | PT.PackageOp.Deprecate(target, kind, message) ->
      do! applyDeprecate branchId commitHash target kind message
    | PT.PackageOp.Undeprecate target ->
      do! applyUndeprecate branchId commitHash target
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
      // Build all SQL statements for atomic execution
      let mutable statements = []
      let sourceItemKind = restoredSourceRef.kind

      // For each reverted repoint: unlist toRef, un-unlist fromRef
      // Skip repoints for the source item — those are handled by the dedicated
      // source-handling block below (avoids redundant double-toggle in mutual recursion)
      let dependentRepoints =
        revertedRepoints
        |> List.filter (fun r ->
          r.location <> sourceLocation || r.toRef.kind <> sourceItemKind)

      for repoint in dependentRepoints do
        let (Hash toHashStr) = repoint.toRef.hash
        let (Hash fromHashStr) = repoint.fromRef.hash
        statements <-
          statements
          @ [ ("""
               UPDATE locations
               SET unlisted_at = datetime('now')
               WHERE item_hash = @item_hash
                 AND branch_id = @branch_id
                 AND unlisted_at IS NULL
               """,
               [ [ "item_hash", Sql.string toHashStr
                   "branch_id", Sql.uuid branchId ] ])

              ("""
               UPDATE locations
               SET unlisted_at = NULL
               WHERE location_id = (
                 SELECT location_id FROM locations
                 WHERE item_hash = @item_hash
                   AND branch_id = @branch_id
                   AND unlisted_at IS NOT NULL
                 ORDER BY unlisted_at DESC
                 LIMIT 1
               )
               """,
               [ [ "item_hash", Sql.string fromHashStr
                   "branch_id", Sql.uuid branchId ] ]) ]

      // Undo source: unlist WIP location, un-unlist committed location
      let modulesStr = String.concat "." sourceLocation.modules
      let itemTypeStr = sourceItemKind.toString ()
      let (Hash restoredSourceHashStr) = restoredSourceRef.hash

      statements <-
        statements
        @ [ ("""
             UPDATE locations
             SET unlisted_at = datetime('now')
             WHERE owner = @owner
               AND modules = @modules
               AND name = @name
               AND item_type = @item_type
               AND branch_id = @branch_id
               AND unlisted_at IS NULL
               AND commit_hash IS NULL
             """,
             [ [ "owner", Sql.string sourceLocation.owner
                 "modules", Sql.string modulesStr
                 "name", Sql.string sourceLocation.name
                 "item_type", Sql.string itemTypeStr
                 "branch_id", Sql.uuid branchId ] ])

            ("""
             UPDATE locations
             SET unlisted_at = NULL
             WHERE location_id = (
               SELECT location_id FROM locations
               WHERE item_hash = @item_hash
                 AND branch_id = @branch_id
                 AND unlisted_at IS NOT NULL
               ORDER BY unlisted_at DESC
               LIMIT 1
             )
             """,
             [ [ "item_hash", Sql.string restoredSourceHashStr
                 "branch_id", Sql.uuid branchId ] ]) ]

      let _ = Sql.executeTransactionSync statements
      ()
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


/// Apply a list of PackageOps to the projection tables
/// branchId = branch context, commitHash = None means WIP, Some id means committed
let applyOps
  (branchId : PT.BranchId)
  (commitHash : Option<string>)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    let addedHashes = collectAddedHashes ops
    for op in ops do
      do! applyOp branchId commitHash addedHashes op
  }
