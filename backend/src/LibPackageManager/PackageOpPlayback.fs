/// Applies PackageOps to the DB projection tables.
/// These tables (package_types, package_values, package_functions, locations) are projections
/// of the source-of-truth package_ops table.
module LibPackageManager.PackageOpPlayback


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BS = LibBinarySerialization.BinarySerialization
module DE = LibPackageManager.DependencyExtractor


/// Update dependencies for an item atomically.
/// Clears existing dependencies and stores new ones in a single operation.
let private updateDependencies
  (itemId : uuid)
  (deps : List<DE.Dependency>)
  : Task<unit> =
  task {
    if List.isEmpty deps then
      // Just delete, no inserts needed
      do!
        Sql.query "DELETE FROM package_dependencies WHERE item_id = @item_id"
        |> Sql.parameters [ "item_id", Sql.uuid itemId ]
        |> Sql.executeStatementAsync
    else
      // Build a single SQL script: DELETE + batched INSERTs
      // SQLite executes multi-statement scripts atomically within one call
      let insertValues =
        deps
        |> List.mapi (fun i _ -> $"(@item_id, @depends_on_{i})")
        |> String.concat ", "

      let insertParams =
        deps |> List.mapi (fun i dep -> $"depends_on_{i}", Sql.uuid dep)

      let sql =
        $"DELETE FROM package_dependencies WHERE item_id = @item_id; "
        + $"INSERT OR IGNORE INTO package_dependencies (item_id, depends_on_id) VALUES {insertValues}"

      do!
        Sql.query sql
        |> Sql.parameters ([ "item_id", Sql.uuid itemId ] @ insertParams)
        |> Sql.executeStatementAsync
  }


/// Apply a single AddType op to the package_types table
let private applyAddType (typ : PT.PackageType.PackageType) : Task<unit> =
  task {
    let ptDef = BS.PT.PackageType.serialize typ.id typ
    let rtDef = typ |> PT2RT.PackageType.toRT |> BS.RT.PackageType.serialize typ.id

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_types (id, pt_def, rt_def)
        VALUES (@id, @pt_def, @rt_def)
        """
      |> Sql.parameters
        [ "id", Sql.uuid typ.id
          "pt_def", Sql.bytes ptDef
          "rt_def", Sql.bytes rtDef ]
      |> Sql.executeStatementAsync

    // Extract and store dependency references atomically
    let refs = DE.extractFromType typ
    do! updateDependencies typ.id refs
  }

/// Apply a single AddValue op to the package_values table
/// Note: rt_dval and value_type are stored as NULL here.
/// They are evaluated in Phase 3 by LocalExec.evaluateAllValues after all ops are applied.
let private applyAddValue (value : PT.PackageValue.PackageValue) : Task<unit> =
  task {
    let ptDef = BS.PT.PackageValue.serialize value.id value

    // Store NULL for rt_dval and value_type - they're populated by evaluateAllValues
    // after all packages are loaded (so cross-package references resolve correctly)
    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_values (id, pt_def, rt_dval, value_type)
        VALUES (@id, @pt_def, NULL, NULL)
        """
      |> Sql.parameters [ "id", Sql.uuid value.id; "pt_def", Sql.bytes ptDef ]
      |> Sql.executeStatementAsync

    // Extract and store dependency references atomically
    let refs = DE.extractFromValue value
    do! updateDependencies value.id refs
  }

/// Apply a single AddFn op to the package_functions table
let private applyAddFn (fn : PT.PackageFn.PackageFn) : Task<unit> =
  task {
    let ptDef = BS.PT.PackageFn.serialize fn.id fn
    let rtInstrs = fn |> PT2RT.PackageFn.toRT |> BS.RT.PackageFn.serialize fn.id

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_functions (id, pt_def, rt_instrs)
        VALUES (@id, @pt_def, @rt_instrs)
        """
      |> Sql.parameters
        [ "id", Sql.uuid fn.id
          "pt_def", Sql.bytes ptDef
          "rt_instrs", Sql.bytes rtInstrs ]
      |> Sql.executeStatementAsync

    // Extract and store dependency references atomically
    let refs = DE.extractFromFn fn
    do! updateDependencies fn.id refs
  }

/// Apply a Set*Name op to the locations table atomically.
/// branchId = branch context, commitId = None means WIP, Some id means committed
let private applySetName
  (branchId : PT.BranchId)
  (commitId : Option<uuid>)
  (itemId : uuid)
  (location : PT.PackageLocation)
  (itemKind : PT.ItemKind)
  : Task<unit> =
  task {
    let modulesStr = String.concat "." location.modules
    let itemTypeStr = itemKind.toString ()
    let locationId = System.Guid.NewGuid()

    // Execute all three operations atomically:
    // 1. Deprecate any existing location for this item (handles moves)
    // 2. Deprecate any existing location at same path (handles updates)
    // 3. Insert new location entry
    let statements =
      [ ("""
         UPDATE locations
         SET deprecated_at = datetime('now')
         WHERE item_id = @item_id
           AND branch_id = @branch_id
           AND deprecated_at IS NULL
         """,
         [ [ "item_id", Sql.uuid itemId; "branch_id", Sql.uuid branchId ] ])

        ("""
         UPDATE locations
         SET deprecated_at = datetime('now')
         WHERE owner = @owner
           AND modules = @modules
           AND name = @name
           AND item_type = @item_type
           AND deprecated_at IS NULL
           AND branch_id = @branch_id
         """,
         [ [ "owner", Sql.string location.owner
             "modules", Sql.string modulesStr
             "name", Sql.string location.name
             "item_type", Sql.string itemTypeStr
             "branch_id", Sql.uuid branchId ] ])

        ("""
         INSERT INTO locations (location_id, item_id, owner, modules, name, item_type, branch_id, commit_id)
         VALUES (@location_id, @item_id, @owner, @modules, @name, @item_type, @branch_id, @commit_id)
         """,
         [ [ "location_id", Sql.uuid locationId
             "item_id", Sql.uuid itemId
             "owner", Sql.string location.owner
             "modules", Sql.string modulesStr
             "name", Sql.string location.name
             "item_type", Sql.string itemTypeStr
             "branch_id", Sql.uuid branchId
             "commit_id", Sql.uuidOrNone commitId ] ]) ]

    let _ = Sql.executeTransactionSync statements
    ()
  }


/// Apply a single PackageOp to the projection tables
/// branchId = branch context, commitId = None means WIP, Some id means committed
let applyOp
  (branchId : PT.BranchId)
  (commitId : Option<uuid>)
  (op : PT.PackageOp)
  : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType typ
    | PT.PackageOp.AddValue value -> do! applyAddValue value
    | PT.PackageOp.AddFn fn -> do! applyAddFn fn
    | PT.PackageOp.SetTypeName(id, loc) ->
      do! applySetName branchId commitId id loc PT.ItemKind.Type
    | PT.PackageOp.SetValueName(id, loc) ->
      do! applySetName branchId commitId id loc PT.ItemKind.Value
    | PT.PackageOp.SetFnName(id, loc) ->
      do! applySetName branchId commitId id loc PT.ItemKind.Fn
    | PT.PackageOp.PropagateUpdate _ ->
      // Location changes are already handled by the individual SetFnName/SetTypeName/
      // SetValueName ops that accompany this op in the propagation batch.
      // Applying them here too would create duplicate location entries.
      ()
    | PT.PackageOp.RevertPropagation(_,
                                     _,
                                     sourceLocation,
                                     sourceItemKind,
                                     restoredSourceUUID,
                                     revertedRepoints) ->
      // Build all SQL statements for atomic execution
      let mutable statements = []

      // For each reverted repoint: deprecate toUUID, un-deprecate fromUUID
      // Skip repoints for the source item — those are handled by the dedicated
      // source-handling block below (avoids redundant double-toggle in mutual recursion)
      let dependentRepoints =
        revertedRepoints
        |> List.filter (fun r ->
          r.location <> sourceLocation || r.itemKind <> sourceItemKind)

      for repoint in dependentRepoints do
        statements <-
          statements
          @ [ ("""
               UPDATE locations
               SET deprecated_at = datetime('now')
               WHERE item_id = @item_id
                 AND branch_id = @branch_id
                 AND deprecated_at IS NULL
               """,
               [ [ "item_id", Sql.uuid repoint.toUUID
                   "branch_id", Sql.uuid branchId ] ])

              ("""
               UPDATE locations
               SET deprecated_at = NULL
               WHERE location_id = (
                 SELECT location_id FROM locations
                 WHERE item_id = @item_id
                   AND branch_id = @branch_id
                   AND deprecated_at IS NOT NULL
                 ORDER BY deprecated_at DESC
                 LIMIT 1
               )
               """,
               [ [ "item_id", Sql.uuid repoint.fromUUID
                   "branch_id", Sql.uuid branchId ] ]) ]

      // Undo source: deprecate WIP location, un-deprecate committed location
      let modulesStr = String.concat "." sourceLocation.modules
      let itemTypeStr = sourceItemKind.toString ()

      statements <-
        statements
        @ [ ("""
             UPDATE locations
             SET deprecated_at = datetime('now')
             WHERE owner = @owner
               AND modules = @modules
               AND name = @name
               AND item_type = @item_type
               AND branch_id = @branch_id
               AND deprecated_at IS NULL
               AND commit_id IS NULL
             """,
             [ [ "owner", Sql.string sourceLocation.owner
                 "modules", Sql.string modulesStr
                 "name", Sql.string sourceLocation.name
                 "item_type", Sql.string itemTypeStr
                 "branch_id", Sql.uuid branchId ] ])

            ("""
             UPDATE locations
             SET deprecated_at = NULL
             WHERE location_id = (
               SELECT location_id FROM locations
               WHERE item_id = @item_id
                 AND branch_id = @branch_id
                 AND deprecated_at IS NOT NULL
               ORDER BY deprecated_at DESC
               LIMIT 1
             )
             """,
             [ [ "item_id", Sql.uuid restoredSourceUUID
                 "branch_id", Sql.uuid branchId ] ]) ]

      let _ = Sql.executeTransactionSync statements
      ()
  }


/// Apply a list of PackageOps to the projection tables
/// branchId = branch context, commitId = None means WIP, Some id means committed
let applyOps
  (branchId : PT.BranchId)
  (commitId : Option<uuid>)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    for op in ops do
      do! applyOp branchId commitId op
  }
