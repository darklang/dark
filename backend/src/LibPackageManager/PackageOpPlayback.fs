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
let private applyAddValue (value : PT.PackageValue.PackageValue) : Task<unit> =
  task {
    let ptDef = BS.PT.PackageValue.serialize value.id value
    let rtDval =
      value |> PT2RT.PackageValue.toRT |> BS.RT.PackageValue.serialize value.id

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_values (id, pt_def, rt_dval)
        VALUES (@id, @pt_def, @rt_dval)
        """
      |> Sql.parameters
        [ "id", Sql.uuid value.id
          "pt_def", Sql.bytes ptDef
          "rt_dval", Sql.bytes rtDval ]
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

/// Apply a Set*Name op to the locations table
/// commitId = None means WIP, Some id means committed
let private applySetName
  (commitId : Option<uuid>)
  (itemId : uuid)
  (location : PT.PackageLocation)
  (itemType : string)
  : Task<unit> =
  task {
    let modulesStr = String.concat "." location.modules

    // First, deprecate any existing location for this item
    // (handles moves: old location gets deprecated, new location created)
    do!
      Sql.query
        """
        UPDATE locations
        SET deprecated_at = datetime('now')
        WHERE item_id = @item_id
          AND deprecated_at IS NULL
        """
      |> Sql.parameters [ "item_id", Sql.uuid itemId ]
      |> Sql.executeStatementAsync

    // Insert new location entry with unique location_id
    let locationId = System.Guid.NewGuid()
    let isWip = Option.isNone commitId
    do!
      Sql.query
        """
        INSERT INTO locations (location_id, item_id, owner, modules, name, item_type, is_wip, commit_id)
        VALUES (@location_id, @item_id, @owner, @modules, @name, @item_type, @is_wip, @commit_id)
        """
      |> Sql.parameters
        [ "location_id", Sql.uuid locationId
          "item_id", Sql.uuid itemId
          "owner", Sql.string location.owner
          "modules", Sql.string modulesStr
          "name", Sql.string location.name
          "item_type", Sql.string itemType
          "is_wip", Sql.int (if isWip then 1 else 0)
          "commit_id", Sql.uuidOrNone commitId ]
      |> Sql.executeStatementAsync
  }


/// Apply a single PackageOp to the projection tables
/// commitId = None means WIP, Some id means committed
let applyOp (commitId : Option<uuid>) (op : PT.PackageOp) : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType typ
    | PT.PackageOp.AddValue value -> do! applyAddValue value
    | PT.PackageOp.AddFn fn -> do! applyAddFn fn
    | PT.PackageOp.SetTypeName(id, loc) -> do! applySetName commitId id loc "type"
    | PT.PackageOp.SetValueName(id, loc) -> do! applySetName commitId id loc "value"
    | PT.PackageOp.SetFnName(id, loc) -> do! applySetName commitId id loc "fn"
  }


/// Apply a list of PackageOps to the projection tables
/// commitId = None means WIP, Some id means committed
let applyOps (commitId : Option<uuid>) (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    for op in ops do
      do! applyOp commitId op
  }
