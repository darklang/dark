module LibPackageManager.PackageOpPlayback

/// Applies PackageOps to the DB projection tables.
/// These tables (package_types, package_values, package_functions, locations) are projections
/// of the source-of-truth package_ops table.

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


/// Apply a single AddType op to the package_types table
let private applyAddType (typ : PT.PackageType.PackageType) : Task<unit> =
  task {
    let ptDef = BinarySerialization.PT.PackageType.serialize typ.id typ
    let rtDef =
      typ |> PT2RT.PackageType.toRT |> BinarySerialization.RT.PackageType.serialize typ.id

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
  }

/// Apply a single AddValue op to the package_values table
let private applyAddValue (value : PT.PackageValue.PackageValue) : Task<unit> =
  task {
    let ptDef = BinarySerialization.PT.PackageValue.serialize value.id value
    let rtDval =
      value
      |> PT2RT.PackageValue.toRT
      |> BinarySerialization.RT.PackageValue.serialize value.id

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
  }

/// Apply a single AddFn op to the package_functions table
let private applyAddFn (fn : PT.PackageFn.PackageFn) : Task<unit> =
  task {
    let ptDef = BinarySerialization.PT.PackageFn.serialize fn.id fn
    let rtInstrs =
      fn |> PT2RT.PackageFn.toRT |> BinarySerialization.RT.PackageFn.serialize fn.id

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
  }

/// Apply a Set*Name op to the locations table
let private applySetName
  (itemId : uuid)
  (location : PT.PackageLocation)
  (itemType : string)
  (branchId : Option<PT.BranchID>)
  : Task<unit> =
  task {
    let modulesStr = String.concat "." location.modules

    // First, deprecate any existing location for this item in this branch
    // (handles moves: old location gets deprecated, new location created)
    do!
      Sql.query
        """
        UPDATE locations
        SET deprecated_at = datetime('now')
        WHERE item_id = @item_id
          AND deprecated_at IS NULL
          AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
        """
      |> Sql.parameters
        [ "item_id", Sql.uuid itemId
          "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull) ]
      |> Sql.executeStatementAsync

    // Insert new location entry with unique location_id
    let locationId = System.Guid.NewGuid()
    do!
      Sql.query
        """
        INSERT INTO locations (location_id, item_id, branch_id, owner, modules, name, item_type)
        VALUES (@location_id, @item_id, @branch_id, @owner, @modules, @name, @item_type)
        """
      |> Sql.parameters
        [ "location_id", Sql.uuid locationId
          "item_id", Sql.uuid itemId
          "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull)
          "owner", Sql.string location.owner
          "modules", Sql.string modulesStr
          "name", Sql.string location.name
          "item_type", Sql.string itemType ]
      |> Sql.executeStatementAsync
  }


/// Apply a single PackageOp to the projection tables
/// branchId: None = main/merged, Some(id) = branch-specific
let applyOp (branchId : Option<PT.BranchID>) (op : PT.PackageOp) : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType typ
    | PT.PackageOp.AddValue value -> do! applyAddValue value
    | PT.PackageOp.AddFn fn -> do! applyAddFn fn
    | PT.PackageOp.SetTypeName(id, location) ->
      do! applySetName id location "type" branchId
    | PT.PackageOp.SetValueName(id, location) ->
      do! applySetName id location "value" branchId
    | PT.PackageOp.SetFnName(id, location) ->
      do! applySetName id location "fn" branchId
  }


/// Apply a list of PackageOps to the projection tables
/// This is used during package loading/reload
let applyOps (branchId : Option<PT.BranchID>) (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    for op in ops do
      do! applyOp branchId op
  }
