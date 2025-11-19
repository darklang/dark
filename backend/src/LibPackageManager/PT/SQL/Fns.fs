module internal LibPackageManager.PT.SQL.Fns

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


let find
  ((branchID, loc) : Option<PT.BranchID> * PT.PackageLocation)
  : Ply<Option<PT.FQFnName.Package>> =
  uply {
    let modulesStr = String.concat "." loc.modules

    return!
      Sql.query
        """
        SELECT item_id
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = 'fn'
          AND deprecated_at IS NULL
          AND (branch_id IS NULL OR branch_id = @branch_id)
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string loc.owner
          "modules", Sql.string modulesStr
          "name", Sql.string loc.name
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeRowOptionAsync (fun read -> read.uuid "item_id")
  }

let get (id : uuid) : Ply<Option<PT.PackageFn.PackageFn>> =
  uply {
    return!
      """
        SELECT pt_def
        FROM package_functions
        WHERE id = @id
        """
      |> Sql.query
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.bytes "pt_def")
      |> Task.map (Option.map BS.PT.PackageFn.deserialize)
  }


let getLocation
  ((branchID, id) : Option<PT.BranchID> * uuid)
  : Ply<Option<PT.PackageLocation>> =
  uply {
    return!
      // CLEANUP not totally confident this gets the right data...
      Sql.query
        """
        SELECT owner, modules, name
        FROM locations
        WHERE item_id = @item_id
          AND item_type = 'fn'
          AND deprecated_at IS NULL
          AND (branch_id IS NULL OR branch_id = @branch_id)
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "item_id", Sql.uuid id
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeRowOptionAsync (fun read ->
        let modulesStr = read.string "modules"
        { owner = read.string "owner"
          modules = modulesStr.Split('.') |> Array.toList
          name = read.string "name" })
  }
