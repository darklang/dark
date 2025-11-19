module LibPackageManager.RT.SQL

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module BS = LibSerialization.Binary.Serialization


module Type =
  let get (id : uuid) : Ply<Option<RT.PackageType.PackageType>> =
    uply {
      return!
        Sql.query
          """
          SELECT rt_def
          FROM package_types
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_def")
        |> Task.map (Option.map BS.RT.PackageType.deserialize)
    }


module Value =
  let get (id : uuid) : Ply<Option<RT.PackageValue.PackageValue>> =
    uply {
      return!
        Sql.query
          """
          SELECT rt_dval
          FROM package_values
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_dval")
        |> Task.map (Option.map BS.RT.PackageValue.deserialize)
    }


module Fn =
  let get (id : uuid) : Ply<Option<RT.PackageFn.PackageFn>> =
    uply {
      return!
        Sql.query
          """
          SELECT rt_instrs
          FROM package_functions
          WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_instrs")
        |> Task.map (Option.map BS.RT.PackageFn.deserialize)
    }
