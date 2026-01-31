module LibPackageManager.RuntimeTypes

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


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
        |> Task.map (Option.map (BinarySerialization.RT.PackageType.deserialize id))
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
        |> Task.map (Option.map (BinarySerialization.RT.PackageValue.deserialize id))
    }

  /// Find all value IDs that have the given ValueType (exact match)
  let findByValueType (vt : RT.ValueType) : Ply<List<uuid>> =
    uply {
      let vtBytes = BinarySerialization.RT.ValueType.serialize vt
      return!
        Sql.query
          """
          SELECT id
          FROM package_values
          WHERE value_type = @value_type
          """
        |> Sql.parameters [ "value_type", Sql.bytes vtBytes ]
        |> Sql.executeAsync (fun read ->
          read.string "id" |> System.Guid.Parse)
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
        |> Task.map (Option.map (BinarySerialization.RT.PackageFn.deserialize id))
    }
