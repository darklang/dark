module LibPackageManager.RuntimeTypes

open Prelude
open LibExecution.RuntimeTypes

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module BS = LibSerialization.Binary.Serialization


module Type =
  let get (id : ContentHash) : Ply<Option<RT.PackageType.PackageType>> =
    uply {
      let (ContentHash idStr) = id
      return!
        Sql.query
          """
          SELECT rt_def
          FROM package_types
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string idStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_def")
        |> Task.map (Option.map (BS.RT.PackageType.deserialize id))
    }


module Value =
  let get (id : ContentHash) : Ply<Option<RT.PackageValue.PackageValue>> =
    uply {
      let (ContentHash idStr) = id
      return!
        Sql.query
          """
          SELECT rt_dval
          FROM package_values
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string idStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_dval")
        |> Task.map (Option.map (BS.RT.PackageValue.deserialize id))
    }

  /// Find all value hashes that have the given ValueType (exact match)
  let findByValueType (vt : RT.ValueType) : Ply<List<ContentHash>> =
    uply {
      let vtBytes = BS.RT.ValueType.serialize vt
      return!
        Sql.query
          """
          SELECT hash
          FROM package_values
          WHERE value_type = @value_type
          """
        |> Sql.parameters [ "value_type", Sql.bytes vtBytes ]
        |> Sql.executeAsync (fun read -> ContentHash(read.string "hash"))
    }


module Fn =
  let get (id : ContentHash) : Ply<Option<RT.PackageFn.PackageFn>> =
    uply {
      let (ContentHash idStr) = id
      return!
        Sql.query
          """
          SELECT rt_instrs
          FROM package_functions
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string idStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_instrs")
        |> Task.map (Option.map (BS.RT.PackageFn.deserialize id))
    }
