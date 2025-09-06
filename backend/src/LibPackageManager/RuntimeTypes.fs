module LibPackageManager.RuntimeTypes

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


module Type =
  let get (hash : Hash) : Ply<Option<RT.PackageType.PackageType>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT hash, rt_def
          FROM package_types_v0
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read ->
          let hash = read.string "hash"
          let rtDef = read.bytes "rt_def"
          BinarySerialization.RT.PackageType.deserialize hash rtDef)
    }


module Value =
  let get (hash : Hash) : Ply<Option<RT.PackageValue.PackageValue>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT hash, rt_dval
          FROM package_values_v0
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read ->
          let hash = read.string "hash"
          let rtDval = read.bytes "rt_dval"
          BinarySerialization.RT.PackageValue.deserialize hash rtDval)
    }


module Fn =
  let get (hash : Hash) : Ply<Option<RT.PackageFn.PackageFn>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT hash, rt_instrs
          FROM package_functions_v0
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read ->
          let hash = read.string "hash"
          let rtInstrs = read.bytes "rt_instrs"
          BinarySerialization.RT.PackageFn.deserialize hash rtInstrs)
    }
