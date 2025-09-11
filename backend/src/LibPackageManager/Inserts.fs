module LibPackageManager.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization
module Hashing = LibPackageManager.Hashing


let insertTypes (types : List<PT.PackageType.PackageType>) : Task<unit> =
  task {
    if List.isEmpty types then return ()

    types
    |> List.map (fun typ ->
      let sql =
        @"INSERT INTO package_types_v0
            (hash, owner, modules, name, pt_def, rt_def)
          VALUES
            (@hash, @owner, @modules, @name, @pt_def, @rt_def)"

      let (Hash hashStr) = Hashing.hashPackageType typ
      let ptDef = BinarySerialization.PT.PackageType.serialize hashStr typ
      let rtDef =
        typ
        |> PT2RT.PackageType.toRT
        |> BinarySerialization.RT.PackageType.serialize hashStr

      let parameters =
        [ "hash", Sql.string hashStr
          "owner", Sql.string typ.name.owner
          "modules", Sql.string (String.concat "." typ.name.modules)
          "name", Sql.string typ.name.name
          "pt_def", Sql.bytes ptDef
          "rt_def", Sql.bytes rtDef ]

      (sql, [ parameters ]))
    |> Sql.executeTransactionSync
    |> ignore<List<int>>
  }


let insertValues (values : List<PT.PackageValue.PackageValue>) : Task<unit> =
  task {
    if List.isEmpty values then return ()

    values
    |> List.map (fun v ->
      let sql =
        @"INSERT INTO package_values_v0
            (hash, owner, modules, name, pt_def, rt_dval)
          VALUES
            (@hash, @owner, @modules, @name, @pt_def, @rt_dval)"

      let (Hash hashStr) = Hashing.hashPackageValue v
      let dval = PT2RT.PackageValue.toRT v

      let ptBits = BinarySerialization.PT.PackageValue.serialize hashStr v
      let rtBits = dval |> BinarySerialization.RT.PackageValue.serialize hashStr

      let parameters =
        [ "hash", Sql.string hashStr
          "owner", Sql.string v.name.owner
          "modules", Sql.string (String.concat "." v.name.modules)
          "name", Sql.string v.name.name
          "pt_def", Sql.bytes ptBits
          "rt_dval", Sql.bytes rtBits ]

      (sql, [ parameters ]))
    |> Sql.executeTransactionSync
    |> ignore<List<int>>
  }

let insertFns (fns : List<PT.PackageFn.PackageFn>) : Task<unit> =
  task {
    if List.isEmpty fns then return ()

    fns
    |> List.map (fun fn ->
      let sql =
        @"INSERT INTO package_functions_v0
            (hash, owner, modules, name, pt_def, rt_instrs)
          VALUES
            (@hash, @owner, @modules, @name, @pt_def, @rt_instrs)"

      let (Hash hashStr) = Hashing.hashPackageFn fn
      let ptDef = BinarySerialization.PT.PackageFn.serialize hashStr fn
      let rtInstrs =
        fn
        |> PT2RT.PackageFn.toRT
        |> BinarySerialization.RT.PackageFn.serialize hashStr

      let parameters =
        [ "hash", Sql.string hashStr
          "owner", Sql.string fn.name.owner
          "modules", Sql.string (String.concat "." fn.name.modules)
          "name", Sql.string fn.name.name
          "pt_def", Sql.bytes ptDef
          "rt_instrs", Sql.bytes rtInstrs ]

      (sql, [ parameters ]))
    |> Sql.executeTransactionSync
    |> ignore<List<int>>
  }
