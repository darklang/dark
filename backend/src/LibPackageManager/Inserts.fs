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


let insertTypes (types : List<PT.PackageType.PackageType>) : Task<unit> =
  task {
    if List.isEmpty types then return ()

    types
    |> List.map (fun typ ->
      let sql =
        @"INSERT INTO package_types_v0
            (id, owner, modules, name, pt_def, rt_def)
          VALUES
            (@id, @owner, @modules, @name, @pt_def, @rt_def)"

      let ptDef = BinarySerialization.PT.PackageType.serialize typ.id typ
      let rtDef =
        typ
        |> PT2RT.PackageType.toRT
        |> BinarySerialization.RT.PackageType.serialize typ.id

      let parameters =
        [ "id", Sql.uuid typ.id
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
            (id, owner, modules, name, pt_def, rt_dval)
          VALUES
            (@id, @owner, @modules, @name, @pt_def, @rt_dval)"

      let dval = PT2RT.PackageValue.toRT v

      let ptBits = BinarySerialization.PT.PackageValue.serialize v.id v
      let rtBits = dval |> BinarySerialization.RT.PackageValue.serialize v.id


      let parameters =
        [ "id", Sql.uuid v.id
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
            (id, owner, modules, name, pt_def, rt_instrs)
          VALUES
            (@id, @owner, @modules, @name, @pt_def, @rt_instrs)"

      let ptDef = BinarySerialization.PT.PackageFn.serialize fn.id fn
      let rtInstrs =
        fn
        |> PT2RT.PackageFn.toRT
        |> BinarySerialization.RT.PackageFn.serialize fn.id

      let parameters =
        [ "id", Sql.uuid fn.id
          "owner", Sql.string fn.name.owner
          "modules", Sql.string (String.concat "." fn.name.modules)
          "name", Sql.string fn.name.name
          "pt_def", Sql.bytes ptDef
          "rt_instrs", Sql.bytes rtInstrs ]

      (sql, [ parameters ]))
    |> Sql.executeTransactionSync
    |> ignore<List<int>>
  }
