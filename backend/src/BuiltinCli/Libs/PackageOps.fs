module BuiltinCli.Libs.PackageOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization
module Builtin = LibExecution.Builtin

open Builtin.Shortcuts
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


let fns : List<BuiltInFn> =
  [ { name = fn "cliPackageOpsCommit" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TVariable "packageOp")) "" ]
      returnType = TUnit
      description =
        "Commit package ops to the database. branchId None = main branch, Some = specific branch"
      fn =
        (function
        | _, _, _, [ branchIdOpt; DList(_vtTODO, ops) ] ->
          uply {
            let branchId =
              match branchIdOpt with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            // Convert each op from Dval to PT.PackageOp
            let ptOps =
              ops
              |> List.choose (fun opDval ->
                PT2DT.PackageOp.fromDT opDval)

            // Serialize and insert each op
            for op in ptOps do
              let opId = System.Guid.NewGuid()
              let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

              do!
                Sql.query
                  """
                  INSERT INTO package_ops (id, branch_id, op_blob, created_at)
                  VALUES (@id, @branch_id, @op_blob, @created_at)
                  """
                |> Sql.parameters
                  [ "id", Sql.uuid opId
                    "branch_id",
                    (match branchId with
                     | Some id -> Sql.uuid id
                     | None -> Sql.dbnull)
                    "op_blob", Sql.bytes opBlob
                    "created_at",
                    Sql.string (System.DateTime.UtcNow.ToString("o")) ]
                |> Sql.executeNonQueryAsync
                |> Task.map (fun _ -> ())

            // TODO: Trigger op playback to update the projections
            // (package_types, package_values, package_functions, locations tables)
            // For now, this requires a restart or package reload

            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
