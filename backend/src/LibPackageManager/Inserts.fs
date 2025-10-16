module LibPackageManager.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


/// Insert PackageOps into the package_ops table and apply them to projection tables
/// branchId: None = main/merged, Some(id) = branch-specific
let insertOps (branchId : Option<PT.BranchID>) (ops : List<PT.PackageOp>) : Task<unit> =
  task {
    if List.isEmpty ops then return ()

    // Step 1: Insert ops into package_ops table (source of truth)
    let insertStatements =
      ops
      |> List.map (fun op ->
        let opId = System.Guid.NewGuid()
        let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

        let sql =
          """
          INSERT INTO package_ops (id, branch_id, op_blob)
          VALUES (@id, @branch_id, @op_blob)
          """

        let parameters =
          [ "id", Sql.uuid opId
            "branch_id", (match branchId with | Some id -> Sql.uuid id | None -> Sql.dbnull)
            "op_blob", Sql.bytes opBlob ]

        (sql, [ parameters ]))

    insertStatements |> Sql.executeTransactionSync |> ignore<List<int>>

    // Step 2: Apply ops to projection tables (types, values, functions, locations)
    do! LibPackageManager.PackageOpPlayback.applyOps branchId ops
  }
