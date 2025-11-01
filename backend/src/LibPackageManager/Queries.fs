module LibPackageManager.Queries

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


/// Get recent package ops from the database
/// branchID: None = main/merged, Some(id) = branch-specific
let getRecentOps
  (branchID : Option<PT.BranchID>)
  (limit : int64)
  : Task<List<PT.PackageOp>> =
  task {
    match branchID with
    | Some id ->
      // Query specific branch only
      return!
        Sql.query
          """
          SELECT id, op_blob
          FROM package_ops
          WHERE branch_id = @branch_id
          ORDER BY created_at DESC
          LIMIT @limit
          """
        |> Sql.parameters [ "branch_id", Sql.uuid id; "limit", Sql.int64 limit ]
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          BinarySerialization.PT.PackageOp.deserialize opId opBlob)
    | None ->
      return!
        Sql.query
          """
          SELECT id, op_blob
          FROM package_ops
          WHERE branch_id IS NULL
          ORDER BY created_at DESC
          LIMIT @limit
          """
        |> Sql.parameters [ "limit", Sql.int64 limit ]
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          BinarySerialization.PT.PackageOp.deserialize opId opBlob)
  }


/// Get recent package ops from ALL branches (no branch filter)
let getRecentOpsAllBranches (limit : int64) : Task<List<PT.PackageOp>> =
  task {
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        ORDER BY created_at DESC
        LIMIT @limit
        """
      |> Sql.parameters [ "limit", Sql.int64 limit ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        BinarySerialization.PT.PackageOp.deserialize opId opBlob)
  }


/// Get package ops created since the specified datetime
let getOpsSince
  (branchID : Option<PT.BranchID>)
  (since : LibExecution.DarkDateTime.T)
  : Task<List<PT.PackageOp>> =
  task {
    let sinceStr = LibExecution.DarkDateTime.toIsoString since

    match branchID with
    | Some id ->
      // Query specific branch only
      return!
        Sql.query
          """
          SELECT id, op_blob
          FROM package_ops
          WHERE branch_id = @branch_id
            AND created_at > @since
          ORDER BY created_at ASC
          """
        |> Sql.parameters [ "branch_id", Sql.uuid id; "since", Sql.string sinceStr ]
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          BinarySerialization.PT.PackageOp.deserialize opId opBlob)
    | None ->
      // Query main branch only (branch_id IS NULL)
      return!
        Sql.query
          """
          SELECT id, op_blob
          FROM package_ops
          WHERE branch_id IS NULL
            AND created_at > @since
          ORDER BY created_at ASC
          """
        |> Sql.parameters [ "since", Sql.string sinceStr ]
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          BinarySerialization.PT.PackageOp.deserialize opId opBlob)
  }
