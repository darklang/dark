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


/// Get all package ops (from ALL branches) created since the specified datetime
/// Returns ops with their branch IDs for multi-branch sync
///
/// targetInstanceID: Optional target instance ID to filter results for.
///   - None: Return all ops
///   - Some(uuid): Exclude ops where instance_id = uuid (used when pushing to target to avoid sending ops back to their source)
let getAllOpsSince
  (targetInstanceID : Option<System.Guid>)
  (since : LibExecution.DarkDateTime.T)
  : Task<List<PT.PackageOp * Option<PT.BranchID> * Option<System.Guid>>> =
  task {
    let sinceStr = LibExecution.DarkDateTime.toIsoString since

    match targetInstanceID with
    | Some targetID ->
      return!
        Sql.query
          """
          SELECT id, op_blob, branch_id, instance_id
          FROM package_ops
          WHERE datetime(created_at) > datetime(@since)
            AND (instance_id IS NULL OR instance_id != @target_instance_id)
          ORDER BY
            CASE WHEN branch_id IS NULL THEN 0 ELSE 1 END,
            created_at ASC
          """
        |> Sql.parameters
          [ "since", Sql.string sinceStr; "target_instance_id", Sql.uuid targetID ]
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          let branchID = read.uuidOrNone "branch_id"
          let instanceID = read.uuidOrNone "instance_id"
          let op = BinarySerialization.PT.PackageOp.deserialize opId opBlob
          (op, branchID, instanceID))
    | None ->
      return!
        Sql.query
          """
          SELECT id, op_blob, branch_id, instance_id
          FROM package_ops
          WHERE datetime(created_at) > datetime(@since)
          ORDER BY
            CASE WHEN branch_id IS NULL THEN 0 ELSE 1 END,
            created_at ASC
          """
        |> Sql.parameters [ "since", Sql.string sinceStr ]
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          let branchID = read.uuidOrNone "branch_id"
          let instanceID = read.uuidOrNone "instance_id"
          let op = BinarySerialization.PT.PackageOp.deserialize opId opBlob
          (op, branchID, instanceID))

  }
