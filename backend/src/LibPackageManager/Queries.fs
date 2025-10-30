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
/// branchId: None = main/merged, Some(id) = branch-specific
let getRecentOps (branchId : Option<PT.BranchID>) (limit : int64) : Task<List<PT.PackageOp>> =
  task {
    match branchId with
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
      // Query main branch only (branch_id IS NULL)
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
/// branchId: None = main branch, Some = specific branch
let getOpsSince
  (branchId : Option<PT.BranchID>)
  (since : LibExecution.DarkDateTime.T)
  : Task<List<PT.PackageOp>> =
  task {
    let sinceStr = LibExecution.DarkDateTime.toIsoString since

    match branchId with
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


type HistoryEntry =
  { timestamp : System.DateTime
    branchId : Option<uuid>
    itemId : uuid
    opType : string }


/// Get the history of what a name has pointed to over time
/// Returns list of (timestamp, branchId, itemId, opType)
let getNameHistory
  (owner : string)
  (modules : List<string>)
  (name : string)
  (itemType : string) // "fn", "type", or "value"
  : Task<List<HistoryEntry>> =
  task {
    let modulesStr = String.concat "." modules

    // Query package_ops for all Set*Name ops for this location
    let! history =
      Sql.query
        """
        SELECT id, created_at, branch_id, op_blob
        FROM package_ops
        ORDER BY created_at ASC
        """
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let timestamp = read.dateTime "created_at"
        let branchId = read.uuidOrNone "branch_id"
        let opBlob = read.bytes "op_blob"

        let op = BinarySerialization.PT.PackageOp.deserialize opId opBlob

        // Check if this op sets the name we're looking for
        match op with
        | PT.PackageOp.SetFnName(id, location)
          when itemType = "fn"
               && location.owner = owner
               && String.concat "." location.modules = modulesStr
               && location.name = name ->
          Some
            { timestamp = timestamp
              branchId = branchId
              itemId = id
              opType = "SetFnName" }

        | PT.PackageOp.SetTypeName(id, location)
          when itemType = "type"
               && location.owner = owner
               && String.concat "." location.modules = modulesStr
               && location.name = name ->
          Some
            { timestamp = timestamp
              branchId = branchId
              itemId = id
              opType = "SetTypeName" }

        | PT.PackageOp.SetValueName(id, location)
          when itemType = "value"
               && location.owner = owner
               && String.concat "." location.modules = modulesStr
               && location.name = name ->
          Some
            { timestamp = timestamp
              branchId = branchId
              itemId = id
              opType = "SetValueName" }

        | _ -> None)

    return history |> List.choose (fun x -> x)
  }
