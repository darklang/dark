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
/// targetInstanceID: Optional target instance ID to filter results for:
///   - None: Return all ops
///   - Some(uuid): Exclude ops where instance_id = uuid (used when pushing to target to avoid sending ops back to their source)
let getAllOpsSince
  (targetInstanceID : Option<PT.InstanceID>)
  (since : LibExecution.DarkDateTime.T)
  : Task<List<PT.PackageOp * Option<PT.BranchID> * Option<PT.InstanceID>>> =
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


/// A dependency relationship between package items.
/// For dependents: itemId is the item that has the dependency.
/// For dependencies: itemId is what the item depends on.
type PackageDep = { itemId : uuid; kind : string } // "fn", "type", "value"


/// Get items that depend on the given UUID (reverse dependencies / "what uses this?")
/// Filters by visibility: approved items + caller's own pending items.
let getDependents
  (accountID : Option<uuid>)
  (branchID : Option<uuid>)
  (dependsOnId : uuid)
  : Task<List<PackageDep>> =
  task {
    return!
      Sql.query
        """
        SELECT DISTINCT pd.item_id, l.item_type
        FROM package_dependencies pd
        INNER JOIN locations l ON pd.item_id = l.item_id
        WHERE pd.depends_on_id = @depends_on_id
          AND l.deprecated_at IS NULL
          AND (l.approval_status = 'approved' OR l.created_by = @account_id)
          AND (l.branch_id IS NULL OR l.branch_id = @branch_id)
        ORDER BY pd.item_id
        """
      |> Sql.parameters
        [ "depends_on_id", Sql.uuid dependsOnId
          "account_id",
          (match accountID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull)
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeAsync (fun read ->
        { itemId = read.uuid "item_id"; kind = read.string "item_type" })
  }


/// Get UUIDs that the given item depends on (forward dependencies / "what does this use?" / uses)
/// For forward deps, we don't filter by account - you can depend on anyone's code.
let getDependencies (itemId : uuid) : Task<List<PackageDep>> =
  task {
    return!
      Sql.query
        """
        SELECT DISTINCT pd.depends_on_id, l.item_type
        FROM package_dependencies pd
        INNER JOIN locations l ON pd.depends_on_id = l.item_id
        WHERE pd.item_id = @item_id
          AND l.deprecated_at IS NULL
        ORDER BY pd.depends_on_id
        """
      |> Sql.parameters [ "item_id", Sql.uuid itemId ]
      |> Sql.executeAsync (fun read ->
        { itemId = read.uuid "depends_on_id"; kind = read.string "item_type" })
  }


/// Batch result including the dependency target that was queried
type BatchDependent =
  { dependsOnId : uuid // The item that was queried (what is being depended on)
    itemId : uuid // The item that has the dependency
    kind : string }


/// Batch lookup of dependents for a chunk of dependency IDs
let private getDependentsBatchChunk
  (accountID : Option<uuid>)
  (branchID : Option<uuid>)
  (dependsOnIds : List<uuid>)
  : Task<List<BatchDependent>> =
  task {
    if List.isEmpty dependsOnIds then
      return []
    else
      // Build parameterized IN clause
      let depParams = dependsOnIds |> List.mapi (fun i id -> $"dep_{i}", Sql.uuid id)

      let inClause =
        dependsOnIds |> List.mapi (fun i _ -> $"@dep_{i}") |> String.concat ", "

      let sql =
        $"""
          SELECT DISTINCT pd.depends_on_id, pd.item_id, l.item_type
          FROM package_dependencies pd
          INNER JOIN locations l ON pd.item_id = l.item_id
          WHERE pd.depends_on_id IN ({inClause})
            AND l.deprecated_at IS NULL
            AND (l.approval_status = 'approved' OR l.created_by = @account_id)
            AND (l.branch_id IS NULL OR l.branch_id = @branch_id)
          ORDER BY pd.depends_on_id, pd.item_id
          """

      let accountParam =
        match accountID with
        | Some id -> Sql.uuid id
        | None -> Sql.dbnull

      let branchParam =
        match branchID with
        | Some id -> Sql.uuid id
        | None -> Sql.dbnull

      return!
        Sql.query sql
        |> Sql.parameters (
          depParams @ [ "account_id", accountParam; "branch_id", branchParam ]
        )
        |> Sql.executeAsync (fun read ->
          { dependsOnId = read.uuid "depends_on_id"
            itemId = read.uuid "item_id"
            kind = read.string "item_type" })
  }


/// Batch lookup of dependents for multiple dependency IDs.
/// Filters by visibility: approved items + caller's own pending items.
/// Chunks requests to avoid SQLite expression tree depth limits.
let getDependentsBatch
  (accountID : Option<uuid>)
  (branchID : Option<uuid>)
  (dependsOnIds : List<uuid>)
  : Task<List<BatchDependent>> =
  task {
    if List.isEmpty dependsOnIds then
      return []
    else
      // SQLite has a limit on expression tree depth (~1000)
      // Chunk into batches of 100 to stay well under the limit
      let chunks = dependsOnIds |> List.chunkBySize 100

      let! results =
        chunks
        |> List.map (getDependentsBatchChunk accountID branchID)
        |> Task.flatten

      return results |> List.concat
  }


/// Location info for batch lookup results
type LocationInfo =
  { itemId : uuid
    itemType : string
    owner : string
    modules : string
    name : string }


/// Batch lookup of locations for a chunk of item IDs
let private resolveLocationsBatchChunk
  (accountID : Option<uuid>)
  (branchID : Option<uuid>)
  (itemIds : List<uuid>)
  : Task<List<LocationInfo>> =
  task {
    if List.isEmpty itemIds then
      return []
    else
      let itemParams = itemIds |> List.mapi (fun i id -> $"id_{i}", Sql.uuid id)
      let inClause =
        itemIds |> List.mapi (fun i _ -> $"@id_{i}") |> String.concat ", "

      return!
        Sql.query
          $"""
          SELECT item_id, item_type, owner, modules, name
          FROM locations
          WHERE item_id IN ({inClause})
            AND deprecated_at IS NULL
            AND (approval_status = 'approved' OR created_by = @accountID)
            AND (branch_id IS NULL OR branch_id = @branch_id)
          """
        |> Sql.parameters (
          itemParams
          @ [ "branch_id",
              (match branchID with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull)
              "accountID",
              (match accountID with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull) ]
        )
        |> Sql.executeAsync (fun read ->
          { itemId = read.uuid "item_id"
            itemType = read.string "item_type"
            owner = read.string "owner"
            modules = read.string "modules"
            name = read.string "name" })
  }


/// Resolve UUIDs to location info (owner, modules, name).
/// Returns locations for all items found, filtering by accountID/branchID visibility.
let resolveLocations
  (accountID : Option<uuid>)
  (branchID : Option<uuid>)
  (itemIds : List<uuid>)
  : Task<List<LocationInfo>> =
  task {
    if List.isEmpty itemIds then
      return []
    else
      // SQLite has a limit on IN clause (~1000 params)
      let chunks = itemIds |> List.chunkBySize 500

      let! results =
        chunks
        |> List.map (resolveLocationsBatchChunk accountID branchID)
        |> Task.flatten

      return results |> List.concat
  }
