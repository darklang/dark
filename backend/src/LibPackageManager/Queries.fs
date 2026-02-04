module LibPackageManager.Queries

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


/// Get recent package ops from the database
let getRecentOps (limit : int64) : Task<List<PT.PackageOp>> =
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


/// Get all package ops created since the specified datetime
let getAllOpsSince
  (since : LibExecution.DarkDateTime.T)
  : Task<List<PT.PackageOp * bool>> =
  task {
    let sinceStr = LibExecution.DarkDateTime.toIsoString since

    return!
      Sql.query
        """
        SELECT id, op_blob, commit_id
        FROM package_ops
        WHERE datetime(created_at) > datetime(@since)
        ORDER BY created_at ASC
        """
      |> Sql.parameters [ "since", Sql.string sinceStr ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        let isWip = (read.uuidOrNone "commit_id").IsNone
        let op = BinarySerialization.PT.PackageOp.deserialize opId opBlob
        (op, isWip))
  }


/// A dependency relationship between package items.
/// For dependents: itemId is the item that has the dependency.
/// For dependencies: itemId is what the item depends on.
type PackageDep = { itemId : uuid; kind : string } // "fn", "type", "value"


/// Get items that depend on the given UUID (reverse dependencies / "what uses this?")
let getDependents (dependsOnId : uuid) : Task<List<PackageDep>> =
  task {
    return!
      Sql.query
        """
        SELECT DISTINCT pd.item_id, l.item_type
        FROM package_dependencies pd
        INNER JOIN locations l ON pd.item_id = l.item_id
        WHERE pd.depends_on_id = @depends_on_id
          AND l.deprecated_at IS NULL
        ORDER BY pd.item_id
        """
      |> Sql.parameters [ "depends_on_id", Sql.uuid dependsOnId ]
      |> Sql.executeAsync (fun read ->
        { itemId = read.uuid "item_id"; kind = read.string "item_type" })
  }


/// Get UUIDs that the given item depends on (forward dependencies / "what does this use?" / uses)
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
          ORDER BY pd.depends_on_id, pd.item_id
          """

      return!
        Sql.query sql
        |> Sql.parameters depParams
        |> Sql.executeAsync (fun read ->
          { dependsOnId = read.uuid "depends_on_id"
            itemId = read.uuid "item_id"
            kind = read.string "item_type" })
  }


/// Batch lookup of dependents for multiple dependency IDs.
/// Chunks requests to avoid SQLite expression tree depth limits.
let getDependentsBatch (dependsOnIds : List<uuid>) : Task<List<BatchDependent>> =
  task {
    if List.isEmpty dependsOnIds then
      return []
    else
      // SQLite has a limit on expression tree depth (~1000)
      // Chunk into batches of 100 to stay well under the limit
      let chunks = dependsOnIds |> List.chunkBySize 100

      let! results = chunks |> List.map getDependentsBatchChunk |> Task.flatten

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
          """
        |> Sql.parameters itemParams
        |> Sql.executeAsync (fun read ->
          { itemId = read.uuid "item_id"
            itemType = read.string "item_type"
            owner = read.string "owner"
            modules = read.string "modules"
            name = read.string "name" })
  }


/// Resolve UUIDs to location info (owner, modules, name).
/// Returns locations for all items found.
let resolveLocations (itemIds : List<uuid>) : Task<List<LocationInfo>> =
  task {
    if List.isEmpty itemIds then
      return []
    else
      // SQLite has a limit on IN clause (~1000 params)
      let chunks = itemIds |> List.chunkBySize 500

      let! results = chunks |> List.map resolveLocationsBatchChunk |> Task.flatten

      return results |> List.concat
  }


// ===========================================
// SCM Queries (branch-scoped)
// ===========================================

/// Get all WIP ops on a branch (commit_id IS NULL)
let getWipOps (branchId : PT.BranchId) : Task<List<PT.PackageOp>> =
  task {
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        WHERE branch_id = @branch_id AND commit_id IS NULL
        ORDER BY created_at ASC
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        BinarySerialization.PT.PackageOp.deserialize opId opBlob)
  }


/// Summary of WIP changes
type WipSummary =
  { types : int64; values : int64; fns : int64; renames : int64; total : int64 }


/// Get summary of WIP ops by type on a branch
let getWipSummary (branchId : PT.BranchId) : Task<WipSummary> =
  task {
    let! ops = getWipOps branchId

    let types =
      ops
      |> List.filter (function
        | PT.PackageOp.AddType _ -> true
        | _ -> false)
      |> List.length
      |> int64

    let values =
      ops
      |> List.filter (function
        | PT.PackageOp.AddValue _ -> true
        | _ -> false)
      |> List.length
      |> int64

    let fns =
      ops
      |> List.filter (function
        | PT.PackageOp.AddFn _ -> true
        | _ -> false)
      |> List.length
      |> int64

    let renames =
      ops
      |> List.filter (function
        | PT.PackageOp.SetTypeName _
        | PT.PackageOp.SetValueName _
        | PT.PackageOp.SetFnName _ -> true
        | _ -> false)
      |> List.length
      |> int64

    return
      { types = types
        values = values
        fns = fns
        renames = renames
        total = types + values + fns }
  }


/// A commit record
type Commit =
  { id : uuid; message : string; createdAt : NodaTime.Instant; opCount : int64 }


/// Get commits on a branch ordered by date descending
let getCommits (branchId : PT.BranchId) (limit : int64) : Task<List<Commit>> =
  task {
    return!
      Sql.query
        """
        SELECT c.id, c.message, c.created_at,
               (SELECT COUNT(*) FROM package_ops WHERE commit_id = c.id) as op_count
        FROM commits c
        WHERE c.branch_id = @branch_id
        ORDER BY c.created_at DESC
        LIMIT @limit
        """
      |> Sql.parameters [ "branch_id", Sql.uuid branchId; "limit", Sql.int64 limit ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          message = read.string "message"
          createdAt = read.instant "created_at"
          opCount = read.int64 "op_count" })
  }


/// A commit record that also tracks which branch it belongs to
type ChainCommit =
  { id : uuid
    message : string
    createdAt : NodaTime.Instant
    opCount : int64
    branchId : PT.BranchId
    branchName : string }


/// Get commits across the entire branch chain (current + ancestors), ordered by date descending.
/// Each commit is tagged with the branch it belongs to.
let getCommitsForBranchChain
  (branchId : PT.BranchId)
  (limit : int64)
  : Task<List<ChainCommit>> =
  task {
    let! chain = Branches.getBranchChain branchId

    if List.isEmpty chain then
      return []
    else
      // Build parameterized IN clause for branch IDs
      let branchParams = chain |> List.mapi (fun i id -> $"bid_{i}", Sql.uuid id)
      let inClause =
        chain |> List.mapi (fun i _ -> $"@bid_{i}") |> String.concat ", "

      return!
        Sql.query
          $"""
          SELECT c.id, c.message, c.created_at,
                 (SELECT COUNT(*) FROM package_ops WHERE commit_id = c.id) as op_count,
                 c.branch_id,
                 b.name as branch_name
          FROM commits c
          JOIN branches b ON c.branch_id = b.id
          WHERE c.branch_id IN ({inClause})
          ORDER BY c.created_at DESC
          LIMIT @limit
          """
        |> Sql.parameters (branchParams @ [ "limit", Sql.int64 limit ])
        |> Sql.executeAsync (fun read ->
          { id = read.uuid "id"
            message = read.string "message"
            createdAt = read.instant "created_at"
            opCount = read.int64 "op_count"
            branchId = read.uuid "branch_id"
            branchName = read.string "branch_name" })
  }


/// Get ops for a specific commit
let getCommitOps (commitId : uuid) : Task<List<PT.PackageOp>> =
  task {
    return!
      Sql.query
        """
        SELECT id, op_blob
        FROM package_ops
        WHERE commit_id = @commit_id
        ORDER BY created_at ASC
        """
      |> Sql.parameters [ "commit_id", Sql.uuid commitId ]
      |> Sql.executeAsync (fun read ->
        let opId = read.uuid "id"
        let opBlob = read.bytes "op_blob"
        BinarySerialization.PT.PackageOp.deserialize opId opBlob)
  }
