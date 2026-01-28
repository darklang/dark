module LibPackageManager.Queries

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


// --
// Todo status and change type constants
// --

/// Status values for breaking_change_todos
module TodoStatus =
  let pending = "pending"
  let resolved = "resolved"
  let dismissed = "dismissed"
  let applied = "applied"


/// Change type values for breaking_change_todos
module ChangeType =
  let breaking = "breaking"
  let compatible = "compatible"


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


/// Get ALL items that depend on the given UUID (for edit propagation).
/// Does NOT filter by visibility - finds all dependents regardless of approval status.
/// Used when notifying other users about changes to items they depend on.
let getAllDependents (dependsOnId : uuid) : Task<List<PackageDep>> =
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


/// Batch lookup of locations without visibility filtering (for edit propagation)
let private resolveAllLocationsBatchChunk
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


/// Resolve UUIDs to location info WITHOUT visibility filtering.
/// Used for edit propagation to notify all affected users.
let resolveAllLocations (itemIds : List<uuid>) : Task<List<LocationInfo>> =
  task {
    if List.isEmpty itemIds then
      return []
    else
      let chunks = itemIds |> List.chunkBySize 500

      let! results =
        chunks
        |> List.map resolveAllLocationsBatchChunk
        |> Task.flatten

      return results |> List.concat
  }


// --
// Breaking change todos
// --

/// A todo item for a breaking change that affects a user's code
type BreakingChangeTodo =
  { id : uuid
    editId : uuid
    dependentItemId : uuid
    oldItemId : uuid
    newItemId : uuid
    status : string
    createdAt : string }


/// Get pending todos for a user (breaking changes that affect their code)
let getTodosForAccount (accountId : uuid) : Task<List<BreakingChangeTodo>> =
  task {
    return!
      Sql.query
        """
        SELECT t.id, t.edit_id, t.dependent_item_id, t.status, t.created_at,
               e.old_item_id, e.new_item_id
        FROM breaking_change_todos t
        INNER JOIN edit_history e ON t.edit_id = e.id
        WHERE t.owner_id = @account_id
        ORDER BY t.created_at DESC
        """
      |> Sql.parameters [ "account_id", Sql.uuid accountId ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          editId = read.uuid "edit_id"
          dependentItemId = read.uuid "dependent_item_id"
          oldItemId = read.uuid "old_item_id"
          newItemId = read.uuid "new_item_id"
          status = read.string "status"
          createdAt = read.string "created_at" })
  }


/// Get pending todos only (status = 'pending')
let getPendingTodosForAccount (accountId : uuid) : Task<List<BreakingChangeTodo>> =
  task {
    return!
      Sql.query
        """
        SELECT t.id, t.edit_id, t.dependent_item_id, t.status, t.created_at,
               e.old_item_id, e.new_item_id
        FROM breaking_change_todos t
        INNER JOIN edit_history e ON t.edit_id = e.id
        WHERE t.owner_id = @account_id AND t.status = @status
        ORDER BY t.created_at DESC
        """
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId
          "status", Sql.string TodoStatus.pending ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          editId = read.uuid "edit_id"
          dependentItemId = read.uuid "dependent_item_id"
          oldItemId = read.uuid "old_item_id"
          newItemId = read.uuid "new_item_id"
          status = read.string "status"
          createdAt = read.string "created_at" })
  }


/// Resolve a todo (mark as resolved)
let resolveTodo (todoId : uuid) (resolvedBy : uuid) : Task<bool> =
  task {
    let! rowsAffected =
      Sql.query
        """
        UPDATE breaking_change_todos
        SET status = @new_status,
            resolved_at = datetime('now'),
            resolved_by = @resolved_by
        WHERE id = @todo_id AND status = @pending_status
        """
      |> Sql.parameters
        [ "todo_id", Sql.uuid todoId
          "resolved_by", Sql.uuid resolvedBy
          "new_status", Sql.string TodoStatus.resolved
          "pending_status", Sql.string TodoStatus.pending ]
      |> Sql.executeNonQueryAsync

    return rowsAffected > 0
  }


/// Dismiss a todo (mark as dismissed without resolving the underlying issue)
let dismissTodo (todoId : uuid) (dismissedBy : uuid) : Task<bool> =
  task {
    let! rowsAffected =
      Sql.query
        """
        UPDATE breaking_change_todos
        SET status = @new_status,
            resolved_at = datetime('now'),
            resolved_by = @dismissed_by
        WHERE id = @todo_id AND status = @pending_status
        """
      |> Sql.parameters
        [ "todo_id", Sql.uuid todoId
          "dismissed_by", Sql.uuid dismissedBy
          "new_status", Sql.string TodoStatus.dismissed
          "pending_status", Sql.string TodoStatus.pending ]
      |> Sql.executeNonQueryAsync

    return rowsAffected > 0
  }


// --
// Available updates (compatible changes that can be auto-applied)
// --

/// An available update todo with extended fields
type AvailableUpdateTodo =
  { id : uuid
    editId : uuid
    dependentItemId : uuid
    dependentItemType : string
    oldItemId : uuid
    newItemId : uuid
    changeType : string
    status : string
    createdAt : string
    appliedAt : Option<string>
    previousDependentId : Option<uuid> }


/// Get available updates for a user (compatible changes that can be auto-applied)
let getAvailableUpdatesForAccount (accountId : uuid) : Task<List<AvailableUpdateTodo>> =
  task {
    return!
      Sql.query
        """
        SELECT t.id, t.edit_id, t.dependent_item_id, t.change_type, t.status, t.created_at,
               t.applied_at, t.previous_dependent_id,
               e.old_item_id, e.new_item_id,
               COALESCE(l.item_type, 'fn') as dependent_item_type
        FROM breaking_change_todos t
        INNER JOIN edit_history e ON t.edit_id = e.id
        LEFT JOIN locations l ON t.dependent_item_id = l.item_id AND l.deprecated_at IS NULL
        WHERE t.owner_id = @account_id
          AND t.change_type = @change_type
          AND t.status = @status
        ORDER BY t.created_at DESC
        """
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId
          "change_type", Sql.string ChangeType.compatible
          "status", Sql.string TodoStatus.pending ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          editId = read.uuid "edit_id"
          dependentItemId = read.uuid "dependent_item_id"
          dependentItemType = read.string "dependent_item_type"
          oldItemId = read.uuid "old_item_id"
          newItemId = read.uuid "new_item_id"
          changeType = read.string "change_type"
          status = read.string "status"
          createdAt = read.string "created_at"
          appliedAt = read.stringOrNone "applied_at"
          previousDependentId = read.uuidOrNone "previous_dependent_id" })
  }


/// Get recently applied updates for a user (for revert UI)
let getAppliedUpdatesForAccount (accountId : uuid) : Task<List<AvailableUpdateTodo>> =
  task {
    return!
      Sql.query
        """
        SELECT t.id, t.edit_id, t.dependent_item_id, t.change_type, t.status, t.created_at,
               t.applied_at, t.previous_dependent_id,
               e.old_item_id, e.new_item_id,
               COALESCE(l.item_type, 'fn') as dependent_item_type
        FROM breaking_change_todos t
        INNER JOIN edit_history e ON t.edit_id = e.id
        LEFT JOIN locations l ON t.dependent_item_id = l.item_id AND l.deprecated_at IS NULL
        WHERE t.owner_id = @account_id
          AND t.change_type = @change_type
          AND t.status = @status
        ORDER BY t.applied_at DESC
        """
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId
          "change_type", Sql.string ChangeType.compatible
          "status", Sql.string TodoStatus.applied ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          editId = read.uuid "edit_id"
          dependentItemId = read.uuid "dependent_item_id"
          dependentItemType = read.string "dependent_item_type"
          oldItemId = read.uuid "old_item_id"
          newItemId = read.uuid "new_item_id"
          changeType = read.string "change_type"
          status = read.string "status"
          createdAt = read.string "created_at"
          appliedAt = read.stringOrNone "applied_at"
          previousDependentId = read.uuidOrNone "previous_dependent_id" })
  }


/// Get pending breaking change todos for a user (todos requiring manual fix)
let getPendingBreakingTodosForAccount (accountId : uuid) : Task<List<AvailableUpdateTodo>> =
  task {
    return!
      Sql.query
        """
        SELECT t.id, t.edit_id, t.dependent_item_id, t.change_type, t.status, t.created_at,
               t.applied_at, t.previous_dependent_id,
               e.old_item_id, e.new_item_id,
               COALESCE(l.item_type, 'fn') as dependent_item_type
        FROM breaking_change_todos t
        INNER JOIN edit_history e ON t.edit_id = e.id
        LEFT JOIN locations l ON t.dependent_item_id = l.item_id AND l.deprecated_at IS NULL
        WHERE t.owner_id = @account_id
          AND t.change_type = @change_type
          AND t.status = @status
        ORDER BY t.created_at DESC
        """
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId
          "change_type", Sql.string ChangeType.breaking
          "status", Sql.string TodoStatus.pending ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          editId = read.uuid "edit_id"
          dependentItemId = read.uuid "dependent_item_id"
          dependentItemType = read.string "dependent_item_type"
          oldItemId = read.uuid "old_item_id"
          newItemId = read.uuid "new_item_id"
          changeType = read.string "change_type"
          status = read.string "status"
          createdAt = read.string "created_at"
          appliedAt = read.stringOrNone "applied_at"
          previousDependentId = read.uuidOrNone "previous_dependent_id" })
  }


/// Mark a todo as applied (for compatible changes)
/// previousDependentId stores the UUID of the dependent BEFORE the update
let markTodoAsApplied
  (todoId : uuid)
  (appliedBy : uuid)
  (previousDependentId : uuid)
  : Task<bool> =
  task {
    let! rowsAffected =
      Sql.query
        """
        UPDATE breaking_change_todos
        SET status = @new_status,
            applied_at = datetime('now'),
            resolved_by = @applied_by,
            previous_dependent_id = @previous_dependent_id
        WHERE id = @todo_id AND status = @pending_status
        """
      |> Sql.parameters
        [ "todo_id", Sql.uuid todoId
          "applied_by", Sql.uuid appliedBy
          "previous_dependent_id", Sql.uuid previousDependentId
          "new_status", Sql.string TodoStatus.applied
          "pending_status", Sql.string TodoStatus.pending ]
      |> Sql.executeNonQueryAsync

    return rowsAffected > 0
  }


/// Revert an applied update back to pending
let revertAppliedUpdate (todoId : uuid) : Task<bool> =
  task {
    let! rowsAffected =
      Sql.query
        """
        UPDATE breaking_change_todos
        SET status = @new_status,
            applied_at = NULL,
            resolved_by = NULL,
            previous_dependent_id = NULL
        WHERE id = @todo_id AND status = @applied_status
        """
      |> Sql.parameters
        [ "todo_id", Sql.uuid todoId
          "new_status", Sql.string TodoStatus.pending
          "applied_status", Sql.string TodoStatus.applied ]
      |> Sql.executeNonQueryAsync

    return rowsAffected > 0
  }


/// Get an update todo by ID (with extended fields)
let getUpdateTodoById (todoId : uuid) : Task<Option<AvailableUpdateTodo>> =
  task {
    let! results =
      Sql.query
        """
        SELECT t.id, t.edit_id, t.dependent_item_id, t.change_type, t.status, t.created_at,
               t.applied_at, t.previous_dependent_id,
               e.old_item_id, e.new_item_id,
               COALESCE(l.item_type, 'fn') as dependent_item_type
        FROM breaking_change_todos t
        INNER JOIN edit_history e ON t.edit_id = e.id
        LEFT JOIN locations l ON t.dependent_item_id = l.item_id AND l.deprecated_at IS NULL
        WHERE t.id = @todo_id
        """
      |> Sql.parameters [ "todo_id", Sql.uuid todoId ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          editId = read.uuid "edit_id"
          dependentItemId = read.uuid "dependent_item_id"
          dependentItemType = read.string "dependent_item_type"
          oldItemId = read.uuid "old_item_id"
          newItemId = read.uuid "new_item_id"
          changeType = read.string "change_type"
          status = read.string "status"
          createdAt = read.string "created_at"
          appliedAt = read.stringOrNone "applied_at"
          previousDependentId = read.uuidOrNone "previous_dependent_id" })

    return List.tryHead results
  }


// --
// Last approved version lookup (for edit detection)
// --

/// Get the location (owner, modules, name) for a given item UUID.
/// Used by edit detection to find the approved version of referenced items.
let getLocationByItemId
  (itemId : uuid)
  (itemType : string)
  : Task<Option<string * string * string>> =
  task {
    let! results =
      Sql.query
        """
        SELECT owner, modules, name
        FROM locations
        WHERE item_id = @item_id
          AND item_type = @item_type
          AND deprecated_at IS NULL
        LIMIT 1
        """
      |> Sql.parameters
        [ "item_id", Sql.uuid itemId
          "item_type", Sql.string itemType ]
      |> Sql.executeAsync (fun read ->
        (read.string "owner", read.string "modules", read.string "name"))

    return List.tryHead results
  }


/// Get the last approved item UUID for a location.
/// Used by edit detection to compare new content against the approved version,
/// not the current (possibly pending) version.
let getLastApprovedFn
  (branchID : Option<uuid>)
  (owner : string)
  (modules : string)
  (name : string)
  : Task<Option<uuid>> =
  task {
    let! results =
      Sql.query
        """
        SELECT item_id
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = 'fn'
          AND approval_status = 'approved'
          AND deprecated_at IS NULL
          AND (branch_id IS NULL OR branch_id = @branch_id)
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeAsync (fun read -> read.uuid "item_id")

    return List.tryHead results
  }


let getLastApprovedType
  (branchID : Option<uuid>)
  (owner : string)
  (modules : string)
  (name : string)
  : Task<Option<uuid>> =
  task {
    let! results =
      Sql.query
        """
        SELECT item_id
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = 'type'
          AND approval_status = 'approved'
          AND deprecated_at IS NULL
          AND (branch_id IS NULL OR branch_id = @branch_id)
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeAsync (fun read -> read.uuid "item_id")

    return List.tryHead results
  }


let getLastApprovedValue
  (branchID : Option<uuid>)
  (owner : string)
  (modules : string)
  (name : string)
  : Task<Option<uuid>> =
  task {
    let! results =
      Sql.query
        """
        SELECT item_id
        FROM locations
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = 'value'
          AND approval_status = 'approved'
          AND deprecated_at IS NULL
          AND (branch_id IS NULL OR branch_id = @branch_id)
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeAsync (fun read -> read.uuid "item_id")

    return List.tryHead results
  }


// --
// Pending changes (for revert feature)
// --

/// A pending change that a user can revert
type PendingChange =
  { itemId : uuid
    itemType : string
    owner : string
    modules : string
    name : string
    createdAt : string
    locationId : uuid }


/// Get all pending changes for an account
let getPendingChangesForAccount
  (accountId : uuid)
  (branchID : Option<uuid>)
  : Task<List<PendingChange>> =
  task {
    return!
      Sql.query
        """
        SELECT l.item_id, l.item_type, l.owner, l.modules, l.name, l.created_at, l.location_id
        FROM locations l
        WHERE l.created_by = @account_id
          AND l.approval_status = 'pending'
          AND l.deprecated_at IS NULL
          AND (l.branch_id IS NULL OR l.branch_id = @branch_id)
        ORDER BY l.created_at DESC
        """
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeAsync (fun read ->
        { itemId = read.uuid "item_id"
          itemType = read.string "item_type"
          owner = read.string "owner"
          modules = read.string "modules"
          name = read.string "name"
          createdAt = read.string "created_at"
          locationId = read.uuid "location_id" })
  }


/// Revert a pending change by deprecating its location entry.
/// Name resolution will fall back to the last approved version.
/// Returns true if the location was found and deprecated.
let revertPendingChange (locationId : uuid) (_revertedBy : uuid) : Task<bool> =
  task {
    // TODO: track who reverted in a separate audit table if needed
    let! rowsAffected =
      Sql.query
        """
        UPDATE locations
        SET deprecated_at = datetime('now')
        WHERE location_id = @location_id
          AND approval_status = 'pending'
          AND deprecated_at IS NULL
        """
      |> Sql.parameters [ "location_id", Sql.uuid locationId ]
      |> Sql.executeNonQueryAsync

    return rowsAffected > 0
  }


/// Deprecate all pending entries for a specific location (owner, modules, name, item_type).
/// Used when reverting to approved version - we skip emitting new ops and deprecate existing pending entries.
let deprecatePendingEntriesForLocation
  (owner : string)
  (modules : string)
  (name : string)
  (itemType : string)
  (branchID : Option<uuid>)
  : Task<int> =
  task {
    return!
      Sql.query
        """
        UPDATE locations
        SET deprecated_at = datetime('now')
        WHERE owner = @owner
          AND modules = @modules
          AND name = @name
          AND item_type = @item_type
          AND approval_status = 'pending'
          AND deprecated_at IS NULL
          AND (branch_id IS NULL OR branch_id = @branch_id)
        """
      |> Sql.parameters
        [ "owner", Sql.string owner
          "modules", Sql.string modules
          "name", Sql.string name
          "item_type", Sql.string itemType
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeNonQueryAsync
  }


/// Delete edit_history entries for a given approved item ID.
/// This is called when an item is reverted to its approved version.
/// The CASCADE DELETE will also remove associated breaking_change_todos.
let deleteEditHistoryForApprovedItem (approvedItemId : uuid) : Task<int> =
  task {
    return!
      Sql.query
        """
        DELETE FROM edit_history
        WHERE old_item_id = @approved_item_id
        """
      |> Sql.parameters [ "approved_item_id", Sql.uuid approvedItemId ]
      |> Sql.executeNonQueryAsync
  }
