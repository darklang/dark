/// Handles edit propagation: recording edits and creating todos for breaking changes.
/// When a package item is edited (content changed, new UUID assigned), this module:
/// 1. Records the edit in edit_history
/// 2. Queries for dependents
/// 3. Creates todos for owners of dependents affected by breaking changes
module LibPackageManager.EditPropagation

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes


/// An edit that was detected during stabilization
type EditInfo = { oldId : uuid; newId : uuid; isBreaking : bool }


/// Record an edit in the edit_history table
let private recordEdit
  (branchId : Option<uuid>)
  (createdBy : Option<uuid>)
  (edit : EditInfo)
  : Task<uuid> =
  task {
    let editId = System.Guid.NewGuid()

    do!
      Sql.query
        """
        INSERT INTO edit_history (id, old_item_id, new_item_id, is_breaking, branch_id, created_by)
        VALUES (@id, @old_item_id, @new_item_id, @is_breaking, @branch_id, @created_by)
        """
      |> Sql.parameters
        [ "id", Sql.uuid editId
          "old_item_id", Sql.uuid edit.oldId
          "new_item_id", Sql.uuid edit.newId
          "is_breaking", Sql.bool edit.isBreaking
          "branch_id",
          (match branchId with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull)
          "created_by",
          (match createdBy with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeStatementAsync

    return editId
  }


/// Insert a single todo into the database
let private insertTodo
  (todoId : uuid)
  (editId : uuid)
  (dependentItemId : uuid)
  (ownerId : uuid)
  (changeType : string)
  : Task<unit> =
  Sql.query
    """
    INSERT INTO breaking_change_todos (id, edit_id, dependent_item_id, owner_id, status, change_type)
    VALUES (@id, @edit_id, @dependent_item_id, @owner_id, @status, @change_type)
    ON CONFLICT (id) DO NOTHING
    """
  |> Sql.parameters
    [ "id", Sql.uuid todoId
      "edit_id", Sql.uuid editId
      "dependent_item_id", Sql.uuid dependentItemId
      "owner_id", Sql.uuid ownerId
      "status", Sql.string Queries.TodoStatus.pending
      "change_type", Sql.string changeType ]
  |> Sql.executeStatementAsync


/// Create todos for dependents of an edit.
/// - Other users: notified about all changes (breaking or compatible)
/// - Same owner: notified about all changes (breaking shows as todo, compatible as available update)
let private createTodosForEdit
  (editId : uuid)
  (oldItemId : uuid)
  (isBreaking : bool)
  : Task<int> =
  task {
    // Find ALL dependents of the old item (no visibility filtering)
    // This ensures we notify other users even if their items aren't visible to the editor
    let! dependents = Queries.getAllDependents oldItemId

    // Determine change type for the database
    let changeType =
      if isBreaking then
        Queries.ChangeType.breaking
      else
        Queries.ChangeType.compatible

    let! todoCount =
      dependents
      |> List.map (fun dep ->
        task {
          // Look up the location to find the owner (no visibility filtering)
          let! locations = Queries.resolveAllLocations [ dep.itemId ]

          match List.tryHead locations with
          | Some loc ->
            // Look up the account ID from the owner name
            let! ownerIdOpt = Accounts.getByName loc.owner

            match ownerIdOpt with
            | Some ownerId ->
              let todoId = System.Guid.NewGuid()
              do! insertTodo todoId editId dep.itemId ownerId changeType
              return 1
            | None -> return 0
          | None -> return 0
        })
      |> Task.flatten
      |> Task.map List.sum

    return todoCount
  }


/// Process a list of edits: record them and create todos
/// - Other users: notified about all changes to their deps
/// - Creator: notified only about breaking changes to their own code
let processEdits
  (branchId : Option<uuid>)
  (createdBy : Option<uuid>)
  (edits : List<EditInfo>)
  : Task<unit> =
  task {
    for edit in edits do
      // Record the edit
      let! editId = recordEdit branchId createdBy edit

      // Create todos for dependents
      let! _todoCount = createTodosForEdit editId edit.oldId edit.isBreaking

      ()
  }


/// Get the edit history for an item (what was it replaced by, or what did it replace?)
let getEditHistory (itemId : uuid) : Task<List<EditInfo>> =
  task {
    return!
      Sql.query
        """
        SELECT old_item_id, new_item_id, is_breaking
        FROM edit_history
        WHERE old_item_id = @item_id OR new_item_id = @item_id
        ORDER BY created_at DESC
        """
      |> Sql.parameters [ "item_id", Sql.uuid itemId ]
      |> Sql.executeAsync (fun read ->
        { oldId = read.uuid "old_item_id"
          newId = read.uuid "new_item_id"
          isBreaking = read.bool "is_breaking" })
  }


/// Get the latest version of an item by following the edit chain
let getLatestVersion (itemId : uuid) : Task<uuid> =
  task {
    let rec follow (currentId : uuid) : Task<uuid> =
      task {
        let! nextVersion =
          Sql.query
            """
            SELECT new_item_id
            FROM edit_history
            WHERE old_item_id = @item_id
            ORDER BY created_at DESC
            LIMIT 1
            """
          |> Sql.parameters [ "item_id", Sql.uuid currentId ]
          |> Sql.executeAsync (fun read -> read.uuid "new_item_id")

        match List.tryHead nextVersion with
        | Some newId -> return! follow newId
        | None -> return currentId
      }

    return! follow itemId
  }
