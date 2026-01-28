/// Handles applying and reverting compatible updates.
/// When a compatible change is made, dependents can be automatically updated
/// to reference the new version. This module provides the logic for that.
module LibPackageManager.UpdateApplicator

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module SC = SignatureComparer
module EP = EditPropagation


/// Apply a compatible update to a dependent item.
/// Creates a new version of the dependent with the reference updated.
/// Returns the new dependent item ID if successful.
let applyUpdate
  (instanceID : Option<PT.InstanceID>)
  (branchID : Option<PT.BranchID>)
  (appliedBy : uuid)
  (todoId : uuid)
  : Task<Result<uuid, string>> =
  task {
    // Get the todo to understand what update to apply
    let! todoOpt = Queries.getUpdateTodoById todoId

    match todoOpt with
    | None -> return Error "Todo not found"
    | Some todo ->
      if todo.status <> Queries.TodoStatus.pending then
        return Error $"Todo is not pending (status: {todo.status})"
      elif todo.changeType <> Queries.ChangeType.compatible then
        return Error $"Todo is not a compatible change (type: {todo.changeType})"
      else
        // Get the dependent item
        let! locations = Queries.resolveAllLocations [ todo.dependentItemId ]

        match List.tryHead locations with
        | None -> return Error "Could not find dependent item location"
        | Some loc ->
          // Create mapping: oldItemId -> newItemId
          let mapping = System.Collections.Generic.Dictionary<uuid, uuid>()
          mapping[todo.oldItemId] <- todo.newItemId

          // Load and transform the dependent based on its type
          match loc.itemType with
          | "fn" ->
            let! fnOpt = ProgramTypes.Fn.get todo.dependentItemId

            match fnOpt with
            | None -> return Error "Could not load dependent function"
            | Some fn ->
              // Transform the function body to use the new UUID
              let transformedBody = SC.transformExprUuids mapping fn.body

              // Create a new function with a new ID and the transformed body
              let newFnId = System.Guid.NewGuid()
              let newFn = { fn with id = newFnId; body = transformedBody }

              // Insert the new function
              let ops =
                [ PT.PackageOp.AddFn newFn
                  PT.PackageOp.SetFnName(
                    newFnId,
                    { PT.PackageLocation.owner = loc.owner
                      modules = loc.modules.Split('.') |> Array.toList |> List.filter ((<>) "")
                      name = loc.name }
                  ) ]

              let! (inserted, _) = Inserts.insertAndApplyOps instanceID branchID (Some appliedBy) ops

              if inserted > 0L then
                // Mark the todo as applied
                let! _ = Queries.markTodoAsApplied todoId appliedBy todo.dependentItemId

                // Propagate to dependents of this item (compatible change since signature unchanged)
                let edit : EP.EditInfo =
                  { oldId = todo.dependentItemId
                    newId = newFnId
                    isBreaking = false }
                do! EP.processEdits branchID (Some appliedBy) [ edit ]

                return Ok newFnId
              else
                return Error "Failed to insert updated function"

          | "value" ->
            let! valueOpt = ProgramTypes.Value.get todo.dependentItemId

            match valueOpt with
            | None -> return Error "Could not load dependent value"
            | Some value ->
              // Transform the value body to use the new UUID
              let transformedBody = SC.transformExprUuids mapping value.body

              // Create a new value with a new ID and the transformed body
              let newValueId = System.Guid.NewGuid()
              let newValue = { value with id = newValueId; body = transformedBody }

              // Insert the new value
              let ops =
                [ PT.PackageOp.AddValue newValue
                  PT.PackageOp.SetValueName(
                    newValueId,
                    { PT.PackageLocation.owner = loc.owner
                      modules = loc.modules.Split('.') |> Array.toList |> List.filter ((<>) "")
                      name = loc.name }
                  ) ]

              let! (inserted, _) = Inserts.insertAndApplyOps instanceID branchID (Some appliedBy) ops

              if inserted > 0L then
                // Mark the todo as applied
                let! _ = Queries.markTodoAsApplied todoId appliedBy todo.dependentItemId

                // Propagate to dependents of this item (compatible change since signature unchanged)
                let edit : EP.EditInfo =
                  { oldId = todo.dependentItemId
                    newId = newValueId
                    isBreaking = false }
                do! EP.processEdits branchID (Some appliedBy) [ edit ]

                return Ok newValueId
              else
                return Error "Failed to insert updated value"

          | "type" ->
            // Types don't have bodies with references, so they shouldn't need updates
            return Error "Type updates are not supported"

          | other ->
            return Error $"Unknown item type: {other}"
  }


/// Revert an applied update.
/// Marks the todo as pending again and deprecates the applied version.
let revertUpdate
  (branchID : Option<PT.BranchID>)
  (_revertedBy : uuid)
  (todoId : uuid)
  : Task<Result<unit, string>> =
  task {
    // Get the todo to understand what to revert
    let! todoOpt = Queries.getUpdateTodoById todoId

    match todoOpt with
    | None -> return Error "Todo not found"
    | Some todo ->
      if todo.status <> Queries.TodoStatus.applied then
        return Error $"Todo is not applied (status: {todo.status})"
      else
        match todo.previousDependentId with
        | None -> return Error "No previous dependent ID stored for revert"
        | Some _previousDependentId ->
          // Get the location of the dependent item
          // We need to find the current (applied) version to deprecate it
          let! locations = Queries.resolveAllLocations [ todo.dependentItemId ]

          match List.tryHead locations with
          | None ->
            // Location not found - this is an error, we can't properly revert
            return Error "Could not find dependent item location for revert"

          | Some loc ->
            // Deprecate any pending entries for this location
            // This effectively "reverts" to whatever was there before
            let! deprecatedCount =
              Queries.deprecatePendingEntriesForLocation
                loc.owner
                loc.modules
                loc.name
                loc.itemType
                branchID

            if deprecatedCount = 0 then
              return Error "No pending entries found to deprecate for revert"
            else
              // Mark the todo as pending again
              let! reverted = Queries.revertAppliedUpdate todoId

              if reverted then
                return Ok ()
              else
                return Error "Failed to revert todo status"
  }


/// Apply multiple updates at once.
/// Returns a list of results (one per todoId).
let applyUpdates
  (instanceID : Option<PT.InstanceID>)
  (branchID : Option<PT.BranchID>)
  (appliedBy : uuid)
  (todoIds : List<uuid>)
  : Task<List<uuid * Result<uuid, string>>> =
  task {
    let results = ResizeArray<uuid * Result<uuid, string>>()

    for todoId in todoIds do
      let! result = applyUpdate instanceID branchID appliedBy todoId
      results.Add((todoId, result))

    return results |> Seq.toList
  }


/// Revert multiple updates at once.
/// Returns a list of results (one per todoId).
let revertUpdates
  (branchID : Option<PT.BranchID>)
  (revertedBy : uuid)
  (todoIds : List<uuid>)
  : Task<List<uuid * Result<unit, string>>> =
  task {
    let results = ResizeArray<uuid * Result<unit, string>>()

    for todoId in todoIds do
      let! result = revertUpdate branchID revertedBy todoId
      results.Add((todoId, result))

    return results |> Seq.toList
  }
