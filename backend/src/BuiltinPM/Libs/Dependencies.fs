/// Builtin functions for dependency tracking between package items.
/// Enables "what calls this?" and "what does this call?" queries.
/// Shows approved items + caller's own pending items.
module BuiltinPM.Libs.Dependencies

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module PackageIDs = LibExecution.PackageIDs


let tupleVT = VT.tuple VT.uuid VT.string []

let todoTypeName = FQTypeName.fqPackage PackageIDs.Type.SCM.Todos.todo
let availableUpdateTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Todos.availableUpdate
let appliedUpdateTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Todos.appliedUpdate


let fns : List<BuiltInFn> =
  [ { name = fn "dependenciesGetDependents" 0
      typeParams = []
      parameters =
        [ Param.make
            "accountID"
            (TypeReference.option TUuid)
            "Account ID for visibility filtering"
          Param.make
            "branchID"
            (TypeReference.option TUuid)
            "Branch ID for visibility filtering"
          Param.make "targetId" TUuid "The UUID to find dependents for" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Returns items that reference the given UUID (reverse dependencies). Shows approved + caller's pending items."
      fn =
        (function
        | _, _, _, [ accountID; branchID; DUuid targetId ] ->
          uply {
            let accountID =
              match accountID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let branchID =
              match branchID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let! results =
              LibPackageManager.Queries.getDependents accountID branchID targetId

            let dvals =
              results
              |> List.map (fun ref -> DTuple(DUuid ref.itemId, DString ref.kind, []))
            return DList(tupleVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dependenciesGetDependencies" 0
      typeParams = []
      parameters =
        [ Param.make "sourceId" TUuid "The UUID to find dependencies for" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Returns all UUIDs that the given item references (forward dependencies)"
      fn =
        (function
        | _, _, _, [ DUuid sourceId ] ->
          uply {
            let! results = LibPackageManager.Queries.getDependencies sourceId
            let dvals =
              results
              |> List.map (fun ref -> DTuple(DUuid ref.itemId, DString ref.kind, []))
            return DList(tupleVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dependenciesGetDependentsBatch" 0
      typeParams = []
      parameters =
        [ Param.make
            "accountID"
            (TypeReference.option TUuid)
            "Account ID for visibility filtering"
          Param.make
            "branchID"
            (TypeReference.option TUuid)
            "Branch ID for visibility filtering"
          Param.make "targetIds" (TList TUuid) "List of UUIDs to find dependents for" ]
      returnType = TList(TTuple(TUuid, TUuid, [ TString ]))
      description =
        "Batch lookup of dependents. Returns (dependsOnId, itemId, kind) tuples. Shows approved + caller's pending items."
      fn =
        (function
        | _, _, _, [ accountID; branchID; DList(_, targetIds) ] ->
          uply {
            let accountID =
              match accountID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let branchID =
              match branchID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let ids =
              targetIds
              |> List.choose (fun dval ->
                match dval with
                | DUuid id -> Some id
                | _ -> None)

            let! results =
              LibPackageManager.Queries.getDependentsBatch accountID branchID ids

            let resultVT = VT.tuple VT.uuid VT.uuid [ VT.string ]

            let dvals =
              results
              |> List.map (fun dep ->
                DTuple(
                  DUuid dep.dependsOnId,
                  DUuid dep.itemId,
                  [ DString dep.kind ]
                ))

            return DList(resultVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dependenciesResolveNames" 0
      typeParams = []
      parameters =
        [ Param.make
            "accountID"
            (TypeReference.option TUuid)
            "Account ID for visibility"
          Param.make
            "branchID"
            (TypeReference.option TUuid)
            "Branch ID for visibility"
          Param.make "itemIds" (TList TUuid) "List of item UUIDs to resolve" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Resolve UUIDs to display names. Returns (itemId, displayName) tuples."
      fn =
        (function
        | _, _, _, [ accountID; branchID; DList(_, itemIds) ] ->
          uply {
            let accountID =
              match accountID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let branchID =
              match branchID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let ids =
              itemIds
              |> List.choose (fun item ->
                match item with
                | DUuid id -> Some id
                | _ -> None)

            let! results =
              LibPackageManager.Queries.resolveLocations accountID branchID ids

            let dvals =
              results
              |> List.map (fun loc ->
                let displayName =
                  if System.String.IsNullOrEmpty(loc.modules) then
                    $"{loc.owner}.{loc.name}"
                  else
                    $"{loc.owner}.{loc.modules}.{loc.name}"
                DTuple(DUuid loc.itemId, DString displayName, []))

            return DList(VT.tuple VT.uuid VT.string [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // --- Breaking change todos ---

    { name = fn "todosGetPending" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "Account ID to get todos for" ]
      returnType = TList(TCustomType(Ok todoTypeName, []))
      description =
        "Returns pending dependency change todos for the given account. Each todo represents a dependency that was modified."
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! results =
              LibPackageManager.Queries.getPendingTodosForAccount accountID

            let dvals =
              results
              |> List.map (fun todo ->
                let fields =
                  [ ("id", DUuid todo.id)
                    ("dependentItemId", DUuid todo.dependentItemId)
                    ("oldItemId", DUuid todo.oldItemId)
                    ("newItemId", DUuid todo.newItemId)
                    ("status", DString todo.status)
                    ("createdAt", DString todo.createdAt) ]
                  |> Map

                DRecord(todoTypeName, todoTypeName, [], fields))

            return DList(VT.customType todoTypeName [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "todosGetAll" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "Account ID to get todos for" ]
      returnType = TList(TCustomType(Ok todoTypeName, []))
      description =
        "Returns all dependency change todos for the given account (including resolved/dismissed)."
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! results = LibPackageManager.Queries.getTodosForAccount accountID

            let dvals =
              results
              |> List.map (fun todo ->
                let fields =
                  [ ("id", DUuid todo.id)
                    ("dependentItemId", DUuid todo.dependentItemId)
                    ("oldItemId", DUuid todo.oldItemId)
                    ("newItemId", DUuid todo.newItemId)
                    ("status", DString todo.status)
                    ("createdAt", DString todo.createdAt) ]
                  |> Map

                DRecord(todoTypeName, todoTypeName, [], fields))

            return DList(VT.customType todoTypeName [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "todosResolve" 0
      typeParams = []
      parameters =
        [ Param.make "todoId" TUuid "The todo ID to resolve"
          Param.make "resolvedBy" TUuid "Account ID resolving the todo" ]
      returnType = TBool
      description =
        "Marks a todo as resolved. Returns true if successful, false if todo not found or already resolved."
      fn =
        (function
        | _, _, _, [ DUuid todoId; DUuid resolvedBy ] ->
          uply {
            let! success = LibPackageManager.Queries.resolveTodo todoId resolvedBy
            return DBool success
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "todosDismiss" 0
      typeParams = []
      parameters =
        [ Param.make "todoId" TUuid "The todo ID to dismiss"
          Param.make "dismissedBy" TUuid "Account ID dismissing the todo" ]
      returnType = TBool
      description =
        "Dismisses a todo without resolving the underlying issue. Returns true if successful."
      fn =
        (function
        | _, _, _, [ DUuid todoId; DUuid dismissedBy ] ->
          uply {
            let! success = LibPackageManager.Queries.dismissTodo todoId dismissedBy
            return DBool success
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // --- Available updates (compatible changes) ---

    { name = fn "todosGetAvailableUpdates" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "Account ID to get updates for" ]
      returnType = TList(TCustomType(Ok availableUpdateTypeName, []))
      description =
        "Returns available compatible updates for the given account. These can be automatically applied."
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! results =
              LibPackageManager.Queries.getAvailableUpdatesForAccount accountID

            let dvals =
              results
              |> List.map (fun update ->
                let fields =
                  [ ("id", DUuid update.id)
                    ("dependentItemId", DUuid update.dependentItemId)
                    ("dependentItemType", DString update.dependentItemType)
                    ("oldItemId", DUuid update.oldItemId)
                    ("newItemId", DUuid update.newItemId)
                    ("changeType", DString update.changeType)
                    ("status", DString update.status)
                    ("createdAt", DString update.createdAt) ]
                  |> Map

                DRecord(availableUpdateTypeName, availableUpdateTypeName, [], fields))

            return DList(VT.customType availableUpdateTypeName [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "todosGetAppliedUpdates" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "Account ID to get applied updates for" ]
      returnType = TList(TCustomType(Ok appliedUpdateTypeName, []))
      description =
        "Returns recently applied updates for the given account. These can be reverted."
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! results =
              LibPackageManager.Queries.getAppliedUpdatesForAccount accountID

            let dvals =
              results
              |> List.map (fun update ->
                let appliedAt =
                  match update.appliedAt with
                  | Some at -> DString at
                  | None -> DString ""

                let previousDependentId =
                  update.previousDependentId |> Option.map DUuid |> Dval.option KTUuid

                let fields =
                  [ ("id", DUuid update.id)
                    ("dependentItemId", DUuid update.dependentItemId)
                    ("dependentItemType", DString update.dependentItemType)
                    ("oldItemId", DUuid update.oldItemId)
                    ("newItemId", DUuid update.newItemId)
                    ("changeType", DString update.changeType)
                    ("status", DString update.status)
                    ("createdAt", DString update.createdAt)
                    ("appliedAt", appliedAt)
                    ("previousDependentId", previousDependentId) ]
                  |> Map

                DRecord(appliedUpdateTypeName, appliedUpdateTypeName, [], fields))

            return DList(VT.customType appliedUpdateTypeName [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "todosGetPendingBreaking" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "Account ID to get breaking todos for" ]
      returnType = TList(TCustomType(Ok todoTypeName, []))
      description =
        "Returns pending breaking change todos for the given account. These require manual fixes."
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! results =
              LibPackageManager.Queries.getPendingBreakingTodosForAccount accountID

            let dvals =
              results
              |> List.map (fun todo ->
                let fields =
                  [ ("id", DUuid todo.id)
                    ("dependentItemId", DUuid todo.dependentItemId)
                    ("dependentItemType", DString todo.dependentItemType)
                    ("oldItemId", DUuid todo.oldItemId)
                    ("newItemId", DUuid todo.newItemId)
                    ("status", DString todo.status)
                    ("createdAt", DString todo.createdAt) ]
                  |> Map

                DRecord(todoTypeName, todoTypeName, [], fields))

            return DList(VT.customType todoTypeName [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "todosApplyUpdates" 0
      typeParams = []
      parameters =
        [ Param.make "instanceID" (TypeReference.option TUuid) "Instance ID for ops"
          Param.make "branchID" (TypeReference.option TUuid) "Branch ID for ops"
          Param.make "appliedBy" TUuid "Account ID applying the updates"
          Param.make "todoIds" (TList TUuid) "List of todo IDs to apply" ]
      returnType = TList(TTuple(TUuid, TypeReference.result TUuid TString, []))
      description =
        "Apply compatible updates. Returns list of (todoId, Result<newItemId, errorMessage>)."
      fn =
        (function
        | _, _, _, [ instanceID; branchID; DUuid appliedBy; DList(_, todoIds) ] ->
          uply {
            let instanceID =
              match instanceID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let branchID =
              match branchID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let ids =
              todoIds
              |> List.choose (fun dval ->
                match dval with
                | DUuid id -> Some id
                | _ -> None)

            let! results =
              LibPackageManager.UpdateApplicator.applyUpdates instanceID branchID appliedBy ids

            let resultVT =
              VT.tuple VT.uuid (VT.customType Dval.resultType [ VT.uuid; VT.string ]) []

            let dvals =
              results
              |> List.map (fun (todoId, result) ->
                let resultDval =
                  match result with
                  | Ok newId -> Dval.resultOk KTUuid KTString (DUuid newId)
                  | Error msg -> Dval.resultError KTUuid KTString (DString msg)
                DTuple(DUuid todoId, resultDval, []))

            return DList(resultVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "todosRevertUpdates" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) "Branch ID for ops"
          Param.make "revertedBy" TUuid "Account ID reverting the updates"
          Param.make "todoIds" (TList TUuid) "List of todo IDs to revert" ]
      returnType = TList(TTuple(TUuid, TypeReference.result TUnit TString, []))
      description =
        "Revert applied updates. Returns list of (todoId, Result<(), errorMessage>)."
      fn =
        (function
        | _, _, _, [ branchID; DUuid revertedBy; DList(_, todoIds) ] ->
          uply {
            let branchID =
              match branchID with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let ids =
              todoIds
              |> List.choose (fun dval ->
                match dval with
                | DUuid id -> Some id
                | _ -> None)

            let! results =
              LibPackageManager.UpdateApplicator.revertUpdates branchID revertedBy ids

            let resultVT =
              VT.tuple VT.uuid (VT.customType Dval.resultType [ VT.unit; VT.string ]) []

            let dvals =
              results
              |> List.map (fun (todoId, result) ->
                let resultDval =
                  match result with
                  | Ok () -> Dval.resultOk KTUnit KTString DUnit
                  | Error msg -> Dval.resultError KTUnit KTString (DString msg)
                DTuple(DUuid todoId, resultDval, []))

            return DList(resultVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
