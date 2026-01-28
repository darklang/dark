module BuiltinPM.Libs.PackageOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Builtin = LibExecution.Builtin
module C2DT = LibExecution.CommonToDarkTypes
module D = LibExecution.DvalDecoder
module VT = LibExecution.ValueType
module PackageIDs = LibExecution.PackageIDs
module Dval = LibExecution.Dval

open Builtin.Shortcuts


let packageOpTypeName =
  FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.packageOp

let packageOpBatchTypeName =
  FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.packageOpBatch

let pendingChangeTypeName =
  FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.pendingChange


// TODO: review/reconsider the accessibility of these fns
let fns : List<BuiltInFn> =
  [ { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "instanceID" (TypeReference.option TUuid) ""
          Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "createdBy" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TCustomType(Ok packageOpTypeName, []))) "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Add package ops to the database and apply them to projections.
        Pass None for instanceID for local ops, or Some(uuid) for ops from remote instances.
        Pass createdBy to track who created the ops (for approval workflow).
        Returns the number of inserted ops on success (duplicates are skipped), or an error message on failure."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | _, _, _, [ instanceID; branchID; createdBy; DList(_vtTODO, ops) ] ->
          uply {
            try
              // Deserialize dvals
              let branchID = C2DT.Option.fromDT D.uuid branchID
              let instanceID = C2DT.Option.fromDT D.uuid instanceID
              let createdBy = C2DT.Option.fromDT D.uuid createdBy
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              // Insert ops with deduplication, get count of actually inserted ops
              let! (insertedCount, _appliedOps) =
                LibPackageManager.Inserts.insertAndApplyOps
                  instanceID
                  branchID
                  createdBy
                  ops

              // Clear caches so new data is visible immediately
              LibPackageManager.Caching.clearAllCaches ()

              return resultOk (DInt64 insertedCount)
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetRecentOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "limit" TInt64 "" ]
      returnType = TList(TCustomType(Ok packageOpTypeName, []))
      description = "Get recent package ops from the database."
      fn =
        function
        | _, _, _, [ branchID; DInt64 limit ] ->
          uply {
            let branchID = C2DT.Option.fromDT D.uuid branchID

            let! ops = LibPackageManager.Queries.getRecentOps branchID limit

            return
              DList(
                VT.customType PT2DT.PackageOp.typeName [],
                ops |> List.map PT2DT.PackageOp.toDT
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetRecentOpsAllBranches" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt64 "" ]
      returnType = TList(TCustomType(Ok packageOpTypeName, []))
      description = "Get recent package ops from ALL branches (no branch filter)."
      fn =
        function
        | _, _, _, [ DInt64 limit ] ->
          uply {
            let! ops = LibPackageManager.Queries.getRecentOpsAllBranches limit
            return
              DList(
                VT.customType PT2DT.PackageOp.typeName [],
                ops |> List.map PT2DT.PackageOp.toDT
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApproveItem" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "itemId" TUuid ""
          Param.make "reviewerId" TUuid "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Approve an item by creating and applying an ApproveItem op.
        Returns the number of ops inserted (1 on success, 0 if duplicate)."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | _, _, _, [ branchID; DUuid itemId; DUuid reviewerId ] ->
          uply {
            try
              let branchID = C2DT.Option.fromDT D.uuid branchID
              let op = PT.PackageOp.ApproveItem(itemId, branchID, reviewerId)

              let! (insertedCount, _appliedOps) =
                LibPackageManager.Inserts.insertAndApplyOps
                  None // local op, not from sync
                  branchID
                  (Some reviewerId)
                  [ op ]

              return resultOk (DInt64 insertedCount)
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmRejectItem" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "itemId" TUuid ""
          Param.make "reviewerId" TUuid ""
          Param.make "reason" TString "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Reject an item by creating and applying a RejectItem op.
        Returns the number of ops inserted (1 on success, 0 if duplicate)."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | _, _, _, [ branchID; DUuid itemId; DUuid reviewerId; DString reason ] ->
          uply {
            try
              let branchID = C2DT.Option.fromDT D.uuid branchID
              let op = PT.PackageOp.RejectItem(itemId, branchID, reviewerId, reason)

              let! (insertedCount, _appliedOps) =
                LibPackageManager.Inserts.insertAndApplyOps
                  None // local op, not from sync
                  branchID
                  (Some reviewerId)
                  [ op ]

              return resultOk (DInt64 insertedCount)
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetOpsSince" 0
      typeParams = []
      parameters =
        [ Param.make "targetInstanceID" (TypeReference.option TUuid) ""
          Param.make "since" TDateTime "" ]
      returnType = TList(TCustomType(Ok packageOpBatchTypeName, []))
      description =
        "Get all package ops (from ALL branches) created since the given timestamp, grouped by branch and instance.
        Optionally filters for a target instance (pass None to get all ops, or Some(uuid) to exclude ops from that target instance).
        Returns a list of PackageOpBatch, where each batch contains ops from one branch with the same instanceID."
      fn =
        function
        | _, _, _, [ targetInstanceID; DDateTime since ] ->
          uply {
            let targetID = C2DT.Option.fromDT D.uuid targetInstanceID

            let! opsWithMetadata =
              LibPackageManager.Queries.getAllOpsSince targetID since

            // Group by (branchID, instanceID)
            let grouped =
              opsWithMetadata
              |> List.groupBy (fun (_, branchID, instanceID) ->
                (branchID, instanceID))
              |> Map.toList

            // Convert each group to a PackageOpBatch record
            let batches =
              grouped
              |> List.map (fun ((branchID, instanceID), ops) ->
                let opsList =
                  ops
                  |> List.map (fun (op, _, _) -> PT2DT.PackageOp.toDT op)
                  |> fun opDvals ->
                      DList(VT.customType packageOpTypeName [], opDvals)

                let fields =
                  [ ("branchID", branchID |> Option.map DUuid |> Dval.option KTUuid)
                    ("instanceID",
                     instanceID |> Option.map DUuid |> Dval.option KTUuid)
                    ("ops", opsList) ]
                  |> Map

                DRecord(packageOpBatchTypeName, packageOpBatchTypeName, [], fields))

            return DList(VT.customType packageOpBatchTypeName [], batches)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Edit-aware version of scmAddOps that detects edits and creates todos
    { name = fn "scmAddOpsWithEditDetection" 0
      typeParams = []
      parameters =
        [ Param.make "instanceID" (TypeReference.option TUuid) ""
          Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "createdBy" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TCustomType(Ok packageOpTypeName, []))) "" ]
      returnType =
        TypeReference.result
          (TTuple(
            TInt64,
            TList(TTuple(TUuid, TUuid, [ TBool ])),
            [ TList(TCustomType(Ok packageOpTypeName, [])) ]
          ))
          TString
      description =
        "Add package ops with edit detection (immutability model). Every edit creates a NEW UUID -
        the name then points to the new version. Old versions still exist.
        Returns (insertedCount, edits, appliedOps) where edits is a list of (oldId, newId, isBreaking) tuples
        and appliedOps are the ops that were actually applied (duplicates excluded).
        Breaking changes (signature changed) create todos for same-owner dependents.
        Compatible changes auto-propagate via name resolution."
      fn =
        let editTupleVT = VT.tuple VT.uuid VT.uuid [ VT.bool ]
        let opsListVT = VT.list (VT.customType packageOpTypeName [])
        let resultOk =
          Dval.resultOk
            (KTTuple(VT.int64, VT.list editTupleVT, [ opsListVT ]))
            KTString
        let resultError =
          Dval.resultError
            (KTTuple(VT.int64, VT.list editTupleVT, [ opsListVT ]))
            KTString
        (function
        | _, _, _, [ instanceID; branchID; createdBy; DList(_vtTODO, ops) ] ->
          uply {
            try
              let branchID = C2DT.Option.fromDT D.uuid branchID
              let instanceID = C2DT.Option.fromDT D.uuid instanceID
              let createdBy = C2DT.Option.fromDT D.uuid createdBy
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              // Stabilize ops against persistent PM to detect edits
              // Pass createdBy as accountID for visibility rules
              let! (stabilizedOps, edits) =
                LibPackageManager.PackageManager.stabilizeOpsWithEditDetection
                  LibPackageManager.PackageManager.pt
                  createdBy
                  branchID
                  ops

              // Insert stabilized ops, get back only the ones that were actually applied
              let! (insertedCount, appliedOps) =
                LibPackageManager.Inserts.insertAndApplyOps
                  instanceID
                  branchID
                  createdBy
                  stabilizedOps

              // Process edits (record in DB, create todos for breaking changes)
              if not (List.isEmpty edits) then
                do!
                  LibPackageManager.EditPropagation.processEdits
                    branchID
                    createdBy
                    edits

              // Clear caches so new data is visible immediately
              LibPackageManager.Caching.clearAllCaches ()

              // Convert edits to dvals
              let editDvals =
                edits
                |> List.map (fun edit ->
                  DTuple(
                    DUuid edit.oldId,
                    DUuid edit.newId,
                    [ DBool edit.isBreaking ]
                  ))

              // Convert applied ops to dvals
              let appliedOpsDvals = appliedOps |> List.map PT2DT.PackageOp.toDT

              return
                resultOk (
                  DTuple(
                    DInt64 insertedCount,
                    DList(editTupleVT, editDvals),
                    [ DList(VT.customType packageOpTypeName [], appliedOpsDvals) ]
                  )
                )
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmClearAllCaches" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUnit
      description =
        "Clears all PackageManager caches. Call this after switching accounts to ensure fresh data."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            LibPackageManager.Caching.clearAllCaches ()
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Pending changes API
    { name = fn "pmGetPendingChanges" 0
      typeParams = []
      parameters =
        [ Param.make "accountId" TUuid "The account to get pending changes for"
          Param.make "branchId" (TypeReference.option TUuid) "Optional branch filter" ]
      returnType = TList(TCustomType(Ok pendingChangeTypeName, []))
      description =
        "Get all pending changes for an account. Returns a list of pending items that can be reverted."
      fn =
        (function
        | _, _, _, [ DUuid accountId; branchId ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchId

            let! pendingChanges =
              LibPackageManager.Queries.getPendingChangesForAccount accountId branchId

            let pendingChangeVT = VT.customType pendingChangeTypeName []

            let dvals =
              pendingChanges
              |> List.map (fun pc ->
                let fields =
                  [ ("itemId", DUuid pc.itemId)
                    ("itemType", DString pc.itemType)
                    ("owner", DString pc.owner)
                    ("modules", DString pc.modules)
                    ("name", DString pc.name)
                    ("createdAt", DString pc.createdAt)
                    ("locationId", DUuid pc.locationId) ]
                DRecord(pendingChangeTypeName, pendingChangeTypeName, [], Map.ofList fields))

            return DList(pendingChangeVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmRevertPendingChange" 0
      typeParams = []
      parameters =
        [ Param.make "locationId" TUuid "The location ID of the pending change to revert"
          Param.make "revertedBy" TUuid "The account ID of who is reverting" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Revert a pending change by deprecating its location entry.
        Name resolution will fall back to the last approved version.
        Returns Ok() if successful, or Error with message if the location wasn't found or already deprecated."
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, _, [ DUuid locationId; DUuid revertedBy ] ->
          uply {
            let! success =
              LibPackageManager.Queries.revertPendingChange locationId revertedBy

            if success then
              // Clear caches so the revert takes effect immediately
              LibPackageManager.Caching.clearAllCaches ()
              return resultOk DUnit
            else
              return
                resultError (
                  DString
                    "Could not revert: location not found, not pending, or already deprecated"
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
