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


// TODO: review/reconsider the accessibility of these fns
let fns : List<BuiltInFn> =
  [ { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "instanceID" (TypeReference.option TUuid) ""
          Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TCustomType(Ok packageOpTypeName, []))) "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Add package ops to the database and apply them to projections.
        Pass None for instanceID for local ops, or Some(uuid) for ops from remote instances.
        Returns the number of inserted ops on success (duplicates are skipped), or an error message on failure."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | _, _, _, [ instanceID; branchID; DList(_vtTODO, ops) ] ->
          uply {
            try
              // Deserialize dvals
              let branchID = C2DT.Option.fromDT D.uuid branchID
              let instanceID = C2DT.Option.fromDT D.uuid instanceID
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              // Insert ops with deduplication, get count of actually inserted ops
              let! insertedCount =
                LibPackageManager.Inserts.insertAndApplyOps instanceID branchID ops

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
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
