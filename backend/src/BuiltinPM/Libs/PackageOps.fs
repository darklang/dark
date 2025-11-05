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


// TODO: review/reconsider the accessibility of these fns
let fns : List<BuiltInFn> =
  [ { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TCustomType(Ok packageOpTypeName, []))) "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Add package ops to the database and apply them to projections.
        Returns Ok(insertedCount) on success (duplicates are skipped), or Error with message on failure."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | _, _, _, [ branchID; DList(_vtTODO, ops) ] ->
          uply {
            try
              // Deserialize dvals
              let branchID = C2DT.Option.fromDT D.uuid branchID
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              // Insert ops with deduplication, get count of actually inserted ops
              let! insertedCount =
                LibPackageManager.Inserts.insertAndApplyOps branchID ops

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
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "since" TDateTime "" ]
      returnType = TList(TCustomType(Ok packageOpTypeName, []))
      description = "Get package ops created since the given timestamp."
      fn =
        function
        | _, _, _, [ branchID; DDateTime since ] ->
          uply {
            let branchID = C2DT.Option.fromDT D.uuid branchID

            let! ops = LibPackageManager.Queries.getOpsSince branchID since

            return
              DList(
                VT.customType PT2DT.PackageOp.typeName [],
                ops |> List.map PT2DT.PackageOp.toDT
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
