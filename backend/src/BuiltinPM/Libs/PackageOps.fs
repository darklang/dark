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
let fns (pm : PT.PackageManager) : List<BuiltInFn> =
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

              // Insert ops with deduplication
              do! pm.applyOps (branchID, ops)

              // TODO: Return actual inserted count (not total ops count)
              // Currently applyOps doesn't return count, so we return attempted count
              return resultOk (DInt64(int64 (List.length ops)))
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
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "since" TDateTime "" ]
      returnType = TList(TCustomType(Ok packageOpTypeName, []))
      description = "Get package ops created since the given timestamp."
      fn =
        function
        | _, _, _, [ branchID; DDateTime since ] ->
          uply {
            let branchID = C2DT.Option.fromDT D.uuid branchID

            let! ops = LibPackageManager.PT.SQL.Sync.getOpsSince branchID since

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


let builtins (pm : PT.PackageManager) : Builtins =
  LibExecution.Builtin.make [] (fns pm)
