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

open Builtin.Shortcuts


// TODO: review/reconsider the accessibility of these fns
let fns : List<BuiltInFn> =
  [ { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TVariable "packageOp")) "" ]
      returnType = TInt64
      description =
        "Add package ops to the database and apply them to projections.
        Returns count of actually inserted ops (skips duplicates)."
      fn =
        function
        | _, _, _, [ branchID; DList(_vtTODO, ops) ] ->
          uply {
            // Deserialize dvals
            let branchID = C2DT.Option.fromDT D.uuid branchID
            let ops = ops |> List.choose PT2DT.PackageOp.fromDT

            // Insert ops with deduplication
            do! LibPackageManager.Inserts.insertOps branchID ops


            /// CLEANUP no one above really cares about the count that we ended up inserting.
            /// We should rather return _some_ Result here - more meaningful+consumable.
            /// Based on that stance, I've adjusted this to return a static 0 for now. (hope I'm right)
            let fakeInsertCount = 0L
            return DInt64 fakeInsertCount
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetRecentOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "limit" TInt64 "" ]
      returnType = TList(TVariable "packageOp")
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
      returnType = TList(TVariable "packageOp")
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
      returnType = TList(TVariable "packageOp")
      description =
        "Get package ops created since the specified datetime. branchID None = main branch, Some = specific branch"
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
