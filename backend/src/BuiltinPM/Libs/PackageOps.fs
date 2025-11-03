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

open Builtin.Shortcuts


// TODO: Reconsider which of these functions should be public vs admin-only:
// - scmAddOps: Currently public but writes to DB - should this be admin-only?
// - scmGetRecentOps: Read-only, probably OK as public
// - scmGetOpsSince: Read-only, probably OK as public (used by sync)
let fns : List<BuiltInFn> =
  [ { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchID" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TVariable "packageOp")) "" ]
      returnType = TInt64
      description =
        "Add package ops to the database and apply them to projections. Returns count of actually inserted ops (skips duplicates). branchID None = main branch, Some = specific branch"
      fn =
        function
        | _, _, _, [ branchID; DList(_vtTODO, ops) ] ->
          uply {
            let branchID = C2DT.Option.fromDT D.uuid branchID

            // Convert each op from Dval to PT.PackageOp
            let ptOps =
              ops |> List.choose (fun opDval -> PT2DT.PackageOp.fromDT opDval)

            // Insert ops with deduplication
            let! insertedCount =
              LibPackageManager.Inserts.insertOrIgnore branchID ptOps

            return DInt64(int64 insertedCount)
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

            let! ptOps = LibPackageManager.Queries.getRecentOps branchID limit
            let ops = ptOps |> List.map PT2DT.PackageOp.toDT

            let opVT = LibExecution.ValueType.customType PT2DT.PackageOp.typeName []
            return DList(opVT, ops)
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
            let! ptOps = LibPackageManager.Queries.getRecentOpsAllBranches limit
            let ops = ptOps |> List.map PT2DT.PackageOp.toDT

            let opVT = LibExecution.ValueType.customType PT2DT.PackageOp.typeName []
            return DList(opVT, ops)
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

            let! ptOps = LibPackageManager.Queries.getOpsSince branchID since
            let ops = ptOps |> List.map PT2DT.PackageOp.toDT

            let opVT = LibExecution.ValueType.customType PT2DT.PackageOp.typeName []
            return DList(opVT, ops)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
