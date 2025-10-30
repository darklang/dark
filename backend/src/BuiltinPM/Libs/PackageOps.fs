module BuiltinPM.Libs.PackageOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization
module Builtin = LibExecution.Builtin
module C2DT = LibExecution.CommonToDarkTypes
module D = LibExecution.DvalDecoder

open Builtin.Shortcuts
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


/// Compute a content-addressed ID for a PackageOp by hashing its serialized content
/// TODO migrate this to LibExecution or maybe LibBinarySerialization or something
/// maybe we just have LibSerialization, and some of the things in that are Binary
let computeOpHash (op : PT.PackageOp) : System.Guid =
  use memoryStream = new System.IO.MemoryStream()
  use binaryWriter = new System.IO.BinaryWriter(memoryStream)

  // Serialize just the op content (without an ID)
  LibBinarySerialization.Serializers.PT.PackageOp.write binaryWriter op

  let opBytes = memoryStream.ToArray()
  let hashBytes = System.Security.Cryptography.SHA256.HashData(opBytes)

  // Take first 16 bytes to make a UUID
  System.Guid(hashBytes[0..15])


// TODO: Reconsider which of these functions should be public vs admin-only:
// - scmAddOps: Currently public but writes to DB - should this be admin-only?
// - scmGetRecentOps: Read-only, probably OK as public
// - scmGetOpsSince: Read-only, probably OK as public (used by sync)
let fns : List<BuiltInFn> =
  [ { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "ops" (TList(TVariable "packageOp")) "" ]
      returnType = TInt64
      description =
        "Add package ops to the database and apply them to projections. Returns count of actually inserted ops (skips duplicates). branchId None = main branch, Some = specific branch"
      fn =
        function
        | _, _, _, [ branchIdOpt; DList(_vtTODO, ops) ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdOpt

            // Convert each op from Dval to PT.PackageOp
            let ptOps =
              ops |> List.choose (fun opDval -> PT2DT.PackageOp.fromDT opDval)

            // Track how many ops were actually inserted (vs skipped as duplicates)
            let mutable insertedCount = 0

            // Serialize, insert, and playback each op
            for op in ptOps do
              // Use content-addressed hash as the ID
              let opId = computeOpHash op
              let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

              // INSERT OR IGNORE - if hash already exists, skip it (deduplication)
              // TODO: Move this INSERT logic to LibPackageManager.Inserts
              let! rowsAffected =
                Sql.query
                  """
                  INSERT OR IGNORE INTO package_ops (id, branch_id, op_blob, created_at, applied)
                  VALUES (@id, @branch_id, @op_blob, @created_at, @applied)
                  """
                |> Sql.parameters
                  [ "id", Sql.uuid opId
                    "branch_id",
                    (match branchId with
                     | Some id -> Sql.uuid id
                     | None -> Sql.dbnull)
                    "op_blob", Sql.bytes opBlob
                    "created_at", Sql.string (System.DateTime.UtcNow.ToString("o"))
                    "applied", Sql.bool true ]
                |> Sql.executeNonQueryAsync

              // Only apply op if it was actually inserted (not a duplicate)
              if rowsAffected > 0 then
                insertedCount <- insertedCount + 1
                do! LibPackageManager.PackageOpPlayback.applyOp branchId op

            return DInt64(int64 insertedCount)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetRecentOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "limit" TInt64 "" ]
      returnType = TList(TVariable "packageOp")
      description = "Get recent package ops from the database."
      fn =
        function
        | _, _, _, [ branchIdOpt; DInt64 limit ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdOpt

            let! ptOps = LibPackageManager.Queries.getRecentOps branchId limit
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
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "since" TDateTime "" ]
      returnType = TList(TVariable "packageOp")
      description =
        "Get package ops created since the specified datetime. branchId None = main branch, Some = specific branch"
      fn =
        function
        | _, _, _, [ branchIdOpt; DDateTime since ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdOpt

            let! ptOps = LibPackageManager.Queries.getOpsSince branchId since
            let ops = ptOps |> List.map PT2DT.PackageOp.toDT

            let opVT = LibExecution.ValueType.customType PT2DT.PackageOp.typeName []
            return DList(opVT, ops)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
