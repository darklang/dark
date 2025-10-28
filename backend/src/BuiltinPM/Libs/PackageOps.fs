module BuiltinPM.Libs.PackageOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization
module Builtin = LibExecution.Builtin

open Builtin.Shortcuts
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


/// Compute a content-addressed ID for a PackageOp by hashing its serialized content
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
            let branchId =
              match branchIdOpt with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

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
              let! rowsAffected =
                Sql.query
                  """
                  INSERT OR IGNORE INTO package_ops (id, branch_id, op_blob, created_at)
                  VALUES (@id, @branch_id, @op_blob, @created_at)
                  """
                |> Sql.parameters
                  [ "id", Sql.uuid opId
                    "branch_id",
                    (match branchId with
                     | Some id -> Sql.uuid id
                     | None -> Sql.dbnull)
                    "op_blob", Sql.bytes opBlob
                    "created_at", Sql.string (System.DateTime.UtcNow.ToString("o")) ]
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
            let branchId =
              match branchIdOpt with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let! ops =
              Sql.query
                """
                SELECT id, op_blob
                FROM package_ops
                WHERE (branch_id IS NULL OR branch_id = @branch_id)
                ORDER BY created_at DESC
                LIMIT @limit
                """
              |> Sql.parameters
                [ "branch_id",
                  (match branchId with
                   | Some id -> Sql.uuid id
                   | None -> Sql.dbnull)
                  "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                let opId = read.uuid "id"
                let opBlob = read.bytes "op_blob"
                let op = BinarySerialization.PT.PackageOp.deserialize opId opBlob
                PT2DT.PackageOp.toDT op)

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
            let branchId =
              match branchIdOpt with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let sinceStr = LibExecution.DarkDateTime.toIsoString since

            let! ops =
              Sql.query
                """
                SELECT id, op_blob
                FROM package_ops
                WHERE (branch_id IS NULL OR branch_id = @branch_id)
                  AND created_at > @since
                ORDER BY created_at ASC
                """
              |> Sql.parameters
                [ "branch_id",
                  (match branchId with
                   | Some id -> Sql.uuid id
                   | None -> Sql.dbnull)
                  "since", Sql.string sinceStr ]
              |> Sql.executeAsync (fun read ->
                let opId = read.uuid "id"
                let opBlob = read.bytes "op_blob"
                let op = BinarySerialization.PT.PackageOp.deserialize opId opBlob
                PT2DT.PackageOp.toDT op)

            let opVT = LibExecution.ValueType.customType PT2DT.PackageOp.typeName []
            return DList(opVT, ops)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
