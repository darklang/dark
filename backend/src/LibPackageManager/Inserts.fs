module LibPackageManager.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization


/// Compute a content-addressed ID for a PackageOp by hashing its serialized content
///
/// TODO this is really hacky.
/// honestly we should
/// - make hashing more legit.
/// - rebrand LibBinarySerialization as LibSerialization
/// - migrate any remaining hacky JSON serialization things there
///   (if LibExecution needs them, maybe ExecutionState needs to/from json fns to be part of that context)
/// - put all sorts of Hashers there -
let computeOpHash (op : PT.PackageOp) : System.Guid =
  use memoryStream = new System.IO.MemoryStream()
  use binaryWriter = new System.IO.BinaryWriter(memoryStream)

  // Serialize just the op content (without an ID)
  LibBinarySerialization.Serializers.PT.PackageOp.write binaryWriter op

  let opBytes = memoryStream.ToArray()
  let hashBytes = System.Security.Cryptography.SHA256.HashData(opBytes)

  // Take first 16 bytes to make a UUID
  System.Guid(hashBytes[0..15])


/// Insert PackageOps into the package_ops table and apply them to projection tables
/// Returns the count of ops actually inserted (duplicates are skipped via INSERT OR IGNORE)

// CLEANUP: The 'applied' flag is currently always set to true and all ops are applied immediately
let insertAndApplyOps
  (instanceID : Option<System.Guid>)
  (branchID : Option<PT.BranchID>)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      // CLEANUP this should either
      // - be made to be one big transaction
      // OR
      // - be inserted as applied:false, and have 'step 2' trigger them being toggled

      // Step 1: Insert ops into package_ops table (source of truth)
      let insertStatements =
        ops
        |> List.map (fun op ->
          let opId = computeOpHash op
          let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

          let sql =
            """
            INSERT OR IGNORE INTO package_ops (id, branch_id, op_blob, applied, instance_id)
            VALUES (@id, @branch_id, @op_blob, @applied, @instance_id)
            """

          let parameters =
            [ "id", Sql.uuid opId
              "branch_id",
              (match branchID with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull)
              "op_blob", Sql.bytes opBlob
              "applied", Sql.bool true
              "instance_id",
              (match instanceID with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull) ]

          (sql, [ parameters ]))

      let rowsAffected = insertStatements |> Sql.executeTransactionSync

      // Count how many ops were actually inserted (vs skipped as duplicates)
      let insertedCount = rowsAffected |> List.sumBy int64

      // Step 2: Apply ops to projection tables (types, values, functions, locations)
      // Only apply ops that were actually inserted
      let opsToApply =
        List.zip ops rowsAffected
        |> List.filter (fun (_, affected) -> affected > 0)
        |> List.map fst

      do! PackageOpPlayback.applyOps branchID opsToApply

      return insertedCount
  }
