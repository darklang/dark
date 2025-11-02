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
/// branchID: None = main/merged, Some(id) = branch-specific
let insertOps
  (branchID : Option<PT.BranchID>)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    if List.isEmpty ops then return ()

    // Step 1: Insert ops into package_ops table (source of truth)
    let insertStatements =
      ops
      |> List.map (fun op ->
        let opId = computeOpHash op
        let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

        let sql =
          """
          INSERT OR IGNORE INTO package_ops (id, branch_id, op_blob, applied)
          VALUES (@id, @branch_id, @op_blob, @applied)
          """

        let parameters =
          [ "id", Sql.uuid opId
            "branch_id",
            (match branchID with
             | Some id -> Sql.uuid id
             | None -> Sql.dbnull)
            "op_blob", Sql.bytes opBlob
            "applied", Sql.bool true ]

        (sql, [ parameters ]))

    insertStatements |> Sql.executeTransactionSync |> ignore<List<int>>

    // Step 2: Apply ops to projection tables (types, values, functions, locations)
    do! LibPackageManager.PackageOpPlayback.applyOps branchID ops
  }


/// Insert PackageOps with deduplication (INSERT OR IGNORE).
/// Uses content-addressed hashing to skip duplicate ops.
/// Returns count of actually inserted ops (excludes duplicates).
let insertOrIgnore
  (branchID : Option<PT.BranchID>)
  (ops : List<PT.PackageOp>)
  : Task<int> =
  task {
    if List.isEmpty ops then
      return 0
    else
      let mutable insertedCount = 0

      // Process each op individually to track which ones were actually inserted
      for op in ops do
        // Use content-addressed hash as the ID for deduplication
        let opId = computeOpHash op
        let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

        // INSERT OR IGNORE - if hash already exists, skip it
        let! rowsAffected =
          Sql.query
            """
            INSERT OR IGNORE INTO package_ops (id, branch_id, op_blob, created_at, applied)
            VALUES (@id, @branch_id, @op_blob, @created_at, @applied)
            """
          |> Sql.parameters
            [ "id", Sql.uuid opId
              "branch_id",
              (match branchID with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull)
              "op_blob", Sql.bytes opBlob
              "created_at", Sql.string (System.DateTime.UtcNow.ToString("o"))
              "applied", Sql.bool true ]
          |> Sql.executeNonQueryAsync

        // Only apply op if it was actually inserted (not a duplicate)
        if rowsAffected > 0 then
          insertedCount <- insertedCount + 1
          do! LibPackageManager.PackageOpPlayback.applyOp branchID op

      return insertedCount
  }
