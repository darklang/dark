module LibPackageManager.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

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


/// Insert PackageOps into the package_ops table and apply them to projection tables.
/// commitId = None means WIP (commit_id = NULL), Some id means committed
/// Returns the count of ops actually inserted (duplicates are skipped via INSERT OR IGNORE)
let insertAndApplyOps
  (commitId : Option<System.Guid>)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      let isWip = Option.isNone commitId

      // Step 1: Insert ops into package_ops table (source of truth)
      let insertStatements =
        ops
        |> List.map (fun op ->
          let opId = computeOpHash op
          let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

          let sql =
            """
            INSERT OR IGNORE INTO package_ops (id, op_blob, is_wip, applied, commit_id)
            VALUES (@id, @op_blob, @is_wip, @applied, @commit_id)
            """

          let parameters =
            [ "id", Sql.uuid opId
              "op_blob", Sql.bytes opBlob
              "is_wip", Sql.int (if isWip then 1 else 0)
              "applied", Sql.bool true
              "commit_id", Sql.uuidOrNone commitId ]

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

      do! PackageOpPlayback.applyOps commitId opsToApply

      return insertedCount
  }


/// Create a new commit and insert ops with that commit_id
/// Returns the commit ID
let insertAndApplyOpsWithCommit
  (message : string)
  (ops : List<PT.PackageOp>)
  : Task<System.Guid> =
  task {
    let commitId = System.Guid.NewGuid()

    // Create the commit record
    do!
      Sql.query
        """
        INSERT INTO commits (id, message, created_at)
        VALUES (@id, @message, datetime('now'))
        """
      |> Sql.parameters [ "id", Sql.uuid commitId; "message", Sql.string message ]
      |> Sql.executeStatementAsync

    // Insert ops with the commit_id
    let! _ = insertAndApplyOps (Some commitId) ops

    return commitId
  }


/// Insert ops as WIP (commit_id = NULL)
/// Returns count of inserted ops
let insertAndApplyOpsAsWip (ops : List<PT.PackageOp>) : Task<int64> =
  insertAndApplyOps None ops


/// Commit all WIP ops by creating a new commit and assigning commit_id
/// Returns the commit ID on success
let commitWipOps (message : string) : Task<Result<System.Guid, string>> =
  task {
    try
      // Check if there are any WIP ops
      let! wipCount =
        Sql.query "SELECT COUNT(*) as cnt FROM package_ops WHERE commit_id IS NULL"
        |> Sql.executeAsync (fun read -> read.int64 "cnt")

      match wipCount with
      | [ 0L ] -> return Error "Nothing to commit"
      | [ _ ] ->
        let commitId = System.Guid.NewGuid()

        // Create the commit record
        do!
          Sql.query
            """
            INSERT INTO commits (id, message, created_at)
            VALUES (@id, @message, datetime('now'))
            """
          |> Sql.parameters
            [ "id", Sql.uuid commitId; "message", Sql.string message ]
          |> Sql.executeStatementAsync

        // Update all WIP ops to point to this commit
        do!
          Sql.query
            """
            UPDATE package_ops
            SET commit_id = @commit_id, is_wip = 0
            WHERE commit_id IS NULL
            """
          |> Sql.parameters [ "commit_id", Sql.uuid commitId ]
          |> Sql.executeStatementAsync

        // Update all WIP locations to point to this commit
        do!
          Sql.query
            """
            UPDATE locations
            SET commit_id = @commit_id, is_wip = 0
            WHERE commit_id IS NULL
            """
          |> Sql.parameters [ "commit_id", Sql.uuid commitId ]
          |> Sql.executeStatementAsync

        return Ok commitId
      | _ -> return Error "Unexpected query result"
    with ex ->
      return Error ex.Message
  }


/// Discard all WIP ops by deleting them and their effects
/// Returns the count of discarded ops
let discardWipOps () : Task<Result<int64, string>> =
  task {
    try
      // Get count before deleting
      let! wipOps =
        Sql.query
          """
          SELECT id FROM package_ops WHERE commit_id IS NULL
          """
        |> Sql.executeAsync (fun read -> read.uuid "id")

      let count = int64 (List.length wipOps)

      if count = 0L then
        return Ok 0L
      else
        // Delete WIP locations first (foreign key-like cleanup)
        do!
          Sql.query "DELETE FROM locations WHERE commit_id IS NULL"
          |> Sql.executeStatementAsync

        // Delete WIP ops
        do!
          Sql.query "DELETE FROM package_ops WHERE commit_id IS NULL"
          |> Sql.executeStatementAsync

        // Note: We don't delete from package_types/values/functions because
        // they're content-addressed and might be referenced by committed ops.
        // They'll be cleaned up by garbage collection if truly orphaned.

        return Ok count
    with ex ->
      return Error ex.Message
  }
