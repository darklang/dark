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


/// For local ops in own namespace, generate ApproveItem ops so approval syncs correctly
let private generateApprovalOps
  (instanceID : Option<PT.InstanceID>)
  (branchID : Option<PT.BranchID>)
  (createdBy : Option<uuid>)
  (creatorName : Option<string>)
  (ops : List<PT.PackageOp>)
  : List<PT.PackageOp> =
  // Only generate approval ops for local operations (not from sync)
  match instanceID, createdBy, creatorName with
  | None, Some creatorId, Some name ->
    // Local op with known creator - auto-approve only if in own namespace
    ops
    |> List.choose (fun op ->
      match op with
      | PT.PackageOp.SetTypeName(itemId, loc) -> Some(itemId, loc)
      | PT.PackageOp.SetValueName(itemId, loc) -> Some(itemId, loc)
      | PT.PackageOp.SetFnName(itemId, loc) -> Some(itemId, loc)
      | _ -> None)
    |> List.choose (fun (itemId, loc) ->
      // Only auto-approve if adding to own namespace
      if loc.owner = name then
        Some(PT.PackageOp.ApproveItem(itemId, branchID, creatorId))
      else
        None)
  | _ ->
    // From sync or no creator - don't auto-generate approval ops
    []


/// Insert PackageOps into the package_ops table and apply them to projection tables
/// Returns the count of ops actually inserted (duplicates are skipped via INSERT OR IGNORE)

// CLEANUP: The 'applied' flag is currently always set to true and all ops are applied immediately
let insertAndApplyOps
  (instanceID : Option<PT.InstanceID>)
  (branchID : Option<PT.BranchID>)
  (createdBy : Option<uuid>)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  task {
    if List.isEmpty ops then
      return 0L
    else
      // Look up creator name for auto-approval logic
      let! creatorName =
        match createdBy with
        | Some id -> Accounts.getName id
        | None -> Task.FromResult None

      // Generate ApproveItem ops for local items in own namespace
      let approvalOps =
        generateApprovalOps instanceID branchID createdBy creatorName ops
      let allOps = ops @ approvalOps

      // CLEANUP this should either
      // - be made to be one big transaction
      // OR
      // - be inserted as applied:false, and have 'step 2' trigger them being toggled

      // Step 1: Insert ops into package_ops table (source of truth)
      let insertStatements =
        allOps
        |> List.map (fun op ->
          let opId = computeOpHash op
          let opBlob = BinarySerialization.PT.PackageOp.serialize opId op

          let sql =
            """
            INSERT OR IGNORE INTO package_ops (id, instance_id, branch_id, op_blob, applied)
            VALUES (@id, @instance_id, @branch_id, @op_blob, @applied)
            """

          let parameters =
            [ "id", Sql.uuid opId
              "instance_id",
              (match instanceID with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull)
              "branch_id",
              (match branchID with
               | Some id -> Sql.uuid id
               | None -> Sql.dbnull)
              "op_blob", Sql.bytes opBlob
              "applied", Sql.bool true ]

          (sql, [ parameters ]))

      let rowsAffected = insertStatements |> Sql.executeTransactionSync

      // Count how many ops were actually inserted (vs skipped as duplicates)
      let insertedCount = rowsAffected |> List.sumBy int64

      // Step 2: Apply ops to projection tables (types, values, functions, locations)
      // Only apply ops that were actually inserted
      let opsToApply =
        List.zip allOps rowsAffected
        |> List.filter (fun (_, affected) -> affected > 0)
        |> List.map fst

      do! PackageOpPlayback.applyOps instanceID branchID createdBy opsToApply

      return insertedCount
  }
