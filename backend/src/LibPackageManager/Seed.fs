/// Package seed: extract and grow.
///
/// A seed is a copy of data.db with projection tables emptied and ops marked
/// unapplied. It has the full schema so it can be used directly as a data.db.
///
/// Export ("extract"): copy data.db, strip derived data, VACUUM.
/// Grow: apply unapplied ops to rebuild projection tables, evaluate values.
///
/// On CLI startup the grow step runs automatically — if everything is already
/// applied it's a single fast SELECT COUNT and returns immediately.
module LibPackageManager.Seed

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module BS = LibSerialization.Binary.Serialization


// ---------------------
// Export
// ---------------------

/// Export a seed database to the given output path.
/// Copies the full source DB, then strips derived data and archived branches.
let export (outputPath : string) : Task<unit> =
  task {
    let sourcePath = LibConfig.Config.dbPath

    if System.IO.File.Exists outputPath then System.IO.File.Delete outputPath

    // Checkpoint WAL before copying to ensure all data is in the main file
    let sourceConnStr = $"Data Source={sourcePath};Mode=ReadOnly;Cache=Private"
    use sourceConn = new SqliteConnection(sourceConnStr)
    sourceConn.Open()
    use checkpointCmd = sourceConn.CreateCommand()
    checkpointCmd.CommandText <- "PRAGMA wal_checkpoint(TRUNCATE);"
    checkpointCmd.ExecuteNonQuery() |> ignore<int>
    sourceConn.Close()

    System.IO.File.Copy(sourcePath, outputPath)

    let connStr = $"Data Source={outputPath};Mode=ReadWriteCreate;Cache=Private"

    use conn = new SqliteConnection(connStr)
    conn.Open()

    use pragmaCmd = conn.CreateCommand()
    pragmaCmd.CommandText <-
      "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000;"
    pragmaCmd.ExecuteNonQuery() |> ignore<int>

    use cleanCmd = conn.CreateCommand()
    cleanCmd.CommandText <-
      """
      DELETE FROM locations;
      DELETE FROM package_types;
      DELETE FROM package_values;
      DELETE FROM package_functions;
      DELETE FROM package_dependencies;

      DELETE FROM package_ops WHERE branch_id IN (
        SELECT id FROM branches WHERE archived_at IS NOT NULL);
      DELETE FROM commits WHERE branch_id IN (
        SELECT id FROM branches WHERE archived_at IS NOT NULL);
      DELETE FROM branches WHERE archived_at IS NOT NULL;

      UPDATE package_ops SET applied = 0;
      UPDATE branch_ops SET applied = 1;
      """
    cleanCmd.ExecuteNonQuery() |> ignore<int>

    use vacuumCmd = conn.CreateCommand()
    vacuumCmd.CommandText <- "VACUUM;"
    vacuumCmd.ExecuteNonQuery() |> ignore<int>

    conn.Close()
  }


// ---------------------
// Grow
// ---------------------

/// Apply all unapplied package_ops in the database.
/// Returns the count of ops applied.
let applyUnappliedOps () : Task<int64> =
  task {
    // Fast check: are there any unapplied ops? Avoids loading blobs when count is 0.
    let! count =
      Sql.query "SELECT COUNT(*) as n FROM package_ops WHERE applied = 0"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    if count = 0L then
      return 0L
    else

      let! unappliedOps =
        Sql.query
          """
        SELECT id, op_blob, branch_id, commit_hash
        FROM package_ops
        WHERE applied = 0
        ORDER BY created_at ASC
        """
        |> Sql.executeAsync (fun read ->
          let opId = read.uuid "id"
          let opBlob = read.bytes "op_blob"
          let branchId : PT.BranchId = read.uuid "branch_id"
          let commitHash = read.stringOrNone "commit_hash"
          let op = BS.PT.PackageOp.deserialize opId opBlob
          (opId, op, branchId, commitHash))

      if List.isEmpty unappliedOps then
        return 0L
      else
        let groups =
          unappliedOps
          |> List.groupBy (fun (_, _, branchId, commitHash) ->
            (branchId, commitHash))
          |> Map.toList

        for ((branchId, commitHash), ops) in groups do
          let opsOnly = ops |> List.map (fun (_, op, _, _) -> op)
          do! PackageOpPlayback.applyOps branchId commitHash opsOnly

        let opIds = unappliedOps |> List.map (fun (opId, _, _, _) -> opId)
        let updateStatements =
          opIds
          |> List.map (fun opId ->
            let sql = "UPDATE package_ops SET applied = 1 WHERE id = @id"
            let parameters = [ "applied", Sql.bool true; "id", Sql.uuid opId ]
            (sql, [ parameters ]))
        let _ = Sql.executeTransactionSync updateStatements

        return int64 (List.length unappliedOps)
  }


/// Evaluate all package values that have NULL rt_dval.
/// Multi-pass: values may depend on other values, so we retry until convergence.
let evaluateAllValues
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  : Task<Result<unit, string list>> =
  task {
    let program : RT.Program =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        dbs = Map.empty
        secrets = [] }

    let notify _ _ _ _ = uply { return () }
    let sendException _ _ _ _ = uply { return () }

    let exeState =
      Execution.createState
        builtins
        pm
        Execution.noTracing
        sendException
        notify
        PT.mainBranchId
        program

    let maxPasses = 10
    let mutable pass = 0
    let mutable keepGoing = true
    let mutable lastErrors : string list = []

    while keepGoing do
      pass <- pass + 1

      let! unevaluatedValues =
        Sql.query
          """
          SELECT pv.hash, pv.pt_def, l.owner, l.modules, l.name
          FROM package_values pv
          LEFT JOIN locations l ON l.item_hash = pv.hash AND l.deprecated_at IS NULL
          WHERE pv.rt_dval IS NULL
          """
        |> Sql.executeAsync (fun read ->
          let hash = Hash(read.string "hash")
          let ptDef = read.bytes "pt_def"
          let owner = read.stringOrNone "owner" |> Option.defaultValue "?"
          let modules = read.stringOrNone "modules" |> Option.defaultValue ""
          let name = read.stringOrNone "name" |> Option.defaultValue "?"
          let fullName =
            if modules = "" then $"{owner}.{name}" else $"{owner}.{modules}.{name}"
          (hash, ptDef, fullName))

      if List.isEmpty unevaluatedValues then
        keepGoing <- false
        lastErrors <- []
      else if pass > maxPasses then
        keepGoing <- false
        lastErrors <-
          [ $"Gave up after {maxPasses} passes with {List.length unevaluatedValues} values remaining" ]
      else
        let errors = ResizeArray<string>()
        let mutable successCount = 0

        for (valueHash, ptDefBytes, fullName) in unevaluatedValues do
          try
            let ptValue = BS.PT.PackageValue.deserialize valueHash ptDefBytes
            let instrs = PT2RT.Expr.toRT Map.empty 0 None ptValue.body
            let! result = Execution.executeExpr exeState instrs

            match result with
            | Error(rte, _callStack) ->
              let! errorResult = Execution.runtimeErrorToString exeState rte
              let errorMsg =
                match errorResult with
                | Ok(RT.DString s) -> s
                | Ok other -> $"{other}"
                | Error(rte2, _) -> $"(could not stringify error: {rte2})"
              errors.Add(
                $"Value {valueHash} ({fullName}): evaluation failed - {errorMsg}"
              )
            | Ok dval ->
              let rtHash = PT2RT.Hash.toRT valueHash
              let rtValue : RT.PackageValue.PackageValue =
                { hash = rtHash; body = dval }
              let (Hash defHash) = valueHash
              let rtDvalBytes = BS.RT.PackageValue.serialize rtHash rtValue
              let valueType = RT.Dval.toValueType dval
              let valueTypeBytes = BS.RT.ValueType.serialize valueType

              do!
                Sql.query
                  """
                  UPDATE package_values
                  SET rt_dval = @rt_dval, value_type = @value_type
                  WHERE hash = @hash
                  """
                |> Sql.parameters
                  [ "hash", Sql.string defHash
                    "rt_dval", Sql.bytes rtDvalBytes
                    "value_type", Sql.bytes valueTypeBytes ]
                |> Sql.executeStatementAsync

              successCount <- successCount + 1
          with ex ->
            errors.Add($"Value {valueHash} ({fullName}): exception - {ex.Message}")

        if successCount = 0 then
          keepGoing <- false
          lastErrors <- errors |> List.ofSeq

    if List.isEmpty lastErrors then return Ok() else return Error lastErrors
  }


/// The grow step for CLI/test startup.
/// Applies any unapplied ops, generates package ref hashes, then evaluates values.
/// On a warm DB this is a single fast SELECT COUNT and returns immediately.
///
/// builtins is a function (not a value) because it must be constructed AFTER
/// hashes are generated — builtin construction triggers PackageRefs hash lookups.
let growIfNeeded
  (getBuiltins : unit -> RT.Builtins)
  (pm : RT.PackageManager)
  (log : string -> unit)
  : Task<bool> =
  task {
    use _span = Telemetry.span "seed.growIfNeeded" []
    let! appliedCount =
      Telemetry.timeTask "seed.applyOps" [] (fun () -> applyUnappliedOps ())
    if appliedCount > 0L then
      log $"Growing package DB from ops ({appliedCount} ops to apply)..."
      Telemetry.event "seed.applyOps.count" [ ("count", string appliedCount) ]
      do!
        Telemetry.timeTask "seed.generateRefs" [] (fun () ->
          task {
            do! PackageRefsGenerator.generate ()
            LibExecution.PackageRefs.reloadHashes ()
          })
      let! _evalResult =
        Telemetry.timeTask "seed.evaluateValues" [] (fun () ->
          evaluateAllValues (getBuiltins ()) pm)
      do!
        Telemetry.timeTask "seed.walCheckpoint" [] (fun () ->
          Sql.query "PRAGMA wal_checkpoint(TRUNCATE);" |> Sql.executeStatementAsync)
      log "Package DB ready"
      return true
    else
      return false
  }
