/// Shared transport for isolated package tests and explicit test callbacks.
module LibDB.TestProcess

open Prelude
open LibExecution.RuntimeTypes

module Permissions = LibExecution.Permissions
module Host = LibExecution.HostTypes

/// The two things a disposable worker can execute, both pinned to exact hashes.
[<RequireQualifiedAccess>]
type Target =
  /// A test declaration selected by the runner.
  | PackageTest of hash : string
  /// A package function passed to Stdlib.Test.Process.run so the calling test
  /// can check the child process's result, output, exit status or timeout.
  | TestCallback of hash : string * argument : Dval

type Request = { accountID : Option<System.Guid>; target : Target }

module Request =
  module Binary = LibSerialization.Binary.Serializers.Common
  module Value = LibSerialization.Binary.Serializers.RT.Dval

  // Requests live only for this process launch; parent and worker use the same build.
  let serialize (request : Request) : byte[] =
    LibSerialization.Binary.Serialization.makeSerializer
      (fun writer request ->
        Binary.Option.write writer Binary.Guid.write request.accountID
        match request.target with
        | Target.PackageTest hash ->
          Binary.String.write writer "PackageTest"
          Binary.String.write writer hash
        | Target.TestCallback(hash, argument) ->
          Binary.String.write writer "TestCallback"
          Binary.String.write writer hash
          Value.write writer argument)
      "test worker request"
      request

  let deserialize (bytes : byte[]) : Request =
    LibSerialization.Binary.Serialization.makeDeserializerV
      (fun version reader ->
        let accountID = Binary.Option.read reader Binary.Guid.read
        let target =
          match Binary.String.read reader with
          | "PackageTest" -> Target.PackageTest(Binary.String.read reader)
          | "TestCallback" ->
            let hash = Binary.String.read reader
            Target.TestCallback(hash, Value.read version reader)
          | tag -> Binary.raiseFormatError $"Unknown test worker request: {tag}"
        { accountID = accountID; target = target })
      "test worker request"
      bytes

/// A handle is valid only inside withStoreSnapshot. Parallel tests pass it explicitly.
let private runs =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, Host.FileSnapshot>()

type private TestHashes =
  System.Collections.Concurrent.ConcurrentDictionary<string, unit>

/// The tests in each run that wrote to the shared store and ran again on a copy.
let private reruns =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, TestHashes>()

let withStoreSnapshot
  (snapshot : Host.FileSnapshot)
  (run : System.Guid -> Ply<'a>)
  : Ply<'a> =
  uply {
    use snapshot = snapshot
    let id = System.Guid.NewGuid()
    runs[id] <- snapshot
    try
      return! run id
    finally
      runs.TryRemove id |> ignore<bool * Host.FileSnapshot>
      reruns.TryRemove id |> ignore<bool * TestHashes>
  }

/// Whether the test with this hash wrote to the run's shared store and so ran
/// again on a copy.
let reranIn (id : System.Guid) (testHash : string) : bool =
  match reruns.TryGetValue id with
  | true, tests -> tests.ContainsKey testHash
  | false, _ -> false

let private changedInRun (id : System.Guid) : bool =
  match runs.TryGetValue id with
  | true, snapshot -> snapshot.Changed()
  | false, _ -> false

let copyFromRun (id : System.Guid) (target : string) : Result<unit, string> =
  match runs.TryGetValue id with
  | true, snapshot -> snapshot.CopyTo target
  | false, _ -> Error "The package test run has already ended"

/// Link <param target> to the run's snapshot, read-only and shared with the other
/// workers. Return an error if the snapshot can't be shared.
let private linkFromRun (id : System.Guid) (target : string) : Result<unit, string> =
  match runs.TryGetValue id with
  | true, snapshot -> snapshot.LinkTo target
  | false, _ -> Error "The package test run has already ended"

/// Where a worker's store comes from.
type Store =
  /// A private writable copy, made by the supplied function.
  | Copy of (string -> Result<unit, string>)
  /// The snapshot of the test run with this id, linked read-only for every
  /// worker. A worker that tries to write to it runs again on a private copy;
  /// only the second result is used. If linking fails, the worker starts on a copy.
  | Shared of run : System.Guid

/// The marker file path for a worker using the shared store. Any `dark` process
/// it starts inherits this variable so it can report refused writes too.
let writeMarkerVariable = "DARK_TEST_WRITE_MARKER"

/// Detect refused writes through SQLite's error log, even when a test catches
/// or ignores the error. The runner uses the marker to retry on a private copy.
module SharedStoreWrites =
  let private gate = obj ()
  let mutable private marker : Option<string> = None

  let private mark (why : string) : unit =
    lock gate (fun () ->
      match marker with
      | Some path ->
        marker <- None
        // The runner checks only whether the file exists. Its contents explain
        // why the test needs a retry.
        System.IO.File.WriteAllText(path, why)
      | None -> ())

  // Kept here so the collector can't free it while SQLite still calls it.
  let private log =
    SQLitePCL.strdelegate_log (fun _ code message ->
      // SQLITE_READONLY, with or without an extended code.
      if code &&& 0xff = 8 then mark message)

  /// Create <param path> on the first refused write, or if the watcher can't be
  /// installed. Call before opening any database to register SQLite's log callback.
  let watch (path : string) : unit =
    marker <- Some path
    SQLitePCL.Batteries_V2.Init()
    // If writes can't be detected, request a retry on a private copy.
    if SQLitePCL.raw.sqlite3_config_log (log, null) <> 0 then
      mark "Could not watch for writes"

let private setConfig (path : string) (settings : List<string * string>) : unit =
  let builder = Microsoft.Data.Sqlite.SqliteConnectionStringBuilder()
  builder.DataSource <- path
  builder.Pooling <- false
  use connection = new Microsoft.Data.Sqlite.SqliteConnection(builder.ToString())
  connection.Open()
  for key, value in settings do
    use command = connection.CreateCommand()
    command.CommandText <-
      "INSERT OR REPLACE INTO config_v0(key, value) VALUES (@key, @value)"
    command.Parameters.AddWithValue("@key", key)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    command.Parameters.AddWithValue("@value", value)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    command.ExecuteNonQuery() |> ignore<int>

/// Disable automatic publishing only in the disposable copy.
let private prepareStore (path : string) : unit =
  setConfig path [ "live.autopush", "off" ]

/// Prepare the run's starting store with its branch and autopush settings so
/// workers can read those settings without writing at startup.
///
/// Use rollback journal mode to avoid WAL sidecar files for the shared snapshot.
/// Backups from the shared store were observed to hang with read-only WAL in this
/// setup. Sql.initializeConnection switches writable copies back to WAL.
let createRunSnapshot
  (branchId : System.Guid)
  (path : string)
  : Result<unit, string> =
  LibDB.Sqlite.Backup.toTestBaseline path
  |> Result.map (fun () ->
    setConfig path [ "live.autopush", "off"; "current_branch", string branchId ]
    let builder = Microsoft.Data.Sqlite.SqliteConnectionStringBuilder()
    builder.DataSource <- path
    builder.Pooling <- false
    use connection = new Microsoft.Data.Sqlite.SqliteConnection(builder.ToString())
    connection.Open()
    use command = connection.CreateCommand()
    command.CommandText <- "PRAGMA journal_mode=DELETE"
    command.ExecuteNonQuery() |> ignore<int>)


type Outcome =
  { exitCode : int
    stdout : string
    stderr : string
    result : Option<Dval>
    cleanupErrors : List<string> }

/// Run one worker. <param shared> records whether it used the shared snapshot;
/// if linking fails, it uses a private copy. A worker using the shared snapshot
/// creates <param marker> when SQLite refuses a write or the watcher can't start.
let private runOnce
  (store : Store)
  (marker : string)
  (shared : bool ref)
  (state : ExecutionState)
  (vm : VMState)
  (access : Permissions.Access)
  (branchId : System.Guid)
  (workerHash : string)
  (request : Request)
  (timeoutMs : int)
  (columns : int)
  (rows : int)
  : Ply<Result<Outcome, string>> =
  uply {
    try
      let request = Request.serialize request
      use buffer = new System.IO.MemoryStream()
      use writer = new System.IO.BinaryWriter(buffer)
      LibSerialization.Binary.Serializers.Permissions.writeExecutionAccess
        writer
        access
      writer.Flush()
      let host = LibExecution.PermissionCheck.performHostWithAccess state vm access
      let! resolved =
        host (Host.Operation.ProcessLauncherPath(Some "DARK_CLI_UNDER_TEST"))
      let executable =
        match resolved with
        | Ok(Host.Response.Path path) -> path
        | Error error -> invalidOp error.message
        | _ -> invalidOp "Invalid launcher response"
      let prepare (directory : string) =
        let dir = directory.Replace('\\', '/')
        let path name = System.IO.Path.Combine(dir, name).Replace('\\', '/')
        let db = path "data.db"
        let copyTo copy = copy db |> Result.map (fun () -> prepareStore db)
        let ready =
          match store with
          | Copy copy -> copyTo copy
          | Shared run ->
            match linkFromRun run db with
            | Ok() ->
              shared.Value <- true
              Ok()
            | Error _ -> copyTo (copyFromRun run)
        ready
        |> Result.map (fun () ->
          let spec : Host.ProcessWorkspace =
            { program = executable
              arguments =
                [ "--entry-point"
                  "#" + workerHash
                  "--branch"
                  string branchId
                  path "request.bin"
                  path "result.bin" ]
              environment =
                [ "DARK_MATTER_WRITE_SECRET", None
                  "DARK_BRANCH", None
                  // Clear the inherited marker when a nested worker uses a copy.
                  writeMarkerVariable, (if shared.Value then Some marker else None)
                  "DARK_CONFIG_RUNDIR", Some(dir + "/")
                  "DARK_CONFIG_DB_NAME", Some "data.db"
                  "HOME", Some dir
                  "XDG_CONFIG_HOME", Some(path "config")
                  "TMPDIR", Some(path "tmp")
                  "DARK_CLI_UNDER_TEST", Some executable
                  "COLUMNS", Some(string columns)
                  "LINES", Some(string rows) ]
              directories = [ "policy"; "tmp" ]
              files =
                [ "request.bin", request
                  "policy/policies.bin", PolicyStore.testWorkerStore ()
                  "access.bin", buffer.ToArray() ]
              resultFile = "result.bin" }
          spec)
      let! outcome =
        host (Host.Operation.ProcessRunInWorkspace("dark-test-", prepare, timeoutMs))
      match outcome with
      | Error error -> return Error error.message
      | Ok(Host.Response.WorkspaceProcessOutcome(code, stdout, stderr, result, errors)) ->
        return
          Ok
            { exitCode = code
              stdout = stdout
              stderr = stderr
              result =
                result
                |> Option.map (
                  LibSerialization.Binary.Serialization.RT.Dval.deserialize
                    "isolated test result"
                )
              cleanupErrors = errors }
      | _ -> return Error "Invalid isolated test response"
    with e ->
      return Error $"Isolated test failed: {e.Message}"
  }

/// The request and result travel separately from stdout. The host owns the
/// temporary directory and process tree, and reports cleanup failures even after success.
let run
  (store : Store)
  (state : ExecutionState)
  (vm : VMState)
  (access : Permissions.Access)
  (branchId : System.Guid)
  (workerHash : string)
  (request : Request)
  (timeoutMs : int)
  (columns : int)
  (rows : int)
  : Ply<Result<Outcome, string>> =
  uply {
    // Keep the marker outside the workspace so it survives cleanup. Check it
    // even if the worker failed or timed out after attempting a write.
    let marker =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-test-{System.Guid.NewGuid()}.wrote"
      )
    let shared = ref false
    let once store =
      runOnce
        store
        marker
        shared
        state
        vm
        access
        branchId
        workerHash
        request
        timeoutMs
        columns
        rows
    let! outcome = once store
    let wrote = shared.Value && System.IO.File.Exists marker
    if wrote then System.IO.File.Delete marker
    match store with
    // Checked after every reader, so the failure lands on the test that was
    // running when it happened, not only on the ones that start after.
    | Shared run when shared.Value && changedInRun run ->
      return
        Error
          "The run's shared store changed while this test was reading it, so this \
           and every later test fail. Something running then made it writable."
    | Shared run when wrote ->
      match request.target with
      | Target.PackageTest testHash ->
        reruns.GetOrAdd(run, (fun _ -> TestHashes()))[testHash] <- ()
      | Target.TestCallback _ -> ()
      shared.Value <- false
      return! once (Copy(copyFromRun run))
    | _ -> return outcome
  }
