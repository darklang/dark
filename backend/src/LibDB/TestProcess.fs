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

/// Moves Dark values between the test runner and a worker process, which pass
/// them as files. The file format can only write saved Bytes, not ones still in
/// memory. We can't just save them: the worker's store is a throwaway copy, so
/// the runner could never read them back. So `detach` takes the contents of each
/// in-memory Bytes out and writes them next to the value, and `attach` puts them
/// back on the other side.
module private Carried =
  module Binary = LibSerialization.Binary.Serializers.Common
  module Value = LibSerialization.Binary.Serializers.RT.Dval
  module Bytes = LibExecution.Bytes

  /// The value with its unsaved Bytes swapped for references, and their contents.
  let detach (dv : Dval) : Ply<Dval * List<string * byte[]>> =
    uply {
      let found = System.Collections.Generic.Dictionary<string, byte[]>()
      let! detached =
        Bytes.promote
          (fun hash bytes ->
            found[hash] <- bytes
            Ply(()))
          dv
      return detached, [ for KeyValue(hash, bytes) in found -> hash, bytes ]
    }

  let attach (contents : List<string * byte[]>) (dv : Dval) : Ply<Dval> =
    if List.isEmpty contents then
      Ply dv
    else
      let byHash = Map contents
      dv
      |> Dval.rewriteWith (fun dv ->
        uply {
          match dv with
          | DBytes(Persistent(hash, _)) ->
            return Map.tryFind hash byHash |> Option.map Bytes.newEphemeral
          | _ -> return None
        })

  let write (w : System.IO.BinaryWriter) (dv : Dval, contents) : unit =
    Value.write w dv
    Binary.List.write
      w
      (fun w (hash, bytes : byte[]) ->
        Binary.String.write w hash
        w.Write bytes.Length
        w.Write bytes)
      contents

  let read
    (version : uint32)
    (r : System.IO.BinaryReader)
    : Dval * List<string * byte[]> =
    let dv = Value.read version r
    let contents =
      Binary.List.read r (fun r ->
        let hash = Binary.String.read r
        hash, r.ReadBytes(r.ReadInt32()))
    dv, contents

module Request =
  module Binary = LibSerialization.Binary.Serializers.Common

  // Requests live only for this process launch; parent and worker use the same build.
  let serialize (request : Request) : Ply<byte[]> =
    uply {
      let! target =
        match request.target with
        | Target.PackageTest hash -> Ply(Choice1Of2 hash)
        | Target.TestCallback(hash, argument) ->
          Carried.detach argument
          |> Ply.map (fun carried -> Choice2Of2(hash, carried))
      return
        LibSerialization.Binary.Serialization.makeSerializer
          (fun writer () ->
            Binary.Option.write writer Binary.Guid.write request.accountID
            match target with
            | Choice1Of2 hash ->
              Binary.String.write writer "PackageTest"
              Binary.String.write writer hash
            | Choice2Of2(hash, carried) ->
              Binary.String.write writer "TestCallback"
              Binary.String.write writer hash
              Carried.write writer carried)
          "test worker request"
          ()
    }

  let deserialize (bytes : byte[]) : Ply<Request> =
    let accountID, target =
      LibSerialization.Binary.Serialization.makeDeserializerV
        (fun version reader ->
          let accountID = Binary.Option.read reader Binary.Guid.read
          let target =
            match Binary.String.read reader with
            | "PackageTest" -> Choice1Of2(Binary.String.read reader)
            | "TestCallback" ->
              let hash = Binary.String.read reader
              Choice2Of2(hash, Carried.read version reader)
            | tag -> Binary.raiseFormatError $"Unknown test worker request: {tag}"
          accountID, target)
        "test worker request"
        bytes
    uply {
      match target with
      | Choice1Of2 hash ->
        return { accountID = accountID; target = Target.PackageTest hash }
      | Choice2Of2(hash, (argument, contents)) ->
        let! argument = Carried.attach contents argument
        return
          { accountID = accountID; target = Target.TestCallback(hash, argument) }
    }

/// What a worker hands back: the outcome, with any unsaved Bytes carried beside it.
module WorkerResult =
  let serialize (result : Dval) : Ply<byte[]> =
    uply {
      let! carried = Carried.detach result
      return
        LibSerialization.Binary.Serialization.makeSerializer
          Carried.write
          "test result"
          carried
    }

  let deserialize (bytes : byte[]) : Ply<Dval> =
    let dv, contents =
      LibSerialization.Binary.Serialization.makeDeserializerV
        Carried.read
        "isolated test result"
        bytes
    Carried.attach contents dv

/// A handle is valid only inside withStoreSnapshot. Parallel tests pass it explicitly.
let private runs =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, Host.FileSnapshot>()

/// How many tests in each run wrote to the shared store and ran again on a copy.
let private retries =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, int>()

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
      retries.TryRemove id |> ignore<bool * int>
  }

/// How many tests in the run have run again on a copy so far.
let retriesIn (id : System.Guid) : int =
  match retries.TryGetValue id with
  | true, count -> count
  | false, _ -> 0

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
      let! request = Request.serialize request
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
        // The worker's HOME is `dir`; `directories` below are made too late for the store.
        System.IO.Directory.CreateDirectory(path ".darklang")
        |> ignore<System.IO.DirectoryInfo>
        let db = path ".darklang/data.db"
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
                  "DARK_CONFIG_DB_NAME", Some "data.db"
                  "HOME", Some dir
                  "USERPROFILE", Some dir
                  "XDG_CONFIG_HOME", Some(path "config")
                  "TMPDIR", Some(path "tmp")
                  "DARK_CLI_UNDER_TEST", Some executable
                  "COLUMNS", Some(string columns)
                  "LINES", Some(string rows)
                  // The test compares a worker's styled stdout, which a pipe would strip.
                  "CLICOLOR_FORCE", Some "1" ]
              directories = [ ".darklang/policy"; "tmp" ]
              files =
                [ "request.bin", request
                  ".darklang/policy/policies.bin", PolicyStore.testWorkerStore ()
                  "access.bin", buffer.ToArray() ]
              resultFile = "result.bin" }
          spec)
      let! outcome =
        host (Host.Operation.ProcessRunInWorkspace("dark-test-", prepare, timeoutMs))
      match outcome with
      | Error error -> return Error error.message
      | Ok(Host.Response.WorkspaceProcessOutcome(code, stdout, stderr, result, errors)) ->
        let! result = result |> Ply.Option.map WorkerResult.deserialize
        return
          Ok
            { exitCode = code
              stdout = stdout
              stderr = stderr
              result = result
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
      retries.AddOrUpdate(run, 1, (fun _ count -> count + 1)) |> ignore<int>
      shared.Value <- false
      return! once (Copy(copyFromRun run))
    | _ -> return outcome
  }
