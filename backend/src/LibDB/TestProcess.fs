/// Shared transport for isolated package tests and explicit test callbacks.
module LibDB.TestProcess

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Permissions = LibExecution.Permissions
module Host = LibExecution.HostTypes

/// The two things a disposable worker can execute, both pinned to exact hashes.
[<RequireQualifiedAccess>]
type Target =
  /// A test declaration selected by the runner, and the run's seed for its random values.
  | PackageTest of hash : string * seed : int64
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
  let detach (dv : Dval) : Dval * List<string * byte[]> =
    let found = System.Collections.Generic.Dictionary<string, byte[]>()
    let detached =
      Bytes.promote
        (fun hash bytes ->
          found[hash] <- bytes
          (()))
        dv
    detached, [ for KeyValue(hash, bytes) in found -> hash, bytes ]

  let attach (contents : List<string * byte[]>) (dv : Dval) : Dval =
    if List.isEmpty contents then
      dv
    else
      let byHash = Map contents
      dv
      |> Dval.rewriteWith (fun dv ->
        match dv with
        | DBytes(Persistent(hash, _)) ->
          Map.tryFind hash byHash |> Option.map Bytes.newEphemeral
        | _ -> None)

  let write (w : System.IO.BinaryWriter) (dv : Dval, contents) : unit =
    Value.write w dv
    Binary.List.write
      w
      (fun w (hash, bytes : byte[]) ->
        Binary.String.write w hash
        w.Write bytes.Length
        w.Write bytes)
      contents

  let read (r : System.IO.BinaryReader) : Dval * List<string * byte[]> =
    let dv = Value.read r
    let contents =
      Binary.List.read r (fun r ->
        let hash = Binary.String.read r
        hash, r.ReadBytes(r.ReadInt32()))
    dv, contents

module Request =
  module Binary = LibSerialization.Binary.Serializers.Common

  // Requests live only for this process launch; parent and worker use the same build.
  let serialize (request : Request) : byte[] =
    let target =
      match request.target with
      | Target.PackageTest(hash, seed) -> (Choice1Of2(hash, seed))
      | Target.TestCallback(hash, argument) ->
        Carried.detach argument |> (fun carried -> Choice2Of2(hash, carried))
    LibSerialization.Binary.Serialization.makeSerializer
      (fun writer () ->
        Binary.Option.write writer Binary.Guid.write request.accountID
        match target with
        | Choice1Of2(hash, seed : int64) ->
          Binary.String.write writer "PackageTest"
          Binary.String.write writer hash
          writer.Write seed
        | Choice2Of2(hash, carried) ->
          Binary.String.write writer "TestCallback"
          Binary.String.write writer hash
          Carried.write writer carried)
      "test worker request"
      ()

  let deserialize (bytes : byte[]) : Request =
    let accountID, target =
      LibSerialization.Binary.Serialization.makeDeserializer
        (fun reader ->
          let accountID = Binary.Option.read reader Binary.Guid.read
          let target =
            match Binary.String.read reader with
            | "PackageTest" ->
              let hash = Binary.String.read reader
              Choice1Of2(hash, reader.ReadInt64())
            | "TestCallback" ->
              let hash = Binary.String.read reader
              Choice2Of2(hash, Carried.read reader)
            | tag -> Binary.raiseFormatError $"Unknown test worker request: {tag}"
          accountID, target)
        "test worker request"
        bytes
    match target with
    | Choice1Of2(hash, seed) ->
      { accountID = accountID; target = Target.PackageTest(hash, seed) }
    | Choice2Of2(hash, (argument, contents)) ->
      let argument = Carried.attach contents argument
      { accountID = accountID; target = Target.TestCallback(hash, argument) }

/// What a worker hands back: the outcome, with any unsaved Bytes carried beside it.
module WorkerResult =
  let serialize (result : Dval) : byte[] =
    let carried = Carried.detach result
    LibSerialization.Binary.Serialization.makeSerializer
      Carried.write
      "test result"
      carried

  let deserialize (bytes : byte[]) : Dval =
    let dv, contents =
      LibSerialization.Binary.Serialization.makeDeserializer
        Carried.read
        "isolated test result"
        bytes
    Carried.attach contents dv

/// A handle is valid only inside withStoreSnapshot. Parallel tests pass it explicitly.
let private runs =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, Host.FileSnapshot>()

type private TestHashes =
  System.Collections.Concurrent.ConcurrentDictionary<string, unit>

/// The tests in each run that wrote to the shared store and ran again on a copy.
let private reruns =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, TestHashes>()

/// Each run's writable twin of its snapshot, which its workers' layers link to (`layerFromRun`).
let private twins =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, Lazy<Result<string, string>>>()

let copyFromRun (id : System.Guid) (target : string) : Result<unit, string> =
  match runs.TryGetValue id with
  | true, snapshot -> snapshot.CopyTo target
  | false, _ -> Error "The package test run has already ended"

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

/// Link <param target> to the run's snapshot, read-only and shared with the other
/// workers. Return an error if the snapshot can't be shared.
let private linkFromRun (id : System.Guid) (target : string) : Result<unit, string> =
  match runs.TryGetValue id with
  | true, snapshot -> snapshot.LinkTo target
  | false, _ -> Error "The package test run has already ended"

/// Where a worker's store comes from.
type Store =
  /// A layer over this process's own store when that is a layer, else a private writable copy,
  /// made by the supplied function.
  | Own of (string -> Result<unit, string>)
  /// A layer over the writable twin of the snapshot of the test run with this id
  /// (`layerFromRun`). If that fails, the worker starts on a copy.
  | Layered of run : System.Guid
  /// The snapshot of the test run with this id, linked read-only for every
  /// worker. A worker that tries to write to it runs again on a private copy;
  /// only the second result is used. If linking fails, the worker starts on a copy.
  | Shared of run : System.Guid

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

  /// A worker on the shared store finds this link beside its store. It points
  /// outside the worker's directory, so the runner can check it after cleanup.
  let markerIn (storeDir : string) : string =
    System.IO.Path.Combine(storeDir, "data.db-wrote")

  /// If the store directory under HOME has a marker link, write through it on the
  /// first refused write, or if the watcher can't be installed. Call before opening
  /// any database, to register SQLite's log callback. Any `dark` process the worker
  /// starts shares its HOME, so it watches too.
  let watchIfShared () : unit =
    let link = markerIn (LibConfig.StoreLocation.defaultDirectory ())
    if not (isNull (System.IO.FileInfo link).LinkTarget) then
      marker <- Some link
      SQLitePCL.Batteries_V2.Init()
      // If writes can't be detected, request a retry on a private copy.
      if SQLitePCL.raw.sqlite3_config_log (log, null) <> 0 then
        mark "Could not watch for writes"

/// Put the test store at <param path> in <param journalMode> and set <param settings> in it.
///
/// Nothing here is synced: a test store is thrown away, never recovered after a crash. On a
/// busy disk the syncs were most of what it cost to start a test on a copy.
let private setConfig
  (path : string)
  (journalMode : string)
  (settings : List<string * string>)
  : unit =
  let builder = Microsoft.Data.Sqlite.SqliteConnectionStringBuilder()
  builder.DataSource <- path
  builder.Pooling <- false
  use connection = new Microsoft.Data.Sqlite.SqliteConnection(builder.ToString())
  connection.Open()
  use pragmas = connection.CreateCommand()
  pragmas.CommandText <- $"PRAGMA synchronous=OFF; PRAGMA journal_mode={journalMode}"
  pragmas.ExecuteNonQuery() |> ignore<int>
  for key, value in settings do
    use command = connection.CreateCommand()
    command.CommandText <-
      "INSERT OR REPLACE INTO config_v0(key, value) VALUES (@key, @value)"
    command.Parameters.AddWithValue("@key", key)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    command.Parameters.AddWithValue("@value", value)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    command.ExecuteNonQuery() |> ignore<int>

/// Disable automatic publishing only in the disposable copy. WAL already, which is what the
/// worker would switch it to.
let private prepareStore (path : string) : unit =
  setConfig path "WAL" [ "live.autopush", "off" ]

let private hardLink (source : string) (target : string) : Result<unit, string> =
  try
    let psi = System.Diagnostics.ProcessStartInfo("ln")
    psi.ArgumentList.Add source
    psi.ArgumentList.Add target
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    use p = System.Diagnostics.Process.Start psi
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    if p.ExitCode = 0 then Ok() else Error err
  with e ->
    Error e.Message

/// Make <param link> a layer (scripts/testing/layer-store) over the layer <param source>, as it is
/// now. Its marker names the file with nothing in its WAL, as layer-store's do.
let private linkWithWal (source : string) (link : string) : Result<unit, string> =
  hardLink source link
  |> Result.map (fun () ->
    if System.IO.File.Exists(source + "-wal") then
      System.IO.File.Copy(source + "-wal", link + "-wal")
    let named = System.IO.File.ReadAllText(source + "-layer")
    System.IO.File.WriteAllText(
      link + "-layer",
      (if named = "" then source else named)
    ))

/// The writable twin of run <param id>'s snapshot: a layer over this runner's store when that is a
/// layer, or else one copy of the snapshot, prepared like any worker's copy before anything links
/// to it.
let private twinOf (id : System.Guid) : Result<string, string> =
  let dir = System.IO.Directory.CreateTempSubdirectory("dark-layer-").FullName
  let path = System.IO.Path.Combine(dir, "data.db")
  let own = LibDB.Sqlite.currentDbPath
  let made =
    if LibDB.Sqlite.isLayer own then
      linkWithWal own path
    else
      copyFromRun id path
      |> Result.map (fun () ->
        prepareStore path
        System.IO.File.WriteAllText(path + "-layer", ""))
  if Result.isError made then System.IO.Directory.Delete(dir, true)
  made |> Result.map (fun () -> path)

/// Make <param target> a layer (scripts/testing/layer-store) over the twin of run <param id>.
let private layerFromRun
  (id : System.Guid)
  (target : string)
  : Result<unit, string> =
  match twins.TryGetValue id with
  | true, twin -> twin.Value |> Result.bind (fun twin -> linkWithWal twin target)
  | false, _ -> Error "The package test run has already ended"

let withStoreSnapshot
  (snapshot : Host.FileSnapshot)
  (run : System.Guid -> Task<'a>)
  : Task<'a> =
  task {
    use snapshot = snapshot
    let id = System.Guid.NewGuid()
    runs[id] <- snapshot
    let twin = lazy (twinOf id)
    // over a layer, the twin is this runner's store as the run starts, so take it now
    if LibDB.Sqlite.isLayer LibDB.Sqlite.currentDbPath then
      twin.Force() |> ignore<Result<string, string>>
    twins[id] <- twin
    try
      return! run id
    finally
      runs.TryRemove id |> ignore<bool * Host.FileSnapshot>
      reruns.TryRemove id |> ignore<bool * TestHashes>
      match twins.TryRemove id with
      | true, twin when twin.IsValueCreated ->
        match twin.Value with
        | Ok path ->
          try
            System.IO.Directory.Delete(System.IO.Path.GetDirectoryName path, true)
          with _ ->
            ()
        | Error _ -> ()
      | _ -> ()
  }

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
    setConfig
      path
      "DELETE"
      [ "live.autopush", "off"; "current_branch", string branchId ])


type Outcome =
  { exitCode : int
    stdout : string
    stderr : string
    result : Option<Dval>
    cleanupErrors : List<string> }

/// Run one worker. <param shared> records whether it used the shared snapshot;
/// if linking fails, it uses a private copy. A worker using the shared snapshot
/// creates <param marker>, through a link beside its store, when SQLite refuses a
/// write or the watcher can't start.
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
  : Task<Result<Outcome, string>> =
  task {
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
        // The worker's HOME is `dir`; `directories` below are made too late for the store.
        System.IO.Directory.CreateDirectory(path ".darklang")
        |> ignore<System.IO.DirectoryInfo>
        let db = path ".darklang/data.db"
        let copyTo copy = copy db |> Result.map (fun () -> prepareStore db)
        let ready =
          match store with
          | Own copy ->
            if LibDB.Sqlite.isLayer LibDB.Sqlite.currentDbPath then
              linkWithWal LibDB.Sqlite.currentDbPath db
            else
              copyTo copy
          | Layered run ->
            match layerFromRun run db with
            | Ok() -> Ok()
            | Error _ -> copyTo (copyFromRun run)
          | Shared run when
            LibDB.Sqlite.isLayer LibDB.Sqlite.currentDbPath
            && Result.isOk (layerFromRun run db)
            ->
            Ok()
          | Shared run ->
            match linkFromRun run db with
            | Ok() ->
              shared.Value <- true
              System.IO.File.CreateSymbolicLink(
                SharedStoreWrites.markerIn (path ".darklang"),
                marker
              )
              |> ignore<System.IO.FileSystemInfo>
              Ok()
            | Error _ -> copyTo (copyFromRun run)
        ready
        |> Result.map (fun () ->
          let spec : Host.ProcessWorkspace =
            { program = executable
              arguments =
                [ "--branch"
                  string branchId
                  path "request.bin"
                  path "result.bin" ]
              environment =
                [ "DARK_TEST_WORKER", Some workerHash
                  "DARK_MATTER_WRITE_SECRET", None
                  "DARK_BRANCH", None
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
        let result = result |> Option.map WorkerResult.deserialize
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
  : Task<Result<Outcome, string>> =
  task {
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
      | Target.PackageTest(testHash, _) ->
        reruns.GetOrAdd(run, (fun _ -> TestHashes()))[testHash] <- ()
      | Target.TestCallback _ -> ()
      shared.Value <- false
      return! once (Layered run)
    | _ -> return outcome
  }
