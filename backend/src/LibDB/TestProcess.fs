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

/// Each run's starting store when this runner's store is a layer: a layer over it as the run
/// started, which the run's workers' layers link to instead of its snapshot (`layerFromRun`).
let private twins =
  System.Collections.Concurrent.ConcurrentDictionary<System.Guid, Result<string, string>>()

let copyFromRun (id : System.Guid) (target : string) : Result<unit, string> =
  match runs.TryGetValue id with
  | true, snapshot -> snapshot.CopyTo target
  | false, _ -> Error "The package test run has already ended"

/// Where a worker's store comes from.
type Store =
  /// A layer over this process's own store when that is a layer, else a private writable copy,
  /// made by the supplied function.
  | Own of (string -> Result<unit, string>)
  /// A layer over the starting store of the test run with this id (`layerFromRun`). If that
  /// fails, the worker starts on a copy.
  | Layered of run : System.Guid

/// Put the test store at <param path> in WAL mode and set <param settings> in it.
///
/// Nothing here is synced: a test store is thrown away, never recovered after a crash. On a
/// busy disk the syncs were most of what it cost to start a test on a copy.
let private setConfig (path : string) (settings : List<string * string>) : unit =
  let builder = Microsoft.Data.Sqlite.SqliteConnectionStringBuilder()
  builder.DataSource <- path
  builder.Pooling <- false
  use connection = new Microsoft.Data.Sqlite.SqliteConnection(builder.ToString())
  connection.Open()
  use pragmas = connection.CreateCommand()
  pragmas.CommandText <- "PRAGMA synchronous=OFF; PRAGMA journal_mode=WAL"
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
  setConfig path [ "live.autopush", "off" ]

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

/// Run <param id>'s starting store, which every worker's store is a layer over: the runner's twin
/// (`twins`) when it has one, else the run's snapshot.
let private startOf (id : System.Guid) : Result<string, string> =
  match twins.TryGetValue id with
  | true, twin -> twin
  | false, _ ->
    match runs.TryGetValue id with
    | true, snapshot -> snapshot.Path()
    | false, _ -> Error "The package test run has already ended"

/// Make <param target> a layer (scripts/testing/layer-store) over run <param id>'s starting store.
let private layerFromRun
  (id : System.Guid)
  (target : string)
  : Result<unit, string> =
  startOf id |> Result.bind (fun start -> linkWithWal start target)

/// A layer over this runner's store as it is now, in a directory of its own.
let private layerOverOwn () : Result<string, string> =
  let dir = System.IO.Directory.CreateTempSubdirectory("dark-layer-").FullName
  let path = System.IO.Path.Combine(dir, "data.db")
  let made = linkWithWal LibDB.Sqlite.currentDbPath path
  if Result.isError made then System.IO.Directory.Delete(dir, true)
  made |> Result.map (fun () -> path)

let withStoreSnapshot
  (snapshot : Host.FileSnapshot)
  (run : System.Guid -> Task<'a>)
  : Task<'a> =
  task {
    use snapshot = snapshot
    let id = System.Guid.NewGuid()
    runs[id] <- snapshot
    if LibDB.Sqlite.isLayer LibDB.Sqlite.currentDbPath then
      twins[id] <- layerOverOwn ()
    try
      return! run id
    finally
      runs.TryRemove id |> ignore<bool * Host.FileSnapshot>
      match twins.TryRemove id with
      | true, Ok path ->
        try
          System.IO.Directory.Delete(System.IO.Path.GetDirectoryName path, true)
        with _ ->
          ()
      | _ -> ()
  }

/// The run's starting store: the live store without its traces, a layer root (`-layer` empty) so
/// that each worker's store is a link to it, never a copy. It records the run's branch and has
/// automatic publishing off.
let createRunSnapshot
  (branchId : System.Guid)
  (path : string)
  : Result<unit, string> =
  LibDB.Sqlite.Backup.toTestBaseline path
  |> Result.map (fun () ->
    setConfig path [ "live.autopush", "off"; "current_branch", string branchId ]
    System.IO.File.WriteAllText(path + "-layer", ""))


type Outcome =
  { exitCode : int
    stdout : string
    stderr : string
    result : Option<Dval>
    cleanupErrors : List<string> }

/// Run one worker. The request and result travel separately from stdout. The host owns the
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
