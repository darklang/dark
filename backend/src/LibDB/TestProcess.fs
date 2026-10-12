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
  }

let copyFromRun (id : System.Guid) (target : string) : Result<unit, string> =
  match runs.TryGetValue id with
  | true, snapshot -> snapshot.CopyTo target
  | false, _ -> Error "The package test run has already ended"

/// Disable automatic publishing only in the disposable copy.
let private prepareStore (path : string) : unit =
  let settings = Microsoft.Data.Sqlite.SqliteConnectionStringBuilder()
  settings.DataSource <- path
  settings.Pooling <- false
  use connection = new Microsoft.Data.Sqlite.SqliteConnection(settings.ToString())
  connection.Open()
  use command = connection.CreateCommand()
  command.CommandText <-
    "INSERT OR REPLACE INTO config_v0(key, value) VALUES ('live.autopush', 'off')"
  command.ExecuteNonQuery() |> ignore<int>


type Outcome =
  { exitCode : int
    stdout : string
    stderr : string
    result : Option<Dval>
    cleanupErrors : List<string> }

/// The request and result travel separately from stdout. The host owns the
/// temporary directory and process tree, and reports cleanup failures even after success.
let run
  (snapshot : Option<string -> Result<unit, string>>)
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
        let backup =
          snapshot |> Option.defaultValue LibDB.Sqlite.Backup.toTestBaseline
        backup (path "data.db")
        |> Result.map (fun () ->
          prepareStore (path "data.db")
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
