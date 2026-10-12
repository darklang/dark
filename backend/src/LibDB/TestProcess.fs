/// Shared transport for isolated package tests and explicit test callbacks.
module LibDB.TestProcess

open Prelude
open LibExecution.RuntimeTypes

module Permissions = LibExecution.Permissions

type Outcome =
  { exitCode : int
    stdout : string
    stderr : string
    result : Option<Dval>
    cleanupErrors : List<string> }

/// The request and result travel separately from stdout. The host owns the
/// snapshot and process tree, and reports cleanup failures even after success.
let run
  (snapshot : Option<LibExecution.HostTypes.TestStoreSnapshot>)
  (state : ExecutionState)
  (vm : VMState)
  (access : Permissions.Access)
  (branchId : System.Guid)
  (request : Dval)
  (timeoutMs : int)
  (columns : int)
  (rows : int)
  : Ply<Result<Outcome, string>> =
  uply {
    try
      let request =
        LibSerialization.Binary.Serialization.RT.Dval.serialize
          "isolated test request"
          request
      use buffer = new System.IO.MemoryStream()
      use writer = new System.IO.BinaryWriter(buffer)
      LibSerialization.Binary.Serializers.Permissions.writeExecutionAccess
        writer
        access
      writer.Flush()
      let! outcome =
        LibExecution.PermissionCheck.performHostWithAccess
          state
          vm
          access
          (LibExecution.HostTypes.Operation.IsolatedTest(
            (match snapshot with
             | Some baseline -> baseline.CopyTo
             | None -> LibDB.Sqlite.Backup.toTestBaseline),
            branchId,
            request,
            PolicyStore.testWorkerStore (),
            buffer.ToArray(),
            timeoutMs,
            columns,
            rows
          ))
      match outcome with
      | Error error -> return Error error.message
      | Ok(LibExecution.HostTypes.Response.IsolatedTestOutcome(code,
                                                               stdout,
                                                               stderr,
                                                               result,
                                                               errors)) ->
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
