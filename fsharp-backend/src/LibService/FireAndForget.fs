module LibService.FireAndForget

open System.Threading.Tasks
open System.Threading
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

/// Execute a function in the backgorund,
/// ignoring any results and forwarding exceptions to Rollbar
let fireAndForgetTask
  (executionID : ExecutionID)
  (name : string)
  (f : unit -> Task<'b>)
  : unit =
  task {
    use _ =
      Telemetry.child
        "fireAndForget"
        [ "taskName", name; "executionID", executionID ]
    try
      let! (_ : 'b) = f ()
      Telemetry.addTag "success" true
      return ()
    with
    | e ->
      Telemetry.addTag "success" false
      Rollbar.sendException
        executionID
        Rollbar.emptyPerson
        [ "fire-and-forget", name ]
        e
      return ()
  }
  |> ignore<Task<unit>>
