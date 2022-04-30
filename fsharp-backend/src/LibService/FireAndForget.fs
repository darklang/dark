module LibService.FireAndForget

open System.Threading.Tasks
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
  // CLEANUP: this should be a backgroundTask, but that doesn't work due to
  // https://github.com/dotnet/fsharp/issues/12761
  task {
    try
      // Resolve to make sure we catch the exception
      let! (_ : 'b) = f ()
      return ()
    with
    | e ->
      Rollbar.sendException
        executionID
        Rollbar.emptyPerson
        [ "fire-and-forget", name ]
        e
      return ()
  }
  |> ignore<Task<unit>>
