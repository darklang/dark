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
  task {
    try
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
