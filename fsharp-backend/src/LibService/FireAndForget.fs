module LibService.FireAndForget

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

let fireAndForgetTask
  (name : string)
  (executionID : ExecutionID)
  (f : unit -> Task<'b>)
  : unit =
  task {
    try
      let! (_ : 'b) = f ()
      return ()
    with
    | e ->
      Rollbar.sendException $"fire-and-forget failed: {name}" executionID [] e
      return ()
  }
  |> ignore<Task<unit>>
