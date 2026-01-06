module LibService.FireAndForget

open System.Threading.Tasks
open System.Threading
open FSharp.Control.Tasks

open Prelude

/// Execute a function in the backgorund,
/// ignoring any results and forwarding exceptions to Rollbar
let fireAndForgetTask (name : string) (f : unit -> Task<'b>) : unit =
  // this should be a backgroundTask, but that doesn't work due to
  // https://github.com/dotnet/fsharp/issues/12761
  task {
    try
      // Resolve to make sure we catch the exception
      let! (_ : 'b) = f ()
      return ()
    with e ->
      Rollbar.sendException None [ "fire-and-forget", name ] e
      return ()
  }
  |> ignore<Task<unit>>
