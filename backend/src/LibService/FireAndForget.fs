module LibService.FireAndForget

open System.Threading.Tasks
open System.Threading
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

/// Execute a function in the backgorund,
/// ignoring any results and forwarding exceptions to Rollbar
let fireAndForgetTask (name : string) (f : unit -> Task<'b>) : unit =
  // this should be a backgroundTask, but that doesn't work due to
  // https://github.com/dotnet/fsharp/issues/12761
  task {
    use _ = Telemetry.child $"fireAndForget: {name}" [ "task_name", name ]
    try
      // Resolve to make sure we catch the exception
      let! (_ : 'b) = f ()
      Telemetry.addTag "success" true
      return ()
    with e ->
      Telemetry.addTag "success" false
      Rollbar.sendException None [ "fire-and-forget", name ] e
      return ()
  }
  |> ignore<Task<unit>>
