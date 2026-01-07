module LibService.FireAndForget

open System.Threading.Tasks
open System.Threading
open FSharp.Control.Tasks

open Prelude

/// Execute a function in the background,
/// ignoring any results and printing exceptions to console
let fireAndForgetTask (name : string) (f : unit -> Task<'b>) : unit =
  // this should be a backgroundTask, but that doesn't work due to
  // https://github.com/dotnet/fsharp/issues/12761
  task {
    try
      // Resolve to make sure we catch the exception
      let! (_ : 'b) = f ()
      return ()
    with e ->
      printException $"[fire-and-forget: {name}]" [] e
      return ()
  }
  |> ignore<Task<unit>>
