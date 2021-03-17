namespace Wasm

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Prelude.Tablecloth

module Exe = LibExecution.Execution


module Program =
  let fns =
    lazy (LibExecution.StdLib.StdLib.fns |> Map.fromListBy (fun fn -> fn.name))

  // call this from JS with DotNet.invokeMethod('Wasm', 'run', 7)
  // or DotNet.invokeMethodAsync('Wasm', 'run', 8)
  [<Microsoft.JSInterop.JSInvokable>]
  let run (arg : int) : Task<string> =
    task {
      let prog = LibExecution.Shortcuts.eInt arg
      let tlid = id 7
      let uuid = System.Guid.NewGuid()

      let state =
        Exe.createState
          uuid
          uuid
          tlid
          (fns.Force())
          Map.empty
          Map.empty
          Map.empty
          Map.empty
          []
          Exe.loadNoResults
          Exe.storeNoResults
          Exe.loadNoArguments
          Exe.storeNoArguments

      let! result = Exe.run state Map.empty prog

      return result.ToString()
    }


  [<EntryPoint>]
  let Main args =
    let builder = WebAssemblyHostBuilder.CreateDefault([||])
    builder.Build().RunAsync() |> ignore
    0
