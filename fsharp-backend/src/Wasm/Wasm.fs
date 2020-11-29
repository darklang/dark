namespace Wasm

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

open System.Threading.Tasks
open FSharp.Control.Tasks

module Program =

  // call this from JS with DotNet.invokeMethod('Wasm', 'run', 7)
  // or DotNet.invokeMethodAsync('Wasm', 'run', 8)
  [<Microsoft.JSInterop.JSInvokable>]
  let run (arg : int) : Task<string> =
    task {
      let prog = LibExecution.Runtime.Shortcuts.eInt arg
      let! result = LibExecution.Execution.run [] LibExecution.StdLib.fns prog
      return result.ToString()
    }


  [<EntryPoint>]
  let Main args =
    let builder = WebAssemblyHostBuilder.CreateDefault([||])
    builder.Build().RunAsync() |> ignore
    0
