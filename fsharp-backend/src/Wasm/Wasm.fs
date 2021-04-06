namespace Wasm

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Prelude.Tablecloth

module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module RT = LibExecution.RuntimeTypes

module Program =
  let stdlib =
    LibExecution.StdLib.StdLib.fns
    |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)


  // call this from JS with DotNet.invokeMethod('Wasm', 'run', 7)
  // or DotNet.invokeMethodAsync('Wasm', 'run', 8)
  [<Microsoft.JSInterop.JSInvokable>]
  let run (arg : int) : Task<string> =
    task {
      let expr = LibExecution.Shortcuts.eInt arg
      // FSTODO: get packages from caller
      let libraries : RT.Libraries = { stdlib = stdlib; packageFns = Map.empty }
      let tracing = LibExecution.Execution.noTracing RT.Preview

      let uuid = System.Guid.NewGuid()

      // FSTODO: get all this info from the caller
      let program : RT.ProgramContext =
        { accountID = uuid
          canvasID = uuid
          userFns = Map.empty
          userTypes = Map.empty
          dbs = Map.empty
          secrets = [] }

      let tlid = id 7
      let state = Exe.createState libraries tracing tlid program
      let! result = Exe.executeExpr state Map.empty expr

      return result.ToString()
    }


  [<EntryPoint>]
  let Main args =
    let builder = WebAssemblyHostBuilder.CreateDefault([||])
    builder.Build().RunAsync() |> ignore
    0
