namespace Wasm

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

module Program =

  // call this from JS with DotNet.invokeMethod('Wasm', 'run', 7)
  // or DotNet.invokeMethodAsync('Wasm', 'run', 8)
  [<Microsoft.JSInterop.JSInvokable>]
  let run (arg : int) = 5 + arg

  [<EntryPoint>]
  let Main args =
      let builder = WebAssemblyHostBuilder.CreateDefault([||])
      builder.Build().RunAsync() |> ignore
      0
