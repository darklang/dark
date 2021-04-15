namespace Wasm

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

module Program =

  [<EntryPoint>]
  let Main args =
    let builder = WebAssemblyHostBuilder.CreateDefault([||])
    builder.Build().RunAsync() |> ignore
    0
