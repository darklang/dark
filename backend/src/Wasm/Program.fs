/// Blazor WASM entry point. Minimal: it just starts the host so the JS side can
/// call `Darklang.Wasm.Repl.Eval`. No Razor components / UI — the UI is plain
/// HTML in wwwroot/index.html (which drives everything via JS interop).
module Darklang.Wasm.Program

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

[<EntryPoint>]
let Main (args : string[]) : int =
  let builder = WebAssemblyHostBuilder.CreateDefault(args)
  builder.Build().RunAsync() |> ignore
  0
