/// Blazor WASM entry point. No Razor components / UI: the pages in wwwroot drive
/// everything through JS interop (`Repl.Eval` for the REPL, `Cli.RunCli` for the CLI).
module Darklang.Wasm.Program

open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.JSInterop

[<EntryPoint>]
let Main (args : string[]) : int =
  // Before anything reads LibConfig: it computes the store path from the environment once.
  Cli.configureEnvironment ()
  let builder = WebAssemblyHostBuilder.CreateDefault(args)
  let host = builder.Build()
  let js = host.Services.GetRequiredService<IJSRuntime>() :?> IJSInProcessRuntime
  Browser.init js
  // Everything the CLI prints, TUI frames included, goes to the page's terminal: the
  // interpreter's own prints through the sink (never via System.Console, see
  // NonBlockingConsole), anything else that writes to Console through the writer.
  NonBlockingConsole.setBrowserSink Browser.writeToTerminal
  let writer = new Browser.TerminalWriter()
  System.Console.SetOut writer
  System.Console.SetError writer
  host.RunAsync() |> ignore
  0
