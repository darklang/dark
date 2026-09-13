/// Blazor WASM entry point. No Razor components / UI: the pages in wwwroot drive
/// everything through JS interop (`Repl.Eval` for the REPL, `Host.Cli.RunCli` for the CLI).
module Darklang.Wasm.Program

open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.JSInterop

[<EntryPoint>]
let Main (args : string[]) : int =
  // Before anything reads LibConfig: it computes the store path from the environment once.
  Host.Cli.configureEnvironment ()
  let builder = WebAssemblyHostBuilder.CreateDefault(args)
  let host = builder.Build()
  let js = host.Services.GetRequiredService<IJSRuntime>() :?> IJSInProcessRuntime
  Host.Browser.init js
  // Everything the CLI prints, TUI frames included, goes to the page's terminal: the
  // interpreter's own prints through the sink (never via System.Console, see
  // NonBlockingConsole), anything else that writes to Console through the writer.
  NonBlockingConsole.setBrowserSink Host.Browser.writeToTerminal
  let writer = new Host.Browser.TerminalWriter()
  System.Console.SetOut writer
  System.Console.SetError writer
  // Where exactly a blocking wait was attempted: the synchronous chain at the throw, which the
  // exception's own trace loses across async boundaries.
  System.AppDomain.CurrentDomain.FirstChanceException.Add(fun e ->
    match e.Exception with
    | :? System.PlatformNotSupportedException ->
      Host.Browser.log ("first-chance PNSE at:\n" + System.Environment.StackTrace)
    | _ -> ())
  host.RunAsync() |> ignore
  0
