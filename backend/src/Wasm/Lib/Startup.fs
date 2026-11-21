/// What runs before JS calls anything: `Exports.Main` in ../Exports.cs calls `Start`.
module Darklang.Wasm.Startup

let Start (log : System.Action<string>) : unit =
  // Before anything reads LibConfig: it computes the store path from the environment once.
  Cli.configureEnvironment ()
  Browser.setLog log
  // Everything the CLI prints, TUI frames included, goes to the page's terminal: the
  // interpreter's own prints through the sink (never via System.Console, see
  // NonBlockingConsole), anything else that writes to Console through the writer.
  NonBlockingConsole.setBrowserSink Browser.writeToTerminal
  let writer = new Browser.TerminalWriter()
  System.Console.SetOut writer
  System.Console.SetError writer
