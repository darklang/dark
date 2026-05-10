module Builtins.Cli.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins () =
  Builtin.combine
    [ Libs.Directory.builtins ()
      Libs.Environment.builtins ()
      Libs.File.builtins ()
      Libs.Execution.builtins ()
      Libs.Output.builtins ()
      Libs.Stdin.builtins ()
      Libs.Terminal.builtins ()
      Libs.Posix.builtins () ]
    fnRenames
