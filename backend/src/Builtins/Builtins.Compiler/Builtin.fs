module Builtins.Compiler.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () =
  Builtin.combine [ Libs.Compiler.builtins () ] fnRenames
