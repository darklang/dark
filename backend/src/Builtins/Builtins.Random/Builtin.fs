module Builtins.Random.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () =
  Builtin.combine [ Libs.Uuid.builtins (); Libs.Random.builtins () ] fnRenames
