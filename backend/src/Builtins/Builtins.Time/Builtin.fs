module Builtins.Time.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () = Builtin.combine [ Libs.Time.builtins () ] fnRenames
