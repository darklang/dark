module Builtins.Http.Client.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () = Builtin.combine [ Libs.HttpClient.builtins () ] fnRenames
