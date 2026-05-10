module Builtins.Http.Client.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins (config : Libs.HttpClient.Configuration) =
  Builtin.combine [ Libs.HttpClient.builtins config ] fnRenames
