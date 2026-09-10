module Builtins.Http.Client.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () = Builtin.combine [ Libs.HttpClient.builtins () ] fnRenames


/// Outbound HTTP. Separate from `HttpServer` because reaching out and listening are different
/// risks, wanted by different programs, and scoped by different rules.
let platform : LibExecution.Platform.Platform =
  { name = "HttpClient"
    version = 0
    description = "Outbound HTTP requests."
    builtins = builtins ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }
