module Builtins.Http.Server.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins () : Builtins =
  Builtin.combine [ Libs.HttpServer.builtins () ] fnRenames


/// Listening for HTTP. See `HttpClient` for why the two are not one platform.
///
/// Its one builtin also prints and reads the clock, but only with request logging on, so those two
/// are `dynamicEffects` rather than static ones: a server that prints nothing should not need
/// permission to print. See `Libs.HttpServer.dynamicEffects`.
let platform : LibExecution.Platform.Platform =
  { name = "HttpServer"
    version = 0
    description = "Serving HTTP on a bound port."
    builtins = builtins ()
    requires = [ "Core" ]
    dynamicEffects = Libs.HttpServer.dynamicEffects
    requiresStore = false }
