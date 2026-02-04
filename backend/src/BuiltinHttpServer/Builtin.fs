module BuiltinHttpServer.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins : Builtins =
  Builtin.combine
    [ Libs.HttpServer.builtins ]
    fnRenames
