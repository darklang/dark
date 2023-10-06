module BuiltinCliHost.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let contents = Builtin.combine [ Libs.Cli.contents ] fnRenames
