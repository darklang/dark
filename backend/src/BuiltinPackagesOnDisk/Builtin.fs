module BuiltinPackagesOnDisk.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let contents = Builtin.combine [ Libs.Packages.contents ] fnRenames
