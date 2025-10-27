module BuiltinPM.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins =
  Builtin.combine
    [ Libs.PackageOps.builtins
      Libs.Sync.builtins
      Libs.Instances.builtins
      Libs.Account.builtins
      Libs.Branches.builtins
      Libs.PackageHistory.builtins ]
    fnRenames
