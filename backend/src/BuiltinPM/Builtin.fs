module BuiltinPM.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
module PT = LibExecution.ProgramTypes


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins (pm : PT.PackageManager) : Builtins =
  Builtin.combine
    [ Libs.PackageOps.builtins
      Libs.Sync.builtins
      Libs.Instances.builtins
      Libs.Account.builtins
      Libs.Branches.builtins
      Libs.PackageHistory.builtins
      Libs.Packages.builtins pm ]
    fnRenames
