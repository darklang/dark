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
    [ Libs.Packages.builtins pm
      Libs.PackageOps.builtins
      Libs.Scripts.builtins
      Libs.Dependencies.builtins ]
    fnRenames
