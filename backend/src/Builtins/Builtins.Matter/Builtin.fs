/// Persistence + state surface — UserDB, package manager, traces.
module Builtins.Matter.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
module PT = LibExecution.ProgramTypes


let fnRenames : Builtin.FnRenames = []

let builtins (pm : PT.PackageManager) : Builtins =
  Builtin.combine
    [ // DB
      Libs.DB.builtins ()

      // PM (package manager — packages, branches, ops, merge, …)
      Libs.PM.Packages.builtins pm
      Libs.PM.PackageOps.builtins pm
      Libs.PM.Branches.builtins ()
      Libs.PM.Rebase.builtins ()
      Libs.PM.Merge.builtins ()
      Libs.PM.Scripts.builtins ()
      Libs.PM.Dependencies.builtins ()
      Libs.PM.Seed.builtins

      // Traces (reader surface)
      Libs.Traces.builtins () ]
    fnRenames
