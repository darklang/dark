/// Persistence + state surface — UserDB, package manager, traces.
module Builtins.Matter.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
module PT = LibExecution.ProgramTypes


let fnRenames : Builtin.FnRenames = []

let builtins (pm : PT.PackageManager) : Builtins =
  Builtin.combine
    [ // DB (UserDB, plus the raw general-purpose SQLite floor)
      Libs.DB.builtins ()
      Libs.Sqlite.builtins ()

      // PM (package manager: packages, branches, ops)
      Libs.PM.Packages.builtins pm
      Libs.PM.PackageOps.builtins pm
      Libs.PM.Seed.builtins
      Libs.PM.Caps.builtins
      Libs.PM.Store.builtins ()

      // Traces (reader surface)
      Libs.Traces.builtins ()

      // Accounts
      Libs.Account.builtins () ]
    fnRenames
