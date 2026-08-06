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

      // (carve: F# sync builtins removed — op-log wire, blob channel, conflict
      // review log. Sync is being rebuilt in Dark on the op substrate, cloud-first.)

      // PM (package manager — packages, branches, ops, merge, …)
      Libs.PM.Packages.builtins pm
      Libs.PM.PackageOps.builtins pm
      // (carve: merge/rebase builtins removed -- single-scope MVP, SCM rebuilt in Dark)
      Libs.PM.Seed.builtins
      Libs.PM.Caps.builtins
      Libs.PM.Store.builtins ()

      // Traces (reader surface)
      Libs.Traces.builtins ()

      // Accounts
      Libs.Account.builtins () ]
    fnRenames
