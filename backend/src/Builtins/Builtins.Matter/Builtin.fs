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

      // Sync (the op-log wire, the blob channel, the conflict review log)
      Libs.Sync.OpLog.builtins ()
      Libs.Sync.Blobs.builtins ()
      Libs.Conflicts.builtins ()

      // PM (package manager — packages, branches, ops, merge, …)
      Libs.PM.Packages.builtins pm
      Libs.PM.AtRestTypeChecker.builtins pm
      Libs.PM.PackageOps.builtins pm
      Libs.PM.Branches.builtins ()
      Libs.PM.Rebase.builtins ()
      Libs.PM.Merge.builtins ()
      Libs.PM.Dependencies.builtins ()
      Libs.PM.Seed.builtins
      Libs.PM.Caps.builtins
      Libs.PM.Store.builtins ()

      // Traces (reader surface)
      Libs.Traces.builtins ()

      // Accounts
      Libs.Account.builtins () ]
    fnRenames
