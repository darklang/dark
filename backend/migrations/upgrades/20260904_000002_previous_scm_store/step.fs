/// The `20260904_000002_previous_scm_store` upgrade; why it exists is in `upgrade.toml` beside this file.
module LibDB.UpgradeSteps.U20260904_000002_previous_scm_store

open Fumble
open LibDB
open LibDB.Sqlite
open LibDB.Upgrades

open Prelude

let run () : unit =
  if tableExists "branch_ops" then
    Exception.raiseInternal
      "this store was made by a Darklang from before the current source-control layout (it has \
       a `branch_ops` table), and nothing in it carries over to this version"
      []
