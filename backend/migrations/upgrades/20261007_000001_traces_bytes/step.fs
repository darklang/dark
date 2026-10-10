/// The `20261007_000001_traces_bytes` upgrade; why it exists is in `upgrade.toml` beside this file.
module LibDB.UpgradeSteps.U20261007_000001_traces_bytes

open Fumble
open LibDB
open LibDB.Sqlite
open LibDB.Upgrades

open Prelude

let run () : unit =
  if tableExists "traces" then
    addColumnIfMissing "traces" "bytes" "INTEGER NOT NULL DEFAULT 0"
    Sql.query
      "UPDATE traces SET bytes =
         COALESCE(LENGTH(input_value), 0) + COALESCE(LENGTH(result_value), 0)
         + COALESCE((SELECT SUM(LENGTH(c.args) + LENGTH(c.result))
                     FROM trace_fn_calls c WHERE c.trace_id = traces.id), 0)
       WHERE bytes = 0"
    |> Sql.executeStatementSync
