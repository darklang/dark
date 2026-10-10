/// The `20260909_000003_secrets_leave_the_store` upgrade; why it exists is in `upgrade.toml` beside this file.
module LibDB.UpgradeSteps.U20260909_000003_secrets_leave_the_store

open Fumble
open LibDB
open LibDB.Sqlite
open LibDB.Upgrades

open Prelude

let run () : unit =
  if tableExists "config_v0" then
    let rows =
      (Sql.query "SELECT key, value FROM config_v0 WHERE key LIKE 'sync.secret.%'"
       |> Sql.executeAsync (fun read -> (read.string "key", read.string "value")))
        .Result

    for (key, value) in rows do
      Config.set key value |> Async.AwaitTask |> Async.RunSynchronously

      Sql.query "DELETE FROM config_v0 WHERE key = @key"
      |> Sql.parameters [ "key", Sql.string key ]
      |> Sql.executeStatementSync

    if not (List.isEmpty rows) then
      print
        $"  release: moved {List.length rows} write secret(s) out of the store into credentials.db"
