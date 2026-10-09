/// The `20260909_000004_candidates_gain_removed` upgrade; why it exists is in `upgrade.toml` beside this file.
module LibDB.UpgradeSteps.U20260909_000004_candidates_gain_removed

open Fumble
open LibDB
open LibDB.Sqlite
open LibDB.Upgrades

open Prelude

let run () : unit =
  if tableExists "conflicts" then
    let rows =
      (Sql.query
        "SELECT id, candidates FROM conflicts WHERE candidates NOT LIKE '%\"removed\"%'"
       |> Sql.executeAsync (fun read -> (read.string "id", read.string "candidates")))
        .Result

    for (id, json) in rows do
      // After the hash, which both shapes carry: a name divergence and a doc divergence
      // write the same field order.
      let patched =
        System.Text.RegularExpressions.Regex.Replace(
          json,
          "(\"hash\":\"[^\"]*\")",
          "$1,\"removed\":false"
        )

      Sql.query "UPDATE conflicts SET candidates = @c WHERE id = @id"
      |> Sql.parameters [ "c", Sql.string patched; "id", Sql.string id ]
      |> Sql.executeStatementSync

    if not (List.isEmpty rows) then
      print $"  release: added `removed` to {List.length rows} stored conflict(s)"
