/// The `20261008_000001_trace_results_in_trace_blobs` upgrade; why it exists is in `upgrade.toml` beside this file.
module LibDB.UpgradeSteps.U20261008_000001_trace_results_in_trace_blobs

open Fumble
open LibDB
open LibDB.Sqlite
open LibDB.Upgrades

open Prelude

let run () : unit =
  let rec moveBatch () =
    let rows =
      Sql.query
        "SELECT rowid AS rid, trace_id, result FROM trace_fn_calls
         WHERE typeof(result) = 'blob' LIMIT 5000"
      |> Sql.execute (fun read ->
        read.int64 "rid", read.string "trace_id", read.bytes "result")
      |> Result.unwrap
    if not (List.isEmpty rows) then
      let hashed =
        rows
        |> List.map (fun (rid, traceId, bytes) ->
          (rid, traceId, bytes, LibExecution.Blob.sha256Hex bytes))
      Sql.executeTransactionSync
        [ "INSERT OR IGNORE INTO trace_blobs (trace_id, hash, bytes)
           VALUES (@t, @h, @b)",
          hashed
          |> List.map (fun (_, t, b, h) ->
            [ "t", Sql.string t; "h", Sql.string h; "b", Sql.bytes b ])
          "UPDATE trace_fn_calls SET result = @h WHERE rowid = @rid",
          hashed
          |> List.map (fun (rid, _, _, h) ->
            [ "h", Sql.string h; "rid", Sql.int64 rid ]) ]
      |> ignore<List<int>>
      moveBatch ()
  if tableExists "trace_fn_calls" then moveBatch ()
