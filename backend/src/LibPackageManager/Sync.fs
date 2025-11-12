/// Manages synchronization of package ops between Darklang instances.
module LibPackageManager.Sync


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module DarkDateTime = LibExecution.DarkDateTime


/// Get the most recent sync time with a specific instance
/// Returns None if no sync has occurred with this instance
let getLastSyncDate (instanceID : System.Guid) : Ply<Option<DarkDateTime.T>> =
  uply {
    let! result =
      Sql.query
        """
        SELECT synced_at
        FROM syncs
        WHERE instance_id = @instance_id
        ORDER BY synced_at DESC
        LIMIT 1
        """
      |> Sql.parameters [ "instance_id", Sql.uuid instanceID ]
      |> Sql.executeAsync (fun read ->
        let syncedAtStr = read.string "synced_at"
        let instant = NodaTime.Instant.ofIsoString syncedAtStr
        DarkDateTime.fromInstant instant)

    match result with
    | [] -> return None
    | dt :: _ -> return Some dt
  }


/// Record a sync event in the database
let recordSync
  (instanceID : System.Guid)
  (opsPushed : int64)
  (opsFetched : int64)
  : Task<unit> =
  task {
    let syncId = System.Guid.NewGuid()

    do!
      Sql.query
        """
        INSERT INTO syncs (id, instance_id, ops_pushed, ops_fetched)
        VALUES (@id, @instance_id, @ops_pushed, @ops_fetched)
        """
      |> Sql.parameters
        [ "id", Sql.uuid syncId
          "instance_id", Sql.uuid instanceID
          "ops_pushed", Sql.int64 opsPushed
          "ops_fetched", Sql.int64 opsFetched ]
      |> Sql.executeStatementAsync
  }
