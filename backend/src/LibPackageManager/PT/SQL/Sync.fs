/// Manages synchronization of package ops between Darklang instances.
module LibPackageManager.PT.SQL.Sync


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module DarkDateTime = LibExecution.DarkDateTime
module BS = LibSerialization.Binary.Serialization

/// Get package ops created since the specified datetime
let getOpsSince
  (branchID : Option<PT.BranchID>)
  (since : DarkDateTime.T)
  : Task<List<PT.PackageOp>> =
  task {
    let sinceStr = DarkDateTime.toIsoString since

    match branchID with
    | Some id ->
      // Query specific branch only
      return!
        Sql.query
          """
          SELECT op_blob
          FROM package_ops
          WHERE branch_id = @branch_id
            AND datetime(created_at) > datetime(@since)
          ORDER BY created_at ASC
          """
        |> Sql.parameters [ "branch_id", Sql.uuid id; "since", Sql.string sinceStr ]
        |> Sql.executeAsync (fun read ->
          let opBlob = read.bytes "op_blob"
          BS.PT.PackageOp.deserialize opBlob)
    | None ->
      // Query main branch only (branch_id IS NULL)
      return!
        Sql.query
          """
          SELECT op_blob
          FROM package_ops
          WHERE branch_id IS NULL
            AND datetime(created_at) > datetime(@since)
          ORDER BY created_at ASC
          """
        |> Sql.parameters [ "since", Sql.string sinceStr ]
        |> Sql.executeAsync (fun read ->
          let opBlob = read.bytes "op_blob"
          BS.PT.PackageOp.deserialize opBlob)
  }


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
