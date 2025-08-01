module LibPackageManager.Utils

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


let flushCheckpoint () : Task<unit> =
  task {
    print "Flushing WAL to DB"
    // Use PASSIVE mode which doesn't require exclusive access
    // It will checkpoint as much as possible without blocking
    [ "PRAGMA wal_checkpoint(PASSIVE)" ]
    |> List.map (fun sql -> (sql, [ [] ]))
    |> Sql.executeTransactionSync
    |> ignore<List<int>>
  }
