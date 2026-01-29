module LibPackageManager.Purge

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

/// CLEANUP
/// This needs work:
/// - no reason this can't just be a few simple sql statements rather than this fanciness.
/// - I'm not even sure this is the right (complete) set of tables to purge
let purge () : Task<unit> =
  task {

    // Helper to check if a table exists
    let tableExists (tableName : string) : bool =
      Sql.query
        "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = @tableName"
      |> Sql.parameters [ "tableName", Sql.string tableName ]
      |> Sql.executeExistsSync

    // Delete from projection tables, source-of-truth ops table, and commits
    // Only delete if tables exist (handles case where migrations haven't run yet)
    let tablesToPurge =
      [ "locations"
        "package_types"
        "package_values"
        "package_functions"
        "package_ops"
        "package_dependencies"
        "commits" ]
      |> List.filter tableExists
      |> List.map (fun table -> ($"DELETE FROM {table}", [ [] ]))

    if not (List.isEmpty tablesToPurge) then
      tablesToPurge |> Sql.executeTransactionSync |> ignore<List<int>>
  }
