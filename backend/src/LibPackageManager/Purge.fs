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
        "commits"
        "branch_ops" ]
      |> List.filter tableExists
      |> List.map (fun table -> ($"DELETE FROM {table}", [ [] ]))

    // Also delete non-main branches
    let branchPurge =
      if tableExists "branches" then
        [ ("DELETE FROM branches WHERE id != '89282547-e4e6-4986-bcb6-db74bc6a8c0f'",
           [ [] ]) ]
      else
        []

    let allPurge = tablesToPurge @ branchPurge
    if not (List.isEmpty allPurge) then
      allPurge |> Sql.executeTransactionSync |> ignore<List<int>>
  }
