/// Empty the package store so it can be refilled from disk.
///
/// The rule: if a table's rows describe rows in `package_ops`, it is emptied with them.
/// Anything surviving the log it was written against is a reference to nothing.
module LibDB.Purge

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite

/// Every table a purge empties. Public because `Purge.Tests` checks it against the
/// schema: any table carrying an `op_id` must appear here. NOT here, deliberately:
/// `branches`, `commits`, `conflicts` and `relay_branches` outlive a reload -- a branch
/// whose ops are gone is empty, not deleted.
let tables : List<string> =
  [ "locations"
    "package_types"
    "package_values"
    "package_functions"
    "package_ops"
    "package_dependencies"
    "deprecations"

    // A cache of clean type-check results; keeping it would describe items that are gone.

    // The branch OVERLAY goes with the ops: emptying the log while keeping these leaves
    // every tag pointing at nothing, so a purge ends a branch's uncommitted work.
    "op_branches"
    "branch_name_bases"

    // The relay's ownership index. Dead rows here are invisible: readers join package_ops.
    "op_owners"

    // Which ops came from a build's embedded seed; a stale entry misreports that.
    "seed_ops" ]


let purge () : Task<unit> =
  task {
    let tableExists (tableName : string) : bool =
      Sql.query
        "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = @tableName"
      |> Sql.parameters [ "tableName", Sql.string tableName ]
      |> Sql.executeExistsSync

    // Existence-filtered: this runs against stores whose migrations haven't caught up.
    let statements =
      tables
      |> List.filter tableExists
      |> List.map (fun table -> ($"DELETE FROM {table}", [ [] ]))

    if not (List.isEmpty statements) then
      statements |> Sql.executeTransactionSync |> ignore<List<int>>
  }
