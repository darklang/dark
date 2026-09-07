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
/// schema: any table carrying an `op_id` must appear here.
///
/// A table can also name the log by CONTENT HASH, and then it depends what it claims. A
/// cache keyed on content survives: `type_checked` and `package_blobs` say "this hash has
/// this property", and a hash IS its content. A claim about the LOG'S STATE does not --
/// `conflicts` and `sync_bases` describe something that did not happen to the new log, and
/// still read as actionable.
///
/// NOT here, deliberately: `branches`, `commits` and `relay_branches` outlive a reload --
/// a branch whose ops are gone is empty, not deleted.
let tables : List<string> =
  [ "locations"
    "package_types"
    "package_values"
    "package_functions"
    "package_ops"
    "package_dependencies"
    "deprecations"

    // Folded from `Decision` ops (main rows by the fold, branch rows by `Branches.refoldBranchDecides`),
    // so it says what the log decided about a name, and a purged log decided nothing.
    "propagation_policy"

    // Re-derived by the fold, and only meaningful against the log that produced them.
    // A conflict naming two hashes the store no longer holds is unreviewable and
    // unresolvable, and `dark conflicts` presents it as neither.
    "conflicts"

    // "This peer and I agreed on this hash." Replacing the log makes that false, and a
    // wrong base makes the NEXT sync compute the wrong diff. Dropping it costs one
    // wholesale adopt on the next pull, which is what a store with no base does anyway.
    "sync_bases"

    // The branch OVERLAY goes with the ops: emptying the log while keeping these leaves
    // every tag pointing at nothing, so a purge ends a branch's uncommitted work.
    "op_branches"
    "branch_name_bases"

    // The relay's ownership index. Dead rows here are invisible: readers join package_ops.
    "op_owners"

    // Which of OUR ops each relay holds. A re-minted log gets re-pushed in full, which the relay dedups.
    "sync_pushed"

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
