/// Empty the package store so it can be refilled from disk.
///
/// The rule: if a table's rows describe rows in `package_ops`, it is emptied with
/// `package_ops`. Anything surviving the log it was written against is a reference
/// to nothing, and `op_id` columns are the direct form of that.
///
/// Two are canonical rather than projections, which makes getting it wrong permanent:
/// a projection left behind is rebuilt by the next fold, while `op_branches` and
/// `op_owners` are rebuilt by nothing and sit stale until deleted by hand.
module LibDB.Purge

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite

/// Every table a purge empties.
///
/// Public because `Purge.Tests` checks it against the schema: any table carrying an
/// `op_id` must appear here, and a new one that does not fails that test rather than
/// quietly leaking rows.
///
/// `deprecations` has FKs to `commits.hash` and `branches.id`, so it is emptied before
/// them in the same transaction. NOT here, deliberately: `branches`, `commits` and
/// `conflicts` outlive a reload -- a branch whose ops are gone is empty, not deleted --
/// and `relay_branches` holds self-contained bundles.
let tables : List<string> =
  [ "locations"
    "package_types"
    "package_values"
    "package_functions"
    "package_ops"
    "package_dependencies"
    "deprecations"

    // A cache of hashes whose type surface came back clean. Strictly it need not be
    // emptied -- a hash's answer is true forever -- but the content tables go here,
    // so keeping it would leave rows about items the store no longer holds, and
    // refilling costs one walk.
    "type_checked"

    // The branch OVERLAY goes with the ops, and it has to. `op_branches` tags ops by id and
    // `branch_name_bases` records the version each branch's names forked from.
    // Emptying the log while keeping them leaves every tag pointing at nothing.
    //
    // Preserving the branch ops instead is not the alternative it looks like. Their
    // content lives in the `package_*` tables, which are emptied here and refilled
    // only from disk, and a branch's work is precisely the part that is not on disk.
    // Keeping the ops would trade dangling tags for live names bound to hashes with
    // nothing behind them. So a purge ends a branch's uncommitted work and leaves
    // the branch row standing but empty.
    "op_branches"
    "branch_name_bases"

    // The relay's ownership index: which instance pushed which op, so it can serve
    // "your stuff" back. Same reasoning, and it is the one that showed how quiet
    // this failure is -- a dev store had 217,709 dead rows out of 379,285, invisible
    // because every reader joins to `package_ops` and the join simply drops them.
    "op_owners" ]


let purge () : Task<unit> =
  task {
    let tableExists (tableName : string) : bool =
      Sql.query
        "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = @tableName"
      |> Sql.parameters [ "tableName", Sql.string tableName ]
      |> Sql.executeExistsSync

    // Existence-filtered because this runs against stores whose migrations haven't
    // caught up yet.
    let statements =
      tables
      |> List.filter tableExists
      |> List.map (fun table -> ($"DELETE FROM {table}", [ [] ]))

    if not (List.isEmpty statements) then
      statements |> Sql.executeTransactionSync |> ignore<List<int>>
  }
