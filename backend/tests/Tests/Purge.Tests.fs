/// What a purge has to empty, checked against the schema rather than against a list someone remembered to
/// update.
///
/// `purge` empties `package_ops` so the store can be refilled from disk. Anything whose rows describe rows
/// in `package_ops` has to go at the same time, or it is left referring to a log that no longer contains
/// what it names. Two of those tables are canonical -- nothing rebuilds them -- so a stale row survives
/// every subsequent reload and can only be removed by hand.
///
/// This is checked statically, by reading the schema, because the honest dynamic test would call `purge ()`
/// and the suite shares one store: it would empty the store out from under every other test in the run.
/// The static form still catches the thing that actually goes wrong, which is a table being ADDED and the
/// purge list not being told.
///
/// It has already caught two. `op_branches` was left behind by every `reload-packages`, which put the
/// branch overlay permanently into the state `dark status` reports as a store problem. `op_owners`, the
/// relay's ownership index, was leaking on the same path so quietly that a dev store had 217,709 dead rows
/// out of 379,285 -- invisible because every reader joins through to `package_ops`, and the join drops
/// them.
module Tests.Purge

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module Purge = LibDB.Purge

open TestUtils.TestUtils


/// Every table in this store carrying a column named `op_id`.
///
/// The column name IS the coupling. A table that stores an op id is making a claim about the contents of
/// `package_ops`, and emptying the log without emptying it leaves that claim false.
let private tablesWithOpId () : Task<List<string>> =
  Sql.query
    """
    SELECT m.name AS name
    FROM sqlite_master m
    JOIN pragma_table_info(m.name) p
    WHERE m.type = 'table' AND p.name = 'op_id'
    """
  |> Sql.executeAsync (fun read -> read.string "name")


let opIdTablesArePurged =
  testTask "every table holding an op_id is emptied by a purge" {
    let! withOpId = tablesWithOpId ()

    // If this is empty the query is wrong, not the schema, and the assertion below would pass vacuously.
    Expect.isNonEmpty withOpId "found tables carrying an op_id"

    let missed =
      withOpId |> List.filter (fun t -> not (List.contains t Purge.tables))
    let missedNames = String.concat ", " missed

    Expect.isEmpty
      missed
      $"every op_id table is in Purge.tables (missing: {missedNames}). \
        A table naming ops has to be emptied with them, or a purge leaves it pointing at a log that no \
        longer holds what it names. Add it to `Purge.tables`, or if it genuinely should outlive a reload, \
        say why in the NOT-here note there."
  }


let purgeTablesAllExist =
  testTask "every table a purge names exists in the schema" {
    // The other direction, and the reason it is worth asserting: `purge` filters its list by existence, so
    // a table renamed out from under it is silently skipped rather than failing. That is right for a store
    // mid-migration and wrong as a permanent state, because the rows it meant to delete stop being deleted
    // and nothing says so.
    let! present =
      Sql.query "SELECT name FROM sqlite_master WHERE type = 'table'"
      |> Sql.executeAsync (fun read -> read.string "name")

    let ghosts = Purge.tables |> List.filter (fun t -> not (List.contains t present))
    let ghostNames = String.concat ", " ghosts

    Expect.isEmpty
      ghosts
      $"no table in Purge.tables is missing from the schema (ghosts: {ghostNames}). \
        `purge` skips tables that don't exist, so a renamed one stops being emptied silently."
  }


let tests = testList "Purge" [ opIdTablesArePurged; purgeTablesAllExist ]
