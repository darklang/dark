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
/// A leak here is quiet: every reader joins through to `package_ops`, so the dead rows simply never
/// appear.
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


/// The OTHER coupling, and the one the test above cannot see: a table can name the log by content
/// hash rather than by `op_id`.
///
/// Two kinds, and they need opposite answers, so this cannot be automatic. A cache keyed on content
/// (`type_checked`, `package_blobs`) stays TRUE across a reload, because a hash is its content. A claim
/// about the log's STATE (`conflicts`, `sync_bases`) does not: replace the log and it describes something
/// that never happened to it.
///
/// So the guard is an allowlist. A new table with a hash column fails this until someone writes down which
/// kind it is. That is the whole point: without the forced answer a table simply sits outside the
/// purge, and nothing says so.
let private hashCoupledSurvivors =
  [ "commits" // canonical; a commit whose ops are gone is empty, not wrong
    "package_blobs" // content-addressed cache: hash IS the content
    "package_caps" // ditto, keyed on the item's hash
    "schema_state_v0" // describes the SCHEMA, not the log
    "trace_fn_calls" // observations of past runs; deliberately outlive their code
    "type_checked" ] // content-addressed cache: a clean result stays clean


let hashCoupledTablesAreClassified =
  testTask
    "every table naming the log by hash is either purged or a declared survivor" {
    let! withHash =
      Sql.query
        """
        SELECT DISTINCT m.name AS name
        FROM sqlite_master m
        JOIN pragma_table_info(m.name) p
        WHERE m.type = 'table' AND p.name LIKE '%hash%'
        """
      |> Sql.executeAsync (fun read -> read.string "name")

    Expect.isNonEmpty withHash "found tables carrying a hash column"

    let unclassified =
      withHash
      |> List.filter (fun t ->
        not (List.contains t Purge.tables)
        && not (List.contains t hashCoupledSurvivors))

    let names = String.concat ", " unclassified

    Expect.isEmpty
      unclassified
      $"every hash-coupled table is classified (unclassified: {names}). \
        A table naming the log by content hash is either a cache that stays true across a reload, or a \
        claim about the log's state that does not. Put it in `Purge.tables` or in \
        `hashCoupledSurvivors` with a note saying which."
  }


let logStateProjectionsArePurged =
  testTask "conflicts and sync_bases are emptied by a purge" {
    // Named rather than derived, because what makes these two different from `type_checked` is
    // meaning, not shape. Left behind, they describe a log that no longer exists: the store reports
    // "1 conflict to review" and then shows neither version, since the ops it names are gone.
    for t in [ "conflicts"; "sync_bases" ] do
      Expect.isTrue
        (List.contains t Purge.tables)
        $"`{t}` is in Purge.tables. It asserts something about the LOG (versions competed; a peer and I \
          agreed), so replacing the log makes it false, and it reads as actionable while being neither \
          reviewable nor resolvable."
  }


/// Every table the fold regenerates is purged. Derived from `Seed.projectionTables` rather than listed,
/// so it catches the next projection whatever its shape: `propagation_policy`, for one, carries neither
/// an `op_id` nor a hash column, so neither guard above can see it.
let foldProjectionsArePurged =
  testTask "every table the fold regenerates is in Purge.tables" {
    for t in LibDB.Seed.projectionTables do
      Expect.isTrue
        (List.contains t Purge.tables)
        $"`{t}` is a fold projection (Seed.projectionTables) and must be in Purge.tables: a projection \
          that outlives the log it was folded from is a stale claim."
  }


/// `Seed.export`'s source, which the two tests below assert against rather than running it: `export`
/// copies a whole store, and driving it here would need a second one.
let private seedSource () : string =
  System.IO.File.ReadAllText(
    System.IO.Path.Combine("..", "backend", "src", "LibDB", "Seed.fs")
  )


/// The seed is a fresh install's starting store, and these tables are one install's RELATIONSHIPS:
/// with a relay (`sync_pushed`, `sync_bases`) and with itself (`config_v0`, which holds the relay url and
/// its write secret). A seed built from a developer's store would otherwise ship that developer's
/// relationships to everyone. Reads the export's SQL rather than trusting a list, so a per-install
/// table added later is not silently left in.
let seedExportStripsPerInstallState =
  test "Seed.export strips every per-install table" {
    let source = seedSource ()
    for t in [ "sync_pushed"; "sync_bases"; "config_v0"; "conflicts"; "op_owners" ] do
      Expect.isTrue
        (source.Contains $"DELETE FROM {t};")
        $"`Seed.export` must strip `{t}`: it is one install's state, not the tree's"
  }


/// A seed is committed history. The builder's DRAFT is per-install state in the same way the tables
/// above are, and shipping it puts whatever they were part-way through on a stranger's first run as
/// "1 item changed" under a name nobody has heard of.
///
/// The guard `check-seed-carries-refs` is the other half, and it is the one a release build trips:
/// the F# suite leaves its own fixtures in main's draft, so without this strip whether a build
/// succeeds depends on what you last ran.
let seedExportStripsTheBuildersDraft =
  test "Seed.export strips uncommitted main ops" {
    // Whitespace-collapsed before matching: this pins WHAT the export deletes, and an assertion on
    // Seed.fs's literal indentation broke on a reformat that changed nothing.
    let source =
      System.Text.RegularExpressions.Regex.Replace(seedSource (), @"\s+", " ")

    Expect.isTrue
      (source.Contains "DELETE FROM package_ops WHERE commit_hash IS NULL")
      "`Seed.export` must strip main's uncommitted ops: a seed is committed history"
  }


let tests =
  testList
    "Purge"
    [ opIdTablesArePurged
      purgeTablesAllExist
      hashCoupledTablesAreClassified
      logStateProjectionsArePurged
      foldProjectionsArePurged
      seedExportStripsPerInstallState
      seedExportStripsTheBuildersDraft ]
