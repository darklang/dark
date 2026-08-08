/// Regression tests for the op-transport hardening (LibDB.Inserts.importOpsBulk +
/// LibDB.Seed.applyUnappliedOps): a malformed / poison op arriving on the wire must never
/// throw or brick the import OR the fold. Import skips malformed records; the fold skips +
/// quarantines unparseable ops (marks them applied so they aren't re-scanned forever).
///
/// These run against the shared test DB, so they use a distinctive `fada0000-` id prefix
/// and clean it up defensively (before + after) -- a failed assert can't leave test ops
/// behind to confuse other tests.
module Tests.OpTransport

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module Inserts = LibDB.Inserts
module Seed = LibDB.Seed

let private ts = "2026-01-01T00:00:00.000Z"

let private cleanup () : Task<unit> =
  task {
    do!
      Sql.query "DELETE FROM op_branches WHERE op_id LIKE 'fada0000-%'"
      |> Sql.executeStatementAsync
    do!
      Sql.query "DELETE FROM package_ops WHERE id LIKE 'fada0000-%'"
      |> Sql.executeStatementAsync
  }

/// Shield a test op from a CONCURRENT test's authoring: `WipRefresh` can trigger `discardWipOps`, which
/// deletes every main op not tagged to a branch, and the shared test DB means that lands on our rows too.
/// Tagging the op to a branch takes it out of that sweep without changing what the fold does with it
/// (`effective` is untouched, so `applyUnappliedOps` still picks it up).
let private shieldFromDiscard (id : string) : Task<unit> =
  Sql.query
    "INSERT OR IGNORE INTO op_branches (op_id, branch_id) VALUES (@id, 'optransport-test')"
  |> Sql.parameters [ "id", Sql.string id ]
  |> Sql.executeStatementAsync

let private appliedOf (id : string) : Task<int64> =
  Sql.query "SELECT applied FROM package_ops WHERE id = @id"
  |> Sql.parameters [ "id", Sql.string id ]
  |> Sql.executeRowAsync (fun read -> read.int64 "applied")

let private effectiveOf (id : string) : Task<int64> =
  Sql.query "SELECT effective FROM package_ops WHERE id = @id"
  |> Sql.parameters [ "id", Sql.string id ]
  |> Sql.executeRowAsync (fun read -> read.int64 "effective")

let private countOf (id : string) : Task<int64> =
  Sql.query "SELECT COUNT(*) as n FROM package_ops WHERE id = @id"
  |> Sql.parameters [ "id", Sql.string id ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")


let importSkipsAllMalformed =
  testTask "importOpsBulk: an all-malformed batch inserts nothing and doesn't throw" {
    // non-uuid ids and non-hex blobs -- every record is bad
    let! n =
      Inserts.importOpsBulk "" [ ("not-a-uuid", "01", ts); ("also-bad", "zz", ts) ]
    Expect.equal n 0L "nothing inserted, no exception"
  }

let importKeepsGoodSkipsBad =
  testTask
    "importOpsBulk: imports the good record and skips the bad one in the same batch" {
    do! cleanup ()
    let good = "fada0000-0000-0000-0000-000000000001"
    // good = valid uuid + valid hex (a dummy blob; INSERT doesn't validate op content).
    // bad = non-uuid id.
    let! n =
      Inserts.importOpsBulk "" [ (good, "01020304", ts); ("bad-uuid", "zz", ts) ]
    Expect.equal n 1L "one good record imported, bad skipped"
    let! present = countOf good
    Expect.equal present 1L "the good record is in the log"
    do! cleanup ()
  }

let foldQuarantinesPoison =
  testTask
    "applyUnappliedOps: an unparseable op is skipped and quarantined (applied=1), not fatal" {
    do! cleanup ()
    let poison = "fada0000-0000-0000-0000-000000000002"
    // valid uuid, but "deadbeef" is 4 bytes -- not a real serialized PackageOp, so the fold's
    // deserialize throws and the op must be skipped, not fatal.
    let! _ = Inserts.importOpsBulk "" [ (poison, "deadbeef", ts) ]
    do! shieldFromDiscard poison
    let! _ = Seed.applyUnappliedOps () // must NOT throw
    let! applied = appliedOf poison
    Expect.equal
      applied
      1L
      "poison op quarantined (applied=1), won't re-poison later folds"
    do! cleanup ()
    // leave the shared store with nothing unapplied
    let! _ = Seed.applyUnappliedOps ()
    return ()
  }

/// A relay stores what it is pushed; it must never adopt it as its own code.
///
/// The store path always said it does not fold, and it does not -- but it used to store ops
/// `applied = 0, effective = 1`, which is precisely the pair `growIfNeeded` folds on the NEXT startup. So
/// the relay did fold them, one restart later, with nothing to see in between. On a running relay that
/// showed up as `locations` gaining bindings its clients had pushed: names are resolved last-writer-wins
/// over the whole store, and the relay's own router is just another name in it.
///
/// This asserts the flag rather than the consequence because the consequence needs a second process.
let relayStoreDoesNotQueueForFolding =
  testTask "storeOpsWithOwner: hosted ops are never queued for this store's fold" {
    do! cleanup ()

    let id = "fada0000-0000-0000-0000-0000000000ef"
    let! n = Inserts.storeOpsWithOwner "someone" [ (id, "00", ts) ]
    Expect.equal n 1L "stored the op"

    let! effective = effectiveOf id
    Expect.equal
      effective
      0L
      "hosted ops are stored effective=0, so growIfNeeded never folds them into this store's main"

    let! applied = appliedOf id
    Expect.equal applied 0L "and they are not pretended to have been applied"

    do! cleanup ()
  }


// These assert on specific rows in the SHARED test store, and a concurrent test that authors a package
// triggers WipRefresh's discard-and-reinsert of the whole main log. Branch-tagging the poison op keeps it
// out of that sweep, but the ordering is still racy enough to flake.
let tests =
  // testSequenced, NOT testSequencedGroup. The group form only stops the tests INSIDE it from running
  // alongside each other; the group still runs in the parallel phase next to everything else, which is
  // where the actual hazard is. testSequenced is what moves them to the phase that runs alone.
  testSequenced
  <| testList
    "OpTransport"
    [ importSkipsAllMalformed
      importKeepsGoodSkipsBad
      foldQuarantinesPoison
      relayStoreDoesNotQueueForFolding ]
