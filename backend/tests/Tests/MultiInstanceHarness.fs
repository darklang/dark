/// Two or more real stores, and the wire between them.
///
/// The fixture the MultiInstance tests share: each instance is a file copy of the seeded store,
/// WAL sidecars and all, so instances start out agreeing and any divergence is the test's doing.
/// `Builtin.localDbPath` follows the store swap, which is what lets a Dark expression run against
/// whichever instance is active.
module Tests.MultiInstanceHarness


open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module Seed = LibDB.Seed
module Inserts = LibDB.Inserts
module Branches = LibDB.Branches
module Queries = LibDB.Queries
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing

open TestUtils.TestUtils


// --- instances -----------------------------------------------------------------------------------

type Instance = { name : string; path : string }

let deleteStore (path : string) : unit =
  for suffix in [ ""; "-wal"; "-shm" ] do
    try
      System.IO.File.Delete(path + suffix)
    with _ ->
      ()

/// Copy a store AND its WAL/SHM sidecars, so the copy opens as a consistent snapshot.
let copyStore (src : string) (dst : string) : unit =
  deleteStore dst
  for suffix in [ ""; "-wal"; "-shm" ] do
    if System.IO.File.Exists(src + suffix) then
      System.IO.File.Copy(src + suffix, dst + suffix, overwrite = true)

let tmpPath (name : string) : string =
  $"/tmp/dark-multiinstance-{name}-{System.Guid.NewGuid()}.db"

/// Snapshot of the default seeded store, taken once. Every instance is a file copy of it, which is far
/// cheaper than seeding each one, and means instances start out agreeing -- which is the interesting
/// starting point for a convergence test.
let baseline : Lazy<string> =
  lazy
    (let template = "/tmp/dark-multiinstance-baseline.db"
     copyStore LibConfig.Config.dbPath template
     template)

let instance (name : string) : Instance =
  let path = tmpPath name
  copyStore (baseline.Force()) path
  Sql.useStoreForTesting path
  { name = name; path = path }

/// Point this process at <param inst>'s store: F#, and Dark's `Stdlib.Sqlite` with it.
///
/// The cache drop is not optional. The package manager and the branch overlay memoize by content
/// hash and by branch id, and two copies of one store share both, so without it instance B answers
/// a name lookup with the row instance A had cached.
let activate (inst : Instance) : unit =
  Sql.useStoreForTesting inst.path
  LibDB.Caching.invalidateAll ()

/// Back to the shared store, caches dropped. Every test here must end with this or the next test in
/// the sequenced list reads an instance store that is about to be deleted.
let teardown (insts : List<Instance>) : unit =
  Sql.resetStoreForTesting ()
  LibDB.Caching.invalidateAll ()
  insts |> List.iter (fun i -> deleteStore i.path)


/// A testTask over one fresh instance named <param store>: makes it, runs the body,
/// and always hands the default store back, whatever the body did.
let oneStoreTest
  (name : string)
  (store : string)
  (body : Instance -> Task<unit>)
  : Test =
  testTask name {
    let inst = instance store
    try
      do! body inst
    finally
      teardown [ inst ]
  }

/// The two-instance frame: stores "a" and "b", torn down together.
let twoStoreTest (name : string) (body : Instance -> Instance -> Task<unit>) : Test =
  testTask name {
    let a = instance "a"
    let b = instance "b"
    try
      do! body a b
    finally
      teardown [ a; b ]
  }


// --- the wire ------------------------------------------------------------------------------------

/// One op as it crosses the wire: (id, hex blob, origin stamp).
///
/// The stamp is passed in rather than minted, because it is the entire input to the last-write-wins rule
/// and a test that let it default would be asserting on wall-clock ordering.
let wireOp (op : PT.PackageOp) (originTs : string) : string * string * string =
  let opId = Inserts.computeOpHash op
  let blob = BS.PT.PackageOp.serialize opId op
  (string opId, System.Convert.ToHexString blob, originTs)

/// Receive ops the way a pull does: bulk insert, then fold.
let receive (records : List<string * string * string>) : Task<int64> =
  task {
    let! n = Inserts.importOpsBulk "" records
    let! _ = Seed.applyUnappliedOps ()
    return n
  }

let loc (name : string) : PT.PackageLocation =
  { owner = "MultiInstance"; modules = [ "Converge" ]; name = name }

let hashOf (s : string) : PT.Hash =
  // A stable stand-in for real content. These tests are about which BINDING wins, so the hashes only need
  // to be distinct and reproducible, not to name anything that exists.
  PT.Hash(
    System.Convert.ToHexString(
      System.Security.Cryptography.SHA256.HashData(UTF8.toBytes s)
    )
    |> String.toLowercase
  )

let setName (name : string) (content : string) : PT.PackageOp =
  // None: these fixtures stand for two instances each authoring a name from nothing, which is exactly
  // the case `previous` exists to make visible.
  PT.PackageOp.SetName(loc name, PT.Reference.PackageFn(hashOf content), None)

/// What this store currently thinks the name means.
let boundHash (name : string) : Task<Option<string>> = liveBoundHash (loc name)


/// The fold's mark on one op: 0 pending, 1 applied, 2 deferred (folded, did nothing, waiting for the
/// branch it names). Deferred is the state that separates "not folded yet" from "folded and had no
/// work to do", and only the flag can tell them apart from outside.
let appliedFlag (opId : string) : Task<int64> =
  Sql.query "SELECT applied AS a FROM package_ops WHERE id = @id"
  |> Sql.parameters [ "id", Sql.string opId ]
  |> Sql.executeRowAsync (fun read -> read.int64 "a")


/// A propagation decision as an op: `pin` this location, with the author's words attached.
///
/// `decidedAt` is what makes each decision a DISTINCT op. Passed in for the same reason `originTs` is:
/// letting it default would make the test depend on the wall clock.
let pin (name : string) (reason : string) (decidedAt : string) : PT.PackageOp =
  PT.PackageOp.Decision(
    $"pin:{name}:{decidedAt}",
    loc name,
    reason,
    PT.DecisionKind.Propagation PT.PropagationPolicy.Pin
  )

/// The policy this store holds for a name on main, as (policy, note).
let policyFor (name : string) : Task<Option<string * string>> =
  let l = loc name
  Sql.query
    // Main's id comes from the product's own constant, never a literal: a hand-spelled one asks for
    // a branch nothing writes, so the query answers None and the test passes while asserting nothing.
    "SELECT policy, COALESCE(note, '') AS note FROM propagation_policy
     WHERE branch_id = @main AND owner = @o AND modules = @m AND name = @n"
  |> Sql.parameters
    [ "main", Sql.string (string PT.BranchId.Main)
      "o", Sql.string l.owner
      "m", Sql.string (String.concat "." l.modules)
      "n", Sql.string l.name ]
  |> Sql.executeRowOptionAsync (fun read ->
    (read.string "policy", read.string "note"))
