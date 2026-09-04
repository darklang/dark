/// A tight, general-purpose harness for spinning up FRESH, ISOLATED Darklang instances in-process — each
/// instance is its own store. Two flavours:
///   freshInstance  — an empty store (schema only; `main` from schema.sql). For structure-level tests.
///   seededInstance — a COPY of a ready-to-go baseline store (the full package seed), so a test that needs
///                    real functions/types/values (most CLI experiences) is cheap: the baseline is snapshotted
///                    once, then each instance is a fast file-copy of it.
/// Switch the active store with `activate`, sync the wire between instances via the real receive path, and
/// assert they converge. Spinning up an instance is a couple of lines:
///   let! a = seededInstance "a"
///   let! b = seededInstance "b"
///
/// `testSequenced` + a `finally` that always restores the default store, so swapping the process-global
/// connection can't disturb the parallel store tests (they've completed by the sequenced phase; and we hand
/// the default store back no matter what). The swap lives behind `Sql.useStoreForTesting` (test-only).
module Tests.MultiInstanceHarness

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module Seed = LibDB.Seed
module Inserts = LibDB.Inserts
module Resolutions = LibDB.Resolutions
module Account = LibCloud.Account
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing
module File = LibCloud.File
module Config = LibCloud.Config
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval

// ── the fresh-instance harness ──────────────────────────────────────────────────────────────────────

type Instance = { name : string; path : string }

let schemaSql = lazy (File.readfile Config.Migrations "schema.sql")

let deleteStore (path : string) : unit =
  for suffix in [ ""; "-wal"; "-shm" ] do
    try
      System.IO.File.Delete(path + suffix)
    with _ ->
      ()

/// Copy a SQLite store + its WAL/SHM sidecars (so the copy is a consistent snapshot on open).
let copyStore (src : string) (dst : string) : unit =
  deleteStore dst
  for suffix in [ ""; "-wal"; "-shm" ] do
    if System.IO.File.Exists(src + suffix) then
      System.IO.File.Copy(src + suffix, dst + suffix, overwrite = true)

let tmpPath (name : string) : string =
  $"/tmp/dark-test-instance-{name}-{System.Guid.NewGuid()}.db"

/// Snapshotted ONCE: a baseline of the seeded default store, so `seededInstance` is a cheap local copy.
/// Captured lazily during the sequenced phase, when no parallel test is writing the default store.
let baselineSeed : Lazy<string> =
  lazy
    (let template = "/tmp/dark-test-seed-baseline.db"
     copyStore LibConfig.Config.dbPath template
     template)

/// A fresh, EMPTY, isolated instance (schema only; `main` from schema.sql), made the active store.
let freshInstance (name : string) : Task<Instance> =
  task {
    let path = tmpPath name
    deleteStore path
    Sql.useStoreForTesting path
    Sql.query (schemaSql.Force()) |> Sql.executeStatementSync
    return { name = name; path = path }
  }

/// A fresh isolated instance that is a COPY of the seeded baseline (full package seed), made the active store.
let seededInstance (name : string) : Task<Instance> =
  task {
    let path = tmpPath name
    copyStore (baselineSeed.Force()) path
    Sql.useStoreForTesting path
    return { name = name; path = path }
  }

let activate (inst : Instance) : unit = Sql.useStoreForTesting inst.path

let teardown (insts : List<Instance>) : unit =
  Sql.resetStoreForTesting ()
  insts |> List.iter (fun i -> deleteStore i.path)

// ── helpers (build wire events, inspect projections, convert the JSON-ish wire) ───────────────────────

let branchEventAt (op : PT.BranchOp) (originTs : string) : string * byte[] * string =
  let (PT.Hash h) = Hashing.computeBranchOpHash op
  (h, BS.PT.BranchOp.serialize h op, originTs)

let branchEvent (op : PT.BranchOp) : string * byte[] * string =
  branchEventAt op "2026-07-08T00:00:00.000Z"

/// A SetName package-op event, exactly as it crosses the wire / arrives at `receiveOps`.
let setNameEvent
  (loc : PT.PackageLocation)
  (fnHash : string)
  (commitHash : string)
  (ts : string)
  : System.Guid * byte[] * System.Guid * string * string =
  let op = PT.PackageOp.SetName(loc, PT.Reference.PackageFn(PT.Hash fnHash))
  let opId = Inserts.computeOpHash op
  (opId, BS.PT.PackageOp.serialize opId op, PT.mainBranchId, commitHash, ts)

/// A SetName event binding a location to `hash` AS `kind` — for testing that a name holds ONE item
/// regardless of kind (binding a value over a fn replaces it). `setNameEvent` is the fn-kind case.
let setNameEventOfKind
  (loc : PT.PackageLocation)
  (hash : string)
  (kind : PT.ItemKind)
  (commitHash : string)
  (ts : string)
  : System.Guid * byte[] * System.Guid * string * string =
  let op =
    PT.PackageOp.SetName(loc, PT.Reference.fromHashAndKind (PT.Hash hash, kind))
  let opId = Inserts.computeOpHash op
  (opId, BS.PT.PackageOp.serialize opId op, PT.mainBranchId, commitHash, ts)

/// The live (hash, kind) bindings at a location — keyed by name only, so it sees every kind bound there.
/// One item per name means this returns at most one row.
let liveBindings (loc : PT.PackageLocation) : Task<List<string * string>> =
  Sql.query
    "SELECT item_hash, item_type FROM locations WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL"
  |> Sql.parameters
    [ "o", Sql.string loc.owner
      "m", Sql.string (String.concat "." loc.modules)
      "n", Sql.string loc.name ]
  |> Sql.executeAsync (fun read ->
    (read.string "item_hash", read.string "item_type"))

/// A Deprecate package-op event marking a real seeded fn Obsolete — as it crosses the wire / arrives at
/// `receiveOps`. Folds into the `deprecations` projection (a non-SetName op kind).
let deprecateEvent
  (fnHash : string)
  (commitHash : string)
  (ts : string)
  : System.Guid * byte[] * System.Guid * string * string =
  let target = PT.Reference.fromHashAndKind (PT.Hash fnHash, PT.ItemKind.Fn)
  let op =
    PT.PackageOp.Deprecate(target, PT.DeprecationKind.Obsolete, "obsolete (test)")
  let opId = Inserts.computeOpHash op
  (opId, BS.PT.PackageOp.serialize opId op, PT.mainBranchId, commitHash, ts)

/// A seeded function hash that is NOT already deprecated — so a deprecation test starts from a clean slate.
let undeprecatedFunctionHash () : Task<string> =
  Sql.query
    "SELECT hash FROM package_functions WHERE hash NOT IN (SELECT item_hash FROM deprecations) LIMIT 1"
  |> Sql.executeRowAsync (fun read -> read.string "hash")

/// An existing commit hash from the seed. Sync ops reference commits that already exist (commits travel in the
/// wire alongside their ops), so a test references a real seed commit rather than minting accounts + commits.
let existingCommit () : Task<string> =
  Sql.query "SELECT hash FROM commits LIMIT 1"
  |> Sql.executeRowAsync (fun read -> read.string "hash")

/// The active instance's branch-op-log high-water mark (so a `branchOpsSince` returns only NEW branch ops).
let currentBranchCursor () : Task<int64> =
  Sql.query "SELECT COALESCE(MAX(rowid), 0) AS c FROM branch_ops"
  |> Sql.executeRowAsync (fun read -> read.int64 "c")

let branchNames () : Task<List<string>> =
  Sql.query "SELECT name FROM branches ORDER BY name"
  |> Sql.executeAsync (fun read -> read.string "name")

let liveHash (loc : PT.PackageLocation) : Task<List<string>> =
  Sql.query
    "SELECT item_hash FROM locations WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL"
  |> Sql.parameters
    [ "o", Sql.string loc.owner
      "m", Sql.string (String.concat "." loc.modules)
      "n", Sql.string loc.name ]
  |> Sql.executeAsync (fun read -> read.string "item_hash")

/// Two distinct real function hashes from the seed (both instances share the seed, so a hash on A exists on B).
let twoFunctionHashes () : Task<string * string> =
  task {
    let! hs =
      Sql.query "SELECT hash FROM package_functions LIMIT 2"
      |> Sql.executeAsync (fun read -> read.string "hash")
    match hs with
    | a :: b :: _ -> return (a, b)
    | _ -> return Exception.raiseInternal "seed needs >= 2 functions" []
  }

/// The active instance's current sync high-water mark (max `committed_seq` — the coordinate `eventsSince`
/// pages by), so a later `eventsSince` returns only NEW committed ops (not the whole seed).
let currentCursor () : Task<int64> =
  Sql.query "SELECT COALESCE(MAX(committed_seq), 0) AS c FROM package_ops"
  |> Sql.executeRowAsync (fun read -> read.int64 "c")

/// The count of unreviewed conflicts recorded on the active instance.
let conflictCount () : Task<int64> =
  Sql.query "SELECT COUNT(*) AS n FROM sync_conflicts"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

/// Conflicts recorded for ONE location — so a test asserts on its OWN divergence (an exact count), not a
/// global tally that any baseline/other-test conflict would satisfy.
let conflictCountAt (location : string) : Task<int64> =
  Sql.query "SELECT COUNT(*) AS n FROM sync_conflicts WHERE location = @loc"
  |> Sql.parameters [ "loc", Sql.string location ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

let wireEvent
  ((id, hex, br, ch, ts) : string * string * string * string * string)
  : System.Guid * byte[] * System.Guid * string * string =
  (System.Guid.Parse id,
   System.Convert.FromHexString hex,
   System.Guid.Parse br,
   ch,
   ts)

let wireCommit
  ((h, msg, br, acct, at) : string * string * string * string * string)
  : string * string * System.Guid * System.Guid * string =
  (h, msg, System.Guid.Parse br, System.Guid.Parse acct, at)

/// Read the active instance's NEW package ops since `cursor` as a wire batch, ready to feed another
/// instance's `receiveOps` (this is the real serialize→wire→deserialize round-trip sync does).
let wireSince
  (cursor : int64)
  : Task<List<string * string * System.Guid * System.Guid * string> *
    List<System.Guid * byte[] * System.Guid * string * string>>
  =
  task {
    let! (commitsW, eventsW, _) = Seed.eventsSince cursor 100000L
    return (List.map wireCommit commitsW, List.map wireEvent eventsW)
  }


// ── in-process CLI driver ─────────────────────────────────────────────────────────────────────────────
// Drive the REAL `dark <cmd>` dispatch (the `executeCliCommand` package fn) in-process against whatever
// store is currently active, capturing its stdout. This is the same seam `CliTraces.Tests.fs` uses — no
// subprocess fork, so it's fast enough for the normal suite, yet it exercises the true CLI code path
// (dispatch, rendering, Builtins) end to end. Combined with `seededInstance`/`activate`, a test can build a
// real store state (e.g. a recorded conflict) and assert on what the user would actually see.

let buildCliState () : Task<RT.ExecutionState> =
  task {
    let builtins = Builtins.CliHost.Libs.Cli.builtinsToUse ()
    let pmRT = PT2RT.PackageManager.toRT builtins.values LibDB.PackageManager.pt
    let program : RT.Program = { dbs = Map.empty }
    let notify _ _ _ _ = uply { return () }
    let sendException _ _ _ _ = uply { return () }
    return
      Exe.createState
        builtins
        pmRT
        Exe.noTracing
        sendException
        notify
        PT.mainBranchId
        program
      // The harness runs the outer CLI as trusted host code.
      |> Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll
  }

/// Invoke `dark <args>` in-process against the active store; return trimmed stdout. Redirects `Console.Out`
/// for the duration (the surrounding `testSequenced` keeps the process-global `SetOut` from racing).
let runCli (state : RT.ExecutionState) (args : string list) : Task<string> =
  task {
    let argsDval = args |> List.map RT.DString |> Dval.list RT.KTString
    let fnName =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())
    NonBlockingConsole.wait ()
    let captured = new System.IO.StringWriter()
    let originalOut = System.Console.Out
    try
      System.Console.SetOut(captured)
      let! result = Exe.executeFunction state fnName [] (NEList.singleton argsDval)
      // `Stdlib.printLine` queues to a background thread; drain before reading.
      NonBlockingConsole.wait ()
      match result with
      | Ok _ -> return captured.ToString().Trim()
      | Error(rte, _) ->
        System.Console.SetOut(originalOut)
        return Tests.failtestf "runCli %A errored: %A" args rte
    finally
      System.Console.SetOut(originalOut)
  }
