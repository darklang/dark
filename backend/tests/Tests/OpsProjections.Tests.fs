/// Tests for LibDB.Seed.rebuildProjections: the projection tables are *regenerable from the op log* — drop
/// them, re-fold package_ops, and they come back identical. The op log (package_ops) is canonical, untouched.
module Tests.OpsProjections

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.FSharp.Reflection

open Fumble
open LibDB.Sqlite

module Seed = LibDB.Seed
module PT = LibExecution.ProgramTypes

let private countRows (table : string) : Task<int64> =
  Sql.query $"SELECT COUNT(*) as n FROM {table}"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

/// A content fingerprint of the projections: the sorted set of every projected item's hash. Two folds that
/// produce the same fingerprint produced the same projections (down to identity), regardless of row order or
/// nondeterministic columns like `location_id`.
let private itemHashes () : Task<string> =
  task {
    let q (table : string) =
      Sql.query $"SELECT hash FROM {table} ORDER BY hash"
      |> Sql.executeAsync (fun read -> read.string "hash")
    let! fns = q "package_functions"
    let! typs = q "package_types"
    let! vals = q "package_values"
    return String.concat "\n" (fns @ typs @ vals)
  }


/// The fingerprint that decides what your code MEANS: which NAME binds which hash, whether that binding
/// is still live, what calls what, and which propagation decisions stand.
///
/// `itemHashes` above compares the ITEMS a fold produced. Two folds can agree on every item and disagree
/// about what the names point at, and it is the names that decide which code runs. That half was not
/// compared anywhere.
///
/// `locations.source` is deliberately NOT in here. It records what PUT a binding there -- an op you
/// typed, propagation following your edit, a human resolving a conflict -- and is passed in by whoever
/// calls the fold rather than derived from the ops, so a re-fold marks everything `op`. Measured on a
/// store carrying edits, propagation, a pin, a branch, a merge and a deprecation: that column is the
/// ONLY thing a re-fold changes, one row out of 35,954. It is written up as an open item; putting it in
/// this fingerprint would assert a property the design does not currently have.
let private bindingFingerprint () : Task<string> =
  task {
    let! locs =
      Sql.query
        """
        SELECT owner, modules, name, item_hash, (unlisted_at IS NOT NULL) AS gone,
               COALESCE(previous, '') AS prev
        FROM locations
        ORDER BY owner, modules, name, item_hash
        """
      |> Sql.executeAsync (fun read ->
        let o = read.string "owner"
        let m = read.string "modules"
        let n = read.string "name"
        let h = read.string "item_hash"
        let gone = read.int64 "gone"
        // `previous` is what conflict detection compares to decide that two sides moved the same
        // parent, so a fold that stopped writing it would break detection while every hash still
        // matched. In the fingerprint for that reason.
        let prev = read.string "prev"
        $"LOC {o}.{m}.{n} {h} gone={gone} prev={prev}")

    let! deps =
      Sql.query
        """
        SELECT item_hash,
               COALESCE(depends_on_owner, '') AS o,
               COALESCE(depends_on_modules, '') AS m,
               depends_on_name AS n
        FROM package_dependencies
        ORDER BY item_hash, o, m, n
        """
      |> Sql.executeAsync (fun read ->
        let h = read.string "item_hash"
        let o = read.string "o"
        let m = read.string "m"
        let n = read.string "n"
        $"DEP {h} -> {o}.{m}.{n}")

    let! pol =
      Sql.query
        """
        SELECT owner, modules, name, policy
        FROM propagation_policy
        ORDER BY owner, modules, name
        """
      |> Sql.executeAsync (fun read ->
        let o = read.string "owner"
        let m = read.string "modules"
        let n = read.string "name"
        let p = read.string "policy"
        $"POL {o}.{m}.{n}={p}")

    return String.concat "\n" (locs @ deps @ pol)
  }


/// A re-fold reproduces the BINDINGS, not merely the same number of them.
///
/// The neighbouring determinism test compares row COUNTS across two rebuilds. That catches a fold that
/// drops rows and misses a fold that binds a name to the wrong hash, which is the failure that changes
/// what runs. This compares the ORIGINAL projections against a re-fold of the same log, by content.
let refoldReproducesBindings =
  testTask "a re-fold reproduces every name binding, not just the item set" {
    let! (before : string) = bindingFingerprint ()
    Expect.isFalse (before = "") "there are bindings to compare (not a vacuous test)"

    let! reapplied = Seed.rebuildProjections ()
    Expect.isTrue (reapplied > 0L) "the log was actually re-folded"

    let! (after : string) = bindingFingerprint ()
    Expect.equal
      after
      before
      "every name binds the same hash after a re-fold, with the same liveness, edges and policy"
  }


let rebuildIsDeterministic =
  testTask
    "rebuildProjections deterministically regenerates projections from the op log" {
    // package_blobs is canonical content — a rebuild must never touch it
    let! blobsBefore = countRows "package_blobs"

    // drop the regenerable projections + re-fold the entire op log
    let! reapplied = Seed.rebuildProjections ()
    Expect.isTrue (reapplied > 0L) "ops were re-folded"
    let! fns1 = countRows "package_functions"
    let! locs1 = countRows "locations"
    Expect.isTrue (fns1 > 0L) "projections regenerated (non-empty) from the op log"

    // a SECOND rebuild reproduces the EXACT same projections — the rebuild is a deterministic function of the
    // op log. (Robust to other tests mutating the shared DB: we compare two rebuilds of the *current* log.)
    let! _ = Seed.rebuildProjections ()
    let! fns2 = countRows "package_functions"
    let! locs2 = countRows "locations"
    Expect.equal
      fns2
      fns1
      "package_functions: a re-rebuild reproduces the same projection"
    Expect.equal locs2 locs1 "locations: a re-rebuild reproduces the same projection"

    // canonical content (package_blobs) is NOT a projection — untouched across rebuilds
    let! blobsAfter = countRows "package_blobs"
    Expect.equal
      blobsAfter
      blobsBefore
      "package_blobs (canonical content) preserved, not dropped"
  }

/// A fixed op hashes to a fixed value.
///
/// The companion to `opIdIsItsContentHash`, covering the one thing that test cannot: if the hash function
/// itself changed, the store would be rebuilt under the new definition and both would agree on a different
/// answer.
///
/// This pins the function to a literal, and is meant to fail when someone changes how ops are hashed. That
/// failure is the point: the id is an op's identity, so changing it means every existing store's ids stop
/// matching their content, peers disagree about which ops they hold, and `INSERT OR IGNORE` stops deduping.
/// If you meant it, update the literal and plan a migration.
///
/// A `SetName` rather than an `AddFn`, deliberately: it carries no expression tree, so this pins op hashing
/// and not the whole AST serializer.
let opHashingIsStable =
  test "a fixed op hashes to a fixed value" {
    let op =
      PT.PackageOp.SetName(
        { owner = "GoldenTest"; modules = [ "Hashing" ]; name = "pinned" },
        PT.Reference.PackageFn(
          PT.Hash "1111111111111111111111111111111111111111111111111111111111111111"
        ),
        None
      )

    let (PT.Hash actual) = LibSerialization.Hashing.Hashing.computeOpHash op

    Expect.equal
      actual
      "ac860bb0f035c6c6bb6be16723661060f552811840ae1374f7de28640b8dfcb5"
      "op hashing changed. See this test's comment before updating the literal."
  }

/// The claim everything else rests on: an op's id IS its content.
///
/// Every argument in this design leans on it. `INSERT OR IGNORE` dedups a re-add because the id collides.
/// Two machines authoring identical bytes produce one row rather than two. A projection can be dropped and
/// re-folded because the log is addressed by content rather than by insertion order.
///
/// Checks every row rather than a sample, because the interesting failure is one op, not a trend.
///
/// Derives the expected id from `Hashing.computeOpHash` and the documented truncation, NOT by calling
/// `Inserts.computeOpHash`. That distinction is the whole test: the store is re-folded before every run, so
/// every id in it was minted by `Inserts.computeOpHash` moments earlier, and a test that recomputes with
/// that same function compares it against itself. Breaking the truncation deliberately left the
/// self-comparing version passing.
///
/// It cannot catch a change to `Hashing.computeOpHash` itself, since the store would be rebuilt under the
/// new definition. `opHashingIsStable` pins that against a literal.
let opIdIsItsContentHash =
  testTask "every op's stored id is the first 16 bytes of its content hash" {
    let! rows =
      Sql.query "SELECT id, op_blob FROM package_ops"
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.bytes "op_blob"))

    Expect.isGreaterThan (List.length rows) 0 "there are ops to check"

    let mismatched =
      rows
      |> List.choose (fun (id, blob) ->
        let op = LibDB.Queries.deserializeOp id blob
        let (LibExecution.ProgramTypes.Hash h) =
          LibSerialization.Hashing.Hashing.computeOpHash op
        let bytes : byte[] = System.Convert.FromHexString(h : string)
        let recomputed = System.Guid(bytes[0..15])
        if recomputed = id then None else Some(id, recomputed))

    match mismatched with
    | [] -> ()
    | (stored, recomputed) :: _ ->
      Expect.equal
        (List.length mismatched)
        0
        $"{List.length mismatched} op(s) are stored under an id that isn't their content hash; first is {stored}, whose blob hashes to {recomputed}"
  }

let refoldReproducesContent =
  // Stronger than the count check above: the exact CONTENT of the projections (the set of projected item
  // hashes) is reproduced across a re-fold — the fold is a deterministic function of the op log, not merely
  // cardinality-preserving. This is what makes "drop the projections and re-fold" safe.
  testTask
    "re-fold reproduces identical projection CONTENT (deterministic fold, not just counts)" {
    let! _ = Seed.rebuildProjections ()
    let! fp1 = itemHashes ()
    Expect.isTrue (String.length fp1 > 0) "non-vacuous: the projections have content"
    let! _ = Seed.rebuildProjections ()
    let! fp2 = itemHashes ()
    Expect.equal
      fp2
      fp1
      "the exact set of projected item hashes is identical across a re-fold"
  }

let originTsStrictlyIncreasing =
  // The authoring stamp behind the timestamp-LWW: locally-inserted ops must get a STRICTLY-increasing
  // origin_ts, even within one wall-clock millisecond — otherwise two sequential SetNames to the same name
  // would tie and be reordered by content hash (a later edit could lose its own location).
  test
    "origin_ts stamps are strictly increasing (sequential local edits never tie the LWW)" {
    let stamps = List.init 100 (fun _ -> LibDB.Inserts.nextOriginTs ())
    Expect.equal stamps (List.sort stamps) "stamps are monotonic (non-decreasing)"
    Expect.equal
      (List.length (List.distinct stamps))
      100
      "stamps are strictly distinct — no ties even across a burst within one millisecond"
  }

let durableReleaseCarriesForward =
  // THE POINT, end to end: change the schema under a store WITHOUT losing authored work. The op log is
  // canonical, so you migrate the LOG and never the derived tables -- drop the projections, re-fold them from
  // that same log, and every item comes back with the same hash.
  //
  // This used to drive `Releases.applyRelease`, which went with the release model. It lost nothing real: the
  // `reserialize` hook it exercised took `byte[] -> byte[]` without the op id, so a genuine
  // deserialize-and-re-encode could never be driven through it, and the test passed an identity function.
  // It exercised the path, not the transform, and said so. What's left here is the part that was always
  // load-bearing, driven directly: schema change, then re-fold.
  testTask
    "a schema change keeps your work: the op log is carried forward and the projections re-fold identically" {
    let! opsBefore = countRows "package_ops"
    let! blobsBefore = countRows "package_blobs"
    Expect.isTrue
      (opsBefore > 0L)
      "there is authored work to migrate (not a vacuous test)"
    let! fpBefore = itemHashes ()

    do!
      Sql.query
        "CREATE INDEX IF NOT EXISTS idx_release_migration_demo ON package_ops(origin_ts)"
      |> Sql.executeStatementAsync

    let! _ = Seed.rebuildProjections ()

    let! opsAfter = countRows "package_ops"
    let! blobsAfter = countRows "package_blobs"
    let! fpAfter = itemHashes ()
    Expect.equal
      opsAfter
      opsBefore
      "the authored op log is PRESERVED across the migration — nothing lost"
    Expect.equal blobsAfter blobsBefore "canonical content (package_blobs) preserved"
    Expect.equal
      fpAfter
      fpBefore
      "projections re-fold IDENTICALLY in the new Release — same items"

    let! idxExists =
      Sql.query
        "SELECT COUNT(*) as n FROM sqlite_master WHERE type='index' AND name='idx_release_migration_demo'"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal idxExists 1L "the Release's schema change actually landed"

    // leave the shared store as we found it
    do!
      Sql.query "DROP INDEX IF EXISTS idx_release_migration_demo"
      |> Sql.executeStatementAsync
  }

let registryCoversProjections =
  // The COUNT is in the name on purpose: adding a projection without adding it here is exactly the drift
  // this catches. It caught it, too -- `propagation_policy` was added to the registry on the kernel-substrate
  // branch while this test sat disabled, and re-enabling the file surfaced it immediately.
  test "the projection registry covers exactly the 7 regenerable projections" {
    Expect.equal
      (List.sort Seed.projectionTables)
      (List.sort
        [ "package_functions"
          "package_types"
          "package_values"
          "locations"
          "package_dependencies"
          "deprecations"
          "propagation_policy" ])
      "the registry's tables are exactly Seed.export's stripped projections (incl. deprecations)"
  }

/// Two bodies under one NAME in a single authored batch.
///
/// Ported from main's `testfiles/execution/cli/authoring-duplicates.dark`, which called the Matter
/// builtin `pmDuplicateDeclarations`; the LibExecution testfile harness does not load Matter, so the
/// same assertions live here against the F# they wrap.
///
/// It matters because stabilization keys by name: a batch declaring one name twice would store one
/// body under the other's hash, and whichever landed second would silently become the first.
let duplicateDeclarationsAreNamed =
  test "a batch declaring one name twice is named, and two names are not" {
    let loc (name : string) : PT.PackageLocation =
      { owner = "Test"; modules = [ "Dup" ]; name = name }

    let fnOps (name : string) (body : string) : List<PT.PackageOp> =
      let fn : PT.PackageFn.PackageFn =
        { hash = PT.Hash ""
          body = PT.EInt64(1UL, 1L)
          typeParams = []
          parameters =
            NEList.singleton { name = "p"; typ = PT.TInt64; description = "" }
          returnType = PT.TInt64
          description = body }
      [ PT.PackageOp.AddFn fn
        PT.PackageOp.SetName(loc name, PT.Reference.PackageFn(PT.Hash ""), None) ]

    Expect.equal
      (LibDB.OpValidation.duplicateDeclarations (
        fnOps "twice" "p1" @ fnOps "twice" "p2"
      ))
      [ "fn Test.Dup.twice" ]
      "one name declared twice is reported once, with its kind"

    Expect.equal
      (LibDB.OpValidation.duplicateDeclarations (fnOps "one" "p1" @ fnOps "two" "p2"))
      []
      "two different names are not a duplicate"
  }


let noCanonicalInDropSet =
  // A schema change keeps your work: the bootstrap drops ONLY `projectionTables` and re-folds the op log, so
  // the authored, canonical data must NEVER appear in that drop-set. If it did, a schema bump would delete it.
  testTask
    "a schema change never drops the op log: no canonical table is in the projection drop-set" {
    let canonical =
      [ "package_ops" // the authored op log — the truth
        "package_blobs" // canonical content (op-playback never writes it)
        "op_branches" // which ops belong to which branch; without it a branch's ops are orphaned
        "branches"
        "branch_name_bases"
        "commits"
        "conflicts"
        "sync_bases"
        "config_v0" // the relay url, the write secret, the push cursors
        "accounts_v0"
        "user_data_v0"
        "toplevels_v0"
        "scripts_v0" ]

    // A name that no longer exists asserts nothing, and reads exactly like one that does. Four dead
    // names sat in this list before anyone asked the schema.
    let! live =
      Sql.query
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"
      |> Sql.executeAsync (fun read -> read.string "name")
    let missing = canonical |> List.filter (fun t -> not (List.contains t live))
    Expect.isEmpty
      missing
      $"these are named as canonical but no such table exists: {missing}"

    canonical
    |> List.iter (fun t ->
      Expect.isFalse
        (List.contains t Seed.projectionTables)
        $"{t} is canonical and must NOT be in the projection drop-set (it would be lost on a schema change)")
  }

let schemaChangeKeepsWork =
  // A schema change now runs `rebuildProjections` (drop projections + re-fold). This pins the thing that
  // matters — your authored op LOG (and branch/commit state) come through a full re-fold IDENTICAL.
  testTask "a schema change keeps your work: a full re-fold preserves the op log" {
    let! opsBefore = countRows "package_ops"
    let! branchesBefore = countRows "branches"
    let! commitsBefore = countRows "commits"
    Expect.isTrue (opsBefore > 0L) "there are ops to preserve (not a vacuous test)"

    let! _ = Seed.rebuildProjections ()

    let! opsAfter = countRows "package_ops"
    let! branchesAfter = countRows "branches"
    let! commitsAfter = countRows "commits"
    Expect.equal
      opsAfter
      opsBefore
      "package_ops (the authored op log) is untouched by a re-fold"
    Expect.equal branchesAfter branchesBefore "branches preserved across a re-fold"
    Expect.equal commitsAfter commitsBefore "commits preserved across a re-fold"
  }


// testSequenced because the rebuild cases DELETE + refold the *shared* projection tables and mark all ops
// unapplied — they must not race other DB tests' reads/writes mid-rebuild. The pure cases ride along.
/// Every `PackageOp` case has an arm in the Dark pretty-printer.
///
/// `PrettyPrinter.ProgramTypes.PackageOp.packageOp` is what `dark show` and `dark commits` render ops
/// with, and a Dark `match` that does not cover a case fails at RUNTIME, when that case first turns up.
/// `BranchEvent` shipped without an arm, so `dark show` on any commit that landed a branch died with
/// "No matching case found" -- on exactly the commits a person most wants to read.
///
/// Reflection over the F# DU on one side, text on the other, because the Dark side has no exhaustiveness
/// check to hook. Adding a 10th case fails this until the printer learns it.
let everyPackageOpCaseIsPrintable =
  testTask "every PackageOp case has a pretty-printer arm" {
    let printer =
      System.IO.Path.Combine(
        "..",
        "packages",
        "darklang",
        "prettyPrinter",
        "programTypes.dark"
      )

    let lines = System.IO.File.ReadAllLines printer

    // Just the body of `let packageOp`, so a mention of a case name elsewhere in the file does not
    // count as handling it.
    let start =
      lines |> Array.findIndex (fun l -> l.Trim().StartsWith "let packageOp")

    let indent = lines[start].Length - lines[start].TrimStart().Length

    let finish =
      lines
      |> Array.skip (start + 1)
      |> Array.tryFindIndex (fun l ->
        let t = l.TrimStart()
        t.StartsWith "let " && (l.Length - t.Length) <= indent)
      |> Option.map (fun i -> start + 1 + i)
      |> Option.defaultValue lines.Length

    let body = lines[start .. finish - 1] |> String.concat "\n"

    let missing =
      FSharpType.GetUnionCases typeof<PT.PackageOp>
      |> Array.map _.Name
      |> Array.filter (fun name -> not (body.Contains $"| {name}"))
      |> List.ofArray

    Expect.isEmpty
      missing
      $"PackageOp cases with no arm in `packageOp` ({printer}): {missing}. A Dark match that misses a \
       case throws \"No matching case found\" when that case first appears, which for an op means when \
       someone runs `dark show` on a commit that contains one."
  }


let tests =
  testSequenced
  <| testList
    "OpsProjections"
    [ opHashingIsStable
      opIdIsItsContentHash
      rebuildIsDeterministic
      refoldReproducesBindings
      refoldReproducesContent
      originTsStrictlyIncreasing
      durableReleaseCarriesForward
      registryCoversProjections
      duplicateDeclarationsAreNamed
      noCanonicalInDropSet
      schemaChangeKeepsWork
      everyPackageOpCaseIsPrintable ]
