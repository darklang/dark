/// Proof for the branches-as-overlays model: a "branch" is its delta ops overlaid on a shared
/// core PackageManager (`PM.withExtraOps`).
///
/// Two properties concurrent agents depend on:
///   - ISOLATION FROM CORE: a fn authored on a branch overlay resolves + EXECUTES there, but
///     is invisible to the shared core -- it never leaks into main.
///   - ISOLATION BETWEEN BRANCHES: two overlays over the SAME core see only their own defs;
///     neither can resolve OR fetch the other's, which is what lets N agents run N branches
///     concurrently over a shared read-only core.
module Tests.BranchOverlay

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module PM = LibDB.PackageManager
module Branches = LibDB.Branches
module Sel = LibDB.BranchSelection
module Queries = LibDB.Queries
module Seed = LibDB.Seed
module BS = LibSerialization.Binary.Serialization

open TestUtils.TestUtils


/// A branch id for a test, derived from a readable label.
///
/// A branch id is a uuid, but a test that fails should name something a person can find. Deriving the
/// uuid from the label keeps both: the assertions read as `"chain-a"`, and re-running a test reuses the
/// same row rather than leaving a new branch behind each time.
let private testBranch (label : string) : PT.BranchId =
  use md5 = System.Security.Cryptography.MD5.Create()
  PT.BranchId.Id(
    System.Guid(md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes label))
  )


/// A `List<String>` (or a `Result` wrapping one) from a Dark call, as these assertions expect.
///
/// The SCM verbs the CLI consults live in Dark, so a test that called an F# copy of one would be
/// asserting about a second implementation rather than about the one that runs. A Dark-side `Error`
/// fails the test here: these callers ask questions that are supposed to have answers.
let private darkStringList (code : string) : Task<List<string>> =
  task {
    let strings (items : List<RT.Dval>) =
      items
      |> List.map (fun d ->
        match d with
        | RT.DString s -> s
        | other -> $"{other}")

    match! evalDarkExpr code with
    | Ok(RT.DList(_, items)) -> return strings items
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DList(_, items) ])) -> return strings items
    | Ok(RT.DEnum(_, _, _, "Error", [ RT.DString e ])) ->
      return failtest $"the Dark call failed: {e}"
    | Ok other -> return failtest $"unexpected result shape: {other}"
    | Error(rte, _) -> return failtest $"the Dark call raised: {rte}"
  }

let private darkBranch (branchId : PT.BranchId) : string =
  $"(Stdlib.Uuid.parse \"{branchId}\" |> Builtin.unwrap)"

/// The names the parent has moved since a branch forked, per the merge gate the CLI consults.
let private nameConflicts (branchId : PT.BranchId) : Task<List<string>> =
  darkStringList $"Darklang.SCM.Branches.nameConflicts {darkBranch branchId}"

/// Accept the parent's current state as this branch's new base, and report the names the parent
/// had changed since the fork. Afterwards the branch's own ops layer on top (LWW by origin_ts)
/// and merge is unblocked.
let private rebase (branchId : PT.BranchId) : Task<List<string>> =
  darkStringList $"Darklang.SCM.Branches.rebase {darkBranch branchId}"


/// Run a Dark call answering `Result<Unit, String>`, as the F# result these assertions expect.
/// The `Error` is handed back rather than failing, because refusing is one of the answers.
let private darkUnitResult (code : string) : Task<Result<unit, string>> =
  task {
    match! evalDarkExpr code with
    | Ok(RT.DEnum(_, _, _, "Ok", _)) -> return Ok()
    | Ok(RT.DEnum(_, _, _, "Error", [ RT.DString e ])) -> return Error e
    | Ok other -> return failtest $"unexpected result shape: {other}"
    | Error(rte, _) -> return failtest $"the Dark call raised: {rte}"
  }

let private resolveTakeTheirs
  (branchId : PT.BranchId)
  (fqn : string)
  : Task<Result<unit, string>> =
  darkUnitResult
    $"Darklang.SCM.Branches.resolveTakeTheirs {darkBranch branchId} \"{fqn}\""

let private resolveKeepMine
  (branchId : PT.BranchId)
  (fqn : string)
  : Task<Result<unit, string>> =
  darkUnitResult
    $"Darklang.SCM.Branches.resolveKeepMine {darkBranch branchId} \"{fqn}\""


/// Run a Dark call answering `Result<Int, String>`, as the count these assertions expect.
let private darkIntResult (code : string) : Task<int64> =
  task {
    match! evalDarkExpr code with
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DInt n ])) ->
      return int64 (RT.DarkInt.toBigInt n)
    | Ok(RT.DEnum(_, _, _, "Error", [ RT.DString e ])) ->
      return failtest $"the Dark call failed: {e}"
    | Ok other -> return failtest $"unexpected result shape: {other}"
    | Error(rte, _) -> return failtest $"the Dark call raised: {rte}"
  }

let private retagFrontierToParent
  (branchId : PT.BranchId)
  (parentId : PT.BranchId)
  : Task<int64> =
  darkIntResult (
    "Darklang.SCM.Branches.retagFrontierToParent "
    + darkBranch branchId
    + " "
    + darkBranch parentId
  )

let private markMergedEffective (branchId : PT.BranchId) : Task<int64> =
  darkIntResult $"Darklang.SCM.Branches.markMergedEffective {darkBranch branchId}"

/// The NAMES a branch has recorded a fork base for.
let private baseNames (branchId : PT.BranchId) : Task<List<string>> =
  darkStringList (
    "Darklang.SCM.Branches.nameBases "
    + darkBranch branchId
    + " |> Stdlib.List.map (fun (_o, _m, n, _h) -> n)"
  )

/// "owner\nmodules\nname=hash" per recorded base, as DARK computes the parent's current hash. Spelled
/// the same as the F# side in `parentHashesAgreeAcrossLanguages`, so the two lists compare directly.
let private parentHashLinesFromDark
  (parentId : PT.BranchId)
  (branchId : PT.BranchId)
  : Task<List<string>> =
  let key = "(Darklang.SCM.Branches.nameKey o m n)"

  let hashes =
    "(Darklang.SCM.Branches.parentHashesForBases "
    + darkBranch parentId
    + " "
    + darkBranch branchId
    + ")"

  darkStringList (
    "((Darklang.SCM.Branches.nameBases "
    + darkBranch branchId
    + " |> Stdlib.List.map (fun (o, m, n, _h) -> "
    + key
    + " ++ \"=\" ++ ((Stdlib.Dict.get "
    + hashes
    + " "
    + key
    + ") |> Stdlib.Option.withDefault \"\"))) |> Stdlib.List.sort)"
  )

// A branch's source: one fn `foo` returning `answer`, computed via a CORE call
// (Stdlib.Int64.add), so executing its body ALSO proves the overlay resolves core names.
let private branchSource (answer : int) : string =
  $"""module Darklang.BranchTestOverlay

let foo (x: Int64) : Int64 = Stdlib.Int64.add {answer - 2}L 2L"""

let private fooLoc : PT.PackageLocation =
  { owner = "Darklang"; modules = [ "BranchTestOverlay" ]; name = "foo" }

// A source whose module name varies, so two branches can author DISTINCT fns.
let private namedSource (modName : string) (answer : int) : string =
  $"""module Darklang.{modName}

let foo (x: Int64) : Int64 = Stdlib.Int64.add {answer - 2}L 2L"""

let private fooLocIn (modName : string) : PT.PackageLocation =
  { owner = "Darklang"; modules = [ modName ]; name = "foo" }

/// Execute the body of `foo` from a set of ops against `pm`. Proves the branch's code runs.
let private runFooBody
  (pm : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : Task<RT.Dval> =
  task {
    let body =
      ops
      |> List.tryPick (fun op ->
        match op with
        | PT.PackageOp.AddFn f -> Some f.body
        | _ -> None)
      |> Option.defaultWith (fun () ->
        Exception.raiseInternal "no AddFn op in branch ops" [])
    let! (state : RT.ExecutionState) = executionStateFor pm false Map.empty
    let rtExpr = PT2RT.Expr.toRT Map.empty 0 None body
    match! Exe.executeExpr state rtExpr with
    | Ok dv -> return dv
    | Error(rte, _) ->
      return Exception.raiseInternal "foo body errored" [ "rte", rte ]
  }


let isolationFromCore =
  testTask "a branch fn resolves + executes on its overlay but is INVISIBLE to core" {
    let! ops = parsePackageOps (branchSource 42)
    let branch = PM.withExtraOps pmPT ops

    let! onBranch = branch.findFn fooLoc |> Ply.toTask
    Expect.isSome onBranch "foo resolves on the branch overlay"
    let! onCore = pmPT.findFn fooLoc |> Ply.toTask
    Expect.isNone onCore "foo does NOT leak into the shared core"

    let! dv = runFooBody branch ops
    Expect.equal dv (RT.DInt64 42L) "the branch fn's code runs -> 42"
  }

/// An `Unbind` on a branch hides a name main holds, on that branch only. Taking a name away, rather
/// than only adding one, is what lets a branch delete something.
let unbindHidesACoreNameOnTheBranchOnly =
  testTask "a branch's unbind hides main's name on the branch and leaves main alone" {
    let addLoc : PT.PackageLocation =
      { owner = "Darklang"; modules = [ "Stdlib"; "Int64" ]; name = "add" }
    let! onCore = pmPT.findFn addLoc |> Ply.toTask
    Expect.isSome onCore "Stdlib.Int64.add is a main fn"
    let hash = Option.get onCore

    let branch = PM.withExtraOps pmPT [ PT.PackageOp.Unbind(addLoc, Some hash) ]
    let! onBranch = branch.findFn addLoc |> Ply.toTask
    Expect.isNone onBranch "unbound on the branch"
    let! stillOnCore = pmPT.findFn addLoc |> Ply.toTask
    Expect.equal stillOnCore onCore "and main still has it"

    // The content stays reachable by hash, since callers reference it that way.
    let! body = branch.getFn hash |> Ply.toTask
    Expect.isSome body "the fn's content is still there on the branch"
    let! locs = branch.getFnLocations hash |> Ply.toTask
    Expect.isFalse
      (List.contains addLoc locs)
      "but the name is not among the hash's locations"

    // A search from the branch does not list it either.
    let query : PT.Search.SearchQuery =
      { currentModule = [ "Darklang"; "Stdlib"; "Int64" ]
        text = "add"
        searchDepth = PT.Search.SearchDepth.OnlyDirectDescendants
        entityTypes = [ PT.Search.EntityType.Fn ]
        exactMatch = true }
    let! (found : PT.Search.SearchResults) = branch.search query |> Ply.toTask
    Expect.isEmpty
      (found.fns |> List.filter (fun f -> f.location = addLoc))
      "search on the branch does not list the unbound name"

    // Rebinding after the unbind brings it back.
    let again =
      PM.withExtraOps
        pmPT
        [ PT.PackageOp.Unbind(addLoc, Some hash)
          PT.PackageOp.SetName(addLoc, PT.Reference.PackageFn hash, None) ]
    let! rebound = again.findFn addLoc |> Ply.toTask
    Expect.equal rebound onCore "a later bind on the branch revives the name"
  }

let isolationBetweenBranches =
  testTask
    "two overlays over one core see only their own defs (concurrent-branch isolation)" {
    let! opsA = parsePackageOps (branchSource 42)
    let! opsB = parsePackageOps (branchSource 99)
    let branchA = PM.withExtraOps pmPT opsA
    let branchB = PM.withExtraOps pmPT opsB

    let! hA = branchA.findFn fooLoc |> Ply.toTask
    let! hB = branchB.findFn fooLoc |> Ply.toTask
    Expect.isSome hA "A resolves its own foo"
    Expect.isSome hB "B resolves its own foo"
    Expect.notEqual
      hA
      hB
      "different bodies -> different content hashes (the branches diverge)"

    // Neither can fetch the other's def: an overlay holds only its own ops, falling back to core.
    let! aHasB = branchA.getFn (Option.get hB) |> Ply.toTask
    let! bHasA = branchB.getFn (Option.get hA) |> Ply.toTask
    Expect.isNone aHasB "branch A cannot fetch branch B's fn"
    Expect.isNone bHasA "branch B cannot fetch branch A's fn"

    let! dvA = runFooBody branchA opsA
    let! dvB = runFooBody branchB opsB
    Expect.equal dvA (RT.DInt64 42L) "branch A -> 42"
    Expect.equal dvB (RT.DInt64 99L) "branch B -> 99"
  }

/// The branch a READ verb resolves a name to: the most recent branch of that name that has not been
/// archived. A merged branch still answers, which is what `mergedBranchStaysAddressable` is about;
/// `Branches.liveIdForName` is the stricter lookup a switch uses.
///
/// Spelled as plain SQL rather than through the Dark `SCM.PackageOps` equivalent, so an assertion
/// about the store reads the store.
let private idForName (name : string) : Task<Option<PT.BranchId>> =
  Sql.query
    "SELECT id FROM branches
     WHERE name = @name AND archived_at IS NULL
     ORDER BY created_at DESC, rowid DESC LIMIT 1"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read ->
    PT.BranchId.ParseUnsafe(read.string "id"))

let private isMerged (branchId : PT.BranchId) : Task<bool> =
  task {
    let! found =
      Sql.query
        "SELECT 1 AS n FROM branches WHERE id = @id AND merged_at IS NOT NULL"
      |> Sql.parameters [ "id", Sql.string (string branchId) ]
      |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
    return Option.isSome found
  }


let private cleanupBranch (branchId : PT.BranchId) : Task<unit> =
  task {
    let del (sql : string) =
      Sql.query sql
      |> Sql.parameters [ "b", Sql.string (string branchId) ]
      |> Sql.executeStatementAsync

    do!
      del
        "DELETE FROM package_ops WHERE id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
    do! del "DELETE FROM op_branches WHERE branch_id = @b"
    do! del "DELETE FROM branch_name_bases WHERE branch_id = @b"
    do! del "DELETE FROM branches WHERE id = @b"
  }

/// A fresh branch off main for a test: derives the test-branch id from <param label>,
/// wipes any prior run's rows, and registers the branch as <param name>.
let private freshBranch (label : string) (name : string) : Task<PT.BranchId> =
  task {
    let b = testBranch label
    do! cleanupBranch b
    do! Branches.createBranch b name PT.BranchId.Main
    return b
  }

/// `freshBranch`, but forked off <param parent> rather than main.
let private freshBranchOff
  (parent : PT.BranchId)
  (label : string)
  (name : string)
  : Task<PT.BranchId> =
  task {
    let b = testBranch label
    do! cleanupBranch b
    do! Branches.createBranch b name parent
    return b
  }

/// Pretend the parent moved every name this branch touched, by staling the recorded bases. Doing it
/// this way rather than actually changing the parent keeps the conflict tests off the shared main
/// projection every other test here reads concurrently.
let private staleNameBases (branchId : PT.BranchId) : Task<unit> =
  Sql.query
    "UPDATE branch_name_bases SET base_hash = 'stalehash' WHERE branch_id = @b"
  |> Sql.parameters [ "b", Sql.string (string branchId) ]
  |> Sql.executeStatementAsync

/// The parent's CURRENT hash per name is computed in both languages, and has to come out the same.
///
/// The two exist because the halves of the fork-base model run in different places: RECORDING a base
/// happens mid-author inside `scmAddOps`, which is F#, while DECIDING whether the parent has since
/// moved that name is `SCM.Branches.nameConflicts`, which is Dark. They read the same thing -- main's
/// `locations`, overridden by the parent chain's own rebinds when the parent is not main -- and if they
/// stop agreeing, a branch is either permanently conflicted or never conflicted, with nothing to say
/// which.
///
/// Exercised against a NON-MAIN parent, because that is the case with logic in it: for a main parent
/// both sides are one table read.
let parentHashesAgreeAcrossLanguages =
  testTask "F# and Dark compute the same parent hashes for a branch's bases" {
    let! parent = freshBranch "ph-parent" "ph-parent"
    let! parentOps = parsePackageOps (namedSource "PhTest" 1)
    let! _ = Branches.storeDeltaOps parent parentOps
    do! Branches.recordNameBases parent PT.BranchId.Main parentOps

    let! child = freshBranchOff parent "ph-child" "ph-child"
    let! childOps = parsePackageOps (namedSource "PhTest" 2)
    let! _ = Branches.storeDeltaOps child childOps
    do! Branches.recordNameBases child parent childOps

    let! bases =
      Sql.query
        "SELECT owner, modules, name FROM branch_name_bases WHERE branch_id = @b"
      |> Sql.parameters [ "b", Sql.string (string child) ]
      |> Sql.executeAsync (fun read ->
        read.string "owner", read.string "modules", read.string "name")
    Expect.isNonEmpty bases "the child recorded a base to compare"

    let! parentHashes = Branches.parentNameHashes parent

    let fromFSharp =
      bases
      |> List.map (fun (o, m, n) ->
        let hash = parentHashes |> Map.tryFind (o, m, n) |> Option.defaultValue ""
        $"{o}\n{m}\n{n}={hash}")
      |> List.sort

    let! fromDark = parentHashLinesFromDark parent child

    Expect.equal
      fromDark
      fromFSharp
      "Dark and F# agree on the parent's hash for every name the child based on"

    do! cleanupBranch child
    do! cleanupBranch parent
  }

/// An op this build cannot decode must be SKIPPED by the branch overlay, never raised on.
///
/// A synced store's own log holds ops a peer authored on a newer format. They are stored and left
/// unapplied on purpose, so a later build can apply them, which means they sit in the local log where
/// every local reader meets them -- including `chainOverlayOps`, which is what a process RESOLVES
/// through and loads at boot for whatever branch you are standing on. Raising there does not fail one
/// command, it fails the CLI, permanently, for as long as the op is in the log.
///
/// The other half of the rule is that skipping one for READING never becomes dropping it for WRITING:
/// the junk op is still in the table at the end of this.
let undecodableBranchOpIsSkippedNotFatal =
  testTask
    "an op the build can't decode is skipped by the overlay, and survives in the log" {
    let! bid = freshBranch "undecodable-overlay" "undecodable"
    let! ops = parsePackageOps (namedSource "UndecodableTest" 3)
    let! _ = Branches.storeDeltaOps bid ops

    // A blob no build can read, tagged to the branch the way a peer's newer-format op arrives.
    let junkId = System.Guid.NewGuid()
    do!
      execSqlP
        "INSERT INTO package_ops (id, op_blob, effective, origin_ts)
         VALUES (@id, @blob, 0, '2099-01-01T00:00:00.000Z')"
        [ "id", Sql.string (string junkId)
          "blob", Sql.bytes [| 0xFFuy; 0xFEuy; 0xFDuy; 0xFCuy |] ]
    do!
      execSqlP
        "INSERT INTO op_branches (op_id, branch_id) VALUES (@id, @b)"
        [ "id", Sql.string (string junkId); "b", Sql.string (string bid) ]

    let! loaded = Branches.loadDeltaOps bid
    Expect.equal
      (List.length loaded)
      (List.length ops)
      "the readable ops load, and the unreadable one is not among them"

    let overlay = PM.withExtraOps pmPT loaded
    let! resolved = overlay.findFn (fooLocIn "UndecodableTest") |> Ply.toTask
    Expect.isSome resolved "and the branch still resolves its own fn"

    let! stillThere =
      countSql
        "SELECT count(*) AS n FROM package_ops WHERE id = @id"
        [ "id", Sql.string (string junkId) ]
    Expect.equal stillThere 1L "reading past it did not delete it"

    do! cleanupBranch bid
    do!
      execSqlP
        "DELETE FROM package_ops WHERE id = @id"
        [ "id", Sql.string (string junkId) ]
  }

/// The overlay's SEARCH and its `findFn` must name the same version.
///
/// They are two foldings of the same ops and it is easy for them to disagree: `findFn` goes through the
/// location map (last binding wins) while search enumerates the hash map, which is ordered BY HASH.
/// Callers take the head, so a search ordered by hash makes `dark view` on a branch show whichever
/// version happens to sort lowest while `eval`, `diff` and `log` all run the newest -- reading one
/// thing and running another.
///
/// Three versions, because two cannot tell a hash-ordered answer from a correct one.
let overlaySearchAgreesWithFindFn =
  testTask "the overlay's search names the version its location map binds" {
    let! bid = freshBranch "search-agrees" "search-agrees"

    for answer in [ 1; 2; 3 ] do
      let! ops = parsePackageOps (namedSource "SearchAgrees" answer)
      let! _ = Branches.storeDeltaOps bid ops
      ()

    let! loaded = Branches.loadDeltaOps bid
    let overlay = PM.withExtraOps pmPT loaded
    let loc = fooLocIn "SearchAgrees"

    let! bound = overlay.findFn loc |> Ply.toTask
    Expect.isSome bound "the branch binds the name"

    let query : PT.Search.SearchQuery =
      { currentModule = [ "Darklang"; "SearchAgrees" ]
        text = "foo"
        searchDepth = PT.Search.SearchDepth.OnlyDirectDescendants
        entityTypes = []
        exactMatch = true }

    let! (results : PT.Search.SearchResults) = overlay.search query |> Ply.toTask

    let found =
      results.fns
      |> List.filter (fun (f : PT.LocatedItem<PT.PackageFn.PackageFn>) ->
        f.location = loc)

    Expect.equal
      (List.length found)
      1
      "one hit per location, not one per version the branch has ever bound"

    Expect.equal
      (found |> List.map (fun f -> f.entity.hash))
      [ Option.get bound ]
      "and it is the version the location map binds"

    do! cleanupBranch bid
  }

/// A version a branch has edited PAST still has a name.
///
/// The live lookup folds last-wins per location, so it answers about the newest and nothing else, and
/// main's `getLocationsEverNamed` reads `locations`, which a branch never writes. Between them a
/// superseded branch version would have no name at all, and `dark log` would render it `<hash:...>`
/// beside a newest-version line in the same listing that shows its name.
///
/// A fallback, not an alternative: the live name still wins while there is one.
let supersededBranchVersionsKeepTheirName =
  testTask
    "a version the branch has edited past is still named, not rendered as a hash" {
    let! bid = freshBranch "ever-named" "ever-named"

    let! firstOps = parsePackageOps (namedSource "EverNamed" 1)
    let! _ = Branches.storeDeltaOps bid firstOps
    let! secondOps = parsePackageOps (namedSource "EverNamed" 2)
    let! _ = Branches.storeDeltaOps bid secondOps

    let superseded = hashBoundTo firstOps "foo"
    let live = hashBoundTo secondOps "foo"
    Expect.notEqual
      superseded
      live
      "the two authorings really are different versions"

    let liveNames =
      PM.branchLocationsFor bid PT.ItemKind.Fn live |> List.map (fun l -> l.name)
    Expect.equal liveNames [ "foo" ] "the live version is named by the live lookup"

    Expect.isEmpty
      (PM.branchLocationsFor bid PT.ItemKind.Fn superseded)
      "and the superseded one is not -- that is what the fallback is for"

    let everNames =
      PM.branchLocationsEverNamed bid PT.ItemKind.Fn superseded
      |> List.map (fun l -> l.name)
    Expect.equal everNames [ "foo" ] "the fallback recovers the name it had"

    do! cleanupBranch bid
  }

/// The first item someone authors on a BRANCH counts as having items.
///
/// The check is an index seek on `locations`, which a branch never writes to, so on its own it says
/// "this owner has nothing" while their work sits on the branch -- and the workbench keeps offering
/// the "you have nothing yet" panel to someone who has just written something.
let branchAuthoringCountsAsHavingItems =
  testTask "an owner's first item on a branch counts as having items" {
    let! bid = freshBranch "owner-has-items" "owner-has-items"

    // Store BEFORE asking. The overlay memoises per branch and this test writes ops behind its back
    // (`scmAddOps` refreshes it; `storeDeltaOps` on its own does not), so asking first would cache an
    // empty branch and then answer from that.
    let! ops = parsePackageOps (namedSource "OwnerHasItems" 1)
    let! _ = Branches.storeDeltaOps bid ops

    Expect.isTrue
      (PM.branchOwnerHasItems bid "Darklang")
      "the branch's own binding counts"

    Expect.isFalse
      (PM.branchOwnerHasItems bid "SomeoneElse")
      "and it is scoped to the owner asked about"

    do! cleanupBranch bid
  }

let storeThenOverlay =
  testTask
    "a branch's ops round-trip through the store (effective=0) and overlay to resolve foo" {
    let! branchId = freshBranch "test-branch-store-1" "store-proof"
    let! byName = idForName "store-proof"
    Expect.equal byName (Some branchId) "branch resolves by its name alias"

    let! ops = parsePackageOps (branchSource 42)
    let! stored = Branches.storeDeltaOps branchId ops
    Expect.isGreaterThan stored 0L "ops stored to the branch frontier"

    // stored effective=0 -> in the log, NOT folded into core, so core can't resolve foo.
    let! onCore = pmPT.findFn fooLoc |> Ply.toTask
    Expect.isNone onCore "foo is NOT folded into main (effective=0)"

    let! loaded = Branches.loadDeltaOps branchId
    let overlay = PM.withExtraOps pmPT loaded
    let! onBranch = overlay.findFn fooLoc |> Ply.toTask
    Expect.isSome onBranch "foo resolves via the branch loaded from the store"

    do! cleanupBranch branchId
  }

/// How many ops this branch's own frontier tags, ignoring the parent chain. A retag moves tags off
/// a branch rather than deleting its ops, so this is what says the move happened.
let private ownTagCount (branchId : PT.BranchId) : Task<int64> =
  countSql
    "SELECT count(*) AS n FROM op_branches WHERE branch_id = @b"
    [ "b", Sql.string (string branchId) ]

/// Count a branch's frontier ops at a given effective flag (cache-free, direct SQL).
let private countEffective (branchId : PT.BranchId) (eff : int) : Task<int64> =
  Sql.query
    "SELECT count(*) AS n FROM package_ops p
     JOIN op_branches b ON b.op_id = p.id
     WHERE b.branch_id = @b AND p.effective = @e"
  |> Sql.parameters [ "b", Sql.string (string branchId); "e", Sql.int eff ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

let markMergedFlipsEffective =
  testTask
    "merge half-1: markMergedEffective flips a branch's ops effective 0->1 (fold does the rest)" {
    let! branchId = freshBranch "test-branch-flip-1" "flip-proof"
    let! ops = parsePackageOps (branchSource 42)
    let! _ = Branches.storeDeltaOps branchId ops

    let! pending = countEffective branchId 0
    Expect.isGreaterThan pending 0L "stored ops start effective=0 (branch-pending)"

    let! flipped = markMergedEffective branchId
    Expect.equal flipped pending "all pending ops flip to effective=1"
    let! stillPending = countEffective branchId 0
    Expect.equal stillPending 0L "none left effective=0"

    // (the fold -- Seed.applyUnappliedOps -- is what then brings foo into main; run in a fresh
    // process by `dark merge`, not here, so this test never pollutes the shared main projection.)
    do! cleanupBranch branchId
  }

/// Concurrent `resolveOrCreate` for one name yields ONE branch. The case `DARK_BRANCH` makes
/// ordinary: several agents start in their own shells with the same branch name exported and all
/// reach for it at once. If each minted its own, they would author into different branches while
/// believing they shared one.
let concurrentCreateYieldsOneBranch =
  testTask "racing to create one branch name produces one branch" {
    let name = "race-one-name"
    let! existing = idForName name
    match existing with
    | Some id -> do! cleanupBranch id
    | None -> ()

    let! results =
      Array.init 8 (fun _ -> Branches.resolveOrCreate name PT.BranchId.Main)
      |> System.Threading.Tasks.Task.WhenAll

    let ids = results |> Array.map fst |> Array.distinct
    Expect.equal ids.Length 1 "every caller got the SAME branch id"

    let createdCount = results |> Array.filter snd |> Array.length
    Expect.equal createdCount 1 "and exactly one of them reports having created it"

    // The store agrees, not just the return values.
    let! live =
      Sql.query
        "SELECT count(*) AS n FROM branches
         WHERE name = @name AND archived_at IS NULL AND merged_at IS NULL"
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal live 1L "one live row for the name"

    do! cleanupBranch ids[0]
  }


/// A merged branch stays addressable by name; a switch under that name starts a new one. A UX
/// contract, not an implementation detail: `dark branches` lists merged branches, so every verb
/// that takes a branch name has to accept the names it just printed.
let mergedBranchStaysAddressable =
  testTask "a merged branch resolves by name for reads, but not as a switch target" {
    do! cleanupBranch (testBranch "mergedName")
    do! cleanupBranch (testBranch "mergedName2")

    do! Branches.createBranch (testBranch "mergedName") "reuse-me" PT.BranchId.Main
    let! live = Branches.liveIdForName "reuse-me"
    Expect.equal
      live
      (Some(testBranch "mergedName"))
      "live branch resolves for both reads and writes"

    do!
      execSqlP
        "UPDATE branches SET merged_at = datetime('now') WHERE id = @b"
        [ "b", Sql.string (string (testBranch "mergedName")) ]

    let! readSide = idForName "reuse-me"
    Expect.equal
      readSide
      (Some(testBranch "mergedName"))
      "merged branch still answers a read verb by name"

    let! writeSide = Branches.liveIdForName "reuse-me"
    Expect.isNone
      writeSide
      "but is NOT what `switch <name>` lands on -- its work is already merged"

    let! merged = isMerged (testBranch "mergedName")
    Expect.isTrue
      merged
      "and reports itself merged, so `merge` can say so instead of flipping nothing"

    // Reusing the name starts a separate branch, and reads then mean the NEW one.
    do! Branches.createBranch (testBranch "mergedName2") "reuse-me" PT.BranchId.Main
    let! afterReuse = idForName "reuse-me"
    Expect.equal
      afterReuse
      (Some(testBranch "mergedName2"))
      "the most recent branch wins the name"

    // Archiving is different from merging: it discards the ops, so there is nothing left to address.
    do!
      execSqlP
        "UPDATE branches SET archived_at = datetime('now') WHERE id = @b"
        [ "b", Sql.string (string (testBranch "mergedName2")) ]
    let! afterArchive = idForName "reuse-me"
    Expect.equal
      afterArchive
      (Some(testBranch "mergedName"))
      "archived branches drop out of name resolution"

    do! cleanupBranch (testBranch "mergedName")
    do! cleanupBranch (testBranch "mergedName2")
  }


let processOverlaySelects =
  testTask
    "the branch a process is on resolves through its overlay; leaving it stops that" {
    PM.selectBranch PT.BranchId.Main // ensure clean start (process-global)
    let! before =
      (PM.ptForBranch (PM.currentBranchId ())).findFn fooLoc |> Ply.toTask
    Expect.isNone before "no branch active -> foo unresolved (core only)"

    let! ops = parsePackageOps (branchSource 42)
    PM.selectBranch (testBranch "overlaySel")
    PM.setBranchOverlay ops
    let! during =
      (PM.ptForBranch (PM.currentBranchId ())).findFn fooLoc |> Ply.toTask
    Expect.isSome during "on the branch -> foo resolves through its overlay"

    // Main is still answered from core alone, with the branch's ops loaded and inert.
    let! fromMain = (PM.ptForBranch PT.BranchId.Main).findFn fooLoc |> Ply.toTask
    Expect.isNone fromMain "and main never sees a branch's binding"

    PM.selectBranch PT.BranchId.Main // leave clean for other tests (process-global)
    let! after = (PM.ptForBranch (PM.currentBranchId ())).findFn fooLoc |> Ply.toTask
    Expect.isNone after "back on main -> foo unresolved again"
  }

let branchesOffBranches =
  testTask "a branch off another sees its parent's frontier (branches off branches)" {
    do! cleanupBranch (testBranch "boB")
    do! cleanupBranch (testBranch "boA")
    do! Branches.createBranch (testBranch "boA") "chain-a" PT.BranchId.Main
    do! Branches.createBranch (testBranch "boB") "chain-b" (testBranch "boA") // B off A

    let! opsA = parsePackageOps (namedSource "ChainA" 42)
    let! opsB = parsePackageOps (namedSource "ChainB" 99)
    let! _ = Branches.storeDeltaOps (testBranch "boA") opsA
    let! _ = Branches.storeDeltaOps (testBranch "boB") opsB

    // A parent's WIP is its own: until A commits, B's chain overlay carries none of A's ops.
    let! bWipOps = Branches.loadDeltaOps (testBranch "boB")
    let bWipOverlay = PM.withExtraOps pmPT bWipOps
    let! bSeesAWip = bWipOverlay.findFn (fooLocIn "ChainA") |> Ply.toTask
    Expect.isNone bSeesAWip "B does NOT see its parent A's uncommitted fn"

    // Committed on A, the chain carries it. The stamp is what loadDeltaOps keys on.
    do!
      execSqlP
        "UPDATE package_ops SET commit_hash = 'chain-test-commit'
         WHERE id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
        [ "b", Sql.uuid ((testBranch "boA").Guid) ]

    // B's overlay walks the parent chain: A's committed frontier + B's own (committed or not).
    let! bOps = Branches.loadDeltaOps (testBranch "boB")
    let bOverlay = PM.withExtraOps pmPT bOps
    let! bSeesA = bOverlay.findFn (fooLocIn "ChainA") |> Ply.toTask
    let! bSeesB = bOverlay.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isSome bSeesA "B sees its parent A's committed fn (branches off branches)"
    Expect.isSome bSeesB "B sees its own fn, uncommitted included"

    let! aOps = Branches.loadDeltaOps (testBranch "boA")
    let aOverlay = PM.withExtraOps pmPT aOps
    let! aSeesA = aOverlay.findFn (fooLocIn "ChainA") |> Ply.toTask
    let! aSeesB = aOverlay.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isSome aSeesA "A sees its own fn"
    Expect.isNone aSeesB "A does NOT see its child B's fn"

    // MERGE B INTO A (parent != main): retag B's frontier onto A. A's overlay now folds B's fn, but
    // main is still untouched (a non-main merge never flips effective / folds into main).
    let! parentOfB = Branches.parentOf (testBranch "boB")
    Expect.equal parentOfB (testBranch "boA") "B's parent is A"
    let! merged = retagFrontierToParent (testBranch "boB") parentOfB
    Expect.isGreaterThan merged 0L "B had frontier ops to merge"

    let! aOps2 = Branches.loadDeltaOps (testBranch "boA")
    let aOverlay2 = PM.withExtraOps pmPT aOps2
    let! aNowSeesB = aOverlay2.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isSome aNowSeesB "after merge, A sees B's fn (retagged onto A)"
    // B's OWN frontier tags are gone (moved to A). loadDeltaOps("boB") would still WALK to A, so we
    // check the direct tags, not the chain overlay.
    let! bOwnTags = ownTagCount (testBranch "boB")
    Expect.equal
      bOwnTags
      0L
      "B's own frontier tags are empty after the retag (its ops are A's now)"
    let! stillNotMain = pmPT.findFn (fooLocIn "ChainB") |> Ply.toTask
    Expect.isNone stillNotMain "merge into a non-main parent does NOT leak into main"

    do! cleanupBranch (testBranch "boB")
    do! cleanupBranch (testBranch "boA")
  }

/// Main authoring's WipRefresh must NOT see a branch's ops: `getWipOps` excludes every
/// `op_branches`-tagged op. Let them back into WIP and the draft rewrite folds them
/// into main.
let getWipOpsExcludesBranch =
  testTask
    "getWipOps excludes branch-tagged ops (main authoring can't see branch state)" {
    let! branchId = freshBranch "test-wip-guard" "wip-guard"
    let! ops = parsePackageOps (branchSource 42)
    let! _ = Branches.storeDeltaOps branchId ops

    let! wip = Queries.getWipOps ()
    // `effective = 1`: `getWipOps` excludes the OTHER inert population too (ops a client pushed to this
    // store, and anything a test left inert), which carries no tag to subtract.
    let! total =
      countSql "SELECT count(*) AS n FROM package_ops WHERE effective = 1" []
    let! branchCount = ownTagCount branchId
    // Every TAGGED op, not just this branch's: `getWipOps` excludes `op_branches` wholesale, so
    // subtracting only our own count assumes we're the only branch in the store, and we aren't.
    // DISTINCT because one op can be tagged to several branches.
    let! taggedCount =
      Sql.query
        "SELECT count(DISTINCT ob.op_id) AS n FROM op_branches ob
         JOIN package_ops p ON p.id = ob.op_id
         WHERE p.effective = 1"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    Expect.isGreaterThan branchCount 0L "the branch actually stored ops"
    Expect.equal
      (int64 (List.length wip))
      (total - taggedCount)
      "getWipOps returns every op EXCEPT branch-tagged ones (isolation)"

    do! cleanupBranch branchId
  }

/// `applyUnappliedOps`' final sweep is scoped to `applied=0 AND effective=1`, so merging branch M
/// leaves sibling S's still-pending ops applied=0. A wider sweep marks them applied without folding
/// them, they can never fold afterwards, and S's binding is silently lost by merge order.
let mergeDoesNotConsumeSiblingPendingOps =
  testTask
    "a merge leaves OTHER branches' pending ops applied=0 (applied-flag isolation)" {
    let! bS = freshBranch "test-sweep-sibling" "sweep-sibling"
    let! bM = freshBranch "test-sweep-merging" "sweep-merging"
    // Distinct modules so M's fold pollutes only its own unique name (cleaned up after).
    let! opsS = parsePackageOps (namedSource "SweepSibling" 7)
    let! opsM = parsePackageOps (namedSource "SweepMerging" 8)
    let! _ = Branches.storeDeltaOps bS opsS
    let! _ = Branches.storeDeltaOps bM opsM

    let pendingCount (b : PT.BranchId) : Task<int64> =
      Sql.query
        "SELECT count(*) AS n FROM package_ops
         WHERE applied = 0 AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
      |> Sql.parameters [ "b", Sql.string (string b) ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    let! sBefore = pendingCount bS
    Expect.isGreaterThan sBefore 0L "sibling S starts with pending (applied=0) ops"

    // Merge M into main: flip its frontier effective=1, then fold (which runs the sweep).
    let! _ = markMergedEffective bM
    let! _ = Seed.applyUnappliedOps ()

    let! sAfter = pendingCount bS
    Expect.equal
      sAfter
      sBefore
      "S's pending ops are UNTOUCHED by M's merge (still applied=0)"

    do! cleanupBranch bS
    do! cleanupBranch bM
    do! execSql "DELETE FROM locations WHERE modules = 'SweepMerging'"
  }

/// Two branches bind the SAME name to DIFFERENT hashes, the second authored LATER. Merging older
/// then newer must land main on the NEWER binding (origin_ts LWW), so convergence does not depend on
/// merge order. Folds through the real path (markMergedEffective + applyUnappliedOps).
let sameNameMergesConvergeToLater =
  testTask
    "merging older-then-newer for one name lands on the NEWER binding (origin_ts LWW)" {
    let bOld = testBranch "test-cvg-old"
    let bNew = testBranch "test-cvg-new"
    let liveHash () : Task<Option<string>> =
      liveBoundHash { owner = "Darklang"; modules = [ "ConvergeWin" ]; name = "foo" }
    let mergeFold (b : PT.BranchId) : Task<unit> =
      task {
        let! _ = markMergedEffective b
        let! _ = Seed.applyUnappliedOps ()
        return ()
      }
    do! cleanupBranch bOld
    do! cleanupBranch bNew
    do! execSql "DELETE FROM locations WHERE modules = 'ConvergeWin'"

    do! Branches.createBranch bOld "cvg-old" PT.BranchId.Main
    do! Branches.createBranch bNew "cvg-new" PT.BranchId.Main
    // Same module+name (ConvergeWin.foo), different bodies -> different hashes. bOld stored first, so
    // its ops get an EARLIER origin_ts than bNew's (storeDeltaOps stamps now() per call).
    let! opsOld = parsePackageOps (namedSource "ConvergeWin" 5)
    let! opsNew = parsePackageOps (namedSource "ConvergeWin" 6)
    let! _ = Branches.storeDeltaOps bOld opsOld
    let! _ = Branches.storeDeltaOps bNew opsNew
    let (PT.Hash newerHash) = hashBoundTo opsNew "foo"

    do! mergeFold bOld
    do! mergeFold bNew

    let! live = liveHash ()
    Expect.equal
      live
      (Some newerHash)
      "main lands on the NEWER binding after older-then-newer merge"

    do! cleanupBranch bOld
    do! cleanupBranch bNew
    do! execSql "DELETE FROM locations WHERE modules = 'ConvergeWin'"
  }

/// Locks the reload-stable rebase model: nameConflicts flags a name whose main hash diverged from
/// the branch's recorded base; rebase accepts main's state and clears it. Manipulates the base row
/// directly (no fold into main) so the test never pollutes the shared main projection.
let rebaseDetectsAndClearsConflicts =
  testTask "nameConflicts flags a diverged name; rebase clears it" {
    let! bid = freshBranch "test-rebase-gate" "rebase-gate"
    let! ops = parsePackageOps (namedSource "RebaseGate" 5)
    let! _ = Branches.storeDeltaOps bid ops
    do! Branches.recordNameBases bid PT.BranchId.Main ops

    // clean before divergence: the name isn't in main, so base "" == main's current "".
    let! c0 = nameConflicts bid
    Expect.isEmpty c0 "clean before any divergence"

    // simulate main having changed that name since the fork: stale the recorded base.
    do! staleNameBases bid
    let! c1 = nameConflicts bid
    Expect.isNonEmpty c1 "conflict detected (main's current hash != the stale base)"

    // rebase accepts main -> base := main's current -> no conflict, merge unblocked.
    let! _ = rebase bid
    let! c2 = nameConflicts bid
    Expect.isEmpty c2 "rebase cleared the conflict"

    do! cleanupBranch bid
  }

/// Locks the branch-transfer import path (what scmImportBranchOps does): register a branch, store
/// its ops effective=0 + tag, and re-derive the per-name bases against THIS instance's main. The
/// cross-instance invariant is that a branch stays a branch on the receiving side.
let branchTransferImportReDerivesBases =
  testTask
    "importing a branch's ops recreates it isolated + re-derives its per-name bases locally" {
    // simulate scmImportBranchOps: register + store ops + re-derive bases.
    let! dst = freshBranch "test-xfer-dst" "xfer"
    let! ops = parsePackageOps (namedSource "XferTest" 8)
    let! _ = Branches.storeDeltaOps dst ops
    do! Branches.recordNameBases dst PT.BranchId.Main ops

    let! loaded = Branches.loadDeltaOps dst
    let overlay = PM.withExtraOps pmPT loaded
    let! resolved = overlay.findFn (fooLocIn "XferTest") |> Ply.toTask
    Expect.isSome resolved "imported branch resolves its fn"
    let! onCore = pmPT.findFn (fooLocIn "XferTest") |> Ply.toTask
    Expect.isNone onCore "and it did NOT leak into core"

    // per-name bases were re-derived (against local main), so merge/rebase work on this instance.
    let! bases = baseNames dst
    Expect.isNonEmpty bases "per-name bases re-derived on import"

    do! cleanupBranch dst
  }

/// A name a branch binds ONLY by resolving a conflict still needs a per-name base: the base is what
/// the conflict detector needs to prove BOTH sides moved. Without one that name can never conflict
/// again, and `dark diff` renders it as `+ new` rather than a change. `bindingFromOp` counts
/// `Resolve` as a binding, and `recordNameBases` has to agree with it.
let resolveAloneRecordsANameBase =
  testTask "a name bound only by Resolve still gets a per-name base" {
    let! b = freshBranch "test-resolve-base" "resolve-base"

    // Borrow a real (location, target) off a SetName rather than hand-building a hash: what is under
    // test is which op SHAPE gets counted, not what a Reference looks like.
    let! ops = parsePackageOps (namedSource "ResolveBaseTest" 7)

    let binding =
      ops
      |> List.tryPick (fun op ->
        match op with
        | PT.PackageOp.SetName(loc, target, _) -> Some(loc, target)
        | _ -> None)

    Expect.isSome binding "the fixture produced a SetName to borrow a binding from"
    let (loc, target) = Option.get binding

    do!
      Branches.recordNameBases
        b
        PT.BranchId.Main
        [ PT.PackageOp.Decision(
            "decision-for-the-base-test",
            loc,
            "",
            PT.DecisionKind.Override target
          ) ]

    let! recorded = baseNames b
    Expect.equal
      recorded
      [ loc.name ]
      "the Resolve on its own recorded a base for the name"

    do! cleanupBranch b
  }

/// Locks per-name RESOLUTION (scm-spec 7). take-theirs untags the branch's SetName, so its overlay
/// falls back to the parent for that name; keep-mine leaves the branch binding it, re-stamped. Both
/// clear the conflict. The conflict is set up by staling the base, so nothing folds into shared main.
/// `resolve mine` must not rewrite any EXISTING op's `origin_ts`.
///
/// That stamp is portable: it is supposed to be byte-identical on every machine holding the op, because
/// it is what last-writer-wins compares. A branch's frontier ops travel, so re-stamping one locally makes
/// two peers resolve the same pair of bindings differently, permanently, with nothing to show why. The
/// resolution is recorded as a NEW `Decision`/`Override` instead, which carries its own stamp.
///
/// Asserted over EVERY op in the store rather than the ones we expect to be touched: the failure mode is
/// a stamp moving somewhere nobody was looking.
let resolveKeepMineDoesNotRestampSharedOps =
  testTask
    "resolve keep-mine authors an override and leaves every existing stamp alone" {
    let! b = freshBranch "test-resolve-no-restamp" "restamp"
    let fqn = "Darklang.RestampTest.foo"
    let! ops = parsePackageOps (namedSource "RestampTest" 7)
    let! _ = Branches.storeDeltaOps b ops
    do! Branches.recordNameBases b PT.BranchId.Main ops
    do! staleNameBases b

    let stamps () =
      Sql.query "SELECT id, origin_ts FROM package_ops"
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.string "origin_ts"))

    let! before = stamps ()

    match! resolveKeepMine b fqn with
    | Error e -> failtest $"keep-mine failed: {e}"
    | Ok() -> ()

    let! after = stamps ()
    let afterById = Map.ofList after

    let moved =
      before
      |> List.filter (fun (id, ts) ->
        match Map.tryFind id afterById with
        | Some newTs -> newTs <> ts
        | None -> true)

    Expect.isEmpty
      moved
      "no op that already existed had its origin_ts rewritten (or vanished)"

    // And it did something: the resolution is a new op, not a no-op.
    Expect.isGreaterThan
      (List.length after)
      (List.length before)
      "keep-mine recorded the decision as a new op"

    do! cleanupBranch b
  }


/// `resolve theirs` after `resolve mine` has to undo the first decision. Keep-mine authors an Override
/// and take-theirs drops SetNames; both BIND the name, so take-theirs has to drop every binder, not
/// just the one shape. `opBindsKey` is the single definition of "binds this name" both sides read.
let takeTheirsAfterKeepMineDropsTheOverride =
  testTask "resolve theirs after resolve mine drops the override too" {
    let! b = freshBranch "test-theirs-after-mine" "theirs-after-mine"
    let fqn = "Darklang.TheirsAfterMine.foo"
    let! ops = parsePackageOps (namedSource "TheirsAfterMine" 7)
    let! _ = Branches.storeDeltaOps b ops
    do! Branches.recordNameBases b PT.BranchId.Main ops
    do! staleNameBases b

    match! resolveKeepMine b fqn with
    | Error e -> failtest $"keep-mine failed: {e}"
    | Ok() -> ()
    let! afterMine = countEffective b 0
    Expect.isGreaterThan
      afterMine
      0L
      "keep-mine left the branch binding the name (an override)"

    match! resolveTakeTheirs b fqn with
    | Error e -> failtest $"take-theirs failed: {e}"
    | Ok() -> ()
    let! binders =
      darkStringList
        $"Darklang.SCM.Branches.ownFrontierOps {darkBranch b} |> Stdlib.List.filterMap (fun entry -> let (_, op) = entry in Darklang.SCM.Branches.opBindsKey op (\"Darklang\", \"TheirsAfterMine\", \"foo\") |> Stdlib.Option.map (fun _ -> \"binds\"))"
    Expect.isEmpty
      binders
      "nothing on the branch binds the name any more: the override went with the SetName"

    do! cleanupBranch b
  }


let perNameResolutionMineTheirs =
  testTask
    "resolve take-theirs drops the branch's binding; keep-mine keeps it; both clear the conflict" {
    let bT = testBranch "test-resolve-theirs"
    let bM = testBranch "test-resolve-mine"
    let fqn = "Darklang.ResolveTest.foo"
    let setupConflict (b : PT.BranchId) =
      task {
        do! cleanupBranch b
        do! Branches.createBranch b "resolve" PT.BranchId.Main
        let! ops = parsePackageOps (namedSource "ResolveTest" 5)
        let! _ = Branches.storeDeltaOps b ops
        do! Branches.recordNameBases b PT.BranchId.Main ops
        do! staleNameBases b
      }

    do! setupConflict bT
    let! c0 = nameConflicts bT
    Expect.isNonEmpty c0 "conflict present before take-theirs"
    match! resolveTakeTheirs bT fqn with
    | Error e -> failtest $"take-theirs failed: {e}"
    | Ok() -> ()
    let! c1 = nameConflicts bT
    Expect.isEmpty c1 "take-theirs cleared the conflict"
    let! loadedT = Branches.loadDeltaOps bT
    let overlayT = PM.withExtraOps pmPT loadedT
    let! resolvedT = overlayT.findFn (fooLocIn "ResolveTest") |> Ply.toTask
    Expect.isNone
      resolvedT
      "take-theirs: branch no longer binds foo (falls back to the parent)"

    do! setupConflict bM
    match! resolveKeepMine bM fqn with
    | Error e -> failtest $"keep-mine failed: {e}"
    | Ok() -> ()
    let! c2 = nameConflicts bM
    Expect.isEmpty c2 "keep-mine cleared the conflict"
    let! loadedM = Branches.loadDeltaOps bM
    let overlayM = PM.withExtraOps pmPT loadedM
    let! resolvedM = overlayM.findFn (fooLocIn "ResolveTest") |> Ply.toTask
    Expect.isSome resolvedM "keep-mine: branch still binds foo"

    do! cleanupBranch bT
    do! cleanupBranch bM
  }

/// Locks revive-on-reuse (createBranch upsert): re-creating an archived/merged branch id clears
/// archived_at + merged_at, so a review queue reused after reject/approve is active and visible
/// again. Parent stays first-write-wins.
let reuseBranchIdRevives =
  testTask
    "createBranch on an archived/merged id revives it; parent stays first-write-wins" {
    let! b = freshBranch "test-revive" "revive"

    let flagsSet () : Task<int64> =
      Sql.query
        "SELECT count(*) AS n FROM branches
         WHERE id = @b AND (archived_at IS NOT NULL OR merged_at IS NOT NULL)"
      |> Sql.parameters [ "b", Sql.string (string b) ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    // Set the flags directly: this test is about createBranch's revive-on-reuse, and `archive` is
    // Dark now (SCM.Branches), so SQL keeps the setup on the test's actual subject.
    do!
      execSqlP
        "UPDATE branches SET archived_at = datetime('now'), merged_at = datetime('now')
         WHERE id = @b"
        [ "b", Sql.string (string b) ]
    let! before = flagsSet ()
    Expect.equal before 1L "archived/merged flags are set before reuse"

    do! Branches.createBranch b "revive" PT.BranchId.Main
    let! after = flagsSet ()
    Expect.equal
      after
      0L
      "reuse revived the branch (archived_at + merged_at cleared)"

    // re-creating with a DIFFERENT parent must NOT change the recorded parent (first-write-wins).
    do! Branches.createBranch b "revive" (testBranch "some-other-parent")
    let! parent = Branches.parentOf b
    Expect.equal
      parent
      PT.BranchId.Main
      "parent stays first-write-wins across re-creation"

    do! cleanupBranch b
  }

/// ISOLATION: the branch author path folds a value's AddValue CONTENT into package_values (so eval
/// can read its rt_dval), but must NOT fold the SetName, so the NAME never lands in main's
/// `locations`. CORRECTNESS: after the content fold, `evaluateAllValues` must materialise an
/// EXPRESSION body's Dval into rt_dval, or it stays NULL and getValue returns nothing.
let branchValueContentFoldIsolatesName =
  testTask
    "folding a branch value's AddValue content populates package_values but NOT locations" {
    let source =
      "module Darklang.BranchValFoldTest\n\nval vv = Stdlib.Int64.add 3L 4L"
    let! ops = parsePackageOps source
    let addValueOps =
      ops
      |> List.filter (fun op ->
        match op with
        | PT.PackageOp.AddValue _ -> true
        | _ -> false)
    Expect.isNonEmpty addValueOps "the val source produced an AddValue op"
    let valueHash =
      addValueOps
      |> List.pick (fun op ->
        match op with
        | PT.PackageOp.AddValue v ->
          let (PT.Hash h) = v.hash
          Some h
        | _ -> None)

    // Fold ONLY the AddValue (mirrors the branch author path's content-only fold).
    do! LibDB.PackageOpPlayback.applyOps addValueOps

    let! contentCount =
      countSql
        "SELECT count(*) AS n FROM package_values WHERE hash = @h"
        [ "h", Sql.string valueHash ]
    Expect.isGreaterThan
      contentCount
      0L
      "AddValue folded the content into package_values"

    let! locCount =
      Sql.query
        "SELECT count(*) AS n FROM locations WHERE name = 'vv' AND modules LIKE '%BranchValFoldTest%'"
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal
      locCount
      0L
      "the branch value's NAME is NOT in main locations (content-only fold keeps names branch-isolated)"

    // `applyOps` stores rt_dval NULL (see PackageOpPlayback.fs), so the branch author path must run
    // `evaluateAllValues` for an EXPRESSION-valued branch value to materialise its Dval.
    let builtins = Builtins.CliHost.Libs.Cli.builtinsToUse ()
    let! _ = Seed.evaluateAllValues Seed.TrustedSeed builtins PM.rt
    let! (evaluated : Option<RT.PackageValue.PackageValue>) =
      LibDB.RuntimeTypes.Value.get (RT.Hash valueHash) |> Ply.toTask
    match evaluated with
    | Some pv ->
      Expect.equal
        pv.body
        (RT.DInt64 7L)
        "the expression-valued branch value materialised to 3+4=7 in rt_dval (bug #1 correctness guard)"
    | None ->
      Tests.failtest
        "rt_dval was NULL after evaluateAllValues -- expression-valued branch values would error (bug #1 regression)"

    do!
      execSqlP
        "DELETE FROM package_values WHERE hash = @h"
        [ "h", Sql.string valueHash ]
  }

let branchExists =
  testTask
    "a branch is found by its registry row OR its op tags, and a typo is found by neither" {
    do! cleanupBranch (testBranch "beX")

    let! beforeAnything = Branches.exists (testBranch "beX")
    Expect.isFalse beforeAnything "an unknown branch does not exist"

    // Registered with no ops yet: this is what `dark switch` produces before you author anything, and
    // it has to count, or a fresh branch reads as a typo.
    do! Branches.createBranch (testBranch "beX") "" PT.BranchId.Main
    let! registeredOnly = Branches.exists (testBranch "beX")
    Expect.isTrue registeredOnly "registered with no ops still Branches.exists"

    // Tagged ops with no registry row: this is what a branch bundle from another machine looks like
    // before anything registers it locally. Also has to count.
    do! cleanupBranch (testBranch "beY")
    let! ops = parsePackageOps (namedSource "BeY" 7)
    let! _ = Branches.storeDeltaOps (testBranch "beY") ops
    do!
      execSqlP
        "DELETE FROM branches WHERE id = @b"
        [ "b", Sql.string (string (testBranch "beY")) ]
    let! taggedOnly = Branches.exists (testBranch "beY")
    Expect.isTrue taggedOnly "ops tagged with no registry row still Branches.exists"

    let! typo = Branches.exists (testBranch "beYY")
    Expect.isFalse typo "a prefix of a real branch is not that branch"

    do! cleanupBranch (testBranch "beX")
    do! cleanupBranch (testBranch "beY")
  }

/// A child of a branch inherits its PARENT'S pins, not just main's.
///
/// `getPropagationPolicy` has to walk the whole chain, not just (this branch, main): stopping at one
/// level makes a branch off a branch follow something its parent deliberately pinned. Nested branches
/// are first-class everywhere else in the model -- the overlay chain, merge routing, name bases.
let propagationPinsComeFromTheWholeChain =
  testTask "a child branch inherits its parent's pins, not only main's" {
    let! parent = freshBranch "pin-parent" "pin-parent"
    let! child = freshBranchOff parent "pin-child" "pin-child"

    let loc : PT.PackageLocation =
      { owner = "Darklang"; modules = [ "ChainPin" ]; name = "target" }

    // The parent pins it. Nobody else says anything.
    do!
      execSqlP
        "INSERT INTO propagation_policy (branch_id, owner, modules, name, policy)
         VALUES (@b, @o, @m, @n, 'pin')"
        [ "b", Sql.string (string parent)
          "o", Sql.string loc.owner
          "m", Sql.string (String.concat "." loc.modules)
          "n", Sql.string loc.name ]

    let! childPins = Queries.getPropagationPins child
    Expect.isTrue
      (Set.contains (loc.owner, String.concat "." loc.modules, loc.name) childPins)
      "the child sees its parent's pin"

    // And the child can still say otherwise: nearest in the chain wins, whatever it says.
    do!
      execSqlP
        "INSERT INTO propagation_policy (branch_id, owner, modules, name, policy)
         VALUES (@b, @o, @m, @n, 'follow')"
        [ "b", Sql.string (string child)
          "o", Sql.string loc.owner
          "m", Sql.string (String.concat "." loc.modules)
          "n", Sql.string loc.name ]

    let! overridden = Queries.getPropagationPins child
    Expect.isFalse
      (Set.contains (loc.owner, String.concat "." loc.modules, loc.name) overridden)
      "and its own follow beats the parent's pin"

    let! follows = Queries.getPropagationFollows child
    Expect.isTrue
      (Set.contains (loc.owner, String.concat "." loc.modules, loc.name) follows)
      "which is where it went"

    // An unrelated branch is unaffected: the chain is the scope, not the store.
    let! mainPins = Queries.getPropagationPins PT.BranchId.Main
    Expect.isFalse
      (Set.contains (loc.owner, String.concat "." loc.modules, loc.name) mainPins)
      "main never saw either decision"

    do!
      execSqlP
        "DELETE FROM propagation_policy WHERE branch_id IN (@p, @c)"
        [ "p", Sql.string (string parent); "c", Sql.string (string child) ]
    do! cleanupBranch child
    do! cleanupBranch parent
  }

let mergeCountsWhatItFlipped =
  testTask
    "markMergedEffective reports ops it flipped, not ops that were already effective" {
    do! cleanupBranch (testBranch "mcX")
    do! Branches.createBranch (testBranch "mcX") "" PT.BranchId.Main
    let! ops = parsePackageOps (namedSource "McX" 11)
    let! _ = Branches.storeDeltaOps (testBranch "mcX") ops

    let! pending = countEffective (testBranch "mcX") 0
    Expect.isGreaterThan pending 0L "stored branch ops start effective=0"

    let! first = markMergedEffective (testBranch "mcX")
    Expect.equal first pending "the first merge reports exactly what it flipped"

    // Merging again flips NOTHING -- every tagged op is already effective -- so reporting the tag
    // count would be `MergeOutcome.merged` claiming work it did not do.
    let! second = markMergedEffective (testBranch "mcX")
    Expect.equal second 0L "a re-merge reports 0, not the number of tagged ops"

    do! cleanupBranch (testBranch "mcX")
  }

let importedOpsKeepTheirStamps =
  testTask
    "storeDeltaOpsStamped preserves an incoming op's origin_ts instead of re-stamping it" {
    do! cleanupBranch (testBranch "stX")
    do! Branches.createBranch (testBranch "stX") "" PT.BranchId.Main

    // A stamp from the far past, which the local authoring clock could never produce.
    let farPast = "2001-02-03T04:05:06.007Z"
    let! ops = parsePackageOps (namedSource "StX" 13)
    let! _ =
      Branches.storeDeltaOpsStamped
        (testBranch "stX")
        (ops |> List.map (fun op -> (op, farPast)))

    let! stamps =
      Sql.query
        "SELECT DISTINCT p.origin_ts AS ts FROM package_ops p
         JOIN op_branches ob ON ob.op_id = p.id WHERE ob.branch_id = @b"
      |> Sql.parameters [ "b", Sql.string (string (testBranch "stX")) ]
      |> Sql.executeAsync (fun read -> read.string "ts")

    Expect.equal
      stamps
      [ farPast ]
      "every imported op keeps the stamp it arrived with -- re-stamping locally makes the IMPORTER \
       look like the author, so LWW resolves by who imported last rather than who edited last"

    do! cleanupBranch (testBranch "stX")
  }

let rebuildKeepsBranchPolicy =
  testTask "a projection rebuild re-folds branch-scoped propagation decisions" {
    do! cleanupBranch (testBranch "bpX")
    do! Branches.createBranch (testBranch "bpX") "" PT.BranchId.Main

    let loc : PT.PackageLocation =
      { owner = "Zz"; modules = [ "RebuildTest" ]; name = "pinned" }
    let decide =
      PT.PackageOp.Decision(
        "pin:RebuildTest.pinned:2026-01-02T00:00:00.000Z",
        loc,
        "deliberate",
        PT.DecisionKind.Propagation PT.PropagationPolicy.Pin
      )
    let! _ = Branches.storeDeltaOps (testBranch "bpX") [ decide ]

    let countPolicy () =
      Sql.query
        "SELECT count(*) AS n FROM propagation_policy
         WHERE branch_id = @b AND owner = 'Zz' AND modules = 'RebuildTest' AND name = 'pinned'"
      |> Sql.parameters [ "b", Sql.string (string (testBranch "bpX")) ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")

    let! stored = countPolicy ()
    Expect.equal
      stored
      1L
      "authoring the Decision on a branch folds a branch-scoped policy row"

    // A projection rebuild clears this table and re-folds only `effective = 1` ops, and branch ops
    // are `effective = 0` by design, so without an explicit re-fold the row never comes back.
    do! Sql.query "DELETE FROM propagation_policy" |> Sql.executeStatementAsync
    let! cleared = countPolicy ()
    Expect.equal cleared 0L "cleared, as a rebuild would"

    do! Branches.refoldBranchDecides ()
    let! restored = countPolicy ()
    Expect.equal
      restored
      1L
      "the rebuild path re-folds branch decisions from the log"

    do!
      execSql
        "DELETE FROM propagation_policy WHERE owner = 'Zz' AND modules = 'RebuildTest'"
    do! cleanupBranch (testBranch "bpX")
  }

let branchPMIsPerBranch =
  testTask "ptForBranch answers about a branch this process is NOT on" {
    do! cleanupBranch (testBranch "pfA")
    do! cleanupBranch (testBranch "pfB")
    do! Branches.createBranch (testBranch "pfA") "" PT.BranchId.Main
    do! Branches.createBranch (testBranch "pfB") "" PT.BranchId.Main

    let! opsA = parsePackageOps (namedSource "PfA" 42)
    let! opsB = parsePackageOps (namedSource "PfB" 99)
    let! _ = Branches.storeDeltaOps (testBranch "pfA") opsA
    let! _ = Branches.storeDeltaOps (testBranch "pfB") opsB

    // Sit on A. This is the state a `dark --branch pfA ...` process boots into.
    PM.selectBranch (testBranch "pfA")
    Expect.equal (PM.currentBranchId ()) (testBranch "pfA") "process is on pfA"

    // Asking about a branch we're NOT on is what a process-global overlay alone cannot do, and what
    // the LSP and any daemon will need.
    let! aFromA =
      (PM.ptForBranch (testBranch "pfA")).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isSome aFromA "on pfA, pfA's fn resolves"

    let! bFromA =
      (PM.ptForBranch (testBranch "pfB")).findFn (fooLocIn "PfB") |> Ply.toTask
    Expect.isSome bFromA "while ON pfA, pfB's fn still resolves via ptForBranch"

    let! aFromB =
      (PM.ptForBranch (testBranch "pfB")).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isNone aFromB "pfB's overlay does not contain pfA's fn"

    let! aFromMain =
      (PM.ptForBranch PT.BranchId.Main).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isNone aFromMain "and main sees neither"

    // Switching is a process operation, not a restart: what `dark switch` needs in the REPL.
    PM.selectBranch (testBranch "pfB")
    Expect.equal
      (PM.currentBranchId ())
      (testBranch "pfB")
      "process moved to pfB without restarting"
    let! bAfterSwitch =
      (PM.ptForBranch (PM.currentBranchId ())).findFn (fooLocIn "PfB") |> Ply.toTask
    Expect.isSome bAfterSwitch "the active overlay followed the switch"
    let! aAfterSwitch =
      (PM.ptForBranch (PM.currentBranchId ())).findFn (fooLocIn "PfA") |> Ply.toTask
    Expect.isNone aAfterSwitch "and stopped answering about the branch we left"

    PM.selectBranch PT.BranchId.Main // leave clean for other tests (process-global)
    do! cleanupBranch (testBranch "pfA")
    do! cleanupBranch (testBranch "pfB")
  }

let branchNamesResolveButDontShadowMain =
  testTask
    "a branch supplies names for hashes main can't name, and never relabels ones it can" {
    do! cleanupBranch (testBranch "lnX")
    do! Branches.createBranch (testBranch "lnX") "" PT.BranchId.Main

    let! ops = parsePackageOps (namedSource "LnX" 42)
    let! _ = Branches.storeDeltaOps (testBranch "lnX") ops

    let branchHash =
      ops
      |> List.tryPick (fun op ->
        match op with
        | PT.PackageOp.SetName(l, target, _) when l.name = "foo" -> Some target.hash
        | _ -> None)
      |> Option.get

    // Main cannot name this hash: a branch's SetNames never fold into `locations`, so without the
    // overlay the caller has nothing to render but 64 hex characters.
    let! fromMain = PM.pt.getFnLocations branchHash |> Ply.toTask
    Expect.isEmpty fromMain "main has no name for a branch-authored hash"

    PM.selectBranch (testBranch "lnX")
    let onBranch =
      PM.locationsFor (PM.currentBranchId ()) PT.ItemKind.Fn branchHash []
    Expect.equal
      (onBranch |> List.map (fun l -> l.name))
      [ "foo" ]
      "the overlay supplies the name main is missing"

    // ... but main WINS when it has an answer. Identical content is one item, so a hash is routinely
    // live at several names, and preferring the branch's renders a main item under a branch label.
    let mainLoc : PT.PackageLocation =
      { owner = "Darklang"; modules = [ "SomeMainModule" ]; name = "mainName" }
    let withMain =
      PM.locationsFor (PM.currentBranchId ()) PT.ItemKind.Fn branchHash [ mainLoc ]
    Expect.equal
      (withMain |> List.map (fun l -> l.name) |> List.tryHead)
      (Some "mainName")
      "main's name comes first, so a duplicated body is never relabelled to the branch's"

    let asType =
      PM.locationsFor (PM.currentBranchId ()) PT.ItemKind.Type branchHash []
    Expect.isEmpty asType "the overlay answers per kind, not per hash alone"

    PM.selectBranch PT.BranchId.Main
    let offBranch = PM.locationsFor PT.BranchId.Main PT.ItemKind.Fn branchHash []
    Expect.isEmpty offBranch "off the branch, the name is gone again (isolation)"

    do! cleanupBranch (testBranch "lnX")
  }


/// The three ways a run picks its branch. Each tier is scoped tighter than the one below on purpose:
/// the FLAG is this command, the ENV is this SHELL, the config is this machine. The env tier is what
/// lets several agents work on several branches at once without fighting over the single config key
/// `dark switch` writes. Tested on the product's own `BranchSelection.select` rather than on a `pick`
/// defined inside the test, which would notice nothing done to `Cli.fs`.
let branchResolutionOrder =
  testTask "the flag beats DARK_BRANCH beats the stored branch" {
    let flagB = testBranch "sel-flag"
    let envB = testBranch "sel-env"
    let storedB = testBranch "sel-stored"
    do! cleanupBranch flagB
    do! cleanupBranch envB
    do! cleanupBranch storedB
    do! Branches.createBranch flagB "sel-flag" PT.BranchId.Main
    do! Branches.createBranch envB "sel-env" PT.BranchId.Main
    do! Branches.createBranch storedB "sel-stored" PT.BranchId.Main
    let! before = LibDB.Config.get "current_branch"
    let! beforeName = LibDB.Config.get "current_branch_name"

    let selected
      (r : Result<Sel.Selection, Sel.Refusal>)
      : Option<PT.BranchId> * Sel.Tier =
      match r with
      | Ok s -> (s.branchId, s.tier)
      | Error e -> failtest $"refused: {e}"

    try
      // The id, with the name beside it, as `dark switch` writes them.
      do! LibDB.Config.set "current_branch" (string storedB)
      do! LibDB.Config.set "current_branch_name" "sel-stored"
      let! s = Sel.select (Some "sel-flag") (Some "sel-env")
      Expect.equal (selected s) (Some flagB, Sel.Flag) "the flag wins"
      let! s = Sel.select None (Some "sel-env")
      Expect.equal (selected s) (Some envB, Sel.Env) "then the env"
      let! s = Sel.select None None
      Expect.equal (selected s) (Some storedB, Sel.Stored) "then the stored branch"
      let! s = Sel.select (Some "main") (Some "sel-env")
      Expect.equal
        (selected s)
        (None, Sel.Flag)
        "the flag can name main, and that beats the env too"

      // A foreign uuid or an ambiguous prefix is refused, not started as a branch of that name.
      let! s = Sel.select (Some(string (System.Guid.NewGuid()))) None
      Expect.isError s "a uuid this store lacks is refused"

      do!
        execSqlP
          "UPDATE branches SET archived_at = datetime('now') WHERE id = @b"
          [ "b", Sql.string (string storedB) ]
      let! s = Sel.select None None
      match s with
      | Ok(s : Sel.Selection) ->
        Expect.equal
          (s.branchId, s.goneStored)
          (None, Some "sel-stored")
          "a gone stored branch degrades to main and says which"
      | Error e -> failtest $"refused: {e}"
      let! s = Sel.select None None
      match s with
      | Ok(s : Sel.Selection) ->
        Expect.isNone s.goneStored "and says so once: the config was reset to main"
      | Error e -> failtest $"refused: {e}"

      do! LibDB.Config.set "current_branch" ""
      let! s = Sel.select None None
      Expect.equal
        (selected s)
        (None, Sel.Default)
        "and main is the absence of all three"
    finally
      (LibDB.Config.set "current_branch" (Option.defaultValue "" before)).Wait()
      (LibDB.Config.set "current_branch_name" (Option.defaultValue "" beforeName))
        .Wait()
      (cleanupBranch flagB).Wait()
      (cleanupBranch envB).Wait()
      (cleanupBranch storedB).Wait()
  }


/// A merge that does not travel: the merged OPS already cross (they are main ops once merged, and
/// the two mains converge on identical hashes), but without the event the FACT of the merge does
/// not, so a colleague's copy of the branch still lists as live work they could keep authoring on.
let branchEventMarksMerged =
  testTask "a BranchEvent(Merged) op folds to marking that branch merged" {
    let! branchId = freshBranch "test-branch-event-merged" "event-proof"

    let! before = isMerged branchId
    Expect.isFalse before "not merged before the event"

    let op =
      PT.PackageOp.BranchEvent(branchId, PT.Merged [], "2026-01-01T00:00:00.000Z")
    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]

    let! after = isMerged branchId
    Expect.isTrue after "the event folded, so the branch reads as merged"

    // Monotonic: this is what lets the event travel with no stamp on `branches` to arbitrate with, and
    // what makes re-receiving it on a third machine harmless.
    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]
    let! twice = isMerged branchId
    Expect.isTrue twice "applying it again lands in the same place"

    do! cleanupBranch branchId
  }


/// An op is one row whatever authored it, so a branch's op and main's identical op share an id. Main
/// authoring it is main saying it runs here: the row flips effective and folds, and the tag goes.
///
/// `INSERT OR IGNORE` is the wrong verb for that. The row already exists, so the insert does nothing
/// while the CLI reports that it authored -- `dark deprecate` saying "Deprecated" over a fn that keeps
/// running.
let mainRetakesABranchsOp =
  testTask
    "authoring on main an op a branch already holds makes it effective, untagged, and live" {
    let! branchId = freshBranch "test-branch-main-retake" "retake-proof"

    let! ops = parsePackageOps (namedSource "MainRetake" 42)
    let ids = ops |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))
    let! _ = Branches.storeDeltaOps branchId ops
    let! onMainBefore = pmPT.findFn (fooLocIn "MainRetake") |> Ply.toTask
    Expect.isNone onMainBefore "held by the branch only, main cannot see it"

    let! inserted = LibDB.Inserts.insertAndApplyOps ops
    Expect.equal
      inserted
      (int64 (List.length ops))
      "every op counted as taken, none as a duplicate"

    let! effectiveTagged =
      Sql.query
        "SELECT
           sum(p.effective) AS eff,
           (SELECT count(*) FROM op_branches WHERE branch_id = @b) AS tagged
         FROM package_ops p WHERE p.id IN (SELECT value FROM json_each(@ids))"
      |> Sql.parameters
        [ "b", Sql.string (string branchId)
          "ids", Sql.string (System.Text.Json.JsonSerializer.Serialize ids) ]
      |> Sql.executeRowAsync (fun read -> (read.int64 "eff", read.int64 "tagged"))
    Expect.equal
      effectiveTagged
      (int64 (List.length ops), 0L)
      "all effective, and no tag left on any"

    let! onMainAfter = pmPT.findFn (fooLocIn "MainRetake") |> Ply.toTask
    Expect.isSome onMainAfter "and main resolves it"

    // The tag is gone, so cleanupBranch would not find the rows; drop them by id.
    do!
      execSqlP
        "DELETE FROM locations WHERE op_id IN (SELECT value FROM json_each(@ids))"
        [ "ids", Sql.string (System.Text.Json.JsonSerializer.Serialize ids) ]
    do!
      execSqlP
        "DELETE FROM package_ops WHERE id IN (SELECT value FROM json_each(@ids))"
        [ "ids", Sql.string (System.Text.Json.JsonSerializer.Serialize ids) ]
    LibDB.Caching.invalidateAll ()
    do! cleanupBranch branchId
  }


/// A merged or archived branch is finished, and authoring on it refuses rather than reviving it.
///
/// The revival route is `createBranch`, whose upsert clears `merged_at`: a process still holding the id
/// after a merge elsewhere would put its next edit on a branch nothing merges again, listed as live.
let authoringOnAFinishedBranchRefuses =
  testTask "authoring on a merged branch is refused rather than reviving it" {
    let! branchId = freshBranch "test-branch-finished-refuses" "finished-proof"
    do!
      execSqlP
        "UPDATE branches SET merged_at = datetime('now') WHERE id = @b"
        [ "b", Sql.string (string branchId) ]

    // An empty op list reaches the guard before anything else, so it exercises exactly that.
    let! outcome =
      darkUnitResult $"Darklang.SCM.PackageOps.add {darkBranch branchId} []"
    match outcome with
    | Ok() -> failtest "the edit was accepted onto a merged branch"
    | Error e ->
      Expect.stringContains e "merged or archived" "and the refusal says why"

    let! still = isMerged branchId
    Expect.isTrue still "the branch stays merged"
    do! cleanupBranch branchId
  }


/// `SCM.PackageOps.liveBindingFor` is THE branch-aware read of a live binding: the chain overlay first,
/// then main's projection. A direct read of `locations` answers about MAIN from a branch, plausibly and
/// wrongly, which is the recurring bug class here.
let liveBindingReadsTheBranchThenMain =
  testTask
    "liveBindingFor answers the branch's binding, and main's where the branch is silent" {
    let! branchId = freshBranch "test-branch-live-binding" "live-binding-proof"

    let! ops = parsePackageOps (namedSource "LiveBind" 42)
    let! _ = Branches.storeDeltaOps branchId ops
    let branchHash =
      ops
      |> List.tryPick (fun op ->
        match op with
        | PT.PackageOp.AddFn f -> (let (PT.Hash h) = f.hash in Some h)
        | _ -> None)
      |> Option.defaultValue ""

    let hashOrNone
      (branch : string)
      (owner : string)
      (modules : string)
      (name : string)
      =
      $"(match Darklang.SCM.PackageOps.liveBindingFor {branch} "
      + $"(Darklang.LanguageTools.ProgramTypes.PackageLocation {{ owner = \"{owner}\"; "
      + $"modules = [ \"{modules}\" ]; name = \"{name}\" }}) with "
      + "| Some b -> b.hash | None -> \"none\")"
    let main = darkBranch PT.BranchId.Main

    let! answers =
      darkStringList (
        "[ "
        + hashOrNone (darkBranch branchId) "Darklang" "LiveBind" "foo"
        + ", "
        + hashOrNone main "Darklang" "LiveBind" "foo"
        + ", "
        + hashOrNone (darkBranch branchId) "Darklang" "Stdlib.List" "map"
        + ", "
        + hashOrNone main "Darklang" "Stdlib.List" "map"
        + " ]"
      )

    match answers with
    | [ onBranch; onMain; mainNameFromBranch; mainNameFromMain ] ->
      Expect.equal onBranch branchHash "on the branch, the branch's hash"
      Expect.equal onMain "none" "main does not have the branch's name"
      Expect.notEqual mainNameFromMain "none" "a name main has answers from main"
      Expect.equal
        mainNameFromBranch
        mainNameFromMain
        "and answers the same from the branch"
    | other -> failtest $"expected four answers, got {other}"

    do! cleanupBranch branchId
  }


/// The other half of `mainRetakesABranchsOp`: storing on a branch an op main already runs must not tag it.
/// Every draft query excludes tagged ids, so a tag on main's own op hid it from `status` and `commit`.
let aBranchNeverTagsWhatMainRuns =
  testTask "storing an op main already runs on a branch leaves it untagged" {
    let! branchId = freshBranch "test-branch-no-tag-on-main" "no-tag-proof"

    let! ops = parsePackageOps (namedSource "NoTagOnMain" 42)
    let ids = ops |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))
    let! _ = LibDB.Inserts.insertAndApplyOps ops
    let! _ = Branches.storeDeltaOps branchId ops

    let! tagged = ownTagCount branchId
    Expect.equal tagged 0L "nothing main runs was tagged"
    let! draft = Queries.getDraftOps ()
    let draftIds =
      draft |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))
    for id in ids do
      Expect.contains draftIds id "and main's draft still lists its own op"

    // A fresh op on the same branch is tagged as before.
    let! fresh = parsePackageOps (namedSource "NoTagOnMainFresh" 43)
    let! stored = Branches.storeDeltaOps branchId fresh
    Expect.equal stored (int64 (List.length fresh)) "fresh ops are stored"
    let! taggedNow = ownTagCount branchId
    Expect.equal taggedNow (int64 (List.length fresh)) "and tagged"

    do!
      execSqlP
        "DELETE FROM locations WHERE op_id IN (SELECT value FROM json_each(@ids))"
        [ "ids", Sql.string (System.Text.Json.JsonSerializer.Serialize ids) ]
    do!
      execSqlP
        "DELETE FROM package_ops WHERE id IN (SELECT value FROM json_each(@ids))"
        [ "ids", Sql.string (System.Text.Json.JsonSerializer.Serialize ids) ]
    LibDB.Caching.invalidateAll ()
    do! cleanupBranch branchId
  }


/// Merging a branch into a non-main parent retags its ops onto the parent. Its name BASES have to move
/// too: a name without a base can never conflict again, so a grandparent that moves one of the child's
/// names would be invisible at the parent's merge.
let retagMovesTheBasesToo =
  testTask
    "retagging a child's frontier onto its parent carries the child's name bases" {
    let parent = testBranch "test-branch-bases-parent"
    let child = testBranch "test-branch-bases-child"
    do! cleanupBranch child
    do! cleanupBranch parent
    do! Branches.createBranch parent "bases-parent" PT.BranchId.Main
    do! Branches.createBranch child "bases-child" parent

    let! ops = parsePackageOps (namedSource "BasesMove" 42)
    let! _ = Branches.storeDeltaOps child ops
    do!
      execSqlP
        "INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
         VALUES (@b, 'Darklang', 'BasesMove', 'foo', 'the-fork-hash')"
        [ "b", Sql.string (string child) ]

    let! _ = retagFrontierToParent child parent

    let baseNames (b : PT.BranchId) =
      Sql.query "SELECT name FROM branch_name_bases WHERE branch_id = @b"
      |> Sql.parameters [ "b", Sql.string (string b) ]
      |> Sql.executeAsync (fun read -> read.string "name")
    let! onParent = baseNames parent
    let! onChild = baseNames child
    Expect.equal
      onParent
      [ "foo" ]
      "the parent now holds the child's base for the name"
    Expect.isEmpty onChild "and the child, finished, holds none"

    do! cleanupBranch child
    do! cleanupBranch parent
  }


/// `lookupRef` says WHY it missed, because only one kind of miss should start a branch. A bare `None`
/// stands for a foreign uuid, an ambiguous prefix and an unknown name alike, which turns
/// `--branch <a peer's id>` into a new branch named after that id.
let refLookupSaysWhyItMissed =
  testTask
    "lookupRef distinguishes a foreign id, an ambiguous prefix and an unknown name" {
    let one = PT.BranchId.Id(System.Guid "aaaaaaaa-0000-4000-8000-000000000001")
    let two = PT.BranchId.Id(System.Guid "aaaaaaaa-0000-4000-8000-000000000002")
    do! cleanupBranch one
    do! cleanupBranch two
    do! Branches.createBranch one "ref-one" PT.BranchId.Main
    do! Branches.createBranch two "ref-two" PT.BranchId.Main

    let! byName = Branches.lookupRef "ref-one"
    Expect.equal byName (Branches.Found one) "a live name is found"
    let! byId = Branches.lookupRef (string one)
    Expect.equal byId (Branches.Found one) "a full id is found"
    let! byPrefix = Branches.lookupRef "aaaaaaaa"
    Expect.equal
      byPrefix
      (Branches.Ambiguous "aaaaaaaa")
      "a prefix two branches share is ambiguous"
    let foreign = PT.BranchId.Id(System.Guid.NewGuid())
    let! unknown = Branches.lookupRef (string foreign)
    Expect.equal
      unknown
      (Branches.UnknownId foreign)
      "a full id nobody has is a foreign id, not a name"
    let! noSuch = Branches.lookupRef "no-such-branch-here"
    Expect.equal
      noSuch
      (Branches.NoSuchName "no-such-branch-here")
      "and only a name nobody has is a name"

    do! cleanupBranch one
    do! cleanupBranch two
  }


/// The overlay pairs an item with its SetName by the hash the item carries, not by adjacency. Chain ops
/// are ordered by origin_ts across authors, so after a bundle import two authors' ops interleave, and
/// "the Add before this SetName" is as likely to be the other author's: your name, their body.
let overlayPairsByHashNotAdjacency =
  testTask "two authors' interleaved ops resolve each name to its own body" {
    let! opsA = parsePackageOps (namedSource "InterleaveA" 42)
    let! opsB = parsePackageOps (namedSource "InterleaveB" 99)
    let adds ops =
      ops
      |> List.filter (fun op ->
        match op with
        | PT.PackageOp.AddFn _ -> true
        | _ -> false)
    let sets ops =
      ops
      |> List.filter (fun op ->
        match op with
        | PT.PackageOp.SetName _ -> true
        | _ -> false)
    // [addA; addB; setA; setB]: adjacency pairs setA with addB.
    let interleaved = adds opsA @ adds opsB @ sets opsA @ sets opsB
    let overlay = PM.withExtraOps pmPT interleaved

    let bodyOf (m : string) =
      task {
        let! found = overlay.findFn (fooLocIn m) |> Ply.toTask
        let hash = Expect.wantSome found $"{m}.foo resolves"
        let! fn = overlay.getFn hash |> Ply.toTask
        let fn = Expect.wantSome fn $"{m}.foo's item is in the overlay"
        return! runFooBody overlay [ PT.PackageOp.AddFn fn ]
      }
    let! a = bodyOf "InterleaveA"
    let! b = bodyOf "InterleaveB"
    Expect.equal a (RT.DInt64 42L) "A's name runs A's body"
    Expect.equal b (RT.DInt64 99L) "and B's runs B's"
  }


/// A bundle op this build cannot decode is stored raw and inert beside the ones it can, the way main
/// sync stores such ops, rather than refusing the whole bundle. The next build that reads it applies it.
let anUndecodableBundleOpIsKeptNotRefused =
  testTask
    "a branch bundle with one unreadable op stores it inert and keeps the rest" {
    let! branchId = freshBranch "test-branch-raw-op" "raw-proof"

    let! ops = parsePackageOps (namedSource "RawOp" 42)
    let! stored =
      Branches.storeDeltaOpsStamped
        branchId
        (ops |> List.map (fun op -> (op, "2026-01-01T00:00:00.000Z")))
    let alien = System.Guid.NewGuid()
    let! storedRaw =
      Branches.storeDeltaBlobsStamped
        branchId
        [ (alien, [| 0xFFuy; 0x39uy; 0x07uy |], "2026-01-01T00:00:01.000Z") ]
    Expect.equal storedRaw 1L "the raw op is stored"

    let! (rows, tags) =
      Sql.query
        "SELECT
           (SELECT count(*) FROM package_ops WHERE id = @a AND effective = 0) AS r,
           (SELECT count(*) FROM op_branches WHERE op_id = @a AND branch_id = @b) AS t"
      |> Sql.parameters
        [ "a", Sql.string (string alien); "b", Sql.string (string branchId) ]
      |> Sql.executeRowAsync (fun read -> (read.int64 "r", read.int64 "t"))
    Expect.equal (rows, tags) (1L, 1L) "inert, and on the branch"

    let! loaded = Branches.loadDeltaOps branchId
    Expect.equal
      (int64 (List.length loaded))
      stored
      "the readable ops load; the raw one is skipped, not fatal"

    do! cleanupBranch branchId
  }


/// The receiving side has no obligation to know every branch its peers have. Branch ids travel with
/// a bundle, so the branches you actually share match; the rest are none of this store's business.
let branchEventForUnknownBranchIsIgnored =
  testTask "a BranchEvent for a branch this store has never seen folds to nothing" {
    // A real id that no `branches` row carries. Since the op field became a `BranchId`, "not an id at
    // all" is no longer representable, so unknown-but-well-formed is the only case left to cover.
    let unknown = testBranch "test-branch-that-does-not-exist"
    let op =
      PT.PackageOp.BranchEvent(unknown, PT.Merged [], "2026-01-01T00:00:00.000Z")

    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]

    let! rows =
      countSql
        "SELECT COUNT(*) as n FROM branches WHERE id = @b"
        [ "b", Sql.string (string unknown) ]
    Expect.equal rows 0L "no branch was conjured up to receive the event"

    // The one op this test wrote, back out: every test in this file leaves the shared store as it
    // found it, and this one was the exception.
    let opId = LibSerialization.Hashing.Hashing.computeOpRowId op
    do! execSqlP "DELETE FROM package_ops WHERE id = @id" [ "id", Sql.uuid opId ]
  }


/// The fold marks ops applied by PREDICATE, not by id, and folding an op can change that predicate.
///
/// A merge event arriving from another machine flips its branch's frontier to effective=1 mid-fold.
/// An applied=1 sweep running afterwards marks those ops applied without anything having folded
/// them, so the branch reads `[merged]` next to a main that does not have its code. The sweep runs
/// BEFORE the fold for exactly this reason.
let foldDoesNotStrandOpsItMadeEffective =
  testTask
    "an op that makes other ops effective does not leave them applied-but-unfolded" {
    let! branchId = freshBranch "test-branch-stranded" "stranded-proof"

    let! ops = parsePackageOps (namedSource "BranchTestStranded" 77)
    let! _ = Branches.storeDeltaOps branchId ops

    let! pending = countEffective branchId 0
    Expect.isGreaterThan pending 0L "the branch's ops start effective=0"

    // The event has to arrive the way a SYNC delivers it: inserted unapplied-and-effective, then
    // folded by `applyUnappliedOps`, which puts the flip and the sweep inside ONE pass. Authoring it
    // locally folds it in its own call, and a later pass picks the branch ops up regardless.
    // The event names what the merge moved: with an empty list it would, correctly, fold nothing.
    let mergedIds = ops |> List.map LibDB.Inserts.computeOpHash
    let event =
      PT.PackageOp.BranchEvent(
        branchId,
        PT.Merged mergedIds,
        "2026-01-01T00:00:00.000Z"
      )
    let eventId = LibDB.Inserts.computeOpHash event
    let eventBlob = BS.PT.PackageOp.serialize eventId event
    do!
      execSqlP
        "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
         VALUES (@id, @blob, 0, 1, @ts)"
        [ "id", Sql.uuid eventId
          "blob", Sql.bytes eventBlob
          "ts", Sql.string "2026-01-01T00:00:00.000Z" ]

    let! _ = Seed.applyUnappliedOps ()

    // Asserted over the ids captured BEFORE the event, because the event clears the branch tags: a
    // query that looks them up by tag afterwards finds nothing and passes for the wrong reason.
    let ids = ops |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))
    let! unfolded =
      Sql.query
        $"""SELECT COUNT(*) as n FROM package_ops
            WHERE applied = 0 AND id IN ({ids |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    Expect.equal unfolded 0L "every op the event made effective was actually folded"

    let! inMain = pmPT.findFn (fooLocIn "BranchTestStranded") |> Ply.toTask
    Expect.isSome
      inMain
      "the merged branch's fn resolves on main after the event folded"

    do! cleanupBranch branchId
  }



/// Dark code outside the SCM silos must not query `locations`. It is MAIN's projection and has no
/// `branch_id`, so a read that goes straight to it answers about main while the caller stands on a
/// branch, and answers plausibly. The overlay helpers in `SCM.PackageOps` are the branch-aware way.
///
/// Checked by reading the source, because the failure is invisible at run time on a single-branch
/// store. The relay (`sync/relay/server.dark`) is exempt: a relay holds no branches, so main's
/// projection IS its answer.
let noDirectLocationsReadsOutsideTheSilos =
  testTask
    "only the SCM silos query `locations` from Dark, and each such read says it is main-scoped" {
    let root = System.IO.Path.Combine("..", "packages", "darklang")

    // `sync/relay/server.dark` is the relay's store side: it serves the public package browser and
    // its counts, which are main's by definition -- the relay has no branch to be standing on, so a
    // main-scoped read is the correct answer there rather than a drifted one.
    let isRelay (path : string) : bool =
      path.Replace("\\", "/").EndsWith "sync/relay/server.dark"
    // "darklang/scm/", not "/scm/": the CLI's command files under `cli/scm/` are not the silo.
    let isSilo (path : string) : bool =
      path.Replace("\\", "/").Contains "darklang/scm/"

    let isRead (line : string) : bool =
      let t = line.Trim()
      not (t.StartsWith "//")
      && (t.Contains "FROM locations" || t.Contains "JOIN locations")

    // Inside the silos the rule is per READ, not per file. Every read was classified: the ones that
    // are correctly main-scoped say so in a comment between the enclosing top-level `let` and the read,
    // with the token `main-scoped`. A read with no such comment is either a bug (it answers about main
    // from a branch, the class that has cost the most) or an unclassified one, and the fix for the
    // second is to read its callers, not to add the token.
    let unmarkedInSilo (path : string) : List<string> =
      let lines = System.IO.File.ReadAllLines path
      [ for i in 0 .. lines.Length - 1 do
          if isRead lines[i] then
            let mutable j = i
            let mutable marked = false
            while j >= 0 && not (lines[j].StartsWith "let ") do
              if lines[j].Contains "main-scoped" then marked <- true
              j <- j - 1
            // The doc block above the `let` counts too.
            let mutable k = j - 1
            while k >= 0 && lines[k].TrimStart().StartsWith "///" do
              if lines[k].Contains "main-scoped" then marked <- true
              k <- k - 1
            if not marked then
              let shown = path.Replace("\\", "/")
              yield $"{shown}:{i + 1}" ]

    let files =
      System.IO.Directory.GetFiles(
        root,
        "*.dark",
        System.IO.SearchOption.AllDirectories
      )
      |> Array.filter (isRelay >> not)

    let outside =
      files
      |> Array.filter (isSilo >> not)
      |> Array.filter (fun path ->
        System.IO.File.ReadAllLines path |> Array.exists isRead)
      |> Array.map (fun p -> p.Replace("\\", "/"))
      |> List.ofArray

    Expect.isEmpty
      outside
      "no Dark file outside packages/darklang/scm may read `locations` directly -- \
       it is main-only, so it answers about main while you stand on a branch. \
       Use `SCM.PackageOps.liveBindingFor` or the overlay helpers."

    let unmarked =
      files |> Array.filter isSilo |> Array.toList |> List.collect unmarkedInSilo

    Expect.isEmpty
      unmarked
      "every direct `locations` read in the SCM silos must say `main-scoped` (and why) in a comment \
       between its enclosing `let` and the read, or in that fn's doc block. Read the callers first: \
       the token is a claim, and a read that answers about main from a branch is the bug class."
  }

/// No SQL in Dark may compare a branch column against the literal `'main'`.
///
/// Dead is the good case. The bad one is a comparison that half-fires: a store carrying BOTH spellings
/// in `branches.parent_id` matches for some rows and not others, so what the query does depends on
/// which rows it happens to reach.
///
/// Comments are exempt: explaining the trap is not falling into it.
let noMainLiteralInDarkSql =
  testTask "no Dark SQL compares a branch column against the literal 'main'" {
    let root = System.IO.Path.Combine("..", "packages", "darklang")

    let offenders =
      System.IO.Directory.GetFiles(
        root,
        "*.dark",
        System.IO.SearchOption.AllDirectories
      )
      |> Array.collect (fun path ->
        let shown = path.Replace("\\", "/")

        System.IO.File.ReadAllLines path
        |> Array.mapi (fun i line -> (i + 1, line))
        |> Array.filter (fun (_, line) ->
          let t = line.Trim()
          not (t.StartsWith "//") && t.Contains "'main'")
        |> Array.map (fun (n, _) -> shown + ":" + string n))
      |> List.ofArray

    Expect.isEmpty
      offenders
      "SQL in Dark must bind main's id as a parameter, never compare against the literal 'main'. \
       `branches.parent_id` holds a UUID, so the literal matches nothing and the comparison is dead. \
       Use `SCM.Branch.mainBranchId` and pass it as a bound parameter."
  }


/// A branch ID never reaches a person. Names do.
///
/// Ids everywhere inside, a NAME wherever a human reads it. The rule breaks in strings nothing else
/// tests -- conflict candidate labels, the `BranchEvent` line in `dark show`, the `rebase` and
/// `resolve` messages -- and it reads as `rebased onto "00000000-0000-0000-0000-000000000001"`.
///
/// Textual, and a tripwire rather than a proof: it knows the spellings we actually use for a branch id,
/// and only in the two forms that reach a person (`Error` and `Dval.string`). Resolve the name with
/// `SCM.PackageOps.branchName`, which falls back to the id for a branch this store has no row for.
let branchIdsNeverReachAPerson =
  testTask "no user-facing F# string interpolates a branch id" {
    // `..` is the REPO ROOT here, the same as the `locations` test above uses for `packages/`.
    let roots =
      [ System.IO.Path.Combine("..", "backend", "src", "LibDB")
        System.IO.Path.Combine("..", "backend", "src", "Builtins") ]

    // A root that is not there means this test scanned nothing and passed, which is worse than a
    // failure. It is how the first version of it "passed" against a bug sitting in the tree.
    for root in roots do
      Expect.isTrue
        (System.IO.Directory.Exists root)
        $"{root} does not exist, so this test would scan nothing"

    let pattern =
      System.Text.RegularExpressions.Regex(
        // `.*`, not `[^"]*`: these strings quote the branch themselves (`branch \"{name}\"`), so a
        // character class that stops at a quote stops before the interpolation every time. That is
        // how the first version of this test passed against the very bug it was written for.
        @"(Error|Dval\.string) \$"".*\{(branchId|parentId|sourceId|targetId|bid)\}"
      )

    let offenders =
      roots
      |> List.collect (fun root ->
        System.IO.Directory.GetFiles(
          root,
          "*.fs",
          System.IO.SearchOption.AllDirectories
        )
        |> Array.collect (fun path ->
          System.IO.File.ReadAllLines path
          |> Array.mapi (fun i line -> (i + 1, line))
          |> Array.filter (fun (_, line) -> pattern.IsMatch line)
          |> Array.map (fun (n, _) -> $"  {path.Replace('\\', '/')}:{n}"))
        |> List.ofArray)

    Expect.isEmpty
      offenders
      "These strings put a branch ID in front of a person:\n\
       Resolve it with `SCM.PackageOps.branchName` first. A person typed a name; showing them a uuid \
       back is the boundary this branch Branches.exists to draw."
  }


/// One LOCATION can hold a fn AND a value at once, so a conflict is identified by (name, item kind).
///
/// The fold's UPDATE has to match on both. Matching on the name alone closes the other kind's conflict
/// as a side effect of overriding this one, and the conflict it closes never gets an answer: it leaves
/// `dark conflicts` while its binding is still contested.
let overrideClosesOnlyItsOwnKind =
  testTask
    "overriding a conflict on one kind leaves the other kind's conflict pending" {
    let loc : PT.PackageLocation =
      { owner = "Zz"; modules = [ "B1" ]; name = "shared" }
    let modules = String.concat "." loc.modules

    let insertConflict (id : string) (itemType : string) =
      Sql.query
        "INSERT OR REPLACE INTO conflicts
           (id, owner, modules, name, item_type, kind, candidates, auto_resolved_to, reason, status,
            origin_ts)
         VALUES (@id, @owner, @modules, @name, @itemType, 'same-name-different-hash', '[]', '', '',
                 'pending', '2026-01-01T00:00:00.000Z')"
      |> Sql.parameters
        [ "id", Sql.string id
          "owner", Sql.string loc.owner
          "modules", Sql.string modules
          "name", Sql.string loc.name
          "itemType", Sql.string itemType ]
      |> Sql.executeStatementAsync

    do! insertConflict "b1-fn" "fn"
    do! insertConflict "b1-value" "value"

    // Override the FN. The value's conflict is a different question and nobody has answered it.
    let target = PT.Reference.PackageFn(PT.Hash "b1fnhash")
    let op =
      PT.PackageOp.Decision(
        "b1-decision",
        loc,
        "taking mine",
        PT.DecisionKind.Override target
      )
    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]

    let statusOf (id : string) =
      Sql.query "SELECT status FROM conflicts WHERE id = @id"
      |> Sql.parameters [ "id", Sql.string id ]
      |> Sql.executeRowAsync (fun read -> read.string "status")

    let! fnStatus = statusOf "b1-fn"
    let! valueStatus = statusOf "b1-value"

    Expect.equal fnStatus "overridden" "the fn conflict was the one answered"
    Expect.equal
      valueStatus
      "pending"
      "the value conflict at the same name is a separate question and stays open"

    do! execSql "DELETE FROM conflicts WHERE id IN ('b1-fn', 'b1-value')"
  }


/// A merge event for a branch this store never held must update NOTHING. Op ids are
/// content-addressed, so the event's id list can name an op that exists here as local draft
/// work; without the op_branches predicate on the stamp UPDATE, that op got the merger's
/// commit_hash while the effective-flip correctly no-opped.
let unknownBranchEventStampsNothing =
  testTask
    "a merge event for a never-held branch does not stamp a same-content draft op" {
    let! ops = parsePackageOps (namedSource "StampGuard" 7)
    let! _ = LibDB.Inserts.insertAndApplyOps ops
    let opIds = ops |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))

    // The event, from a branch id this store has no row for, carrying a commit to copy.
    let ghostBranch = testBranch "neverHeldStamp"
    let event =
      PT.PackageOp.BranchEvent(
        ghostBranch,
        PT.Merged(ops |> List.map LibDB.Inserts.computeOpHash),
        "2026-01-01T00:00:00.000Z"
      )
    let eventId = LibDB.Inserts.computeOpHash event
    let eventBlob = BS.PT.PackageOp.serialize eventId event
    do!
      execSqlP
        "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts, commit_hash)
         VALUES (@id, @blob, 0, 1, @ts, 'stamp-guard-commit')"
        [ "id", Sql.uuid eventId
          "blob", Sql.bytes eventBlob
          "ts", Sql.string "2026-01-01T00:00:00.000Z" ]

    let! _ = Seed.applyUnappliedOps ()

    let! stamped =
      countSql
        $"""SELECT COUNT(*) as n FROM package_ops
            WHERE commit_hash IS NOT NULL AND id IN ({opIds |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""
        []
    Expect.equal stamped 0L "the local draft ops keep commit_hash NULL"

    do! execSql "DELETE FROM locations WHERE modules = 'StampGuard'"
    do! execSqlP "DELETE FROM package_ops WHERE id = @id" [ "id", Sql.uuid eventId ]
    do!
      execSql
        $"""DELETE FROM package_ops WHERE id IN ({opIds |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""
  }


/// The migrations path defers its refold to `growIfNeeded`, which reads effective=1 only --
/// so branch-scoped Decisions (a branch's pins, folded into `propagation_policy`) came back
/// from a schema change as nothing. `growIfNeeded` now re-runs `refoldBranchDecides` whenever
/// it folded anything; this walks the exact drop-then-grow sequence for one branch's pin.
let migrationsRefoldKeepsBranchPins =
  testTask "a branch's propagation pin survives the drop-and-grow migration sequence" {
    let branchId = testBranch "pinRefold"
    do! cleanupBranch branchId
    do! Branches.createBranch branchId "pin-refold" PT.BranchId.Main

    let pin =
      PT.PackageOp.Decision(
        "test-pin-refold",
        fooLocIn "PinRefold",
        "test",
        PT.DecisionKind.Propagation PT.PropagationPolicy.Pin
      )
    let! _ = Branches.storeDeltaOps branchId [ pin ]
    do! Branches.refoldBranchDecides ()

    let! before =
      countSql
        "SELECT COUNT(*) as n FROM propagation_policy WHERE branch_id = @b"
        [ "b", Sql.string (string branchId.Guid) ]
    Expect.equal before 1L "the pin folded into propagation_policy"

    // The migration's harm, scoped to what this test owns: the policy row gone, and one main
    // op unapplied so growIfNeeded's fold actually runs.
    do!
      execSqlP
        "DELETE FROM propagation_policy WHERE branch_id = @b"
        [ "b", Sql.string (string branchId.Guid) ]
    let! mainOps = parsePackageOps (namedSource "PinRefoldMain" 3)
    let! _ = LibDB.Inserts.insertAndApplyOps mainOps
    let mainIds =
      mainOps |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))
    do!
      execSql
        $"""UPDATE package_ops SET applied = 0 WHERE id IN ({mainIds |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""

    let! _ =
      Seed.growIfNeeded
        Seed.TrustedSeed
        (fun () -> localBuiltIns pmPT)
        pmRT
        (fun _ -> ())

    let! after =
      countSql
        "SELECT COUNT(*) as n FROM propagation_policy WHERE branch_id = @b"
        [ "b", Sql.string (string branchId.Guid) ]
    Expect.equal after 1L "the pin is back after the grow"

    do!
      execSqlP
        "DELETE FROM propagation_policy WHERE branch_id = @b"
        [ "b", Sql.string (string branchId.Guid) ]
    do! execSql "DELETE FROM locations WHERE modules = 'PinRefoldMain'"
    do!
      execSql
        $"""DELETE FROM package_ops WHERE id IN ({mainIds |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""
    do! cleanupBranch branchId
  }


let tests =
  // These mutate the process-global branch overlay AND delete from `package_ops`, either of which
  // can make a concurrent reader see the store mid-change. testSequenced, NOT testSequencedGroup:
  // the group form only sequences the tests inside it, and still runs in the parallel phase next
  // to everything else.
  testSequenced
  <| testList
    "BranchOverlay"
    [ branchResolutionOrder
      isolationFromCore
      unbindHidesACoreNameOnTheBranchOnly
      propagationPinsComeFromTheWholeChain
      branchExists
      mergeCountsWhatItFlipped
      importedOpsKeepTheirStamps
      rebuildKeepsBranchPolicy
      branchPMIsPerBranch
      branchNamesResolveButDontShadowMain
      isolationBetweenBranches
      parentHashesAgreeAcrossLanguages
      undecodableBranchOpIsSkippedNotFatal
      overlaySearchAgreesWithFindFn
      supersededBranchVersionsKeepTheirName
      branchAuthoringCountsAsHavingItems
      storeThenOverlay
      mergedBranchStaysAddressable
      concurrentCreateYieldsOneBranch
      markMergedFlipsEffective
      processOverlaySelects
      branchesOffBranches
      getWipOpsExcludesBranch
      mergeDoesNotConsumeSiblingPendingOps
      sameNameMergesConvergeToLater
      rebaseDetectsAndClearsConflicts
      perNameResolutionMineTheirs
      resolveKeepMineDoesNotRestampSharedOps
      resolveAloneRecordsANameBase
      reuseBranchIdRevives
      branchValueContentFoldIsolatesName
      branchEventMarksMerged
      branchEventForUnknownBranchIsIgnored
      foldDoesNotStrandOpsItMadeEffective
      branchTransferImportReDerivesBases
      noDirectLocationsReadsOutsideTheSilos
      noMainLiteralInDarkSql
      branchIdsNeverReachAPerson
      overrideClosesOnlyItsOwnKind
      unknownBranchEventStampsNothing
      migrationsRefoldKeepsBranchPins
      mainRetakesABranchsOp
      authoringOnAFinishedBranchRefuses
      liveBindingReadsTheBranchThenMain
      aBranchNeverTagsWhatMainRuns
      retagMovesTheBasesToo
      refLookupSaysWhyItMissed
      overlayPairsByHashNotAdjacency
      anUndecodableBundleOpIsKeptNotRefused
      takeTheirsAfterKeepMineDropsTheOverride ]
