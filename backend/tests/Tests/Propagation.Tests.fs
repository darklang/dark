/// Propagation: when an item changes, what follows it up, and what doesn't.
///
/// Editing an item gives it a new content hash, so everything referring to it now
/// refers to a version that is no longer what the name means. The cascade closes
/// that gap: it re-authors each dependent against the new hash, recursively, and
/// overridable per item.
///
/// These drive `LibDB.Propagation.propagate` against a real store, the same call the authoring path makes.
module Tests.Propagation

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PM = LibDB.PackageManager
module HS = LibDB.HashStabilization
module Package = LibParser.Package
module NR = LibParser.NameResolver
module Inserts = LibDB.Inserts
module WipRefresh = LibDB.WipRefresh
module Propagation = LibDB.Propagation

open TestUtils.TestUtils

let private pmPT = PM.pt

/// Author source into MAIN the way the CLI does: parse, stabilize SCC-aware hashes,
/// insert + fold.
let private author (source : string) : Task<List<PT.PackageOp>> =
  task {
    let builtins = localBuiltIns pmPT
    let! parsed =
      Package.parse builtins pmPT NR.OnMissing.ThrowError source |> Ply.toTask
    match parsed with
    | Ok ops ->
      let stabilized = HS.computeRealHashes ops
      let! _ = Inserts.insertAndApplyOpsAsWip stabilized
      // The same second step the authoring builtin does: re-resolve names and
      // recompute SCC-aware hashes now that the new items exist. Skipping it leaves
      // forward references unresolved, so the dependency edges the cascade reads are
      // never written.
      let! _ = WipRefresh.refresh pmPT
      return stabilized
    | Error errs ->
      return Exception.raiseInternal "propagation test parse failed" [ "errs", errs ]
  }

let private loc (m : string) (name : string) : PT.PackageLocation =
  { owner = "Darklang"; modules = [ m ]; name = name }

/// What `locations` currently binds a name to.
let private liveHash (l : PT.PackageLocation) : Task<Option<string>> =
  Sql.query
    "SELECT item_hash FROM locations
     WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL LIMIT 1"
  |> Sql.parameters
    [ "o", Sql.string l.owner
      "m", Sql.string (String.concat "." l.modules)
      "n", Sql.string l.name ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "item_hash")

/// The hash a set of authored ops BOUND to a name. Read off the SetName, not the
/// AddFn: an AddFn carries content and no name at all -- naming is a separate op,
/// which is the whole point of the model.
let private hashOfFn (ops : List<PT.PackageOp>) (name : string) : PT.Hash =
  ops
  |> List.tryPick (fun op ->
    match op with
    | PT.PackageOp.SetName(l, target, _) when l.name = name -> Some target.hash
    | _ -> None)
  |> Option.defaultWith (fun () ->
    Exception.raiseInternal "no SetName for name" [ "name", name ])

let private hashStr (h : PT.Hash) : string =
  let (PT.Hash s) = h
  s

/// Run the cascade for a source item that moved from <fromHash> to <toHash>,
/// applying the ops it produces -- i.e. exactly what the authoring path does after
/// an edit.
let private cascade
  (l : PT.PackageLocation)
  (fromHash : PT.Hash)
  (toHash : PT.Hash)
  : Task<List<string>> =
  task {
    match!
      Propagation.propagate PT.BranchId.Main l PT.ItemKind.Fn [ fromHash ] toHash
    with
    | Ok(Some(result, ops)) ->
      let! _ = Inserts.insertAndApplyPropagatedOps ops
      return result.repoints |> List.map (fun r -> r.location.name)
    | Ok None -> return []
    | Error e -> return Exception.raiseInternal "propagate errored" [ "e", e ]
  }

/// `cascade` for a source that is not a fn. The KIND is part of the question: the dependency edge
/// records what kind it points at, which is what keeps a fn and a type that happen to share a name (or
/// a hash) from being treated as the same target.
let private cascadeKind
  (kind : PT.ItemKind)
  (l : PT.PackageLocation)
  (fromHash : PT.Hash)
  (toHash : PT.Hash)
  : Task<List<string>> =
  task {
    match! Propagation.propagate PT.BranchId.Main l kind [ fromHash ] toHash with
    | Ok(Some(result, ops)) ->
      let! _ = Inserts.insertAndApplyPropagatedOps ops
      return result.repoints |> List.map (fun r -> r.location.name)
    | Ok None -> return []
    | Error e -> return Exception.raiseInternal "propagate errored" [ "e", e ]
  }

/// Remove a test module's rows so a re-run starts clean. The op log is append-only and
/// content-addressed, so every test also varies its bodies by a unique suffix.
let private cleanupFor (owner : string) (m : string) : Task<unit> =
  task {
    do!
      Sql.query "DELETE FROM locations WHERE owner = @o AND modules = @m"
      |> Sql.parameters [ "o", Sql.string owner; "m", Sql.string m ]
      |> Sql.executeStatementAsync
    do!
      Sql.query "DELETE FROM propagation_policy WHERE owner = @o AND modules = @m"
      |> Sql.parameters [ "o", Sql.string owner; "m", Sql.string m ]
      |> Sql.executeStatementAsync
  }

let private cleanup (m : string) : Task<unit> = cleanupFor "Darklang" m


let singleHop =
  testTask "a dependent repoints when its dependency moves" {
    let m = "PropTestHop"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""

    let! _ =
      author
        $"""module Darklang.{m}

let dep (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let! depBefore = liveHash (loc m "dep")
    Expect.isSome depBefore "dep is bound after authoring"

    let baseV1 = hashOfFn v1 "base'"

    let! v2 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1000L"""

    let baseV2 = hashOfFn v2 "base'"
    Expect.notEqual baseV1 baseV2 "editing the body moves the content hash"

    let! repointed = cascade (loc m "base'") baseV1 baseV2
    Expect.contains repointed "dep" "the cascade reports repointing dep"

    let! depAfter = liveHash (loc m "dep")
    Expect.notEqual depAfter depBefore "dep is now bound to a NEW version of itself"

    do! cleanup m
  }

let transitive =
  testTask "the cascade is transitive: a repoint moves its own dependents too" {
    let m = "PropTestChain"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let a (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let b (x: Int64) : Int64 = Stdlib.Int64.add ({m}.a x) 10L
let c (x: Int64) : Int64 = Stdlib.Int64.add ({m}.b x) 100L"""

    let! cBefore = liveHash (loc m "c")
    let aV1 = hashOfFn v1 "a"

    let! v2 =
      author
        $"""module Darklang.{m}

let a (x: Int64) : Int64 = Stdlib.Int64.add x 2000L"""

    let! repointed = cascade (loc m "a") aV1 (hashOfFn v2 "a")

    // b has to move because a did; c has to move because b did. Stopping at b would
    // leave c calling a version of b that nothing points at -- which is a real state
    // (that's what `dark constraints` is for), but it is not what the cascade is
    // supposed to leave behind.
    Expect.contains repointed "b" "b repoints because a moved"
    Expect.contains
      repointed
      "c"
      "c repoints because b moved -- the cascade recurses"

    let! cAfter = liveHash (loc m "c")
    Expect.notEqual cAfter cBefore "c really moved, not just reported"

    do! cleanup m
  }

let multipleDependents =
  testTask "every dependent moves, not just the first one found" {
    let m = "PropTestFan"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let shared (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let one (x: Int64) : Int64 = Stdlib.Int64.add ({m}.shared x) 10L
let two (x: Int64) : Int64 = Stdlib.Int64.add ({m}.shared x) 20L
let three (x: Int64) : Int64 = Stdlib.Int64.add ({m}.shared x) 30L"""

    let sharedV1 = hashOfFn v1 "shared"

    let! v2 =
      author
        $"""module Darklang.{m}

let shared (x: Int64) : Int64 = Stdlib.Int64.add x 3000L"""

    let! repointed = cascade (loc m "shared") sharedV1 (hashOfFn v2 "shared")

    Expect.contains repointed "one" "first dependent repoints"
    Expect.contains repointed "two" "second dependent repoints"
    Expect.contains repointed "three" "third dependent repoints"

    do! cleanup m
  }

let pinStopsIt =
  testTask "an explicit pin holds a dependent where it is" {
    let m = "PropTestPin"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let held (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L
let free (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 20L"""

    let! heldBefore = liveHash (loc m "held")
    let baseV1 = hashOfFn v1 "base'"

    // A pin on main. This is what a propagation `Decision` op folds to, and it
    // is the whole point of the policy table: the cascade is a rule the machine
    // applies TO you until you can overrule it.
    do!
      Sql.query
        // Main's id from the product's constant. Spelled '' by hand, this row was invisible to the
        // cascade -- and the cascade was ALSO asking for '', so the test passed while asserting the bug.
        "INSERT INTO propagation_policy (branch_id, owner, modules, name, policy, note, origin_ts)
         VALUES (@branch, 'Darklang', @m, 'held', 'pin', 'test', '2026-01-02T00:00:00.000Z')"
      |> Sql.parameters
        [ "m", Sql.string m; "branch", Sql.string (string PT.BranchId.Main) ]
      |> Sql.executeStatementAsync

    let! v2 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 4000L"""

    let! repointed = cascade (loc m "base'") baseV1 (hashOfFn v2 "base'")

    Expect.contains repointed "free" "the unpinned dependent follows"
    Expect.isFalse (List.contains "held" repointed) "the pinned one does not"

    let! heldAfter = liveHash (loc m "held")
    Expect.equal heldAfter heldBefore "and it really didn't move"

    do! cleanup m
  }

let crossesOwners =
  testTask
    "the cascade crosses owners, because that is a person's call and not a rule" {
    let m = "PropTestOwner"
    do! cleanup m
    do! cleanupFor "Zz" m

    let! v1 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let mine (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let baseV1 = hashOfFn v1 "base'"

    // A dependent owned by someone else entirely. The first module segment is the
    // owner, so this is a genuinely foreign caller rather than another module under
    // ours.
    let! _ =
      author
        $"""module Zz.{m}

let theirs (x: Int64) : Int64 = Stdlib.Int64.add (Darklang.{m}.base' x) 20L"""

    let! v2 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 5000L"""

    let! repointed = cascade (loc m "base'") baseV1 (hashOfFn v2 "base'")

    Expect.contains repointed "mine" "same-owner dependents follow"

    // The claim under test. `Propagation.propagate` reports the FULL candidate set
    // and infers nothing from ownership: which of them actually move is chosen at
    // commit time. Ownership is a fine default and a bad rule, so refusing here
    // would be automating a decision that belongs to a person.
    Expect.contains
      repointed
      "theirs"
      "a dependent owned by someone else is a candidate like any other"

    do! cleanupFor "Zz" m
    do! cleanup m
  }

let noChangeNoCascade =
  testTask "a source that didn't actually move produces no repoints" {
    let m = "PropTestNoop"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""

    let! _ =
      author
        $"""module Darklang.{m}

let dep (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let! depBefore = liveHash (loc m "dep")
    let baseV1 = hashOfFn v1 "base'"

    // from == to: nothing changed. A cascade here would author a new version of
    // every dependent for no reason, and each of those is four ops that live in the
    // log forever.
    let! repointed = cascade (loc m "base'") baseV1 baseV1
    Expect.isEmpty repointed "no repoints when the hash didn't move"

    let! depAfter = liveHash (loc m "dep")
    Expect.equal depAfter depBefore "and the dependent is untouched"

    do! cleanup m
  }

let mutualRecursion =
  testTask "a mutually recursive pair authors and evaluates" {
    let m = "PropTestMutual"
    do! cleanup m

    // A references B and B references A, so whichever is parsed first has a forward
    // reference. If resolution or SCC-aware hashing gets this wrong the authoring
    // fails outright, and if the hashes don't converge the pair never stops
    // re-hashing each other.
    let! _ =
      author
        $"""module Darklang.{m}

let a (x: Int64) : Int64 =
  if x <= 0L then 0L else Stdlib.Int64.add ({m}.b (Stdlib.Int64.subtract x 1L)) 1L
let b (x: Int64) : Int64 =
  if x <= 0L then 0L else Stdlib.Int64.add ({m}.a (Stdlib.Int64.subtract x 1L)) 1L"""

    let! aBound = liveHash (loc m "a")
    let! bBound = liveHash (loc m "b")
    Expect.isSome aBound "a is bound"
    Expect.isSome bBound "b is bound"
    Expect.notEqual aBound bBound "the two sides of the cycle are distinct items"

    do! cleanup m
  }

let finalVersionWins =
  testTask
    "after several edits the dependent lands on the FINAL version, never an intermediate" {
    let m = "PropTestFinal"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let r (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""
    let! _ =
      author
        $"""module Darklang.{m}

let rd (x: Int64) : Int64 = Stdlib.Int64.add ({m}.r x) 0L"""

    // Three edits with no commit in between. Each one cascades, so `rd` is
    // re-authored three times; what must hold is that it ends on the LAST version of
    // `r`, not on whichever intermediate it saw first.
    let mutable prev = hashOfFn v1 "r"
    for n in [ 10; 20; 30 ] do
      let! v =
        author
          $"""module Darklang.{m}

let r (x: Int64) : Int64 = Stdlib.Int64.add x {n}L"""
      let next = hashOfFn v "r"
      let! _ = cascade (loc m "r") prev next
      prev <- next

    let! rLive = liveHash (loc m "r")
    Expect.equal rLive (Some(hashStr prev)) "r is on its last version"

    // And the dependent points at THAT r, not at an earlier one.
    let! edges =
      Sql.query
        "SELECT DISTINCT pd.depends_on_hash AS h FROM package_dependencies pd
         JOIN locations l ON l.item_hash = pd.item_hash AND l.unlisted_at IS NULL
         WHERE l.owner = 'Darklang' AND l.modules = @m AND l.name = 'rd'
           AND pd.depends_on_name = 'r'"
      |> Sql.parameters [ "m", Sql.string m ]
      |> Sql.executeAsync (fun read -> read.string "h")
    Expect.equal edges [ hashStr prev ] "rd references only the final r"

    do! cleanup m
  }

let sharedHashesAllRepoint =
  testTask
    "identical content is ONE item at several names, and every dependent of it repoints" {
    let m = "PropTestShared"
    do! cleanup m

    // `sh1` and `sh2` have the same body, so they are the same item under two names.
    // Resolving that hash to a single location would silently drop one of the two
    // dependents from the cascade, which is a wrong answer rather than an incomplete
    // one.
    let! v1 =
      author
        $"""module Darklang.{m}

let sh1 (x: Int64) : Int64 = Stdlib.Int64.add x 77L"""
    let! _ =
      author
        $"""module Darklang.{m}

let sh2 (x: Int64) : Int64 = Stdlib.Int64.add x 77L"""

    let! h1 = liveHash (loc m "sh1")
    let! h2 = liveHash (loc m "sh2")
    Expect.equal h1 h2 "same body, same hash: one item at two names"

    let! _ =
      author
        $"""module Darklang.{m}

let d1 (x: Int64) : Int64 = Stdlib.Int64.add ({m}.sh1 x) 3L"""
    let! _ =
      author
        $"""module Darklang.{m}

let d2 (x: Int64) : Int64 = Stdlib.Int64.add ({m}.sh2 x) 4L"""

    let! d1Before = liveHash (loc m "d1")
    let! d2Before = liveHash (loc m "d2")

    let! v2 =
      author
        $"""module Darklang.{m}

let sh1 (x: Int64) : Int64 = Stdlib.Int64.add x 88L"""

    let! repointed = cascade (loc m "sh1") (hashOfFn v1 "sh1") (hashOfFn v2 "sh1")
    Expect.contains repointed "d1" "the dependent of the name we edited repoints"

    let! d1After = liveHash (loc m "d1")
    Expect.notEqual d1After d1Before "d1 really moved"

    // `d2` reached the same CONTENT, but through the name `sh2`, and `sh2` still
    // means what it meant. So it must NOT move. This is the direction that's easy to
    // get wrong: a cascade driven by hash rather than by name would drag `d2` along,
    // silently rewriting code whose dependency nobody touched.
    Expect.isFalse
      (List.contains "d2" repointed)
      "the other name's dependent does NOT repoint"
    let! d2After = liveHash (loc m "d2")
    Expect.equal d2After d2Before "d2 is untouched"
    let! sh2After = liveHash (loc m "sh2")
    Expect.equal sh2After h2 "and sh2 still means what it meant"

    do! cleanup m
  }

/// What `locations` says PUT a binding there: 'op' (you authored it), 'propagation'
/// (it followed), or 'resolution' (a forced rebind).
let private bindingSource (l : PT.PackageLocation) : Task<Option<string>> =
  Sql.query
    "SELECT source FROM locations
     WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL LIMIT 1"
  |> Sql.parameters
    [ "o", Sql.string l.owner
      "m", Sql.string (String.concat "." l.modules)
      "n", Sql.string l.name ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "source")


let repointIsMarkedAsFollowed =
  testTask "a repointed binding records that it followed, not that you authored it" {
    let m = "PropTestProv"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 71L"""

    let! _ =
      author
        $"""module Darklang.{m}

let dependent (x: Int64) : Int64 = Darklang.{m}.base' x"""

    let! authoredSource = bindingSource (loc m "dependent")
    Expect.equal authoredSource (Some "op") "authoring records itself as authoring"

    let! v2 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 72L"""

    let! repointed =
      cascade (loc m "base'") (hashOfFn v1 "base'") (hashOfFn v2 "base'")
    Expect.contains repointed "dependent" "the dependent followed"

    // The whole point: a repoint changes only the item's resolved references, so the
    // binding it writes is otherwise identical in shape to one you typed. Without
    // this column, `dark commit` cannot tell you which entries you edited and which
    // followed.
    let! followedSource = bindingSource (loc m "dependent")
    Expect.equal
      followedSource
      (Some "propagation")
      "the repoint records that it followed"

    // The item you actually edited is NOT marked as having followed.
    let! editedSource = bindingSource (loc m "base'")
    Expect.equal editedSource (Some "op") "the edited item is still yours"

    do! cleanup m
  }


/// Regression: propagation running a SECOND time over an edit that already propagated must
/// author nothing.
///
/// `dark commit` propagates before it commits, so an edit made interactively -- which already
/// propagated at authoring time -- gets a second pass. The dependent has already followed, so it
/// stabilizes to the hash it already holds, and the op that used to be emitted for it was
/// `SetName(dependent, X, previous = X)`: a name rebound to what it already holds.
///
/// That op is not merely redundant. It is a SECOND naming of one name in a single draft, so
/// `Draft.collapse` keeps it at commit and drops the FIRST -- the binding the fold actually
/// recorded -- and dropping that relists the pre-edit version. Committing a propagated edit
/// reverted its callers, and `dark eval` on the caller returned the old answer.
let secondPassIsSilent =
  testTask "propagating an already-propagated edit authors nothing" {
    let m = "PropTestSecondPass"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""

    let! _ =
      author
        $"""module Darklang.{m}

let dep (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let! v2 =
      author
        $"""module Darklang.{m}

let base' (x: Int64) : Int64 = Stdlib.Int64.add x 7000L"""

    let baseV1 = hashOfFn v1 "base'"
    let baseV2 = hashOfFn v2 "base'"

    let! repointed = cascade (loc m "base'") baseV1 baseV2
    Expect.contains repointed "dep" "the first pass repoints dep"

    let! depAfterFirst = liveHash (loc m "dep")

    // The second pass, over the same edit. This is what `dark commit` does.
    let! (secondPass :
      Result<Option<Propagation.PropagationResult * List<PT.PackageOp>>, string>) =
      Propagation.propagate
        PT.BranchId.Main
        (loc m "base'")
        PT.ItemKind.Fn
        [ baseV1 ]
        baseV2

    match secondPass with
    | Ok None -> ()
    | Ok(Some(_, ops)) ->
      Expect.equal
        ops
        []
        "a dependent that has already followed produces no ops the second time"
    | Error e -> Exception.raiseInternal "propagate errored" [ "e", e ]

    let! depAfterSecond = liveHash (loc m "dep")
    Expect.equal
      depAfterSecond
      depAfterFirst
      "dep is still on the version it followed to"

    do! cleanup m
  }


/// A TYPE moving repoints the fns that use it.
///
/// Every other test in this file moves a FN. Propagation is kind-specific throughout -- the affected
/// item is transformed by `transformType` / `transformFn` / `transformValue`, and the dependency edge
/// records the kind it points at -- and none of that was covered for types or values. `dark type --help`
/// promises this in so many words: "When updating an existing type, dependents are automatically
/// updated to use the new version."
let typeMovesItsUsers =
  testTask "a fn that uses a type repoints when the type moves" {
    let m = "PropTestType"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

type Rec = {{ a: Int64 }}"""

    let! _ =
      author
        $"""module Darklang.{m}

let mk (): {m}.Rec = {m}.Rec {{ a = 1L }}"""

    let! mkBefore = liveHash (loc m "mk")
    Expect.isSome mkBefore "mk is bound after authoring"

    let! v2 =
      author
        $"""module Darklang.{m}

type Rec = {{ a: Int64; b: Int64 }}"""

    let recV1 = hashOfFn v1 "Rec"
    let recV2 = hashOfFn v2 "Rec"
    Expect.notEqual recV1 recV2 "editing the type moves its content hash"

    let! repointed = cascadeKind PT.ItemKind.Type (loc m "Rec") recV1 recV2
    Expect.contains
      repointed
      "mk"
      "the cascade reports repointing the fn that uses the type"

    let! mkAfter = liveHash (loc m "mk")
    Expect.notEqual mkAfter mkBefore "mk is now bound to a NEW version of itself"

    do! cleanup m
  }


/// A VALUE moving repoints the fns that read it.
let valueMovesItsReaders =
  testTask "a fn that reads a value repoints when the value moves" {
    let m = "PropTestValue"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

val basis = 5L"""

    let! _ =
      author
        $"""module Darklang.{m}

let reads (): Int64 = Stdlib.Int64.add {m}.basis 10L"""

    let! readsBefore = liveHash (loc m "reads")
    Expect.isSome readsBefore "the reader is bound after authoring"

    let! v2 =
      author
        $"""module Darklang.{m}

val basis = 500L"""

    let baseV1 = hashOfFn v1 "basis"
    let baseV2 = hashOfFn v2 "basis"
    Expect.notEqual baseV1 baseV2 "editing the value moves its content hash"

    let! repointed = cascadeKind PT.ItemKind.Value (loc m "basis") baseV1 baseV2
    Expect.contains
      repointed
      "reads"
      "the cascade reports repointing the fn that reads it"

    let! readsAfter = liveHash (loc m "reads")
    Expect.notEqual
      readsAfter
      readsBefore
      "the reader is bound to a NEW version of itself"

    do! cleanup m
  }


let tests =
  // These author into the shared main store and assert on `locations`, and other
  // tests re-fold that projection. A reader caught mid-rewrite sees a name that
  // plainly exists as missing. testSequenced, NOT testSequencedGroup. The group form
  // only stops the tests INSIDE it from running alongside each other; the group
  // still runs in the parallel phase next to everything else, which is where the
  // actual hazard is. testSequenced is what moves them to the phase that runs alone.
  testSequenced
  <| testList
    "Propagation"
    [ singleHop
      transitive
      multipleDependents
      pinStopsIt
      crossesOwners
      noChangeNoCascade
      mutualRecursion
      finalVersionWins
      sharedHashesAllRepoint
      repointIsMarkedAsFollowed
      secondPassIsSilent
      typeMovesItsUsers
      valueMovesItsReaders ]
