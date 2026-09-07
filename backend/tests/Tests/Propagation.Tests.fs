/// Propagation: when an item changes, what follows it up, and what doesn't.
///
/// Editing an item gives it a new content hash, so everything referring to it now
/// refers to a version that is no longer what the name means. The cascade closes
/// that gap: it re-authors each dependent against the new hash, recursively, and it is
/// overridable per item.
///
/// These drive `LibDB.Propagation.propagate` against a real store: the same call the
/// authoring path makes.
module Tests.Propagation

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module Inserts = LibDB.Inserts
module Propagation = LibDB.Propagation

open TestUtils.TestUtils

/// Every fixture here authors a fresh module of `m`; only the declarations differ.
let private authorIn (m : string) (decls : string) : Task<List<PT.PackageOp>> =
  authorIntoMain $"module Darklang.{m}\n\n{decls}"

let private loc (m : string) (name : string) : PT.PackageLocation =
  { owner = "Darklang"; modules = [ m ]; name = name }

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

/// Remove a test module's rows so a re-run starts clean. This clears the PROJECTIONS only: the op log
/// is append-only and content-addressed, so re-authoring an identical body re-binds the hash that is
/// already there rather than making a new version. That is why each test's constants are its own.
let private cleanupFor (owner : string) (m : string) : Task<unit> =
  task {
    do!
      execSqlP
        "DELETE FROM locations WHERE owner = @o AND modules = @m"
        [ "o", Sql.string owner; "m", Sql.string m ]
    do!
      execSqlP
        "DELETE FROM propagation_policy WHERE owner = @o AND modules = @m"
        [ "o", Sql.string owner; "m", Sql.string m ]
  }

let private cleanup (m : string) : Task<unit> = cleanupFor "Darklang" m


let singleHop =
  testTask "a dependent repoints when its dependency moves" {
    let m = "PropTestHop"
    do! cleanup m

    let! v1 = authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""

    let! _ =
      authorIn
        m
        $"""let dep (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let! depBefore = liveBoundHash (loc m "dep")
    Expect.isSome depBefore "dep is bound after authoring"

    let baseV1 = hashBoundTo v1 "base'"

    let! v2 =
      authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1000L"""

    let baseV2 = hashBoundTo v2 "base'"
    Expect.notEqual baseV1 baseV2 "editing the body moves the content hash"

    let! repointed = cascade (loc m "base'") baseV1 baseV2
    Expect.contains repointed "dep" "the cascade reports repointing dep"

    let! depAfter = liveBoundHash (loc m "dep")
    Expect.notEqual depAfter depBefore "dep is now bound to a NEW version of itself"

    do! cleanup m
  }

let transitive =
  testTask "the cascade is transitive: a repoint moves its own dependents too" {
    let m = "PropTestChain"
    do! cleanup m

    let! v1 =
      authorIn
        m
        $"""let a (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let b (x: Int64) : Int64 = Stdlib.Int64.add ({m}.a x) 10L
let c (x: Int64) : Int64 = Stdlib.Int64.add ({m}.b x) 100L"""

    let! cBefore = liveBoundHash (loc m "c")
    let aV1 = hashBoundTo v1 "a"

    let! v2 = authorIn m """let a (x: Int64) : Int64 = Stdlib.Int64.add x 2000L"""

    let! repointed = cascade (loc m "a") aV1 (hashBoundTo v2 "a")

    // b has to move because a did; c has to move because b did. Stopping at b would
    // leave c calling a version of b that nothing points at -- which is a real state
    // (that's what `dark constraints` is for), but it is not what the cascade is
    // supposed to leave behind.
    Expect.contains repointed "b" "b repoints because a moved"
    Expect.contains
      repointed
      "c"
      "c repoints because b moved -- the cascade recurses"

    let! cAfter = liveBoundHash (loc m "c")
    Expect.notEqual cAfter cBefore "c really moved, not just reported"

    do! cleanup m
  }

let multipleDependents =
  testTask "every dependent moves, not just the first one found" {
    let m = "PropTestFan"
    do! cleanup m

    let! v1 =
      authorIn
        m
        $"""let shared (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let one (x: Int64) : Int64 = Stdlib.Int64.add ({m}.shared x) 10L
let two (x: Int64) : Int64 = Stdlib.Int64.add ({m}.shared x) 20L
let three (x: Int64) : Int64 = Stdlib.Int64.add ({m}.shared x) 30L"""

    let sharedV1 = hashBoundTo v1 "shared"

    let! v2 =
      authorIn m """let shared (x: Int64) : Int64 = Stdlib.Int64.add x 3000L"""

    let! repointed = cascade (loc m "shared") sharedV1 (hashBoundTo v2 "shared")

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
      authorIn
        m
        $"""let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let held (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L
let free (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 20L"""

    let! heldBefore = liveBoundHash (loc m "held")
    let baseV1 = hashBoundTo v1 "base'"

    // A pin on main. This is what a propagation `Decision` op folds to, and it
    // is the whole point of the policy table: the cascade is a rule the machine
    // applies TO you until you can overrule it.
    do!
      execSqlP
        // Main's id from the product's constant, never spelled by hand: the cascade looks the
        // row up by that same id, so a hand-typed one would be a row nothing can find and a
        // test that passes while asserting nothing.
        "INSERT INTO propagation_policy (branch_id, owner, modules, name, policy, note, origin_ts)
         VALUES (@branch, 'Darklang', @m, 'held', 'pin', 'test', '2026-01-02T00:00:00.000Z')"
        [ "m", Sql.string m; "branch", Sql.string (string PT.BranchId.Main) ]

    let! v2 =
      authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 4000L"""

    let! repointed = cascade (loc m "base'") baseV1 (hashBoundTo v2 "base'")

    Expect.contains repointed "free" "the unpinned dependent follows"
    Expect.isFalse (List.contains "held" repointed) "the pinned one does not"

    let! heldAfter = liveBoundHash (loc m "held")
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
      authorIn
        m
        $"""let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L
let mine (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let baseV1 = hashBoundTo v1 "base'"

    // A dependent owned by someone else entirely. The first module segment is the
    // owner, so this is a genuinely foreign caller rather than another module under
    // ours.
    let! _ =
      authorIntoMain
        $"""module Zz.{m}

let theirs (x: Int64) : Int64 = Stdlib.Int64.add (Darklang.{m}.base' x) 20L"""

    let! v2 =
      authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 5000L"""

    let! repointed = cascade (loc m "base'") baseV1 (hashBoundTo v2 "base'")

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

    let! v1 = authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""

    let! _ =
      authorIn
        m
        $"""let dep (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let! depBefore = liveBoundHash (loc m "dep")
    let baseV1 = hashBoundTo v1 "base'"

    // from == to: nothing changed. A cascade here would author a new version of
    // every dependent for no reason, and each of those is four ops that live in the
    // log forever.
    let! repointed = cascade (loc m "base'") baseV1 baseV1
    Expect.isEmpty repointed "no repoints when the hash didn't move"

    let! depAfter = liveBoundHash (loc m "dep")
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
      authorIn
        m
        $"""let a (x: Int64) : Int64 =
  if x <= 0L then 0L else Stdlib.Int64.add ({m}.b (Stdlib.Int64.subtract x 1L)) 1L
let b (x: Int64) : Int64 =
  if x <= 0L then 0L else Stdlib.Int64.add ({m}.a (Stdlib.Int64.subtract x 1L)) 1L"""

    let! aBound = liveBoundHash (loc m "a")
    let! bBound = liveBoundHash (loc m "b")
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

    let! v1 = authorIn m """let r (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""
    let! _ =
      authorIn m $"""let rd (x: Int64) : Int64 = Stdlib.Int64.add ({m}.r x) 0L"""

    // Three edits with no commit in between. Each one cascades, so `rd` is
    // re-authored three times; what must hold is that it ends on the LAST version of
    // `r`, not on whichever intermediate it saw first.
    let mutable prev = hashBoundTo v1 "r"
    for n in [ 10; 20; 30 ] do
      let! v = authorIn m $"""let r (x: Int64) : Int64 = Stdlib.Int64.add x {n}L"""
      let next = hashBoundTo v "r"
      let! _ = cascade (loc m "r") prev next
      prev <- next

    let! rLive = liveBoundHash (loc m "r")
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
    let! v1 = authorIn m """let sh1 (x: Int64) : Int64 = Stdlib.Int64.add x 77L"""
    let! _ = authorIn m """let sh2 (x: Int64) : Int64 = Stdlib.Int64.add x 77L"""

    let! h1 = liveBoundHash (loc m "sh1")
    let! h2 = liveBoundHash (loc m "sh2")
    Expect.equal h1 h2 "same body, same hash: one item at two names"

    let! _ =
      authorIn m $"""let d1 (x: Int64) : Int64 = Stdlib.Int64.add ({m}.sh1 x) 3L"""
    let! _ =
      authorIn m $"""let d2 (x: Int64) : Int64 = Stdlib.Int64.add ({m}.sh2 x) 4L"""

    let! d1Before = liveBoundHash (loc m "d1")
    let! d2Before = liveBoundHash (loc m "d2")

    let! v2 = authorIn m """let sh1 (x: Int64) : Int64 = Stdlib.Int64.add x 88L"""

    let! repointed =
      cascade (loc m "sh1") (hashBoundTo v1 "sh1") (hashBoundTo v2 "sh1")
    Expect.contains repointed "d1" "the dependent of the name we edited repoints"

    let! d1After = liveBoundHash (loc m "d1")
    Expect.notEqual d1After d1Before "d1 really moved"

    // `d2` reached the same CONTENT, but through the name `sh2`, and `sh2` still
    // means what it meant. So it must NOT move. This is the direction that's easy to
    // get wrong: a cascade driven by hash rather than by name would drag `d2` along,
    // silently rewriting code whose dependency nobody touched.
    Expect.isFalse
      (List.contains "d2" repointed)
      "the other name's dependent does NOT repoint"
    let! d2After = liveBoundHash (loc m "d2")
    Expect.equal d2After d2Before "d2 is untouched"
    let! sh2After = liveBoundHash (loc m "sh2")
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

    let! v1 = authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 71L"""

    let! _ =
      authorIn m $"""let dependent (x: Int64) : Int64 = Darklang.{m}.base' x"""

    let! authoredSource = bindingSource (loc m "dependent")
    Expect.equal authoredSource (Some "op") "authoring records itself as authoring"

    let! v2 = authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 72L"""

    let! repointed =
      cascade (loc m "base'") (hashBoundTo v1 "base'") (hashBoundTo v2 "base'")
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


/// Propagation running a SECOND time over an edit that already propagated must author
/// nothing.
///
/// A repeat op is not merely redundant. It is a SECOND naming of one name in a single draft,
/// so `Draft.collapse` keeps it at commit and drops the FIRST -- the binding the fold actually
/// recorded -- and dropping that relists the pre-edit version. The visible cost is a commit
/// that reverts its own callers: `dark eval` on the caller answers with the pre-edit body.
let secondPassIsSilent =
  testTask "propagating an already-propagated edit authors nothing" {
    let m = "PropTestSecondPass"
    do! cleanup m

    let! v1 = authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 1L"""

    let! _ =
      authorIn
        m
        $"""let dep (x: Int64) : Int64 = Stdlib.Int64.add ({m}.base' x) 10L"""

    let! v2 =
      authorIn m """let base' (x: Int64) : Int64 = Stdlib.Int64.add x 7000L"""

    let baseV1 = hashBoundTo v1 "base'"
    let baseV2 = hashBoundTo v2 "base'"

    let! repointed = cascade (loc m "base'") baseV1 baseV2
    Expect.contains repointed "dep" "the first pass repoints dep"

    let! depAfterFirst = liveBoundHash (loc m "dep")

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

    let! depAfterSecond = liveBoundHash (loc m "dep")
    Expect.equal
      depAfterSecond
      depAfterFirst
      "dep is still on the version it followed to"

    do! cleanup m
  }


/// A TYPE moving repoints the fns that use it.
///
/// Every other test in this file moves a FN, and propagation is kind-specific throughout: the
/// affected item is transformed by `transformType` / `transformFn` / `transformValue`, and the
/// dependency edge records the kind it points at, so a fn's coverage says nothing about a type's.
/// `dark type --help` promises this in so many words: "When updating an existing type, dependents
/// are automatically updated to use the new version."
let typeMovesItsUsers =
  testTask "a fn that uses a type repoints when the type moves" {
    let m = "PropTestType"
    do! cleanup m

    let! v1 = authorIn m $"""type Rec = {{ a: Int64 }}"""

    let! _ = authorIn m $"""let mk (): {m}.Rec = {m}.Rec {{ a = 1L }}"""

    let! mkBefore = liveBoundHash (loc m "mk")
    Expect.isSome mkBefore "mk is bound after authoring"

    let! v2 = authorIn m $"""type Rec = {{ a: Int64; b: Int64 }}"""

    let recV1 = hashBoundTo v1 "Rec"
    let recV2 = hashBoundTo v2 "Rec"
    Expect.notEqual recV1 recV2 "editing the type moves its content hash"

    let! repointed = cascadeKind PT.ItemKind.Type (loc m "Rec") recV1 recV2
    Expect.contains
      repointed
      "mk"
      "the cascade reports repointing the fn that uses the type"

    let! mkAfter = liveBoundHash (loc m "mk")
    Expect.notEqual mkAfter mkBefore "mk is now bound to a NEW version of itself"

    do! cleanup m
  }


/// A VALUE moving repoints the fns that read it.
let valueMovesItsReaders =
  testTask "a fn that reads a value repoints when the value moves" {
    let m = "PropTestValue"
    do! cleanup m

    let! v1 = authorIn m """val basis = 5L"""

    let! _ = authorIn m $"""let reads (): Int64 = Stdlib.Int64.add {m}.basis 10L"""

    let! readsBefore = liveBoundHash (loc m "reads")
    Expect.isSome readsBefore "the reader is bound after authoring"

    let! v2 = authorIn m """val basis = 500L"""

    let baseV1 = hashBoundTo v1 "basis"
    let baseV2 = hashBoundTo v2 "basis"
    Expect.notEqual baseV1 baseV2 "editing the value moves its content hash"

    let! repointed = cascadeKind PT.ItemKind.Value (loc m "basis") baseV1 baseV2
    Expect.contains
      repointed
      "reads"
      "the cascade reports repointing the fn that reads it"

    let! readsAfter = liveBoundHash (loc m "reads")
    Expect.notEqual
      readsAfter
      readsBefore
      "the reader is bound to a NEW version of itself"

    do! cleanup m
  }


/// A SetName binds its own name and no other.
///
/// There is no rename op. Naming a hash somewhere new must therefore leave every other name for
/// that hash alone -- unlisting them would take out a colleague's name because you renamed yours.
/// A rename that does want to retire the old name is two ops, the second naming the OLD location.
let private namingElsewhereLeavesTheOldNameBound =
  testTask "a SetName at a new name leaves the old name bound to the same hash" {
    let m = "RenameKeep"
    do! cleanupFor "Darklang" m
    let! ops = authorIn m "let old (x: Int64) : Int64 = x"
    let h = hashBoundTo ops "old"
    let! _ =
      Inserts.insertAndApplyOpsAsWip
        [ PT.PackageOp.SetName(loc m "new", PT.PackageFn h, None) ]
    let! atNew = liveBoundHash (loc m "new")
    let! atOld = liveBoundHash (loc m "old")
    Expect.equal atNew (Some(hashStr h)) "the new name binds the hash"
    Expect.equal atOld (Some(hashStr h)) "and the old name still does"
    do! cleanupFor "Darklang" m
  }


/// Two names, one body, one caller each -- and the callers are the same content too, so the same
/// op, which the log already holds and never folds a second time.
///
/// Dependency edges therefore have to accumulate per hash rather than being written once when an
/// Add is first folded: a parse whose Add is already in the log still has to record the names it
/// reached its callees through, or `deps usedby <the second name>` answers that nobody calls it.
let private callersFoundByEitherNameOfOneBody =
  testTask "a caller of either name of one body is found by either name" {
    let m1 = "TwinOne"
    let m2 = "TwinTwo"
    do! cleanupFor "Darklang" m1
    do! cleanupFor "Darklang" m2
    let! _ = authorIn m1 "let twin (x: Int64) : Int64 = Stdlib.Int64.add x 4001L"
    let! _ = authorIn m2 "let twin (x: Int64) : Int64 = Stdlib.Int64.add x 4001L"
    let! _ =
      authorIn
        m1
        "let caller (x: Int64) : Int64 = Stdlib.Int64.add (Darklang.TwinOne.twin x) 10L"
    let! _ =
      authorIn
        m2
        "let caller (x: Int64) : Int64 = Stdlib.Int64.add (Darklang.TwinTwo.twin x) 10L"

    let edgesTo (m : string) =
      Sql.query
        "SELECT count(*) AS n FROM package_dependencies
         WHERE depends_on_owner = 'Darklang' AND depends_on_modules = @m AND depends_on_name = 'twin'"
      |> Sql.parameters [ "m", Sql.string m ]
      |> Sql.executeRowAsync (fun read -> read.int64 "n")
    let! one = edgesTo m1
    let! two = edgesTo m2
    Expect.isGreaterThan one 0L "the first name's twin has a caller edge"
    Expect.isGreaterThan
      two
      0L
      "and so does the second name's, though its caller is the same op"

    do! cleanupFor "Darklang" m1
    do! cleanupFor "Darklang" m2
  }


let tests =
  // These author into the shared main store and assert on `locations`, and other tests
  // re-fold that projection. A reader caught mid-rewrite sees a name that plainly exists
  // as missing. testSequenced, NOT testSequencedGroup: the group form only sequences the
  // tests inside it, and still runs in the parallel phase next to everything else.
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
      valueMovesItsReaders
      namingElsewhereLeavesTheOldNameBound
      callersFoundByEitherNameOfOneBody ]
