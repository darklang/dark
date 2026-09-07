/// The draft: what `dark discard` drops, and what it must not touch.
///
/// A draft op is already folded and already running -- "draft" means uncommitted, not inert. Dropping one
/// therefore has to un-do a fold, and a fold has no inverse: `locations` records the RESULT of the whole op
/// sequence, so a draft op may have overwritten a row an older op wrote. `SCM.Draft` handles that by
/// rebuilding from the ops that survive, and these are the assertions that the rebuild is faithful:
/// a committed binding is restored rather than merely left alone, and a committed op keeps its commit.
///
/// Driven through the Dark fns rather than the F# underneath them, because the Dark ones are what the
/// CLI runs; asserting against a second copy of the logic would prove nothing about `dark discard`.
module Tests.Draft

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Inserts = LibDB.Inserts
module Propagation = LibDB.Propagation

open TestUtils.TestUtils


/// Run a Dark call answering `Result<Int, String>`, as the F# result these assertions expect.
let private runDarkResult (code : string) : Task<Result<int64, string>> =
  task {
    match! evalDarkExpr code with
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DInt n ])) ->
      return Ok(int64 (RT.DarkInt.toBigInt n))
    | Ok(RT.DEnum(_, _, _, "Error", [ RT.DString e ])) -> return Error e
    | Ok other -> return Error $"unexpected result shape: {other}"
    | Error(rte, _) -> return Error $"{rte}"
  }

/// A `PackageLocation` literal, as Dark source.
let private darkLoc (m : string) (name : string) : string =
  "(Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"Darklang\"; "
  + $"modules = [\"{m}\"]; name = \"{name}\" }})"


/// Commit everything uncommitted on main, the way `dark commit` does.
let private commitAll (message : string) : Task<string> =
  Inserts.commitAllAsBaseline message

/// What `locations` currently binds a name to.
let private liveHash (m : string) (name : string) : Task<Option<string>> =
  liveBoundHash { owner = "Darklang"; modules = [ m ]; name = name }

let private draftOpCount () : Task<int64> =
  Sql.query
    "SELECT count(*) AS n FROM package_ops
     WHERE commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches)"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

let private committedOpCount () : Task<int64> =
  Sql.query
    "SELECT count(*) AS n FROM package_ops
     WHERE commit_hash IS NOT NULL AND id NOT IN (SELECT op_id FROM op_branches)"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

/// The op count a discard reported, or a failure that says why.
let private unwrap (r : Result<int64, string>) : int64 =
  match r with
  | Ok n -> n
  | Error e -> Exception.raiseInternal "discard errored" [ "e", e ]

/// The rowid of a committed op, which a REBUILD changes (it deletes and re-inserts every main op) and a
/// surgical drop does not. This is how a test tells the two paths apart from outside.
let private aCommittedRowid () : Task<int64> =
  Sql.query
    "SELECT MIN(rowid) AS r FROM package_ops
     WHERE commit_hash IS NOT NULL AND id NOT IN (SELECT op_id FROM op_branches)"
  |> Sql.executeRowAsync (fun read -> read.int64 "r")

let private cleanup (m : string) : Task<unit> =
  Sql.query "DELETE FROM locations WHERE owner = 'Darklang' AND modules = @m"
  |> Sql.parameters [ "m", Sql.string m ]
  |> Sql.executeStatementAsync


let discardsOnlyTheDraft =
  testTask "discard drops uncommitted ops and leaves committed ones alone" {
    let m = "DraftTestOnly"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet keep () : Int64 = 4001L"
    let! _ = commitAll "draft test: committed"
    let! committedBefore = committedOpCount ()
    let! keepBefore = liveHash m "keep"

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet dropMe () : Int64 = 4002L"
    let! draftBefore = draftOpCount ()
    Expect.isGreaterThan draftBefore 0L "the second author left a draft"

    let! result = runDarkResult "Darklang.SCM.Draft.discardAll ()"
    let n = unwrap result
    Expect.equal n draftBefore "discard reports every draft op it dropped"

    let! draftAfter = draftOpCount ()
    Expect.equal draftAfter 0L "no draft is left"

    let! committedAfter = committedOpCount ()
    Expect.equal
      committedAfter
      committedBefore
      "every committed op survived, with its commit"

    let! dropped = liveHash m "dropMe"
    Expect.isNone dropped "the discarded name no longer resolves"

    let! kept = liveHash m "keep"
    Expect.equal kept keepBefore "the committed name still means what it meant"

    do! cleanup m
  }

let restoresASupersededBinding =
  testTask "discard restores the binding an uncommitted edit overwrote" {
    let m = "DraftTestRestore"
    do! cleanup m

    // This is the case a per-op undo cannot handle: the draft edit REPLACED a locations row rather than
    // adding one, so dropping its op is only correct if the committed row comes back.
    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet v () : Int64 = 5001L"
    let! _ = commitAll "draft test: v1 committed"
    let! committedHash = liveHash m "v"

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet v () : Int64 = 5002L"
    let! editedHash = liveHash m "v"
    Expect.notEqual editedHash committedHash "the edit moved the name"

    let! result = runDarkResult "Darklang.SCM.Draft.discardAll ()"
    let _ = unwrap result

    let! afterHash = liveHash m "v"
    Expect.equal afterHash committedHash "the name is back on its committed version"

    do! cleanup m
  }

let emptyDraftIsANoOp =
  testTask "discarding an empty draft changes nothing" {
    let m = "DraftTestEmpty"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet e () : Int64 = 6001L"
    let! _ = commitAll "draft test: empty"
    let! committedBefore = committedOpCount ()
    let! before = liveHash m "e"

    let! result = runDarkResult "Darklang.SCM.Draft.discardAll ()"
    Expect.equal (unwrap result) 0L "nothing to drop"

    // The no-op path must not take the delete-and-reinsert route: a rebuild that runs when there is
    // nothing to remove is pure risk, and re-stamping ops is how the fold's LWW gets poisoned.
    let! committedAfter = committedOpCount ()
    Expect.equal committedAfter committedBefore "the committed log is untouched"

    let! after = liveHash m "e"
    Expect.equal after before "and the store is exactly as it was"

    do! cleanup m
  }

let dropsOnlyWhatTheDraftWrote =
  testTask "discarding a draft leaves the committed log's rows where they were" {
    let m = "DraftTestScoped"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet s () : Int64 = 7001L"
    let! _ = commitAll "draft test: scoped"
    let! rowidBefore = aCommittedRowid ()

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet d () : Int64 = 7002L"

    let! result = runDarkResult "Darklang.SCM.Draft.discardAll ()"
    Expect.isGreaterThan (unwrap result) 0L "something was dropped"

    // The point of the whole exercise. Rebuilding main to remove a draft op means every other reader sees
    // a half-empty store for as long as it takes, which is not a price a rewritable draft can pay on every
    // edit. A surgical drop touches the rows the draft wrote and nothing else, and an untouched committed
    // rowid is the evidence.
    let! rowidAfter = aCommittedRowid ()
    Expect.equal
      rowidAfter
      rowidBefore
      "the committed ops were never deleted and re-inserted"

    do! cleanup m
  }


let private loc (m : string) (name : string) : PT.PackageLocation =
  { owner = "Darklang"; modules = [ m ]; name = name }

let unstagesARepointButNotAnEdit =
  testTask "un-staging drops a repoint that followed, and refuses one you authored" {
    let m = "DraftTestUnstage"
    do! cleanup m

    let! v1 =
      authorIntoMain
        $"""module Darklang.{m}

let src (x: Int64) : Int64 = Stdlib.Int64.add x 8001L"""

    let! _ =
      authorIntoMain
        $"""module Darklang.{m}

let follower (x: Int64) : Int64 = Darklang.{m}.src x"""

    let! _ = commitAll "draft test: unstage"
    let! committedFollower = liveHash m "follower"

    // Edit the source and let the cascade repoint the follower, as authoring does.
    let! v2 =
      authorIntoMain
        $"""module Darklang.{m}

let src (x: Int64) : Int64 = Stdlib.Int64.add x 8002L"""

    let fromHash = hashBoundTo v1 "src"
    let toHash = hashBoundTo v2 "src"

    match!
      Propagation.propagate
        PT.BranchId.Main
        (loc m "src")
        PT.ItemKind.Fn
        [ fromHash ]
        toHash
    with
    | Ok(Some(_, ops)) ->
      let! _ = Inserts.insertAndApplyPropagatedOps ops
      ()
    | _ -> Exception.raiseInternal "the cascade produced nothing to un-stage" []

    let! movedFollower = liveHash m "follower"
    Expect.notEqual movedFollower committedFollower "the follower moved"

    // A pin before commit says the repoint never happened, rather than authoring a second op to put it
    // back. The staged binding goes and the committed one underneath it comes back.
    let! dropped =
      runDarkResult ("Darklang.SCM.Draft.unstageRepoint " + darkLoc m "follower")
    Expect.isGreaterThan (unwrap dropped) 0L "the staged repoint was dropped"

    let! afterFollower = liveHash m "follower"
    Expect.equal
      afterFollower
      committedFollower
      "the follower is back where it was committed"

    // The item YOU edited is not a repoint, and un-staging must refuse it -- otherwise a pin would throw
    // away someone's work while reporting that it undid a consequence.
    let! refused =
      runDarkResult ("Darklang.SCM.Draft.unstageRepoint " + darkLoc m "src")
    Expect.equal (unwrap refused) 0L "an authored edit is not something to un-stage"

    let! srcAfter = liveHash m "src"
    let (PT.Hash toStr) = toHash
    Expect.equal srcAfter (Some toStr) "and your edit is still there"

    do! cleanup m
  }


let private namingOpCount (m : string) (name : string) : Task<int64> =
  Sql.query
    "SELECT count(*) AS n FROM locations
     WHERE owner = 'Darklang' AND modules = @m AND name = @n"
  |> Sql.parameters [ "m", Sql.string m; "n", Sql.string name ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")


let collapseKeepsTheLastNamingOnly =
  testTask
    "collapsing a draft keeps one naming per name, and the store still means the same thing" {
    let m = "DraftTestCollapse"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet c () : Int64 = 9001L"
    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet c () : Int64 = 9002L"
    let! v3 = authorIntoMain $"module Darklang.{m}\n\nlet c () : Int64 = 9003L"

    let! before = liveHash m "c"
    let! draftBefore = draftOpCount ()

    let! result = runDarkResult "Darklang.SCM.Draft.collapse ()"
    Expect.isGreaterThan (unwrap result) 0L "superseded namings were dropped"

    // What the name means is the whole point: collapsing removes namings of versions that stopped being
    // what the name meant before anyone else saw them, and must not touch what it means NOW.
    let! after = liveHash m "c"
    Expect.equal after before "the name still means the last version"

    let! rows = namingOpCount m "c"
    Expect.equal rows 1L "exactly one binding row is left for the name"

    let! draftAfter = draftOpCount ()
    Expect.isLessThan draftAfter draftBefore "the draft shrank"

    // The content Adds are deliberately kept -- dropping them needs a reachability check, since a pinned
    // dependent or a surviving dependency edge may be the only thing still referring to an old version.
    // So the log still re-folds to a store with every version's content present and one name bound.
    let! stillThere = liveHash m "c"
    let (PT.Hash v3Hash) = hashBoundTo v3 "c"
    Expect.equal
      stillThere
      (Some v3Hash)
      "and it's the version the last edit authored"

    do! cleanup m
  }


/// A store that has synced holds ops it did not write. One from a peer on a different build cannot be
/// decoded here, and is kept unapplied on purpose so a later build can read it -- which puts it in the
/// main log, where every reader meets it.
///
/// Two things have to hold, and they pull against each other. Reading the log must not raise, or one
/// such op takes down every command that rewrites the draft, permanently. And the rewrite deletes the
/// main log and re-inserts what it read, so a read that merely SKIPS the op would delete a colleague's
/// committed work for being unparseable. It has to be spared, not skipped.
let keepsAnOpItCannotRead =
  testTask "a rewrite spares the ops this build cannot decode" {
    let m = "DraftTestUnreadable"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet u () : Int64 = 7001L"
    let! _ = commitAll "draft test: unreadable v1"
    let! committedHash = liveHash m "u"

    // An uncommitted edit over a committed binding, so the rewrite has real work to do.
    let! editOps = authorIntoMain $"module Darklang.{m}\n\nlet u () : Int64 = 7002L"

    // Stands in for an op off the wire that this build's deserializer rejects. Committed and
    // unapplied, which is where they actually land.
    let alienId = System.Guid.NewGuid()

    let! _ =
      Sql.query
        "INSERT INTO package_ops (id, op_blob, applied, effective, commit_hash, origin_ts)
         VALUES (@id, @blob, 0, 1, 'alien-commit', '2026-08-28T00:00:00Z')"
      |> Sql.parameters
        [ "id", Sql.string (string alienId)
          "blob", Sql.bytes [| 0xFFuy; 0x39uy; 0x07uy; 0x2Auy |] ]
      |> Sql.executeNonQueryAsync

    // A Deprecate in the draft is what forces the REBUILD route rather than the surgical one: it
    // writes a row no origin_ts traces back, so `discard` rebuilds from the surviving ops. That is
    // the route that deletes the main log, and so the only route the alien op can be lost on.
    let target = PT.Reference.PackageFn(hashBoundTo editOps "u")

    let! _ =
      Inserts.insertAndApplyOpsAsWip
        [ PT.PackageOp.Deprecate(target, PT.DeprecationKind.Harmful, "draft test") ]

    let! result = runDarkResult "Darklang.SCM.Draft.discardAll ()"
    let _ = unwrap result

    let! afterHash = liveHash m "u"
    Expect.equal afterHash committedHash "the rewrite still did its job"

    let! survivors =
      countSql
        "SELECT COUNT(*) AS n FROM package_ops WHERE id = @id"
        [ "id", Sql.string (string alienId) ]

    Expect.equal survivors 1L "the op it could not read is still in the log"

    let! _ =
      Sql.query "DELETE FROM package_ops WHERE id = @id"
      |> Sql.parameters [ "id", Sql.string (string alienId) ]
      |> Sql.executeNonQueryAsync

    do! cleanup m
  }


let discardNameDropsOneAndKeepsTheRest =
  testTask "discarding one name leaves the rest of the draft, and its content, alone" {
    let m = "DraftTestOneName"
    do! cleanup m

    let! _ =
      authorIntoMain
        $"module Darklang.{m}\n\nlet keepMe () : Int64 = 5001L\n\nlet dropMe () : Int64 = 5002L"

    let! draftBefore = draftOpCount ()
    Expect.isGreaterThan draftBefore 0L "both authors left a draft"

    let! keepBefore = liveHash m "keepMe"
    Expect.isSome keepBefore "keepMe resolves before the discard"

    let! result =
      runDarkResult (
        "Darklang.SCM.Draft.discardName \"Darklang\" [\"" + m + "\"] \"dropMe\""
      )
    let n = unwrap result
    Expect.isGreaterThan n 0L "it reports what it dropped"

    let! dropped = liveHash m "dropMe"
    Expect.isNone dropped "the named item no longer resolves"

    let! kept = liveHash m "keepMe"
    Expect.equal kept keepBefore "the OTHER draft item is untouched"

    let! draftAfter = draftOpCount ()
    Expect.isGreaterThan draftAfter 0L "the rest of the draft is still a draft"

    do! cleanup m
  }


let discardNameKeepsContentSomethingElseNeeds =
  testTask "discarding a name drops its NAMING only, never the content" {
    // Dropping the `Add` alongside the `SetName` would leave `package_functions` holding content
    // no op provides: a store a re-fold would not reproduce. A dependent references content by
    // HASH, so content is allowed to be live with nothing naming it.
    //
    // Asserted as the OP COUNT, deliberately. The obvious assertion -- that `package_functions`
    // still has the row -- passes either way, because that projection keeps the row even when the
    // Add is dropped, which is the very inconsistency being guarded against. The count is what
    // goes red: 2 ops instead of 1.
    let m = "DraftTestNeededContent"
    do! cleanup m

    let! _ =
      authorIntoMain
        $"module Darklang.{m}\n\nlet basis () : Int64 = 6001L\n\nlet uses () : Int64 = basis ()"

    let! basisHash = liveHash m "basis"
    Expect.isSome basisHash "basis resolves before the discard"

    let! result =
      runDarkResult (
        "Darklang.SCM.Draft.discardName \"Darklang\" [\"" + m + "\"] \"basis\""
      )
    let n = unwrap result

    Expect.equal
      n
      1L
      "exactly one op goes: the naming. Dropping the Add too would report 2 and would strand \
       `package_functions` with content no op provides."

    let! gone = liveHash m "basis"
    Expect.isNone gone "the NAME is gone"

    do! cleanup m
  }



/// The rewrite is delete-then-reinsert, and it is one transaction. Split across two, a crash in
/// between deletes the draft for good; as one, a failure after the delete leaves the draft exactly
/// where it was. The crash is injected as a statement that cannot run, placed after the delete.
let aFailedRewriteLeavesTheDraftIntact =
  testTask "a rewrite that fails after its delete rolls the delete back" {
    let m = "DraftTestAtomic"
    do! cleanup m

    let! ops = authorIntoMain $"module Darklang.{m}\n\nlet a () : Int64 = 7101L"
    let! before = liveHash m "a"
    Expect.isSome before "the draft binding is live"
    let! draftBefore = draftOpCount ()

    let poisoned = Inserts.draftDeletes @ [ "DELETE FROM no_such_table_draft_test" ]
    let! outcome =
      task {
        try
          let! _ =
            Inserts.rewriteOpsAtomically
              poisoned
              (fun _ -> Inserts.nextOriginTs ())
              (fun _ -> None)
              "op"
              ops
          return Ok()
        with e ->
          return Error e.Message
      }
    Expect.isError outcome "the poisoned rewrite raised"

    let! after = liveHash m "a"
    Expect.equal
      after
      before
      "the binding the delete would have dropped is still there"
    let! draftAfter = draftOpCount ()
    Expect.equal draftAfter draftBefore "and so is every draft op"

    // And the same rewrite without the poison does its job, so the test is not passing on a no-op.
    let! _ =
      Inserts.rewriteOpsAtomically
        Inserts.draftDeletes
        (fun _ -> Inserts.nextOriginTs ())
        (fun _ -> None)
        "op"
        ops
    let! again = liveHash m "a"
    Expect.equal again before "a clean rewrite lands the same binding"

    do! cleanup m
  }

let tests =
  // The draft is SHARED: `discard` drops every uncommitted op on main, which includes whatever a
  // concurrently-running test just authored. Anything that drops the draft has to run alone, so
  // testSequenced, NOT testSequencedGroup: the group form only sequences the tests inside it, and
  // still runs in the parallel phase next to everything else.
  testSequenced
  <| testList
    "Draft"
    [ discardsOnlyTheDraft
      restoresASupersededBinding
      emptyDraftIsANoOp
      dropsOnlyWhatTheDraftWrote
      unstagesARepointButNotAnEdit
      collapseKeepsTheLastNamingOnly
      keepsAnOpItCannotRead
      discardNameDropsOneAndKeepsTheRest
      discardNameKeepsContentSomethingElseNeeds
      aFailedRewriteLeavesTheDraftIntact ]
