/// The draft rewrite under conditions only F# can stage.
///
/// What `dark discard` and collapse drop, and what they must leave, is tested in Dark, in
/// `packages/darklang/tests/scm/draft.dark`. The tests here stay because they inject a failure
/// into the rewrite (`rewriteOpsAtomically`), open a race window through an F# hook
/// (`refreshWith`, `rebuildWith`, a `rebuild` handed a stale read), or plant an op blob this
/// build cannot decode.
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

/// The op count a discard reported, or a failure that says why.
let private unwrap (r : Result<int64, string>) : int64 =
  match r with
  | Ok n -> n
  | Error e -> Exception.raiseInternal "discard errored" [ "e", e ]

let private cleanup (m : string) : Task<unit> =
  Sql.query "DELETE FROM locations WHERE owner = 'Darklang' AND modules = @m"
  |> Sql.parameters [ "m", Sql.string m ]
  |> Sql.executeStatementAsync



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
    use output = new OutputCapture()
    let m = "DraftTestUnreadable"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet u(): Int64 = 7001L"
    let! _ = commitAll "draft test: unreadable v1"
    let! committedHash = liveHash m "u"

    // An uncommitted edit over a committed binding, so the rewrite has real work to do.
    let! editOps = authorIntoMain $"module Darklang.{m}\n\nlet u(): Int64 = 7002L"

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
        [ PT.PackageOp.Deprecate(
            target,
            PT.DeprecationKind.Harmful,
            "draft test",
            None
          ) ]

    let! result = runDarkResult "Darklang.SCM.Draft.discardAll()"
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
    output.Check(fun stdout stderr ->
      Expect.equal stdout "" "the rebuild has no command output"
      // The store warning is emitted only once per process.
      if stderr <> "" then
        Expect.stringContains stderr "format this build cannot read" "the alien op"
        Expect.stringContains
          stderr
          "They are kept, not dropped"
          "preserved for later")
  }


/// The rewrite is delete-then-reinsert, and it is one transaction. Split across two, a crash in
/// between deletes the draft for good; as one, a failure after the delete leaves the draft exactly
/// where it was. The crash is injected as a statement that cannot run, placed after the delete.
let aFailedRewriteLeavesTheDraftIntact =
  testTask "a rewrite that fails after its delete rolls the delete back" {
    let m = "DraftTestAtomic"
    do! cleanup m

    let! ops = authorIntoMain $"module Darklang.{m}\n\nlet a(): Int64 = 7101L"
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

/// `WipRefresh.refresh` reads the draft, works on it, then deletes and re-inserts it. Another author
/// (another process on the same store, or the parallel test phase) can land in between, and its op is
/// in the set the delete covers but not in the set that was read. The window is opened by hand here:
/// a whole second author, insert and refresh, runs after the first refresh's read.
let aRefreshDoesNotEatAnAuthorThatLandedMidway =
  testTask "an author that lands inside another refresh's window survives it" {
    let m = "DraftTestRefreshWindow"
    do! cleanup m

    let! _ =
      authorIntoMain
        $"module Darklang.{m}\n\nlet caller(): Int64 = Darklang.{m}.callee() + 1L"

    let mutable opened = 0
    let! _ =
      LibDB.WipRefresh.refreshWith
        (fun () ->
          task {
            opened <- opened + 1
            if opened = 1 then
              let! _ =
                authorIntoMain $"module Darklang.{m}\n\nlet callee(): Int64 = 41L"
              ()
          })
        pmPT
    Expect.equal
      opened
      2
      "the first attempt saw the draft move, and went round again"

    let! callee = liveHash m "callee"
    Expect.isSome callee "the author that landed midway is still bound"
    let! r = evalDarkExpr $"Darklang.{m}.caller()"
    match r with
    | Ok(RT.DInt64 n) -> Expect.equal n 42L "and the caller runs"
    | Ok other -> failtest $"the caller answered {other}"
    | Error(rte, _) -> failtest $"the caller did not run: %A{rte}"

    do! cleanup m
  }

/// The draft as `SCM.Draft.mainDraftOps` reads it, which is what `rewriteBy` hands the rebuild.
let private draftAsDarkReadsIt () : Task<Set<System.Guid>> =
  task {
    let! rows = LibDB.Queries.getMainOpsWithIds ()
    return
      rows
      |> List.filter (fun r -> Option.isSome r.op && Option.isNone r.commitHash)
      |> List.map _.id
      |> Set.ofList
  }

let private opIsInTheLog (id : System.Guid) : Task<bool> =
  task {
    let! n =
      countSql
        "SELECT COUNT(*) AS n FROM package_ops WHERE id = @id"
        [ "id", Sql.uuid id ]
    return n = 1L
  }

/// `Draft.rebuild` deletes the whole of main and re-inserts what it read. A pull landing between the
/// read and the delete brings ops that are in the set deleted and not in the set read: a peer's
/// committed work, and an op from a newer build that the delete is meant to spare by id. The window is
/// opened by hand through `rebuildWith`.
let aPullInsideARebuildsWindowSurvivesIt =
  testTask "a pull that lands inside a rebuild's window survives it, readable or not" {
    use output = new OutputCapture()
    let m = "DraftTestRebuildPull"
    do! cleanup m

    let! mine = authorIntoMain $"module Darklang.{m}\n\nlet mine(): Int64 = 1L"
    let! peerOps = parsePackageOps $"module Darklang.{m}\n\nlet peer(): Int64 = 2L"

    // An op kind this build has no tag for, which is what a newer build's op looks like here.
    let alienId = System.Guid.NewGuid()
    let peerOp =
      match peerOps with
      | op :: _ -> op
      | [] -> failtest "the peer source authored nothing"
    let alienBytes =
      LibSerialization.Binary.Serialization.PT.PackageOp.serialize alienId peerOp
    alienBytes[8] <- 200uy
    Expect.isNone
      (LibSerialization.Binary.Serialization.PT.PackageOp.tryDeserialize
        alienId
        alienBytes)
      "the alien op really is undecodable"

    let records =
      (peerOps
       |> List.map (fun op ->
         let id = Inserts.computeOpHash op
         (string id,
          System.Convert.ToHexString(
            LibSerialization.Binary.Serialization.PT.PackageOp.serialize id op
          ),
          Inserts.nextOriginTs ())))
      @ [ (string alienId,
           System.Convert.ToHexString alienBytes,
           Inserts.nextOriginTs ()) ]

    // `scmImportOps`, which is what a pull runs.
    let pull () =
      task {
        let! _ = Inserts.importOpsBulk "draft-test-pull" records
        let! _ = LibDB.Seed.applyUnappliedOps ()
        return ()
      }

    let! draft = draftAsDarkReadsIt ()
    let mutable opened = 0
    let! rebuilt =
      LibDB.Draft.rebuildWith
        (fun () ->
          task {
            opened <- opened + 1
            if opened = 1 then do! pull ()
          })
        draft
        Set.empty

    let! peer = liveHash m "peer"
    Expect.isSome peer "the pulled, committed fn is still bound"
    let! alienKept = opIsInTheLog alienId
    Expect.isTrue alienKept "the op this build cannot decode is still in the log"
    Expect.isFalse rebuilt "main moved under the rebuild, so it wrote nothing"
    let! mineBefore = liveHash m "mine"
    let (PT.Hash mineHash) = hashBoundTo mine "mine"
    Expect.equal mineBefore (Some mineHash) "and nothing of the draft went"

    // From a fresh read, as `rewriteBy` goes round again, the rebuild does its job and spares both.
    let! draft = draftAsDarkReadsIt ()
    let! rebuilt = LibDB.Draft.rebuild draft Set.empty
    Expect.isTrue rebuilt "the second read matches, so the rebuild writes"
    let! mineAfter = liveHash m "mine"
    Expect.isNone mineAfter "the draft is gone"
    let! peerAfter = liveHash m "peer"
    Expect.equal peerAfter peer "the pulled fn is still bound after a real rebuild"
    let! alienAfter = opIsInTheLog alienId
    Expect.isTrue alienAfter "and so is the alien op"

    let! _ =
      Sql.query "DELETE FROM package_ops WHERE id = @id"
      |> Sql.parameters [ "id", Sql.uuid alienId ]
      |> Sql.executeNonQueryAsync
    do! cleanup m
    output.Check(fun stdout stderr ->
      Expect.equal stdout "" "the rebuild has no command output"
      // The store warning is emitted only once per process.
      if stderr <> "" then
        Expect.stringContains stderr "format this build cannot read" "the alien op"
        Expect.stringContains
          stderr
          "They are kept, not dropped"
          "preserved for later")
  }

/// `rewriteBy` chooses `keptIds` from its own read of the draft, before the rebuild reads anything, so
/// an author landing in between is not kept and would be deleted. The rebuild refuses instead.
let anAuthorAfterTheDraftWasChosenFromSurvivesARebuild =
  testTask
    "an author that lands after the draft was chosen from survives the rebuild" {
    let m = "DraftTestRebuildAuthor"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet early(): Int64 = 1L"
    let! draft = draftAsDarkReadsIt ()
    let! late = authorIntoMain $"module Darklang.{m}\n\nlet late(): Int64 = 2L"

    let! rebuilt = LibDB.Draft.rebuild draft Set.empty

    let! lateBound = liveHash m "late"
    let (PT.Hash lateHash) = hashBoundTo late "late"
    Expect.equal
      lateBound
      (Some lateHash)
      "the author nobody chose to drop is still bound"
    Expect.isFalse rebuilt "the draft is not the one the caller chose from"

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
    [ keepsAnOpItCannotRead
      aFailedRewriteLeavesTheDraftIntact
      aRefreshDoesNotEatAnAuthorThatLandedMidway
      aPullInsideARebuildsWindowSurvivesIt
      anAuthorAfterTheDraftWasChosenFromSurvivesARebuild ]
