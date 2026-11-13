/// The draft rewrite under conditions only F# can stage.
///
/// What `dark discard` and collapse drop, and what they must leave, is tested in Dark, in
/// `packages/darklang/tests/scm/draft.dark`. The tests here stay because they stop a discard partway
/// (`ScmActions.Testing.failAt`), open a race window through an F# hook (`refreshWith`), plant an op
/// blob this build cannot decode, or read where committed ops sit in the log, which nothing in the SCM
/// reports.
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


/// Run a Dark call answering `Result<Int64, String>`, as the F# result these assertions expect.
let private runDarkResult (code : string) : Task<Result<int64, string>> =
  task {
    match! evalDarkExpr code with
    | Ok(RT.DEnum(_, _, _, "Ok", [ RT.DInt64 n ])) -> return Ok n
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
     WHERE commit_hash IS NULL AND branch_id = '00000000-0000-0000-0000-000000000001'"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

/// The op count a discard reported, or a failure that says why.
let private unwrap (r : Result<int64, string>) : int64 =
  match r with
  | Ok n -> n
  | Error e -> Exception.raiseInternal "discard errored" [ "e", e ]

let private cleanup (m : string) : Task<unit> =
  task {
    do!
      Sql.query
        "DELETE FROM package_ops WHERE id IN
           (SELECT op_id FROM locations WHERE owner = 'Darklang' AND modules = @m)"
      |> Sql.parameters [ "m", Sql.string m ]
      |> Sql.executeStatementAsync
    do!
      Sql.query "DELETE FROM locations WHERE owner = 'Darklang' AND modules = @m"
      |> Sql.parameters [ "m", Sql.string m ]
      |> Sql.executeStatementAsync
    LibDB.Caching.invalidateAll ()
  }



/// The log can hold ops this build cannot decode -- written by an older or newer build -- kept
/// unapplied on purpose so a build that can read them still will. Reading the draft must not raise on
/// one, and nothing that rewrites the draft may delete one for being unparseable: it is spared, not
/// skipped.
let keepsAnOpItCannotRead =
  testTask "a discard and a draft rewrite spare the ops this build cannot decode" {
    use output = new OutputCapture()
    let m = "DraftTestUnreadable"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet u(): Int64 = 7001"
    let! _ = commitAll "draft test: unreadable v1"
    let! committedHash = liveHash m "u"

    // An uncommitted edit over a committed binding, so the discard has real work to do.
    let! editOps = authorIntoMain $"module Darklang.{m}\n\nlet u(): Int64 = 7002"

    // Two ops this build's deserializer rejects: one committed, one in the draft itself.
    let alienCommitted = System.Guid.NewGuid()
    let alienDraft = System.Guid.NewGuid()
    for (id, commit) in
      [ (alienCommitted, box "alien-commit"); (alienDraft, box System.DBNull.Value) ] do
      let! _ =
        Sql.query
          "INSERT INTO package_ops (id, branch_id, authored_on, op_blob, commit_hash, origin_ts)
           VALUES (@id, '00000000-0000-0000-0000-000000000001', '00000000-0000-0000-0000-000000000001', @blob, @commit, '2026-08-28T00:00:00Z')"
        |> Sql.parameters
          [ "id", Sql.string (string id)
            "blob", Sql.bytes [| 0xFFuy; 0x39uy; 0x07uy; 0x2Auy |]
            "commit",
            (match commit with
             | :? string as c -> Sql.string c
             | _ -> Sql.dbnull) ]
        |> Sql.executeNonQueryAsync
      ()

    // A Deprecate in the draft as well, so the discard drops a row from every layered table.
    let target = PT.Reference.PackageFn(hashBoundTo editOps "u")
    let uAt : PT.PackageLocation =
      { owner = "Darklang"; modules = [ m ]; name = "u" }
    let! _ =
      Inserts.insertAndApplyOps
        [ PT.PackageOp.Deprecate(
            uAt,
            target,
            PT.DeprecationKind.Harmful,
            "draft test",
            None
          ) ]

    // The draft rewrite WipRefresh does, then the discard.
    let! _ = LibDB.WipRefresh.refresh pmPT
    let! result = runDarkResult "Darklang.SCM.Draft.discardAll()"
    let _ = unwrap result

    let! afterHash = liveHash m "u"
    Expect.equal afterHash committedHash "the discard still did its job"
    let! deprecated =
      LibDB.Queries.getCurrentDeprecation
        uAt
        (hashBoundTo editOps "u")
        PT.ItemKind.Fn
    Expect.isNone deprecated "and took the deprecation with it"

    for id in [ alienCommitted; alienDraft ] do
      let! survivors =
        countSql
          "SELECT COUNT(*) AS n FROM package_ops WHERE id = @id"
          [ "id", Sql.string (string id) ]
      Expect.equal survivors 1L "an op it could not read is still in the log"
      let! _ =
        Sql.query "DELETE FROM package_ops WHERE id = @id"
        |> Sql.parameters [ "id", Sql.string (string id) ]
        |> Sql.executeNonQueryAsync
      ()

    do! cleanup m
    output.Check(fun stdout stderr ->
      Expect.equal stdout "" "the discard has no command output"
      if stderr <> "" then
        Expect.stringContains stderr "format this build cannot read" "the alien op"
        Expect.stringContains
          stderr
          "They are kept, not dropped"
          "preserved for later")
  }


/// A discard is one transaction: stopped after it has deleted the draft's rows, it leaves the draft
/// exactly where it was, and the same discard run again does its job.
let aFailedRewriteLeavesTheDraftIntact =
  testTask "a discard stopped after deleting its rows rolls the delete back" {
    let m = "DraftTestAtomic"
    do! cleanup m

    let! ops = authorIntoMain $"module Darklang.{m}\n\nlet a(): Int64 = 7101"
    let! before = liveHash m "a"
    Expect.isSome before "the draft binding is live"
    let! draftBefore = draftOpCount ()
    let ids = ops |> List.map (fun op -> string (Inserts.computeOpHash op))

    LibDB.ScmActions.Testing.failAt.Value <- "drop.rows"
    let! outcome =
      task {
        try
          return!
            LibDB.ScmActions.discard PT.BranchId.Main (LibDB.ScmActions.Ops ids)
        finally
          LibDB.ScmActions.Testing.failAt.Value <- null
      }
    LibDB.ScmActions.Testing.failAt.Value <- null
    Expect.isError outcome "the stopped discard failed"

    let! after = liveHash m "a"
    Expect.equal
      after
      before
      "the binding the delete would have dropped is still there"
    let! draftAfter = draftOpCount ()
    Expect.equal draftAfter draftBefore "and so is every draft op"

    // And the same discard without the stop does its job, so the test is not passing on a no-op.
    let! dropped =
      LibDB.ScmActions.discard PT.BranchId.Main (LibDB.ScmActions.Ops ids)
    Expect.equal dropped (Ok(List.length ids)) "a clean discard drops them"
    let! again = liveHash m "a"
    Expect.isNone again "and the name goes with them"

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
        $"module Darklang.{m}\n\nlet caller(): Int64 = Darklang.{m}.callee() + 1"

    let mutable opened = 0
    let! _ =
      LibDB.WipRefresh.refreshWith
        (fun () ->
          task {
            opened <- opened + 1
            if opened = 1 then
              let! _ =
                authorIntoMain $"module Darklang.{m}\n\nlet callee(): Int64 = 41"
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

/// Where main's oldest committed op sits in the log, which only a rewrite of the log would move.
let private firstCommittedRowid () : Task<int64> =
  Sql.query
    "SELECT COALESCE(MIN(rowid), -1) AS n FROM package_ops
     WHERE commit_hash IS NOT NULL AND branch_id = @main"
  |> Sql.parameters [ "main", Sql.string (string PT.BranchId.Main) ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

/// A discard touches only the rows the draft wrote, with or without a draft to drop, so every other
/// reader keeps a whole store; an unmoved committed rowid is the evidence.
let aDiscardLeavesTheCommittedLogInPlace =
  testTask "a discard leaves the committed log in place" {
    let m = "DraftTestScoped"
    do! cleanup m

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet s(): Int64 = 7001"
    let! _ = commitAll "draft test: scoped"
    let! rowidBefore = firstCommittedRowid ()
    Expect.notEqual rowidBefore -1L "there is a committed op to watch"

    let! empty = runDarkResult "Darklang.SCM.Draft.discardAll()"
    Expect.equal empty (Ok 0L) "an empty draft drops nothing"
    let! afterEmpty = firstCommittedRowid ()
    Expect.equal afterEmpty rowidBefore "and rewrites nothing"

    let! _ = authorIntoMain $"module Darklang.{m}\n\nlet d(): Int64 = 7002"
    let! dropped = runDarkResult "Darklang.SCM.Draft.discardAll()"
    Expect.isGreaterThan (unwrap dropped) 0L "the draft op was dropped"
    let! afterDrop = firstCommittedRowid ()
    Expect.equal afterDrop rowidBefore "and the committed log did not move"

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
      aDiscardLeavesTheCommittedLogInPlace ]
