/// The draft: what `dark discard` drops, and what it must not touch.
///
/// A draft op is already folded and already running -- "draft" means uncommitted, not inert. Dropping one
/// therefore has to un-do a fold, and a fold has no inverse: `locations` records the RESULT of the whole op
/// sequence, so a draft op may have overwritten a row an older op wrote. `LibDB.Draft.discard` handles that
/// by rebuilding from the ops that survive, and these are the assertions that the rebuild is faithful:
/// a committed binding is restored rather than merely left alone, and a committed op keeps its commit.
module Tests.Draft

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module PM = LibDB.PackageManager
module HS = LibDB.HashStabilization
module Package = LibParser.Package
module NR = LibParser.NameResolver
module Inserts = LibDB.Inserts
module WipRefresh = LibDB.WipRefresh
module Draft = LibDB.Draft
module Propagation = LibDB.Propagation

open TestUtils.TestUtils

let private pmPT = PM.pt

/// Author source into MAIN the way the CLI does: parse, stabilize SCC-aware hashes, insert + fold.
let private author (source : string) : Task<List<PT.PackageOp>> =
  task {
    let builtins = localBuiltIns pmPT
    let! parsed =
      Package.parse builtins pmPT NR.OnMissing.ThrowError source |> Ply.toTask
    match parsed with
    | Ok ops ->
      let stabilized = HS.computeRealHashes ops
      let! _ = Inserts.insertAndApplyOpsAsWip stabilized
      let! _ = WipRefresh.refresh pmPT
      return stabilized
    | Error errs ->
      return Exception.raiseInternal "draft test parse failed" [ "errs", errs ]
  }

/// Commit everything uncommitted on main, the way `dark commit` does.
let private commitAll (message : string) : Task<string> =
  Inserts.commitAllAsBaseline message

/// What `locations` currently binds a name to.
let private liveHash (m : string) (name : string) : Task<Option<string>> =
  Sql.query
    "SELECT item_hash FROM locations
     WHERE owner = 'Darklang' AND modules = @m AND name = @n AND unlisted_at IS NULL
     LIMIT 1"
  |> Sql.parameters [ "m", Sql.string m; "n", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "item_hash")

let private draftOpCount () : Task<int64> =
  Sql.query
    "SELECT count(*) AS n FROM package_ops
     WHERE commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches)"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

let private opCountIn () : Task<int64> =
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

    let! _ = author $"module Darklang.{m}\n\nlet keep () : Int64 = 4001L"
    let! _ = commitAll "draft test: committed"
    let! committedBefore = opCountIn ()
    let! keepBefore = liveHash m "keep"

    let! _ = author $"module Darklang.{m}\n\nlet dropMe () : Int64 = 4002L"
    let! draftBefore = draftOpCount ()
    Expect.isGreaterThan draftBefore 0L "the second author left a draft"

    let! result = Draft.discard ()
    let n = unwrap result
    Expect.equal n draftBefore "discard reports every draft op it dropped"

    let! draftAfter = draftOpCount ()
    Expect.equal draftAfter 0L "no draft is left"

    let! committedAfter = opCountIn ()
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
    let! _ = author $"module Darklang.{m}\n\nlet v () : Int64 = 5001L"
    let! _ = commitAll "draft test: v1 committed"
    let! committedHash = liveHash m "v"

    let! _ = author $"module Darklang.{m}\n\nlet v () : Int64 = 5002L"
    let! editedHash = liveHash m "v"
    Expect.notEqual editedHash committedHash "the edit moved the name"

    let! result = Draft.discard ()
    let _ = unwrap result

    let! afterHash = liveHash m "v"
    Expect.equal afterHash committedHash "the name is back on its committed version"

    do! cleanup m
  }

let emptyDraftIsANoOp =
  testTask "discarding an empty draft changes nothing" {
    let m = "DraftTestEmpty"
    do! cleanup m

    let! _ = author $"module Darklang.{m}\n\nlet e () : Int64 = 6001L"
    let! _ = commitAll "draft test: empty"
    let! committedBefore = opCountIn ()
    let! before = liveHash m "e"

    let! result = Draft.discard ()
    Expect.equal (unwrap result) 0L "nothing to drop"

    // The no-op path must not take the delete-and-reinsert route: a rebuild that runs when there is
    // nothing to remove is pure risk, and re-stamping ops is how the fold's LWW gets poisoned.
    let! committedAfter = opCountIn ()
    Expect.equal committedAfter committedBefore "the committed log is untouched"

    let! after = liveHash m "e"
    Expect.equal after before "and the store is exactly as it was"

    do! cleanup m
  }

let dropsOnlyWhatTheDraftWrote =
  testTask "discarding a draft leaves the committed log's rows where they were" {
    let m = "DraftTestScoped"
    do! cleanup m

    let! _ = author $"module Darklang.{m}\n\nlet s () : Int64 = 7001L"
    let! _ = commitAll "draft test: scoped"
    let! rowidBefore = aCommittedRowid ()

    let! _ = author $"module Darklang.{m}\n\nlet d () : Int64 = 7002L"

    let! result = Draft.discard ()
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

let private hashOfSetName (ops : List<PT.PackageOp>) (name : string) : PT.Hash =
  ops
  |> List.tryPick (fun op ->
    match op with
    | PT.PackageOp.SetName(l, target, _) when l.name = name -> Some target.hash
    | _ -> None)
  |> Option.defaultWith (fun () ->
    Exception.raiseInternal "no SetName for name" [ "name", name ])


let unstagesARepointButNotAnEdit =
  testTask "un-staging drops a repoint that followed, and refuses one you authored" {
    let m = "DraftTestUnstage"
    do! cleanup m

    let! v1 =
      author
        $"""module Darklang.{m}

let src (x: Int64) : Int64 = Stdlib.Int64.add x 8001L"""

    let! _ =
      author
        $"""module Darklang.{m}

let follower (x: Int64) : Int64 = Darklang.{m}.src x"""

    let! _ = commitAll "draft test: unstage"
    let! committedFollower = liveHash m "follower"

    // Edit the source and let the cascade repoint the follower, as authoring does.
    let! v2 =
      author
        $"""module Darklang.{m}

let src (x: Int64) : Int64 = Stdlib.Int64.add x 8002L"""

    let fromHash = hashOfSetName v1 "src"
    let toHash = hashOfSetName v2 "src"

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
    let! dropped = Draft.unstageRepoint (loc m "follower")
    Expect.isGreaterThan (unwrap dropped) 0L "the staged repoint was dropped"

    let! afterFollower = liveHash m "follower"
    Expect.equal
      afterFollower
      committedFollower
      "the follower is back where it was committed"

    // The item YOU edited is not a repoint, and un-staging must refuse it -- otherwise a pin would throw
    // away someone's work while reporting that it undid a consequence.
    let! refused = Draft.unstageRepoint (loc m "src")
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

    let! _ = author $"module Darklang.{m}\n\nlet c () : Int64 = 9001L"
    let! _ = author $"module Darklang.{m}\n\nlet c () : Int64 = 9002L"
    let! v3 = author $"module Darklang.{m}\n\nlet c () : Int64 = 9003L"

    let! before = liveHash m "c"
    let! draftBefore = draftOpCount ()

    let! result = Draft.collapse ()
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
    let (PT.Hash v3Hash) = hashOfSetName v3 "c"
    Expect.equal
      stillThere
      (Some v3Hash)
      "and it's the version the last edit authored"

    do! cleanup m
  }


let tests =
  // `testSequenced`, not a sequenced GROUP. Not because of the rewrite any more -- that is scoped to the
  // draft now -- but because the draft is SHARED: `discard` drops every uncommitted op on main, which includes
  // whatever a concurrently-running test just authored. Anything that drops the draft has to be alone.
  testSequenced
  <| testList
    "Draft"
    [ discardsOnlyTheDraft
      restoresASupersededBinding
      emptyDraftIsANoOp
      dropsOnlyWhatTheDraftWrote
      unstagesARepointButNotAnEdit
      collapseKeepsTheLastNamingOnly ]
