/// The SCM ACTIONS: commit, discard, the branch lifecycle, merge, rebase, and settling a conflict.
///
/// Each one is a whole operation, run here end to end, in ONE transaction wherever the work allows it.
/// So no crash leaves a half-state behind: a commit row naming no ops, a merged branch whose landed ops
/// sat in main's draft, a deleted op whose rows still bound a name.
///
/// Where one transaction is not possible, the step that cannot join it comes FIRST and is complete on
/// its own, and the step after it is idempotent, so running the action again finishes it:
///   - a rebase's repoints are propagations, each a whole edit, and they run before the transaction
///     that moves the bases and records the conflicts;
///   - settling a conflict authors an op through the ordinary authoring path, then closes the record.
/// Evaluating merged values runs AFTER the merge's transaction, never inside one: evaluation runs user
/// code, which reads the store.
///
/// What stays in Dark: whether an action is wanted (the CLI and its prompts), the checks a commit or a
/// merge has to pass first (type check, unresolved names, constraints), which ops a selection pulls in,
/// and every sentence a person reads. Decisions about the store -- what the draft is, which ops a
/// discard spares, how a commit id is minted, which side a conflict names as the winner -- are here.
module LibDB.ScmActions

open System.Threading.Tasks

open Microsoft.Data.Sqlite

open LibDB.Sqlite

open Prelude

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Detect = LibDB.ScmDetect


// ---------------------------------------------------------------------------------------------
// test seam
// ---------------------------------------------------------------------------------------------

/// Stopping an action partway, for the tests that check a store left by an interrupted action.
///
/// An `AsyncLocal`, not a global: it flows into the tasks a test starts and nowhere else, so a test
/// that sets it cannot fail an action some other test is running at the same moment.
module Testing =
  let failAt = System.Threading.AsyncLocal<string>()

/// Raise at the named step if a test asked for it. Called INSIDE the transaction, so this is what a
/// crash at that point does to the store: the transaction never commits.
let private step (name : string) : unit =
  match Testing.failAt.Value with
  | null -> ()
  | wanted when wanted = name ->
    Exception.raiseInternal $"stopped at {name} (test)" [ "step", name ]
  | _ -> ()


// ---------------------------------------------------------------------------------------------
// one connection, one transaction
// ---------------------------------------------------------------------------------------------

type private Param = string * obj

let private command (conn : SqliteConnection) (sql : string) (ps : List<Param>) =
  Telemetry.count "sql.scm"
  let cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  for (n, v) in ps do
    cmd.Parameters.AddWithValue(n, (if isNull v then box System.DBNull.Value else v))
    |> ignore<SqliteParameter>
  cmd

let private exec (conn : SqliteConnection) (sql : string) (ps : List<Param>) : int =
  use cmd = command conn sql ps
  cmd.ExecuteNonQuery()

let private rows
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<Param>)
  (read : SqliteDataReader -> 'a)
  : List<'a> =
  use cmd = command conn sql ps
  let opened = cmd.ExecuteReader()
  use reader = opened
  Sqlite.readRows read reader

let private textOr (r : SqliteDataReader) (i : int) : string =
  if r.IsDBNull i then "" else r.GetString i

let private ids
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<Param>)
  : List<string> =
  let found = rows conn sql ps (fun r -> textOr r 0)
  found |> List.filter (fun s -> s <> "")

let private count
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<Param>)
  : int64 =
  let found = rows conn sql ps (fun r -> r.GetInt64 0)
  List.tryHead found |> Option.defaultValue 0L

/// Run <f> in ONE transaction on its own connection. It commits when <f> answers `Ok`, and rolls back
/// on `Error` or a raise, so a refusal found partway leaves nothing behind either. The caches are
/// dropped after a commit, since every action here writes something they are built from.
let private transact (f : SqliteConnection -> Result<'a, 'e>) : Result<'a, 'e> =
  let result =
    WriteGate.hold (fun () ->
      use conn = LibDB.Sqlite.openConnection (LibDB.Sqlite.connString)
      // IMMEDIATE (the default here): the write lock is taken at BEGIN, so what the action reads
      // inside is what it writes against.
      use tx = conn.BeginTransaction()
      let r = f conn
      match r with
      | Ok _ -> tx.Commit()
      | Error _ -> tx.Rollback()
      r)
  Caching.invalidateAll ()
  result


// ---------------------------------------------------------------------------------------------
// what every action reads
// ---------------------------------------------------------------------------------------------

/// The draft, as one clause over `package_ops p`: the branch's uncommitted ops. The ONE definition;
/// the reads that list the draft (`ScmOps`) spell the same clause.
let private draftWhere (b : PT.BranchId) : string * List<Param> =
  "p.branch_id = @branch AND p.commit_hash IS NULL", [ "@branch", box (string b) ]

/// `Layers.cte`'s parameters, for this file's commands.
let private viewParams (b : PT.BranchId) (own : bool) : List<Param> =
  Layers.parameters "@" b own

/// The newest commit <param b> can see: its chain's committed ops, main's included. "" for none.
let private parentCommitOn (conn : SqliteConnection) (b : PT.BranchId) : string =
  let found =
    ids
      conn
      (Layers.cte "@"
       + " SELECT hash FROM commits WHERE branch_id IN (SELECT bid FROM chain)
           ORDER BY origin_ts DESC, rowid DESC LIMIT 1")
      (viewParams b true)
  List.tryHead found |> Option.defaultValue ""

/// The next commit stamp: the log's latest `origin_ts` plus a second, so a commit sorts after what it
/// holds whatever the wall clock says.
let private nextStampOn (conn : SqliteConnection) : string =
  let found =
    ids
      conn
      "SELECT COALESCE(strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second'),
              strftime('%Y-%m-%dT%H:%M:%fZ','now')) FROM package_ops"
      []
  List.tryHead found |> Option.defaultValue ""

/// A stamp past everything in the log, or "" for an empty one.
let private stampPastEverythingOn (conn : SqliteConnection) : string =
  let found =
    ids
      conn
      "SELECT strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second') FROM package_ops"
      []
  List.tryHead found |> Option.defaultValue ""

/// A content-derived commit id: the same work committed onto the same history gets the same id on any
/// machine. Message, stamp, parent and the op ids (in the order given, which callers sort) are all in
/// the material, so identical ops committed onto different histories get different ids.
let mintCommitHash
  (message : string)
  (stamp : string)
  (opIds : List<string>)
  (parent : string)
  : string =
  let material = $"{message}|{stamp}|{parent}|" + String.concat "," opIds
  let bytes = System.Text.Encoding.UTF8.GetBytes material
  (LibExecution.Bytes.sha256Hex bytes).Substring(0, 16)

let private sortOrdinal (items : List<string>) : List<string> =
  items |> List.sortWith (fun a b -> System.String.CompareOrdinal(a, b))

let private insertCommit
  (conn : SqliteConnection)
  (branch : PT.BranchId)
  (housekeeping : bool)
  (hash : string)
  (message : string)
  (stamp : string)
  (parent : string)
  : unit =
  let _ =
    exec
      conn
      "INSERT OR REPLACE INTO commits (hash, branch_id, message, origin_ts, parent, housekeeping)
       VALUES (@hash, @branch, @message, @ts, @parent, @hk)"
      [ "@hash", box hash
        "@branch", box (string branch)
        "@message", box message
        "@ts", box stamp
        "@parent", box parent
        "@hk", box (if housekeeping then 1L else 0L) ]
  ()

/// A commit on <param b> that holds no ops: what a merge into main, or an archive, records.
let private markerCommitOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (housekeeping : bool)
  (message : string)
  : string =
  let parent = parentCommitOn conn b
  let stamp = nextStampOn conn
  let hash = mintCommitHash message stamp [] parent
  insertCommit conn b housekeeping hash message stamp parent
  hash

/// Stamp <param b>'s uncommitted ops among <param opIds> with a new commit under <param message>, and
/// move what they wrote to the branch's committed layer. None when none of them is uncommitted.
let private commitDraftOpsOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (message : string)
  (opIds : List<string>)
  : Option<string> =
  let draft =
    ids
      conn
      "SELECT id FROM package_ops WHERE branch_id = @b AND commit_hash IS NULL"
      [ "@b", box (string b) ]
  let draft = Set.ofList draft
  let toStamp =
    opIds |> List.filter (fun id -> Set.contains id draft) |> List.distinct
  if List.isEmpty toStamp then
    None
  else
    let parent = parentCommitOn conn b
    let stamp = nextStampOn conn
    let hash = mintCommitHash message stamp (sortOrdinal toStamp) parent
    insertCommit conn b false hash message stamp parent
    let _ =
      exec
        conn
        "UPDATE package_ops SET commit_hash = @hash
         WHERE branch_id = @b AND commit_hash IS NULL
           AND id IN (SELECT value FROM json_each(@ids))"
        [ "@hash", box hash
          "@b", box (string b)
          "@ids", box (jsonStrings toStamp) ]
    let ctx = PreparedBatch.newCtx conn
    try
      Layers.commitRowsOf
        ctx
        b
        "op_id IN (SELECT id FROM package_ops WHERE branch_id = $branch AND commit_hash = $hash)"
        (fun cmd -> PreparedBatch.p cmd "$hash" hash)
    finally
      PreparedBatch.disposeCtx ctx
    Some hash

/// Delete what <param opIds> are on <param b>: their rows in the layered projections, then the ops.
/// Returns how many ops went. Content is shared and stays.
let private dropOpsOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (opIds : List<string>)
  : int =
  if List.isEmpty opIds then
    0
  else
    let ctx = PreparedBatch.newCtx conn
    try
      Layers.dropRowsOf
        ctx
        b
        "op_id IN (SELECT value FROM json_each($ids))"
        (fun cmd -> PreparedBatch.p cmd "$ids" (jsonStrings opIds))
    finally
      PreparedBatch.disposeCtx ctx
    step "drop.rows"
    exec
      conn
      "DELETE FROM package_ops WHERE branch_id = @b AND id IN (SELECT value FROM json_each(@ids))"
      [ "@b", box (string b); "@ids", box (jsonStrings opIds) ]

/// Move every op of <param b> onto <param target> and fold them there, in authoring order: what a
/// merge does. The branch's rows in the layered projections go; the ops write the target's.
/// An op the target already holds stays the target's and is not folded again; the branch's copy goes.
/// Except when the target holds it only in its draft and the branch committed it: then the branch's
/// copy lands, or a discard of that draft would take it out from under what merged.
let private moveOpsOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (target : PT.BranchId)
  : unit =
  let moving =
    rows
      conn
      "SELECT id, op_blob FROM package_ops WHERE branch_id = @b ORDER BY origin_ts, rowid"
      [ "@b", box (string b) ]
      (fun r -> (r.GetString 0, r.GetFieldValue<byte[]> 1))
  let draftOnly =
    ids
      conn
      "SELECT id FROM package_ops WHERE branch_id = @t AND commit_hash IS NULL
         AND id IN (SELECT id FROM package_ops WHERE branch_id = @b AND commit_hash IS NOT NULL)"
      [ "@b", box (string b); "@t", box (string target) ]
  let _ = dropOpsOn conn target draftOnly
  let already =
    ids
      conn
      "SELECT id FROM package_ops WHERE branch_id = @t AND id IN (SELECT value FROM json_each(@ids))"
      [ "@t", box (string target); "@ids", box (jsonStrings (List.map fst moving)) ]
  let already = Set.ofList already
  let bt = [ "@b", box (string b); "@t", box (string target) ]
  let _ =
    exec
      conn
      "DELETE FROM package_ops
       WHERE branch_id = @b AND id IN (SELECT id FROM package_ops WHERE branch_id = @t)"
      bt
  let _ = exec conn "UPDATE package_ops SET branch_id = @t WHERE branch_id = @b" bt
  for t in Layers.all do
    let _ = exec conn $"DELETE FROM {t.name} WHERE branch_id = @b" bt
    ()
  step "merge.moved"
  let ops =
    moving
    |> List.choose (fun (id, blob) ->
      match System.Guid.TryParse id with
      | true, g when not (Set.contains id already) ->
        BS.PT.PackageOp.tryDeserialize g blob
      | _ -> None)
  PackageOpPlayback.applyOpsOnConnection conn target ops


// ---------------------------------------------------------------------------------------------
// bindings: what a run of ops says about names
// ---------------------------------------------------------------------------------------------

let private modulesOf (loc : PT.PackageLocation) : string =
  String.concat "." loc.modules

let private hashText (h : PT.Hash) : string = let (PT.Hash s) = h in s

/// A name's last word on one side: bound to a version, or ended.
type private Word =
  | Bound of Detect.Binding
  | Ended of Detect.Removal

let private boundOf (w : Word) : Option<Detect.Binding> =
  match w with
  | Bound b -> Some b
  | Ended _ -> None

let private endedOf (w : Word) : Option<Detect.Removal> =
  match w with
  | Ended r -> Some r
  | Bound _ -> None

let private bindingOfRow (row : ScmBindings.Binding) : Detect.Binding =
  { Detect.Binding.owner = row.owner
    modules = row.modules
    name = row.name
    itemType = row.itemType
    hash = row.hash
    originTs = row.originTs
    author = ""
    previous = row.previous }

let private removalOfRow (row : ScmBindings.Binding) : Detect.Removal =
  { owner = row.owner
    modules = row.modules
    name = row.name
    originTs = row.originTs
    author = ""
    previous = row.previous }

let private wordOf (row : ScmBindings.Binding) : Word =
  if row.hash = "" then Ended(removalOfRow row) else Bound(bindingOfRow row)

/// What a branch's OWN work last said about each name: the incoming side of its merge.
let private branchWords (b : PT.BranchId) : List<Word> =
  let rows = ScmBindings.query (ScmBindings.Own b)
  rows |> List.map wordOf

/// What the PARENT binds for these bare names: the local side a branch merges into.
let private parentBindings
  (parent : PT.BranchId)
  (names : List<string>)
  : List<Detect.Binding> =
  let rows = ScmBindings.query (ScmBindings.ByBareNames(parent, List.distinct names))
  rows |> List.map bindingOfRow

/// The name a person reads for a branch: "main", its name, or its id when it has none.
let branchName (b : PT.BranchId) : string =
  if b.IsMain then
    PT.BranchId.MainName
  else
    let found = ScmBranches.query (ScmBranches.ById(string b))
    match found with
    | info :: _ when info.name <> "" -> info.name
    | _ -> string b

/// A branch's registered parent; main when it has none, or when the stored id does not parse.
let parentOf (b : PT.BranchId) : PT.BranchId =
  let found = ScmBranches.query (ScmBranches.ById(string b))
  match found with
  | info :: _ ->
    PT.BranchId.Parse info.parentId |> Option.defaultValue PT.BranchId.Main
  | [] -> PT.BranchId.Main

let private nameBases (b : PT.BranchId) : List<Detect.NameBase> =
  let found = ScmBranches.query (ScmBranches.NameBasesOf(string b))
  found
  |> List.collect (fun i -> i.nameBases)
  |> List.map (fun nb ->
    ({ Detect.NameBase.owner = nb.owner
       modules = nb.modules
       name = nb.name
       baseHash = nb.baseHash }
    : Detect.NameBase))

/// The conflicts a merge of <param b> into its parent WOULD record: the branch's own bindings against
/// the parent's, tested against the bases the branch recorded. Records nothing.
///
/// Removals count both ways. A name the branch ended that the parent has since edited, and a name the
/// parent's chain ended that the branch has since edited, are each a name both sides moved; whichever
/// is newer wins, and the other is recorded like any loser. (Main's own removals carry no stamp in
/// what this reads, so a name main ended is not among them.)
let preview (b : PT.BranchId) : List<Detect.Conflict> =
  let words = branchWords b
  let bases = nameBases b
  let baseOf (owner : string) (modules : string) (n : string) =
    bases
    |> List.tryFind (fun nb ->
      nb.owner = owner && nb.modules = modules && nb.name = n)
    |> Option.map _.baseHash
  // What a branch edit replaced, as the PARENT sees it, is what the parent held at the branch's base.
  // The op says what the parent held when it was made (`SCM.Lineage`), and once the parent has moved
  // that is a version the branch never saw: read as written, a branch's second edit after the parent
  // changed the name passes for an edit made on top of the parent's change, and hides the conflict.
  // A branch that did take the parent's version (a rebase, `resolve`) moved its base there.
  let incoming =
    words
    |> List.choose boundOf
    |> List.map (fun (inc : Detect.Binding) ->
      match baseOf inc.owner inc.modules inc.name with
      | Some "" -> { inc with previous = None }
      | Some baseHash -> { inc with previous = Some baseHash }
      | None -> inc)
  let removals = words |> List.choose endedOf
  let parent = parentOf b
  let local =
    parentBindings
      parent
      ((incoming |> List.map _.name) @ (removals |> List.map _.name))
  let parentEnded =
    if parent.IsMain then
      []
    else
      let rows = ScmBindings.query (ScmBindings.RemovedOffMain parent)
      rows |> List.map removalOfRow
  let parentName = branchName parent
  let name = branchName b
  // A removal the branch made of a name the parent has not moved since the base is the branch's own
  // business, whatever the removal says it replaced (it may be the branch's own earlier edit).
  let parentMoved =
    local |> List.filter (fun l -> baseOf l.owner l.modules l.name <> Some l.hash)
  // `detectRemovals` words its reason for a pull, where the local side is you. Here it is the
  // parent, so the reason names the edit rather than whose it is, as `detectEndedLocally` does.
  let removed =
    Detect.detectRemovals parentName name parentMoved removals
    |> List.map (fun c ->
      if c.reason = "your edit is newer" then
        { c with reason = "the edit is newer" }
      else
        c)
  Detect.detect parentName name local incoming bases
  @ removed
  @ Detect.detectEndedLocally parentName name parentEnded incoming bases

/// Record <param conflicts> against <param branchId>, on the caller's connection. An id already
/// recorded is left as it is, unless it is still pending on another branch, in which case it moves
/// here: a rebase records the divergence on the branch, and the merge after it re-detects the same pair
/// against the parent.
let private recordConflictsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  (conflicts : List<Detect.Conflict>)
  : unit =
  for c in conflicts do
    let _ =
      exec
        conn
        "INSERT OR IGNORE INTO conflicts
           (id, owner, modules, name, item_type, part, kind, candidates, auto_resolved_to, reason,
            status, origin_ts, branch_id)
         VALUES (@id, @owner, @modules, @name, @itemType, @part, @kind, @candidates, @resolvedTo,
                 @reason, 'pending', strftime('%Y-%m-%dT%H:%M:%fZ','now'), @branch)"
        [ "@id", box c.id
          "@owner", box c.owner
          "@modules", box c.modules
          "@name", box c.name
          "@itemType", box c.itemType
          "@part", box c.part
          "@kind", box c.kind
          "@candidates", box (Detect.candidatesJson c.candidates)
          "@resolvedTo", box c.autoResolvedTo
          "@reason", box c.reason
          "@branch", box (string branchId) ]
    let _ =
      exec
        conn
        "UPDATE conflicts SET branch_id = @branch WHERE id = @id AND status = 'pending'"
        [ "@branch", box (string branchId); "@id", box c.id ]
    ()

/// Recorded conflicts matching <param where>, on the caller's connection.
let private conflictsOn
  (conn : SqliteConnection)
  (where : string)
  (ps : List<Param>)
  : List<Detect.Conflict> =
  rows
    conn
    ("SELECT id, owner, modules, name, item_type, COALESCE(part, ''), kind, candidates,
             COALESCE(auto_resolved_to, ''), COALESCE(reason, ''), status,
             COALESCE(resolved_by, '')
      FROM conflicts WHERE "
     + where
     + " ORDER BY rowid")
    ps
    (fun r ->
      ({ Detect.Conflict.id = textOr r 0
         owner = textOr r 1
         modules = textOr r 2
         name = textOr r 3
         itemType = textOr r 4
         part = textOr r 5
         kind = textOr r 6
         candidates = Detect.parseCandidates (textOr r 7)
         autoResolvedTo = textOr r 8
         reason = textOr r 9
         status = textOr r 10
         resolvedBy = textOr r 11 }
      : Detect.Conflict))

/// Move what is still pending on <param branchId> to <param parent>, and answer what moved. A merged
/// branch is finished: nobody can settle a conflict recorded there, so one a rebase recorded on it
/// would otherwise be lost the moment it merged, with its losing version nowhere to override from.
let private carryPendingOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  (parent : PT.BranchId)
  : List<Detect.Conflict> =
  let b = box (string branchId)
  let pending = conflictsOn conn "branch_id = @b AND status = 'pending'" [ "@b", b ]
  let _ =
    exec
      conn
      "UPDATE conflicts SET branch_id = @p WHERE branch_id = @b AND status = 'pending'"
      [ "@p", box (string parent); "@b", b ]
  pending

/// <param recorded>, then what <param carried> adds that it does not already hold.
let private withCarried
  (recorded : List<Detect.Conflict>)
  (carried : List<Detect.Conflict>)
  : List<Detect.Conflict> =
  let ids = recorded |> List.map _.id |> Set.ofList
  recorded @ (carried |> List.filter (fun c -> not (Set.contains c.id ids)))

/// Record conflicts as pending against a branch, in one transaction. For a test that needs a recorded
/// conflict without staging the divergence behind it; actions record their own.
let recordConflicts
  (branchId : PT.BranchId)
  (conflicts : List<Detect.Conflict>)
  : unit =
  let _ =
    transact (fun conn ->
      recordConflictsOn conn branchId conflicts
      (Ok() : Result<unit, string>))
  ()


// ---------------------------------------------------------------------------------------------
// outcomes
// ---------------------------------------------------------------------------------------------

/// Why an action did nothing. Each is a state the store or the request is in, which the caller words.
type Refusal =
  /// The request named no op still in the draft.
  | NothingToCommit
  /// Main has no parent, no name to change, nothing to archive.
  | IsMain
  /// The branch holds no ops.
  | NothingToMerge
  /// Unmerged, unarchived branches sit under it.
  | HasActiveChildren
  /// Another branch that is not archived goes by this name.
  | NameTaken
  /// The branch does not bind that name, so there is nothing of its to keep or drop.
  | NotBound
  /// No conflict has that id.
  | NoSuchConflict
  /// The chosen side of the conflict has no version to bind.
  | NoVersion
  /// The conflict names a part of a declaration this build cannot read.
  | UnreadablePart of part : string
  /// The draft or the branch kept changing while the action ran; nothing was changed.
  | Moved

type Failure =
  | Refused of Refusal
  /// The store, or authoring, failed; the message says how. Nothing past the failed step happened.
  | Failed of string


// ---------------------------------------------------------------------------------------------
// discard: rewriting a draft
// ---------------------------------------------------------------------------------------------

/// One op in a draft.
type private DraftOp = { id : string; ts : string; op : PT.PackageOp }

/// <param b>'s draft, oldest first, on the given connection. An op this build cannot decode is not in
/// it: nothing here may drop what it could not read.
let private draftOn (conn : SqliteConnection) (b : PT.BranchId) : List<DraftOp> =
  let where, ps = draftWhere b
  let found =
    rows
      conn
      ("SELECT p.id, p.origin_ts, p.op_blob FROM package_ops p WHERE "
       + where
       + " ORDER BY p.origin_ts, p.rowid")
      ps
      (fun r -> (r.GetString 0, textOr r 1, r.GetFieldValue<byte[]> 2))
  found
  |> List.choose (fun (id, ts, blob) ->
    match System.Guid.TryParse id with
    | true, g ->
      BS.PT.PackageOp.tryDeserialize g blob
      |> Option.map (fun op -> { id = id; ts = ts; op = op })
    | _ -> None)

/// Which draft ops a discard keeps.
type DiscardWhat =
  /// The whole draft.
  | All
  /// One name's uncommitted bindings: `SetName`s and the overrides an `undo` writes. The content
  /// stays, since dropping content needs a reachability check.
  | Name of PT.PackageLocation
  /// Superseded namings: per name, keep the binding that wrote the row the name has now.
  | Collapse
  /// Exactly these ops.
  | Ops of List<string>

let private sameLocation (a : PT.PackageLocation) (b : PT.PackageLocation) : bool =
  a.owner = b.owner && modulesOf a = modulesOf b && a.name = b.name

/// Does discarding <param loc> from <param branch>'s draft drop this op: a binding of the name, a
/// change to its reference names, or a rename to or from it. A rename goes whole, so it is never left
/// half-applied. On a branch an `Unbind` of the name goes too.
let private touchesLocation
  (branch : PT.BranchId)
  (loc : PT.PackageLocation)
  (op : PT.PackageOp)
  : bool =
  match op with
  | PT.PackageOp.SetReferenceNames(l, _, _, _) -> sameLocation l loc
  | PT.PackageOp.Rename(oldLocation, newLocation, _, _, _) ->
    sameLocation oldLocation loc || sameLocation newLocation loc
  | _ ->
    PT.packageOpBindingEffects op
    |> List.exists (fun effect ->
      match effect with
      | PT.PackageOp.SetName(l, _, _)
      | PT.PackageOp.Decision(_, l, _, PT.DecisionKind.Override _) ->
        sameLocation l loc
      | PT.PackageOp.Unbind(l, _) -> not branch.IsMain && sameLocation l loc
      | _ -> false)

/// What survives <param what> of <param branch>'s draft, in draft order.
let private kept
  (branch : PT.BranchId)
  (what : DiscardWhat)
  (draft : List<DraftOp>)
  : List<DraftOp> =
  match what with
  // Everything, an undo's `Restore` and a conflict's `Override` alike: each is an op in the draft, and
  // a binding the log no longer holds an op for could never be rebuilt from it.
  | All -> []
  | Name loc -> draft |> List.filter (fun d -> not (touchesLocation branch loc d.op))
  | Ops dropIds ->
    let drop = Set.ofList dropIds
    draft |> List.filter (fun d -> not (Set.contains d.id drop))
  | Collapse ->
    // The naming to KEEP for each name is the one that WROTE the row the name has now. A rebind to
    // the hash a name already holds folds to nothing, so the keeper moves on only when the hash
    // changes: A -> A keeps the first, A -> B -> A the third.
    let key (loc : PT.PackageLocation) =
      loc.owner + "|" + modulesOf loc + "|" + loc.name
    let keeper =
      draft
      |> List.fold
        (fun (acc : Map<string, string * string>) d ->
          match d.op with
          | PT.PackageOp.SetName(loc, target, _) ->
            let k = key loc
            let h = hashText target.hash
            match Map.tryFind k acc with
            | Some(_, standing) when standing = h -> acc
            | _ -> Map.add k (d.id, h) acc
          | _ -> acc)
        Map.empty
    // A `Restore` names the revision it went back to, by op id. Keep that revision so undo can still
    // find its predecessor after the draft is committed.
    let restoredRevisions =
      draft
      |> List.choose (fun d ->
        match d.op with
        | PT.PackageOp.Decision(_, _, _, PT.DecisionKind.Restore(revision, _, _)) ->
          Some(string revision)
        | _ -> None)
      |> Set.ofList
    draft
    |> List.filter (fun d ->
      match d.op with
      | PT.PackageOp.SetName(loc, _, _) ->
        match Map.tryFind (key loc) keeper with
        | Some(keeperId, _) ->
          keeperId = d.id || Set.contains d.id restoredRevisions
        | None -> true
      | _ -> true)

/// Drop what <param what> names from <param branchId>'s draft, in one transaction: the ops and what
/// they wrote. What their draft rows had superseded is live again, since a draft never touches the
/// committed layer underneath it. Returns how many ops went.
let discard (branchId : PT.BranchId) (what : DiscardWhat) : Result<int, Failure> =
  try
    transact (fun conn ->
      match what with
      | Collapse when not branchId.IsMain -> Ok 0
      | _ ->
        let draft = draftOn conn branchId
        let keptIds = kept branchId what draft |> List.map _.id |> Set.ofList
        let dropped =
          draft
          |> List.map _.id
          |> List.filter (fun id -> not (Set.contains id keptIds))
        let n = dropOpsOn conn branchId dropped
        step "discard.ops"
        // Delete the row of each dropped value that never evaluated and that no name
        // points at. Left in the store, it would be evaluated again, and fail again,
        // on every start.
        let droppedValues =
          let droppedIds = Set.ofList dropped
          draft
          |> List.choose (fun d ->
            match d.op with
            | PT.PackageOp.AddValue v when Set.contains d.id droppedIds ->
              Some(hashText v.hash)
            | _ -> None)
        if not (List.isEmpty droppedValues) then
          let _ =
            exec
              conn
              "DELETE FROM package_values
               WHERE rt_dval IS NULL
                 AND hash IN (SELECT value FROM json_each(@hashes))
                 AND NOT EXISTS (SELECT 1 FROM locations WHERE item_hash = package_values.hash)"
              [ "@hashes", box (jsonStrings droppedValues) ]
          ()
        if not branchId.IsMain then
          // Once the branch holds nothing, its fork points describe ops that no longer exist.
          let remaining =
            count
              conn
              "SELECT count(*) FROM package_ops WHERE branch_id = @b"
              [ "@b", box (string branchId) ]
          if remaining = 0L then
            let _ =
              exec
                conn
                "DELETE FROM branch_name_bases WHERE branch_id = @b"
                [ "@b", box (string branchId) ]
            ()
        Ok n)
  with e ->
    Error(Failed e.Message)


// ---------------------------------------------------------------------------------------------
// commit
// ---------------------------------------------------------------------------------------------

type CommitScope =
  /// Everything in the draft.
  | Whole
  /// These ops, if still in the draft. The commit id is minted over the ids as given, sorted, so a
  /// selection hashes the same whatever order it was planned in.
  | Only of List<string>

type Committed = { hash : string; stamped : int64; parent : string }

/// Commit <param scope> of <param branchId>'s draft under <param message>: the commit row and the
/// stamping in ONE transaction, so `log` never shows a commit holding nothing. Refuses when nothing
/// would be stamped, writing nothing.
///
/// <param collapse> drops superseded namings from main's draft first. That is a draft rewrite of its
/// own, finished before the commit starts: stopped between the two, the draft is collapsed and
/// uncommitted, which is a state anyone can commit from.
let commit
  (branchId : PT.BranchId)
  (message : string)
  (scope : CommitScope)
  (collapse : bool)
  : Result<Committed, Failure> =
  if collapse && branchId.IsMain then
    // A collapse that fails is not a reason to refuse the commit: the draft is left uncollapsed
    // and still commits, with every naming in it.
    let _ = discard branchId Collapse
    ()
  try
    transact (fun conn ->
      let where, ps = draftWhere branchId
      let draft =
        ids
          conn
          ("SELECT p.id FROM package_ops p WHERE " + where + " ORDER BY p.id")
          ps
      let selected =
        match scope with
        | Whole -> draft
        | Only selected -> selected
      let parent = parentCommitOn conn branchId
      match commitDraftOpsOn conn branchId message selected with
      | None -> Error(Refused NothingToCommit)
      | Some hash ->
        step "commit.row"
        let stamped =
          count
            conn
            "SELECT count(*) FROM package_ops WHERE branch_id = @b AND commit_hash = @h"
            [ "@b", box (string branchId); "@h", box hash ]
        Ok { hash = hash; stamped = stamped; parent = parent })
  with e ->
    Error(Failed e.Message)


// ---------------------------------------------------------------------------------------------
// branches
// ---------------------------------------------------------------------------------------------

type BranchDone = { id : PT.BranchId; created : bool }

/// The branch NAMED <param name>, started off <param parentId> if no live branch has that name.
let start (name : string) (parentId : PT.BranchId) : BranchDone =
  let (id, created) = Branches.resolveOrCreate name parentId
  { id = id; created = created }

/// Give a branch a new name, unless another branch that is not archived already goes by it: the check
/// and the write in one transaction, so two renames cannot both take a name.
let rename (branchId : PT.BranchId) (name : string) : Result<BranchDone, Failure> =
  if branchId.IsMain then
    Error(Refused IsMain)
  else
    try
      transact (fun conn ->
        let holder =
          ids
            conn
            "SELECT id FROM branches WHERE name = @name AND archived_at IS NULL
             ORDER BY created_at DESC, rowid DESC LIMIT 1"
            [ "@name", box name ]
        match holder with
        | h :: _ when h <> string branchId -> Error(Refused NameTaken)
        | _ ->
          let _ =
            exec
              conn
              "UPDATE branches SET name = @name WHERE id = @id"
              [ "@name", box name; "@id", box (string branchId) ]
          Ok { id = branchId; created = false })
    with e ->
      Error(Failed e.Message)

/// Move THIS process onto <param branchId>, and store it as the branch the next command starts on.
let switchTo (branchId : PT.BranchId) : BranchDone =
  PackageManager.selectBranch branchId
  BranchSelection.remember branchId
  { id = branchId; created = false }

/// ARCHIVE: delete the branch's ops, what they wrote, and its bases, and mark it archived, in ONE
/// transaction. A merged branch's ops are its parent's already, so this only marks it.
let archive (branchId : PT.BranchId) : Result<BranchDone, Failure> =
  if branchId.IsMain then
    Error(Refused IsMain)
  else
    let handle = branchName branchId
    try
      transact (fun conn ->
        let b = [ "@b", box (string branchId) ]
        let _ = exec conn "DELETE FROM package_ops WHERE branch_id = @b" b
        for t in Layers.all do
          let _ = exec conn $"DELETE FROM {t.name} WHERE branch_id = @b" b
          ()
        let _ = exec conn "DELETE FROM branch_name_bases WHERE branch_id = @b" b
        let _ =
          exec
            conn
            "UPDATE branches SET archived_at = datetime('now') WHERE id = @b"
            b
        step "archive.marked"
        let _ =
          markerCommitOn conn PT.BranchId.Main true $"archived branch \"{handle}\""
        Ok { id = branchId; created = false })
    with e ->
      Error(Failed e.Message)


// ---------------------------------------------------------------------------------------------
// merge and rebase
// ---------------------------------------------------------------------------------------------

type Reconciled =
  {
    moved : int64
    conflicts : List<Detect.Conflict>
    /// Names the parent changed that the branch had also changed (rebase).
    diverged : List<string>
    /// The branch's callers of names the parent moved, now pointing at the parent's version (rebase).
    repointed : List<string>
    /// The commit that recorded what a merge into main landed, or "".
    commit : string
    /// Something that went wrong after the action landed, for the person to read, or "". The action
    /// itself is done: a merge whose values then fail to evaluate has still merged.
    warning : string
  }

/// One caller of a name the parent moved, to repoint at the parent's version. Which callers are
/// outdated is a language question, answered in Dark (`SCM.Constraints`) and handed in.
type Repoint =
  { usage : string
    dependency : PT.PackageLocation
    kind : PT.ItemKind
    fromHash : PT.Hash
    toHash : PT.Hash }

/// The ids of <param b>'s draft, as `commit` means it.
let private draftIds (b : PT.BranchId) : List<string> =
  use conn = LibDB.Sqlite.openConnection (LibDB.Sqlite.connString)
  let where, ps = draftWhere b
  ids conn ("SELECT p.id FROM package_ops p WHERE " + where) ps

/// Can this branch merge into its parent: it holds ops, and nothing unmerged branches off it.
/// Conflicts do not gate a merge; they are recorded.
let private gate (branchId : PT.BranchId) : Result<unit, Refusal> =
  if branchId.IsMain then
    Error IsMain
  else
    let own = ScmOps.query (ScmOps.Own branchId) 0L
    if own.total = 0L then
      Error NothingToMerge
    else
      let children = ScmBranches.query (ScmBranches.ChildrenOf(string branchId))
      if List.isEmpty children then Ok() else Error HasActiveChildren

let private ownIdsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  : List<string> =
  ids
    conn
    "SELECT id FROM package_ops WHERE branch_id = @b"
    [ "@b", box (string branchId) ]

/// MERGE <param branchId> into its parent.
///
/// One transaction: into MAIN, commit what is uncommitted under one merge commit (a commit of its own
/// when nothing is); move the branch's ops and commits to the parent and fold the ops into the
/// parent's layers; into a BRANCH, move the bases too; mark it merged; record the conflicts. Values
/// that landed in main are evaluated after the transaction commits.
///
/// First, as a rebase does, the branch's callers of names the parent moved (<param repoints>, which
/// Dark finds) are repointed at the parent's version, each a propagation of its own, so what lands
/// calls what the parent runs. Into main they land in the merge's commit; into a branch they are
/// committed on the branch first, so they do not arrive as the parent's uncommitted work. Stopped
/// before the merge, the branch holds the repoints and merging again carries on from there.
///
/// Conflicts still pending on the branch, which a rebase recorded there, move to the parent with it.
///
/// The gate and the conflicts are read next, and the frontier is read again inside the transaction:
/// if the branch moved in between, nothing is written and the answer says so.
let merge
  (branchId : PT.BranchId)
  (repoints : List<Repoint>)
  (repoint : Repoint -> unit)
  (evaluateValues : unit -> unit)
  : Result<Reconciled, Failure> =
  match gate branchId with
  | Error r -> Error(Refused r)
  | Ok() ->
    let parent = parentOf branchId
    let parentName = branchName parent
    let followed =
      if List.isEmpty repoints then
        Ok()
      else
        let before = draftIds branchId
        for r in repoints do
          repoint r
        if parent.IsMain then
          Ok()
        else
          let after = draftIds branchId
          let before = Set.ofList before
          match after |> List.filter (fun id -> not (Set.contains id before)) with
          | [] -> Ok()
          | added ->
            let c = commit branchId $"followed \"{parentName}\"" (Only added) false
            c |> Result.map (fun (_ : Committed) -> ())
    match followed with
    | Error e -> Error e
    | Ok() ->
      let repointed = repoints |> List.map _.usage |> List.distinct
      let conflicts = preview branchId
      let name = branchName branchId
      let frontier = ScmOps.query (ScmOps.Own branchId) 0L
      let moved =
        use conn = LibDB.Sqlite.openConnection (LibDB.Sqlite.connString)
        ownIdsOn conn branchId
      let b = box (string branchId)
      try
        let result =
          transact (fun conn ->
            let now = ownIdsOn conn branchId
            if Set.ofList now <> Set.ofList moved then
              Error(Refused Moved)
            else
              // Into main, what lands is COMMITTED: the branch's uncommitted work would otherwise
              // read as main's draft, work nobody did here.
              let commitHash =
                if parent.IsMain then
                  let message = $"merged branch \"{name}\""
                  match commitDraftOpsOn conn branchId message moved with
                  | Some h -> Some h
                  | None ->
                    let h = markerCommitOn conn branchId false message
                    Some h
                else
                  None
              step "merge.committed"
              // The branch's history becomes the parent's.
              let _ =
                exec
                  conn
                  "UPDATE commits SET branch_id = @p WHERE branch_id = @b"
                  [ "@p", box (string parent); "@b", b ]
              moveOpsOn conn branchId parent
              step "merge.folded"
              if not parent.IsMain then
                // The bases move with the ops: the parent then carries the child's rebinds, and
                // a name without a base could never conflict again.
                let _ =
                  exec
                    conn
                    "INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
                     SELECT @p, owner, modules, name, base_hash FROM branch_name_bases WHERE branch_id = @b"
                    [ "@p", box (string parent); "@b", b ]
                let _ =
                  exec
                    conn
                    "DELETE FROM branch_name_bases WHERE branch_id = @b"
                    [ "@b", b ]
                ()
              let _ =
                exec
                  conn
                  "UPDATE branches SET merged_at = datetime('now') WHERE id = @b"
                  [ "@b", b ]
              step "merge.marked"
              // Filed against the PARENT: the merge folded into it, and the branch is finished.
              recordConflictsOn conn parent conflicts
              let carried = carryPendingOn conn branchId parent
              Ok
                { moved = frontier.total
                  conflicts = withCarried conflicts carried
                  diverged = []
                  repointed = repointed
                  commit = commitHash |> Option.defaultValue ""
                  warning = "" })
        // After the transaction, and outside its `with`: by now the merge HAS landed, so a value that
        // fails to evaluate is a warning on a success, never a failure the store does not reflect.
        match result with
        | Ok r when parent.IsMain ->
          let evaluated =
            try
              evaluateValues ()
              ""
            with e ->
              "merged, but evaluating the values it landed failed: "
              + e.Message
              + " -- they run on the next reload."
          Ok { r with warning = evaluated }
        | _ -> result
      with e ->
        Error(Failed e.Message)



/// What the parent binds now, as <param branchId> sees beneath itself, for each name the branch
/// recorded a base for. A name bound to nothing there reads "".
let private parentHashesForBases (branchId : PT.BranchId) : Map<string, string> =
  let rows = ScmBindings.query (ScmBindings.AboveBases branchId)
  rows
  |> List.map (fun b -> (b.owner + "\n" + b.modules + "\n" + b.name, b.hash))
  |> Map.ofList

/// A branch's own ops, oldest first, decoded.
let private ownOpsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  : List<string * PT.PackageOp> =
  let found =
    rows
      conn
      "SELECT id, op_blob FROM package_ops WHERE branch_id = @b ORDER BY origin_ts, rowid"
      [ "@b", box (string branchId) ]
      (fun r -> (r.GetString 0, r.GetFieldValue<byte[]> 1))
  found
  |> List.choose (fun (id, blob) ->
    match System.Guid.TryParse id with
    | true, g ->
      BS.PT.PackageOp.tryDeserialize g blob |> Option.map (fun op -> (id, op))
    | _ -> None)

/// The location a conflict is about.
let locationOfConflict (c : Detect.Conflict) : PT.PackageLocation =
  { owner = c.owner
    modules = if c.modules = "" then [] else c.modules.Split('.') |> List.ofArray
    name = c.name }

/// Does <param op> bind or end the name, or change its reference names? A rename counts for both of
/// its names and a restore for its one, so the op goes whole.
let private movesKey
  (owner : string)
  (modules : string)
  (name : string)
  (op : PT.PackageOp)
  : bool =
  let isKey (loc : PT.PackageLocation) =
    loc.owner = owner && modulesOf loc = modules && loc.name = name
  match op with
  | PT.PackageOp.SetReferenceNames(loc, _, _, _) -> isKey loc
  | _ ->
    PT.packageOpBindingEffects op
    |> List.exists (fun effect ->
      match effect with
      | PT.PackageOp.SetName(loc, _, _)
      | PT.PackageOp.Unbind(loc, _)
      | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override _) -> isKey loc
      | _ -> false)

/// The parent's side won <param c>: the version a reconciliation keeps is the local candidate's.
let private parentWon (c : Detect.Conflict) : bool =
  c.candidates
  |> List.exists (fun cand -> cand.side = "local" && cand.hash = c.autoResolvedTo)

let private itemKindOf (itemType : string) : PT.ItemKind =
  match itemType with
  | "type" -> PT.ItemKind.Type
  | "value" -> PT.ItemKind.Value
  | "trait" -> PT.ItemKind.Trait
  | "impl" -> PT.ItemKind.TraitImpl
  | "test" -> PT.ItemKind.Test
  | _ -> PT.ItemKind.Fn

/// REBASE <param branchId> onto its parent: accept the parent's current state as its new base.
///
/// Reads first: the conflicts the reconciliation records, and which names the parent moved since the
/// fork (both answered against the store as it stands). Then each outdated caller is repointed, a
/// propagation per caller, each a complete edit of its own. Then ONE transaction moves every base,
/// takes the parent's version of every name the parent's newer edit won (the branch's own ops on it
/// leave the branch, so the branch runs what the record says was kept, and the record keeps the
/// branch's version for an override), and records the conflicts. Stopped before that transaction, the
/// bases still say the old fork point, so running the rebase again redoes what is left and finishes.
/// Last, the branch's callers of a version that lost follow the winner, each a propagation; stopped
/// before those, a re-run finds them outdated and repoints them.
let rebase
  (branchId : PT.BranchId)
  (repoints : List<Repoint>)
  (repoint : Repoint -> unit)
  : Result<Reconciled, Failure> =
  if branchId.IsMain then
    Error(Refused IsMain)
  else
    let conflicts = preview branchId
    let bases = nameBases branchId
    let parentHashes = parentHashesForBases branchId
    let current (nb : Detect.NameBase) =
      Map.tryFind (nb.owner + "\n" + nb.modules + "\n" + nb.name) parentHashes
      |> Option.defaultValue ""
    let diverged =
      bases
      |> List.filter (fun nb -> current nb <> nb.baseHash)
      |> List.map (fun nb -> Detect.fqn nb.owner nb.modules nb.name)
    for r in repoints do
      repoint r
    let repointed = repoints |> List.map _.usage |> List.distinct
    try
      let result =
        transact (fun conn ->
          step "rebase.repointed"
          for nb in bases do
            let _ =
              exec
                conn
                "UPDATE branch_name_bases SET base_hash = @h
                 WHERE branch_id = @b AND owner = @o AND modules = @m AND name = @n"
                [ "@h", box (current nb)
                  "@b", box (string branchId)
                  "@o", box nb.owner
                  "@m", box nb.modules
                  "@n", box nb.name ]
            ()
          step "rebase.bases"
          let own = ownOpsOn conn branchId
          let taken =
            conflicts
            |> List.filter parentWon
            |> List.collect (fun c ->
              own
              |> List.filter (fun (_, op) -> movesKey c.owner c.modules c.name op)
              |> List.map fst)
          let _ = dropOpsOn conn branchId (List.distinct taken)
          step "rebase.took-parents"
          // The mirror of merge: a rebase folds the PARENT's work into the branch, so the
          // conflict is the branch's to answer.
          recordConflictsOn conn branchId conflicts
          Ok
            { moved = 0L
              conflicts = conflicts
              diverged = diverged
              repointed = repointed
              commit = ""
              warning = "" })
      match result with
      | Ok _ ->
        // The callers of a version that lost now follow the one that won, the way an override's
        // callers do. A removal that won leaves nothing to follow.
        for c in conflicts |> List.filter parentWon do
          let lost =
            c.candidates
            |> List.tryFind (fun cand -> cand.side = "incoming" && cand.hash <> "")
          match lost with
          | Some l when c.autoResolvedTo <> "" ->
            repoint
              { usage = ""
                dependency = locationOfConflict c
                kind = itemKindOf c.itemType
                fromHash = PT.Hash l.hash
                toHash = PT.Hash c.autoResolvedTo }
          | _ -> ()
      | Error _ -> ()
      result
    with e ->
      Error(Failed e.Message)


// ---------------------------------------------------------------------------------------------
// resolving conflicts
// ---------------------------------------------------------------------------------------------

/// The base a branch should record for a name once it agrees with its parent: the parent's hash now.
let private setBaseToParentOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  (owner : string)
  (modules : string)
  (name : string)
  : unit =
  let hashes = parentHashesForBases branchId
  let current =
    Map.tryFind (owner + "\n" + modules + "\n" + name) hashes
    |> Option.defaultValue ""
  let _ =
    exec
      conn
      "UPDATE branch_name_bases SET base_hash = @h
       WHERE branch_id = @b AND owner = @o AND modules = @m AND name = @n"
      [ "@h", box current
        "@b", box (string branchId)
        "@o", box owner
        "@m", box modules
        "@n", box name ]
  ()

let private bindsKey
  (owner : string)
  (modules : string)
  (name : string)
  (op : PT.PackageOp)
  =
  PT.packageOpBindingEffects op
  |> List.choose (fun effect ->
    match effect with
    | PT.PackageOp.SetName(loc, target, _)
    | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target) when
      loc.owner = owner && modulesOf loc = modules && loc.name = name
      ->
      Some(loc, target)
    | _ -> None)
  |> List.tryLast

/// KEEP MINE: author an OVERRIDE re-stating the branch's binding of the name with a stamp past
/// everything in the log, so it wins on merge, and move the name's base to the parent's current hash.
/// One transaction.
///
/// A NEW op, never a re-stamp of the existing one: `origin_ts` travels with an op and is what every
/// machine compares, so changing it locally makes two peers settle the same pair differently.
let keepMine
  (branchId : PT.BranchId)
  (owner : string)
  (modules : string)
  (name : string)
  : Result<unit, Failure> =
  try
    transact (fun conn ->
      let own = ownOpsOn conn branchId
      let bound =
        own
        |> List.choose (fun (_, op) -> bindsKey owner modules name op)
        |> List.tryLast
      match bound with
      | None -> Error(Refused NotBound)
      | Some(location, target) ->
        let stamp = stampPastEverythingOn conn
        // Keyed on the name AND the stamp: resolving twice records two decisions rather than
        // deduping into one that then loses to whatever came between them.
        let op =
          PT.PackageOp.Decision(
            $"keep-mine:{owner}.{modules}.{name}:{stamp}",
            location,
            "kept this branch's version when resolving a conflict",
            PT.DecisionKind.Override target
          )
        OriginTs.observe stamp
        let ctx = PreparedBatch.newCtx conn
        try
          let _ =
            Inserts.storeAndFoldOn
              ctx
              branchId
              "resolution"
              (fun _ -> stamp)
              (fun _ -> None)
              [ op ]
          ()
        finally
          PreparedBatch.disposeCtx ctx
        step "resolve.stored"
        setBaseToParentOn conn branchId owner modules name
        Ok())
  with e ->
    Error(Failed e.Message)

/// TAKE THEIRS: drop every op of the branch's that binds the name, so the branch's view falls back to the
/// parent's version, and move the name's base to the parent's. One transaction.
let takeTheirs
  (branchId : PT.BranchId)
  (owner : string)
  (modules : string)
  (name : string)
  : Result<unit, Failure> =
  try
    transact (fun conn ->
      let own = ownOpsOn conn branchId
      let binding =
        own
        |> List.filter (fun (_, op) ->
          match op with
          | PT.PackageOp.SetReferenceNames(loc, _, _, _) ->
            loc.owner = owner && modulesOf loc = modules && loc.name = name
          | _ -> Option.isSome (bindsKey owner modules name op))
        |> List.map fst
      match binding with
      | [] -> Error(Refused NotBound)
      | opIds ->
        let _ = dropOpsOn conn branchId opIds
        step "resolve.untagged"
        setBaseToParentOn conn branchId owner modules name
        Ok())
  with e ->
    Error(Failed e.Message)

/// Close a conflict record: 'acked', 'overridden' or 'superseded', with what settled it.
let setConflictStatus
  (id : string)
  (status : string)
  (resolvedBy : string)
  : Result<unit, Failure> =
  try
    transact (fun conn ->
      let n =
        exec
          conn
          "UPDATE conflicts SET status = @status, resolved_by = @by WHERE id = @id"
          [ "@status", box status; "@by", box resolvedBy; "@id", box id ]
      step "conflict.status"
      if n = 0 then Error(Refused NoSuchConflict) else Ok())
  with e ->
    Error(Failed e.Message)

/// One recorded conflict by its exact id.
let conflictById (id : string) : Option<Detect.Conflict> =
  use conn = LibDB.Sqlite.openConnection (LibDB.Sqlite.connString)
  let found = conflictsOn conn "id = @id" [ "@id", box id ]
  List.tryHead found

/// The part of a declaration a doc conflict names, as `DocPart.fromKey` spells it.
let docPartOfKey (key : string) : Option<PT.DocPart> = Docs.partFromKey key

/// The hash a doc's text is known by.
let hashOfText (text : string) : PT.Hash =
  PT.Hash(LibExecution.Bytes.sha256Hex (System.Text.Encoding.UTF8.GetBytes text))
