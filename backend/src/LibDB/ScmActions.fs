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
open FSharp.Control.Tasks

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
  let cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  for (n, v) in ps do
    cmd.Parameters.AddWithValue(n, (if isNull v then box System.DBNull.Value else v))
    |> ignore<SqliteParameter>
  cmd

let private exec
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<Param>)
  : Ply<int> =
  uply {
    use cmd = command conn sql ps
    return! cmd.ExecuteNonQueryAsync()
  }

let private rows
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<Param>)
  (read : SqliteDataReader -> 'a)
  : Ply<List<'a>> =
  uply {
    use cmd = command conn sql ps
    let! opened = cmd.ExecuteReaderAsync()
    use reader = opened
    let out = ResizeArray<'a>()
    let mutable more = true
    while more do
      let! has = reader.ReadAsync()
      if has then out.Add(read reader) else more <- false
    return List.ofSeq out
  }

let private textOr (r : SqliteDataReader) (i : int) : string =
  if r.IsDBNull i then "" else r.GetString i

let private ids
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<Param>)
  : Ply<List<string>> =
  uply {
    let! found = rows conn sql ps (fun r -> textOr r 0)
    return found |> List.filter (fun s -> s <> "")
  }

let private count
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<Param>)
  : Ply<int64> =
  uply {
    let! found = rows conn sql ps (fun r -> r.GetInt64 0)
    return List.tryHead found |> Option.defaultValue 0L
  }

/// Run <f> in ONE transaction on its own connection. It commits when <f> answers `Ok`, and rolls back
/// on `Error` or a raise, so a refusal found partway leaves nothing behind either. The caches are
/// dropped after a commit, since every action here writes something they are built from.
let private transact
  (f : SqliteConnection -> Ply<Result<'a, 'e>>)
  : Ply<Result<'a, 'e>> =
  uply {
    let! result =
      uply {
        use conn = new SqliteConnection(LibDB.Sqlite.connString)
        do! conn.OpenAsync()
        // IMMEDIATE (the default here): the write lock is taken at BEGIN, so what the action reads
        // inside is what it writes against.
        use tx = conn.BeginTransaction()
        let! r = f conn
        match r with
        | Ok _ -> tx.Commit()
        | Error _ -> tx.Rollback()
        return r
      }
    Caching.invalidateAll ()
    return result
  }


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
let private parentCommitOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  : Ply<string> =
  uply {
    let! found =
      ids
        conn
        (Layers.cte "@"
         + " SELECT hash FROM commits WHERE branch_id IN (SELECT bid FROM chain)
             ORDER BY origin_ts DESC, rowid DESC LIMIT 1")
        (viewParams b true)
    return List.tryHead found |> Option.defaultValue ""
  }

/// The next commit stamp: the log's latest `origin_ts` plus a second, so a commit sorts after what it
/// holds whatever the wall clock says.
let private nextStampOn (conn : SqliteConnection) : Ply<string> =
  uply {
    let! found =
      ids
        conn
        "SELECT COALESCE(strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second'),
                strftime('%Y-%m-%dT%H:%M:%fZ','now')) FROM package_ops"
        []
    return List.tryHead found |> Option.defaultValue ""
  }

/// A stamp past everything in the log, or "" for an empty one.
let private stampPastEverythingOn (conn : SqliteConnection) : Ply<string> =
  uply {
    let! found =
      ids
        conn
        "SELECT strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second') FROM package_ops"
        []
    return List.tryHead found |> Option.defaultValue ""
  }

let private sha256Hex (s : string) : string =
  System.Security.Cryptography.SHA256.HashData(System.Text.Encoding.UTF8.GetBytes s)
  |> System.Convert.ToHexString
  |> fun h -> h.ToLowerInvariant()

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
  (sha256Hex material).Substring(0, 16)

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
  : Ply<unit> =
  uply {
    let! _ =
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
    return ()
  }

/// A commit on <param b> that holds no ops: what a merge into main, or an archive, records.
let private markerCommitOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (housekeeping : bool)
  (message : string)
  : Ply<string> =
  uply {
    let! parent = parentCommitOn conn b
    let! stamp = nextStampOn conn
    let hash = mintCommitHash message stamp [] parent
    do! insertCommit conn b housekeeping hash message stamp parent
    return hash
  }

/// Stamp <param b>'s uncommitted ops among <param opIds> with a new commit under <param message>, and
/// move what they wrote to the branch's committed layer. None when none of them is uncommitted.
let private commitDraftOpsOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (message : string)
  (opIds : List<string>)
  : Ply<Option<string>> =
  uply {
    let! draft =
      ids
        conn
        "SELECT id FROM package_ops WHERE branch_id = @b AND commit_hash IS NULL"
        [ "@b", box (string b) ]
    let draft = Set.ofList draft
    let toStamp =
      opIds |> List.filter (fun id -> Set.contains id draft) |> List.distinct
    if List.isEmpty toStamp then
      return None
    else
      let! parent = parentCommitOn conn b
      let! stamp = nextStampOn conn
      let hash = mintCommitHash message stamp (sortOrdinal toStamp) parent
      do! insertCommit conn b false hash message stamp parent
      let! _ =
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
        do!
          Layers.commitRowsOf
            ctx
            b
            "op_id IN (SELECT id FROM package_ops WHERE branch_id = $branch AND commit_hash = $hash)"
            (fun cmd -> PreparedBatch.p cmd "$hash" hash)
      finally
        PreparedBatch.disposeCtx ctx
      return Some hash
  }

/// Delete what <param opIds> are on <param b>: their rows in the layered projections, then the ops.
/// Returns how many ops went. Content is shared and stays.
let private dropOpsOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (opIds : List<string>)
  : Ply<int> =
  uply {
    if List.isEmpty opIds then
      return 0
    else
      let ctx = PreparedBatch.newCtx conn
      try
        do!
          Layers.dropRowsOf
            ctx
            b
            "op_id IN (SELECT value FROM json_each($ids))"
            (fun cmd -> PreparedBatch.p cmd "$ids" (jsonStrings opIds))
      finally
        PreparedBatch.disposeCtx ctx
      step "drop.rows"
      return!
        exec
          conn
          "DELETE FROM package_ops WHERE branch_id = @b AND id IN (SELECT value FROM json_each(@ids))"
          [ "@b", box (string b); "@ids", box (jsonStrings opIds) ]
  }

/// Move every op of <param b> onto <param target> and fold them there, in authoring order: what a
/// merge does. The branch's rows in the layered projections go; the ops write the target's.
/// An op the target already holds stays the target's and is not folded again; the branch's copy goes.
let private moveOpsOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  (target : PT.BranchId)
  : Ply<unit> =
  uply {
    let! moving =
      rows
        conn
        "SELECT id, op_blob FROM package_ops WHERE branch_id = @b ORDER BY origin_ts, rowid"
        [ "@b", box (string b) ]
        (fun r -> (r.GetString 0, r.GetFieldValue<byte[]> 1))
    let! already =
      ids
        conn
        "SELECT id FROM package_ops WHERE branch_id = @t AND id IN (SELECT value FROM json_each(@ids))"
        [ "@t", box (string target)
          "@ids", box (jsonStrings (List.map fst moving)) ]
    let already = Set.ofList already
    let bt = [ "@b", box (string b); "@t", box (string target) ]
    let! _ =
      exec
        conn
        "DELETE FROM package_ops
         WHERE branch_id = @b AND id IN (SELECT id FROM package_ops WHERE branch_id = @t)"
        bt
    let! _ =
      exec conn "UPDATE package_ops SET branch_id = @t WHERE branch_id = @b" bt
    for t in Layers.all do
      let! _ = exec conn $"DELETE FROM {t.name} WHERE branch_id = @b" bt
      ()
    step "merge.moved"
    let ops =
      moving
      |> List.choose (fun (id, blob) ->
        match System.Guid.TryParse id with
        | true, g when not (Set.contains id already) ->
          BS.PT.PackageOp.tryDeserialize g blob
        | _ -> None)
    do! PackageOpPlayback.applyOpsOnConnection conn target ops
  }


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
let private branchWords (b : PT.BranchId) : Ply<List<Word>> =
  uply {
    let! rows = ScmBindings.query (ScmBindings.Own b)
    return rows |> List.map wordOf
  }

/// What the PARENT binds for these bare names: the local side a branch merges into.
let private parentBindings
  (parent : PT.BranchId)
  (names : List<string>)
  : Ply<List<Detect.Binding>> =
  uply {
    let! rows =
      ScmBindings.query (ScmBindings.ByBareNames(parent, List.distinct names))
    return rows |> List.map bindingOfRow
  }

/// The name a person reads for a branch: "main", its name, or its id when it has none.
let branchName (b : PT.BranchId) : Ply<string> =
  uply {
    if b.IsMain then
      return PT.BranchId.MainName
    else
      let! found = ScmBranches.query (ScmBranches.ById(string b))
      match found with
      | info :: _ when info.name <> "" -> return info.name
      | _ -> return string b
  }

/// A branch's registered parent; main when it has none, or when the stored id does not parse.
let parentOf (b : PT.BranchId) : Ply<PT.BranchId> =
  uply {
    let! found = ScmBranches.query (ScmBranches.ById(string b))
    match found with
    | info :: _ ->
      return PT.BranchId.Parse info.parentId |> Option.defaultValue PT.BranchId.Main
    | [] -> return PT.BranchId.Main
  }

let private nameBases (b : PT.BranchId) : Ply<List<Detect.NameBase>> =
  uply {
    let! found = ScmBranches.query (ScmBranches.NameBasesOf(string b))
    return
      found
      |> List.collect (fun i -> i.nameBases)
      |> List.map (fun nb ->
        ({ Detect.NameBase.owner = nb.owner
           modules = nb.modules
           name = nb.name
           baseHash = nb.baseHash }
        : Detect.NameBase))
  }

/// The conflicts a merge of <param b> into its parent WOULD record: the branch's own bindings against
/// the parent's, tested against the bases the branch recorded. Records nothing.
///
/// Removals count both ways. A name the branch ended that the parent has since edited, and a name the
/// parent's chain ended that the branch has since edited, are each a name both sides moved; whichever
/// is newer wins, and the other is recorded like any loser. (Main's own removals carry no stamp in
/// what this reads, so a name main ended is not among them.)
let preview (b : PT.BranchId) : Ply<List<Detect.Conflict>> =
  uply {
    let! words = branchWords b
    let! bases = nameBases b
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
    let! parent = parentOf b
    let! local =
      parentBindings
        parent
        ((incoming |> List.map _.name) @ (removals |> List.map _.name))
    let! parentEnded =
      uply {
        if parent.IsMain then
          return []
        else
          let! rows = ScmBindings.query (ScmBindings.RemovedOffMain parent)
          return rows |> List.map removalOfRow
      }
    let! parentName = branchName parent
    let! name = branchName b
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
    return
      Detect.detect parentName name local incoming bases
      @ removed
      @ Detect.detectEndedLocally parentName name parentEnded incoming bases
  }

/// Record <param conflicts> against <param branchId>, on the caller's connection. An id already
/// recorded is left as it is, unless it is still pending on another branch, in which case it moves
/// here: a rebase records the divergence on the branch, and the merge after it re-detects the same pair
/// against the parent.
let private recordConflictsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  (conflicts : List<Detect.Conflict>)
  : Ply<unit> =
  uply {
    for c in conflicts do
      let! _ =
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
      let! _ =
        exec
          conn
          "UPDATE conflicts SET branch_id = @branch WHERE id = @id AND status = 'pending'"
          [ "@branch", box (string branchId); "@id", box c.id ]
      ()
  }

/// Recorded conflicts matching <param where>, on the caller's connection.
let private conflictsOn
  (conn : SqliteConnection)
  (where : string)
  (ps : List<Param>)
  : Ply<List<Detect.Conflict>> =
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
  : Ply<List<Detect.Conflict>> =
  uply {
    let b = box (string branchId)
    let! pending =
      conflictsOn conn "branch_id = @b AND status = 'pending'" [ "@b", b ]
    let! _ =
      exec
        conn
        "UPDATE conflicts SET branch_id = @p WHERE branch_id = @b AND status = 'pending'"
        [ "@p", box (string parent); "@b", b ]
    return pending
  }

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
  : Task<unit> =
  uply {
    let! _ =
      transact (fun conn ->
        uply {
          do! recordConflictsOn conn branchId conflicts
          return (Ok() : Result<unit, string>)
        })
    return ()
  }
  |> Ply.toTask


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
let private draftOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  : Ply<List<DraftOp>> =
  uply {
    let where, ps = draftWhere b
    let! found =
      rows
        conn
        ("SELECT p.id, p.origin_ts, p.op_blob FROM package_ops p WHERE "
         + where
         + " ORDER BY p.origin_ts, p.rowid")
        ps
        (fun r -> (r.GetString 0, textOr r 1, r.GetFieldValue<byte[]> 2))
    return
      found
      |> List.choose (fun (id, ts, blob) ->
        match System.Guid.TryParse id with
        | true, g ->
          BS.PT.PackageOp.tryDeserialize g blob
          |> Option.map (fun op -> { id = id; ts = ts; op = op })
        | _ -> None)
  }

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
let discard
  (branchId : PT.BranchId)
  (what : DiscardWhat)
  : Task<Result<int, Failure>> =
  uply {
    try
      return!
        transact (fun conn ->
          uply {
            match what with
            | Collapse when not branchId.IsMain -> return Ok 0
            | _ ->
              let! draft = draftOn conn branchId
              let keptIds = kept branchId what draft |> List.map _.id |> Set.ofList
              let dropped =
                draft
                |> List.map _.id
                |> List.filter (fun id -> not (Set.contains id keptIds))
              let! n = dropOpsOn conn branchId dropped
              step "discard.ops"
              if not branchId.IsMain then
                // Once the branch holds nothing, its fork points describe ops that no longer exist.
                let! remaining =
                  count
                    conn
                    "SELECT count(*) FROM package_ops WHERE branch_id = @b"
                    [ "@b", box (string branchId) ]
                if remaining = 0L then
                  let! _ =
                    exec
                      conn
                      "DELETE FROM branch_name_bases WHERE branch_id = @b"
                      [ "@b", box (string branchId) ]
                  ()
              return Ok n
          })
    with e ->
      return Error(Failed e.Message)
  }
  |> Ply.toTask


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
  : Task<Result<Committed, Failure>> =
  uply {
    if collapse && branchId.IsMain then
      // A collapse that fails is not a reason to refuse the commit: the draft is left uncollapsed
      // and still commits, with every naming in it.
      let! _ = discard branchId Collapse
      ()
    try
      return!
        transact (fun conn ->
          uply {
            let where, ps = draftWhere branchId
            let! draft =
              ids
                conn
                ("SELECT p.id FROM package_ops p WHERE " + where + " ORDER BY p.id")
                ps
            let selected =
              match scope with
              | Whole -> draft
              | Only selected -> selected
            let! parent = parentCommitOn conn branchId
            match! commitDraftOpsOn conn branchId message selected with
            | None -> return Error(Refused NothingToCommit)
            | Some hash ->
              step "commit.row"
              let! stamped =
                count
                  conn
                  "SELECT count(*) FROM package_ops WHERE branch_id = @b AND commit_hash = @h"
                  [ "@b", box (string branchId); "@h", box hash ]
              return Ok { hash = hash; stamped = stamped; parent = parent }
          })
    with e ->
      return Error(Failed e.Message)
  }
  |> Ply.toTask


// ---------------------------------------------------------------------------------------------
// branches
// ---------------------------------------------------------------------------------------------

type BranchDone = { id : PT.BranchId; created : bool }

/// The branch NAMED <param name>, started off <param parentId> if no live branch has that name.
let start (name : string) (parentId : PT.BranchId) : Task<BranchDone> =
  task {
    let! (id, created) = Branches.resolveOrCreate name parentId
    return { id = id; created = created }
  }

/// Give a branch a new name, unless another branch that is not archived already goes by it: the check
/// and the write in one transaction, so two renames cannot both take a name.
let rename
  (branchId : PT.BranchId)
  (name : string)
  : Task<Result<BranchDone, Failure>> =
  uply {
    if branchId.IsMain then
      return Error(Refused IsMain)
    else
      try
        return!
          transact (fun conn ->
            uply {
              let! holder =
                ids
                  conn
                  "SELECT id FROM branches WHERE name = @name AND archived_at IS NULL
                   ORDER BY created_at DESC, rowid DESC LIMIT 1"
                  [ "@name", box name ]
              match holder with
              | h :: _ when h <> string branchId -> return Error(Refused NameTaken)
              | _ ->
                let! _ =
                  exec
                    conn
                    "UPDATE branches SET name = @name WHERE id = @id"
                    [ "@name", box name; "@id", box (string branchId) ]
                return Ok { id = branchId; created = false }
            })
      with e ->
        return Error(Failed e.Message)
  }
  |> Ply.toTask

/// Move THIS process onto <param branchId>, and store it as the branch the next command starts on.
let switchTo (branchId : PT.BranchId) : Task<BranchDone> =
  task {
    PackageManager.selectBranch branchId
    do! BranchSelection.remember branchId
    return { id = branchId; created = false }
  }

/// ARCHIVE: delete the branch's ops, what they wrote, and its bases, and mark it archived, in ONE
/// transaction. A merged branch's ops are its parent's already, so this only marks it.
let archive (branchId : PT.BranchId) : Task<Result<BranchDone, Failure>> =
  uply {
    if branchId.IsMain then
      return Error(Refused IsMain)
    else
      let! handle = branchName branchId
      try
        return!
          transact (fun conn ->
            uply {
              let b = [ "@b", box (string branchId) ]
              let! _ = exec conn "DELETE FROM package_ops WHERE branch_id = @b" b
              for t in Layers.all do
                let! _ = exec conn $"DELETE FROM {t.name} WHERE branch_id = @b" b
                ()
              let! _ =
                exec conn "DELETE FROM branch_name_bases WHERE branch_id = @b" b
              let! _ =
                exec
                  conn
                  "UPDATE branches SET archived_at = datetime('now') WHERE id = @b"
                  b
              step "archive.marked"
              let! _ =
                markerCommitOn
                  conn
                  PT.BranchId.Main
                  true
                  $"archived branch \"{handle}\""
              return Ok { id = branchId; created = false }
            })
      with e ->
        return Error(Failed e.Message)
  }
  |> Ply.toTask


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
let private draftIds (b : PT.BranchId) : Ply<List<string>> =
  uply {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    let where, ps = draftWhere b
    return! ids conn ("SELECT p.id FROM package_ops p WHERE " + where) ps
  }

/// Can this branch merge into its parent: it holds ops, and nothing unmerged branches off it.
/// Conflicts do not gate a merge; they are recorded.
let private gate (branchId : PT.BranchId) : Ply<Result<unit, Refusal>> =
  uply {
    if branchId.IsMain then
      return Error IsMain
    else
      let! own = ScmOps.query (ScmOps.Own branchId) 0L
      if own.total = 0L then
        return Error NothingToMerge
      else
        let! children = ScmBranches.query (ScmBranches.ChildrenOf(string branchId))
        if List.isEmpty children then return Ok() else return Error HasActiveChildren
  }

let private ownIdsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  : Ply<List<string>> =
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
  (repoint : Repoint -> Task<unit>)
  (evaluateValues : unit -> Task<unit>)
  : Task<Result<Reconciled, Failure>> =
  uply {
    match! gate branchId with
    | Error r -> return Error(Refused r)
    | Ok() ->
      let! parent = parentOf branchId
      let! parentName = branchName parent
      let! followed =
        uply {
          if List.isEmpty repoints then
            return Ok()
          else
            let! before = draftIds branchId
            for r in repoints do
              do! repoint r
            if parent.IsMain then
              return Ok()
            else
              let! after = draftIds branchId
              let before = Set.ofList before
              match
                after |> List.filter (fun id -> not (Set.contains id before))
              with
              | [] -> return Ok()
              | added ->
                let! c =
                  commit branchId $"followed \"{parentName}\"" (Only added) false
                return c |> Result.map (fun (_ : Committed) -> ())
        }
      match followed with
      | Error e -> return Error e
      | Ok() ->
        let repointed = repoints |> List.map _.usage |> List.distinct
        let! conflicts = preview branchId
        let! name = branchName branchId
        let! frontier = ScmOps.query (ScmOps.Own branchId) 0L
        let! moved =
          uply {
            use conn = new SqliteConnection(LibDB.Sqlite.connString)
            do! conn.OpenAsync()
            return! ownIdsOn conn branchId
          }
        let b = box (string branchId)
        try
          let! result =
            transact (fun conn ->
              uply {
                let! now = ownIdsOn conn branchId
                if Set.ofList now <> Set.ofList moved then
                  return Error(Refused Moved)
                else
                  // Into main, what lands is COMMITTED: the branch's uncommitted work would otherwise
                  // read as main's draft, work nobody did here.
                  let! commitHash =
                    uply {
                      if parent.IsMain then
                        let message = $"merged branch \"{name}\""
                        match! commitDraftOpsOn conn branchId message moved with
                        | Some h -> return Some h
                        | None ->
                          let! h = markerCommitOn conn branchId false message
                          return Some h
                      else
                        return None
                    }
                  step "merge.committed"
                  // The branch's history becomes the parent's.
                  let! _ =
                    exec
                      conn
                      "UPDATE commits SET branch_id = @p WHERE branch_id = @b"
                      [ "@p", box (string parent); "@b", b ]
                  do! moveOpsOn conn branchId parent
                  step "merge.folded"
                  if not parent.IsMain then
                    // The bases move with the ops: the parent then carries the child's rebinds, and
                    // a name without a base could never conflict again.
                    let! _ =
                      exec
                        conn
                        "INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
                         SELECT @p, owner, modules, name, base_hash FROM branch_name_bases WHERE branch_id = @b"
                        [ "@p", box (string parent); "@b", b ]
                    let! _ =
                      exec
                        conn
                        "DELETE FROM branch_name_bases WHERE branch_id = @b"
                        [ "@b", b ]
                    ()
                  let! _ =
                    exec
                      conn
                      "UPDATE branches SET merged_at = datetime('now') WHERE id = @b"
                      [ "@b", b ]
                  step "merge.marked"
                  // Filed against the PARENT: the merge folded into it, and the branch is finished.
                  do! recordConflictsOn conn parent conflicts
                  let! carried = carryPendingOn conn branchId parent
                  return
                    Ok
                      { moved = frontier.total
                        conflicts = withCarried conflicts carried
                        diverged = []
                        repointed = repointed
                        commit = commitHash |> Option.defaultValue ""
                        warning = "" }
              })
          // After the transaction, and outside its `with`: by now the merge HAS landed, so a value that
          // fails to evaluate is a warning on a success, never a failure the store does not reflect.
          match result with
          | Ok r when parent.IsMain ->
            let! evaluated =
              uply {
                try
                  do! evaluateValues ()
                  return ""
                with e ->
                  return
                    "merged, but evaluating the values it landed failed: "
                    + e.Message
                    + " -- they run on the next reload."
              }
            return Ok { r with warning = evaluated }
          | _ -> return result
        with e ->
          return Error(Failed e.Message)
  }
  |> Ply.toTask



/// What the parent binds now, as <param branchId> sees beneath itself, for each name the branch
/// recorded a base for. A name bound to nothing there reads "".
let private parentHashesForBases
  (branchId : PT.BranchId)
  : Ply<Map<string, string>> =
  uply {
    let! rows = ScmBindings.query (ScmBindings.AboveBases branchId)
    return
      rows
      |> List.map (fun b -> (b.owner + "\n" + b.modules + "\n" + b.name, b.hash))
      |> Map.ofList
  }

/// A branch's own ops, oldest first, decoded.
let private ownOpsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  : Ply<List<string * PT.PackageOp>> =
  uply {
    let! found =
      rows
        conn
        "SELECT id, op_blob FROM package_ops WHERE branch_id = @b ORDER BY origin_ts, rowid"
        [ "@b", box (string branchId) ]
        (fun r -> (r.GetString 0, r.GetFieldValue<byte[]> 1))
    return
      found
      |> List.choose (fun (id, blob) ->
        match System.Guid.TryParse id with
        | true, g ->
          BS.PT.PackageOp.tryDeserialize g blob |> Option.map (fun op -> (id, op))
        | _ -> None)
  }

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
  (repoint : Repoint -> Task<unit>)
  : Task<Result<Reconciled, Failure>> =
  uply {
    if branchId.IsMain then
      return Error(Refused IsMain)
    else
      let! conflicts = preview branchId
      let! bases = nameBases branchId
      let! parentHashes = parentHashesForBases branchId
      let current (nb : Detect.NameBase) =
        Map.tryFind (nb.owner + "\n" + nb.modules + "\n" + nb.name) parentHashes
        |> Option.defaultValue ""
      let diverged =
        bases
        |> List.filter (fun nb -> current nb <> nb.baseHash)
        |> List.map (fun nb -> Detect.fqn nb.owner nb.modules nb.name)
      for r in repoints do
        do! repoint r
      let repointed = repoints |> List.map _.usage |> List.distinct
      try
        let! result =
          transact (fun conn ->
            uply {
              step "rebase.repointed"
              for nb in bases do
                let! _ =
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
              let! own = ownOpsOn conn branchId
              let taken =
                conflicts
                |> List.filter parentWon
                |> List.collect (fun c ->
                  own
                  |> List.filter (fun (_, op) ->
                    movesKey c.owner c.modules c.name op)
                  |> List.map fst)
              let! _ = dropOpsOn conn branchId (List.distinct taken)
              step "rebase.took-parents"
              // The mirror of merge: a rebase folds the PARENT's work into the branch, so the
              // conflict is the branch's to answer.
              do! recordConflictsOn conn branchId conflicts
              return
                Ok
                  { moved = 0L
                    conflicts = conflicts
                    diverged = diverged
                    repointed = repointed
                    commit = ""
                    warning = "" }
            })
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
              do!
                repoint
                  { usage = ""
                    dependency = locationOfConflict c
                    kind = itemKindOf c.itemType
                    fromHash = PT.Hash l.hash
                    toHash = PT.Hash c.autoResolvedTo }
            | _ -> ()
        | Error _ -> ()
        return result
      with e ->
        return Error(Failed e.Message)
  }
  |> Ply.toTask


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
  : Ply<unit> =
  uply {
    let! hashes = parentHashesForBases branchId
    let current =
      Map.tryFind (owner + "\n" + modules + "\n" + name) hashes
      |> Option.defaultValue ""
    let! _ =
      exec
        conn
        "UPDATE branch_name_bases SET base_hash = @h
         WHERE branch_id = @b AND owner = @o AND modules = @m AND name = @n"
        [ "@h", box current
          "@b", box (string branchId)
          "@o", box owner
          "@m", box modules
          "@n", box name ]
    return ()
  }

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
  : Task<Result<unit, Failure>> =
  uply {
    try
      return!
        transact (fun conn ->
          uply {
            let! own = ownOpsOn conn branchId
            let bound =
              own
              |> List.choose (fun (_, op) -> bindsKey owner modules name op)
              |> List.tryLast
            match bound with
            | None -> return Error(Refused NotBound)
            | Some(location, target) ->
              let! stamp = stampPastEverythingOn conn
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
                let! _ =
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
              do! setBaseToParentOn conn branchId owner modules name
              return Ok()
          })
    with e ->
      return Error(Failed e.Message)
  }
  |> Ply.toTask

/// TAKE THEIRS: drop every op of the branch's that binds the name, so the branch's view falls back to the
/// parent's version, and move the name's base to the parent's. One transaction.
let takeTheirs
  (branchId : PT.BranchId)
  (owner : string)
  (modules : string)
  (name : string)
  : Task<Result<unit, Failure>> =
  uply {
    try
      return!
        transact (fun conn ->
          uply {
            let! own = ownOpsOn conn branchId
            let binding =
              own
              |> List.filter (fun (_, op) ->
                match op with
                | PT.PackageOp.SetReferenceNames(loc, _, _, _) ->
                  loc.owner = owner && modulesOf loc = modules && loc.name = name
                | _ -> Option.isSome (bindsKey owner modules name op))
              |> List.map fst
            match binding with
            | [] -> return Error(Refused NotBound)
            | opIds ->
              let! _ = dropOpsOn conn branchId opIds
              step "resolve.untagged"
              do! setBaseToParentOn conn branchId owner modules name
              return Ok()
          })
    with e ->
      return Error(Failed e.Message)
  }
  |> Ply.toTask

/// Close a conflict record: 'acked', 'overridden' or 'superseded', with what settled it.
let setConflictStatus
  (id : string)
  (status : string)
  (resolvedBy : string)
  : Task<Result<unit, Failure>> =
  uply {
    try
      return!
        transact (fun conn ->
          uply {
            let! n =
              exec
                conn
                "UPDATE conflicts SET status = @status, resolved_by = @by WHERE id = @id"
                [ "@status", box status; "@by", box resolvedBy; "@id", box id ]
            step "conflict.status"
            if n = 0 then return Error(Refused NoSuchConflict) else return Ok()
          })
    with e ->
      return Error(Failed e.Message)
  }
  |> Ply.toTask

/// One recorded conflict by its exact id.
let conflictById (id : string) : Task<Option<Detect.Conflict>> =
  uply {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    let! found = conflictsOn conn "id = @id" [ "@id", box id ]
    return List.tryHead found
  }
  |> Ply.toTask

/// The part of a declaration a doc conflict names, as `DocPart.fromKey` spells it.
let docPartOfKey (key : string) : Option<PT.DocPart> =
  match key.Split(':') |> List.ofArray with
  | [ "item" ] -> Some PT.DocPart.WholeItem
  | [ "record-field"; name ] -> Some(PT.DocPart.RecordField name)
  | [ "enum-case"; name ] -> Some(PT.DocPart.EnumCase name)
  | [ "parameter"; index ] ->
    match System.Int32.TryParse index with
    | true, i -> Some(PT.DocPart.Parameter i)
    | _ -> None
  | _ -> None

/// The hash a doc's text is known by.
let hashOfText (text : string) : PT.Hash = PT.Hash(sha256Hex text)
