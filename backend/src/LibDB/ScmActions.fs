/// The SCM ACTIONS: commit, discard, the branch lifecycle, merge, rebase, and settling a conflict.
///
/// Each one is a whole operation, run here end to end, in ONE transaction wherever the work allows it.
/// They used to be fragments sequenced from Dark, a transaction per fragment, and every gap between two
/// was a state a crash could leave behind: a commit row naming no ops, a branch archived here whose
/// event never told anyone, a merged branch whose landed ops sat in main's draft, a deleted op with its
/// tag still pointing at it.
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

/// A JSON array of strings for `json_each`, written by hand: no reflection in shipped code.
let private jsonStrings (items : List<string>) : string =
  use stream = new System.IO.MemoryStream()
  (use w = new System.Text.Json.Utf8JsonWriter(stream)
   w.WriteStartArray()
   for s in items do
     w.WriteStringValue s
   w.WriteEndArray())
  System.Text.Encoding.UTF8.GetString(stream.ToArray())

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

/// The draft, as one clause over `package_ops p`: on main the uncommitted effective ops no branch tags,
/// on a branch that branch's own uncommitted ops. The ONE definition; the reads that list the draft
/// (`ScmOps`) spell the same clause.
let private draftWhere (b : PT.BranchId) : string * List<Param> =
  if b.IsMain then
    "p.commit_hash IS NULL AND p.effective = 1
     AND p.id NOT IN (SELECT op_id FROM op_branches)",
    []
  else
    "p.commit_hash IS NULL
     AND p.id IN (SELECT op_id FROM op_branches WHERE branch_id = @branch)",
    [ "@branch", box (string b) ]

let private chainCte =
  "WITH RECURSIVE chain(bid) AS (
     SELECT @start
     UNION
     SELECT b.parent_id FROM branches b JOIN chain c ON b.id = c.bid
     WHERE b.parent_id != @mainId
   )"

/// The newest commit <param b> can see: its chain's committed ops and main's. "" for none.
let private parentCommitOn
  (conn : SqliteConnection)
  (b : PT.BranchId)
  : Ply<string> =
  uply {
    let where, ps =
      if b.IsMain then
        "p.commit_hash IS NOT NULL AND p.effective = 1
         AND p.id NOT IN (SELECT op_id FROM op_branches)",
        []
      else
        "p.commit_hash IS NOT NULL
         AND (p.id IN (SELECT op_id FROM op_branches WHERE branch_id IN (SELECT bid FROM chain))
              OR (p.effective = 1 AND p.id NOT IN (SELECT op_id FROM op_branches)))",
        [ "@start", box (string b); "@mainId", box (string PT.BranchId.Main) ]
    let prefix = if b.IsMain then "" else chainCte
    let! found =
      ids
        conn
        (prefix
         + " SELECT c.hash FROM commits c WHERE c.hash IN (SELECT p.commit_hash FROM package_ops p WHERE "
         + where
         + ") ORDER BY c.origin_ts DESC, c.rowid DESC LIMIT 1")
        ps
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
  (hash : string)
  (message : string)
  (stamp : string)
  (parent : string)
  : Ply<unit> =
  uply {
    let! _ =
      exec
        conn
        "INSERT OR REPLACE INTO commits (hash, message, origin_ts, parent)
         VALUES (@hash, @message, @ts, @parent)"
        [ "@hash", box hash
          "@message", box message
          "@ts", box stamp
          "@parent", box parent ]
    return ()
  }

/// Commit, on main, whichever of <param opIds> have LANDED there (effective and uncommitted), whatever
/// branch still tags them. For what a branch verb lands by itself -- a merge's frontier, its event, an
/// archive's event -- which nobody typed and so nobody would think to commit. Exactly these ids, never
/// "whatever is uncommitted", which would sweep someone's half-written fn into a commit about a branch.
let private commitLandedOn
  (conn : SqliteConnection)
  (message : string)
  (opIds : List<string>)
  : Ply<Option<string>> =
  uply {
    let! landed =
      ids
        conn
        "SELECT id FROM package_ops WHERE commit_hash IS NULL AND effective = 1"
        []
    let landed = Set.ofList landed
    let toStamp = opIds |> List.filter (fun id -> Set.contains id landed)
    if List.isEmpty toStamp then
      return None
    else
      let! parent = parentCommitOn conn PT.BranchId.Main
      let! stamp = nextStampOn conn
      let hash = mintCommitHash message stamp (sortOrdinal toStamp) parent
      do! insertCommit conn hash message stamp parent
      for id in toStamp do
        let! _ =
          exec
            conn
            "UPDATE package_ops SET commit_hash = @hash
             WHERE id = @id AND commit_hash IS NULL AND effective = 1"
            [ "@hash", box hash; "@id", box id ]
        ()
      return Some hash
  }


/// Author a `BranchEvent` on main and fold it, on the caller's connection. What tells the OTHER
/// machine what happened to a branch; the local tables were already changed by the caller.
let private authorEventOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  (event : PT.BranchEventKind)
  : Ply<System.Guid> =
  uply {
    let at =
      System.DateTime.UtcNow.ToString(
        "yyyy-MM-ddTHH:mm:ss.fffZ",
        System.Globalization.CultureInfo.InvariantCulture
      )
    let op = PT.PackageOp.BranchEvent(branchId, event, at)
    let opId = Inserts.computeOpHash op
    let blob = BS.PT.PackageOp.serialize opId op
    // The same insert authoring uses (`Inserts.insertAndApplyOpsWith`): applied = 0, then folded, then
    // marked applied, so a fold that raises takes the insert back with it.
    let! inserted =
      exec
        conn
        "INSERT INTO package_ops (id, op_blob, op_kind, applied, origin_ts, commit_hash)
         VALUES (@id, @blob, @kind, 0, @ts, NULL)
         ON CONFLICT(id) DO UPDATE
           SET effective = 1, applied = 0, origin_ts = excluded.origin_ts,
               commit_hash = excluded.commit_hash
           WHERE package_ops.effective = 0"
        [ "@id", box (string opId)
          "@blob", box blob
          "@kind", box (BS.PT.PackageOp.kind op)
          "@ts", box (OriginTs.next ()) ]
    let! _ =
      exec
        conn
        "DELETE FROM op_branches WHERE op_id = @id"
        [ "@id", box (string opId) ]
    if inserted > 0 then
      do! PackageOpPlayback.applyOpsOnConnectionFrom conn "op" [ op ]
      let! _ =
        exec
          conn
          "UPDATE package_ops SET applied = 1 WHERE id = @id"
          [ "@id", box (string opId) ]
      ()
    return opId
  }


/// Fold every effective, unapplied op, on the caller's connection and inside its transaction.
///
/// What `Seed.applyUnappliedOps` does on its own connection, here so a merge's flip and its fold land
/// together. Repeats while a pass makes more ops effective; an op this build cannot read stays
/// `applied = 0` (a later build applies it) and counts as nothing folded, which is what stops the loop.
let private foldPendingOn (conn : SqliteConnection) : Ply<int64> =
  uply {
    let mutable total = 0L
    let mutable pass = 0
    let mutable keepGoing = true
    while keepGoing do
      let! pending =
        rows conn "SELECT id, op_blob FROM package_ops WHERE applied = 0 AND effective = 1
           ORDER BY created_at ASC, rowid ASC" [] (fun r ->
          (r.GetString 0, r.GetFieldValue<byte[]> 1))
      let decoded =
        pending
        |> List.choose (fun (id, blob) ->
          match System.Guid.TryParse id with
          | true, g ->
            BS.PT.PackageOp.tryDeserialize g blob |> Option.map (fun op -> (id, op))
          | _ -> None)
      if List.isEmpty decoded then
        keepGoing <- false
      else
        // By id, never by predicate: see `Seed.foldRead`.
        let! _ =
          exec
            conn
            "UPDATE package_ops SET applied = 1 WHERE id IN (SELECT value FROM json_each(@ids))"
            [ "@ids", box (jsonStrings (List.map fst decoded)) ]
        do! PackageOpPlayback.applyOpsOnConnection conn (List.map snd decoded)
        total <- total + int64 (List.length decoded)
        pass <- pass + 1
        if pass >= 10 then
          Exception.raiseInternal
            "the merge fold did not settle: ops keep becoming effective"
            [ "passes", pass; "applied", total ]
    return total
  }


// ---------------------------------------------------------------------------------------------
// bindings: what a run of ops says about names
// ---------------------------------------------------------------------------------------------

let private modulesOf (loc : PT.PackageLocation) : string =
  String.concat "." loc.modules

let private hashText (h : PT.Hash) : string = let (PT.Hash s) = h in s

/// What one op says about a name, keyed for a latest-wins fold. `None` binding = the name ended.
let private nameWord
  (op : PT.PackageOp)
  (ts : string)
  : Option<string * string * Option<Detect.Binding> * PT.PackageLocation> =
  match op with
  | PT.PackageOp.Unbind(loc, _) ->
    let m = modulesOf loc
    Some($"{ts}|0|", Detect.bindingKey loc.owner m loc.name, None, loc)
  | PT.PackageOp.SetName(loc, target, _)
  | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target) ->
    let m = modulesOf loc
    let hash = hashText target.hash
    let previous =
      match op with
      | PT.PackageOp.SetName(_, _, Some h) -> Some(hashText h)
      | _ -> None
    let binding : Detect.Binding =
      { Detect.Binding.owner = loc.owner
        modules = m
        name = loc.name
        itemType = target.kind.toString ()
        hash = hash
        originTs = ts
        author = ""
        previous = previous }
    Some($"{ts}|1|{hash}", Detect.bindingKey loc.owner m loc.name, Some binding, loc)
  | _ -> None

/// A run of (ts, op) folded to each name's last word, in key order. The order the fold would keep is
/// the order sorted here: later stamp, then a binding over an unbind, then the greater hash
/// (`LibExecution.Lww`), whatever order the rows arrived in.
let private lastWords
  (entries : List<string * PT.PackageOp>)
  : List<string * Option<Detect.Binding>> =
  entries
  |> List.collect (fun (ts, op) ->
    PT.packageOpBindingEffects op |> List.map (fun effect -> (ts, effect)))
  |> List.choose (fun (ts, op) -> nameWord op ts)
  |> List.sortWith (fun (a, _, _, _) (b, _, _, _) ->
    System.String.CompareOrdinal(a, b))
  |> List.fold
    (fun (m : Map<string, Option<Detect.Binding>>) (_, k, b, _) -> Map.add k b m)
    Map.empty
  |> Map.toList

let private opEntries (opRows : List<ScmOps.Row>) : List<string * PT.PackageOp> =
  opRows |> List.choose (fun r -> r.op |> Option.map (fun op -> (r.ts, op)))

/// What a branch's OWN ops bind: the incoming side of its merge.
let private branchBindings (b : PT.BranchId) : Ply<List<Detect.Binding>> =
  uply {
    let! page = ScmOps.query (ScmOps.Own b) -1L
    return lastWords (opEntries page.ops) |> List.choose snd
  }

/// What a branch CHAIN binds (the branch, then its ancestors below main), names it ended included.
let private chainWords
  (b : PT.BranchId)
  : Ply<List<string * Option<Detect.Binding>>> =
  uply {
    let! page = ScmOps.query (ScmOps.Chain b) -1L
    return lastWords (opEntries page.ops)
  }

let private bindingOfRow (row : ScmBindings.Binding) : Detect.Binding =
  { Detect.Binding.owner = row.owner
    modules = row.modules
    name = row.name
    itemType = row.itemType
    hash = row.hash
    originTs = row.originTs
    author = ""
    previous = row.previous }

/// What the PARENT binds for these bare names: the local side a branch merges into. Main's live
/// bindings, and for a parent below main its chain's own bindings over them.
let private parentBindings
  (parent : PT.BranchId)
  (names : List<string>)
  : Ply<List<Detect.Binding>> =
  uply {
    let! fromMain =
      if List.isEmpty names then
        Task.FromResult []
      else
        ScmBindings.query (ScmBindings.MainByBareNames(List.distinct names))
    let fromMain = fromMain |> List.map bindingOfRow
    if parent.IsMain then
      return fromMain
    else
      let! words = chainWords parent
      let overlay = words |> List.choose snd
      let overlayKeys =
        overlay
        |> List.map (fun b -> Detect.bindingKey b.owner b.modules b.name)
        |> Set.ofList
      let untouched =
        fromMain
        |> List.filter (fun b ->
          not (
            Set.contains (Detect.bindingKey b.owner b.modules b.name) overlayKeys
          ))
      return untouched @ overlay
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
let preview (b : PT.BranchId) : Ply<List<Detect.Conflict>> =
  uply {
    let! incoming = branchBindings b
    let! parent = parentOf b
    let! local = parentBindings parent (incoming |> List.map _.name)
    let! bases = nameBases b
    let! parentName = branchName parent
    let! name = branchName b
    return Detect.detect parentName name local incoming bases
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

/// One op in main's draft.
type private DraftOp = { id : string; ts : string; op : PT.PackageOp }

let private refHash (r : PT.Reference) : string = hashText r.hash

let private addedHashes (ops : List<PT.PackageOp>) : Set<string> =
  ops
  |> List.choose (fun op ->
    match op with
    | PT.PackageOp.AddType t -> Some(hashText t.hash)
    | PT.PackageOp.AddValue v -> Some(hashText v.hash)
    | PT.PackageOp.AddFn f -> Some(hashText f.hash)
    | PT.PackageOp.AddTrait t -> Some(hashText t.hash)
    | PT.PackageOp.AddTraitImpl i -> Some(hashText i.hash)
    | PT.PackageOp.AddTest t -> Some(hashText t.hash)
    | _ -> None)
  |> Set.ofList

/// Can dropping this op be undone by deleting the rows it wrote and re-listing what it superseded?
/// Content (`Add*`) and a binding that named content added in the same draft, or an override: yes.
/// A standalone rename, an unbind, a deprecation, a doc, a non-override decision, a branch event: no,
/// since what they unlisted or recorded cannot be identified afterwards.
let private surgicallyUndoable (added : Set<string>) (op : PT.PackageOp) : bool =
  match op with
  | PT.PackageOp.AddType _
  | PT.PackageOp.AddValue _
  | PT.PackageOp.AddFn _
  | PT.PackageOp.AddTrait _
  | PT.PackageOp.AddTraitImpl _
  | PT.PackageOp.AddTest _ -> true
  | PT.PackageOp.SetName(_, target, _) -> Set.contains (refHash target) added
  // A `Restore` writes one `locations` row like an override, and its reference names are read from
  // the op log, so deleting its op row undoes them too.
  | PT.PackageOp.Decision(_, _, _, PT.DecisionKind.Override _)
  | PT.PackageOp.Decision(_, _, _, PT.DecisionKind.Restore _) -> true
  | PT.PackageOp.Decision _
  | PT.PackageOp.Unbind _
  | PT.PackageOp.Deprecate _
  | PT.PackageOp.Undeprecate _
  | PT.PackageOp.Rename _
  | PT.PackageOp.SetReferenceNames _
  | PT.PackageOp.UpdateDoc _
  | PT.PackageOp.BranchEvent _ -> false

/// The `locations` row an op wrote: (owner, modules, name, hash, ts).
let private locationRowOf
  (d : DraftOp)
  : Option<string * string * string * string * string> =
  match d.op with
  | PT.PackageOp.SetName(loc, target, _)
  | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override target)
  | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Restore(_, target, _)) ->
    Some(loc.owner, modulesOf loc, loc.name, refHash target, d.ts)
  | _ -> None

/// Main's draft, oldest first, on the given connection. An op this build cannot decode is not in it:
/// nothing here may drop what it could not read.
let private mainDraftOn (conn : SqliteConnection) : Ply<List<DraftOp>> =
  uply {
    let where, ps = draftWhere PT.BranchId.Main
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

/// Does discarding <param loc> from MAIN's draft drop this op: a binding of the name, a change to its
/// reference names, or a rename to or from it. A rename goes whole, so it is never left half-applied.
let private bindsLocation (loc : PT.PackageLocation) (op : PT.PackageOp) : bool =
  match op with
  | PT.PackageOp.Rename(oldLocation, newLocation, _, _, _) ->
    sameLocation oldLocation loc || sameLocation newLocation loc
  | PT.PackageOp.SetReferenceNames(l, _, _, _)
  | PT.PackageOp.SetName(l, _, _)
  | PT.PackageOp.Decision(_, l, _, PT.DecisionKind.Restore _)
  | PT.PackageOp.Decision(_, l, _, PT.DecisionKind.Override _) -> sameLocation l loc
  | _ -> false

/// The same for a BRANCH's draft, where an `Unbind` of the name goes too.
let private branchOpTouches (loc : PT.PackageLocation) (op : PT.PackageOp) : bool =
  match op with
  | PT.PackageOp.SetReferenceNames(l, _, _, _) -> sameLocation l loc
  | _ ->
    PT.packageOpBindingEffects op
    |> List.exists (fun effect ->
      match effect with
      | PT.PackageOp.Unbind(l, _)
      | PT.PackageOp.SetName(l, _, _)
      | PT.PackageOp.Decision(_, l, _, PT.DecisionKind.Override _) ->
        sameLocation l loc
      | _ -> false)

/// What survives <param what>, in draft order.
let private kept (what : DiscardWhat) (draft : List<DraftOp>) : List<DraftOp> =
  match what with
  | All -> []
  | Name loc -> draft |> List.filter (fun d -> not (bindsLocation loc d.op))
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
            let h = refHash target
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

/// One go at rewriting main's draft. Surgical when every dropped op allows it, in one transaction with
/// the read it decided on; otherwise the whole-draft rebuild, which refuses (`None`) when the draft is
/// no longer the one it chose from, so the caller reads and chooses again.
let private rewriteMainOnce (what : DiscardWhat) : Ply<Option<Result<int, string>>> =
  uply {
    let! surgical =
      transact (fun conn ->
        uply {
          let! draft = mainDraftOn conn
          let keptIds = kept what draft |> List.map _.id |> Set.ofList
          let dropped =
            draft |> List.filter (fun d -> not (Set.contains d.id keptIds))
          let added = addedHashes (draft |> List.map _.op)
          if List.isEmpty dropped then
            return Ok(Some 0)
          elif
            not (List.forall (fun d -> surgicallyUndoable added d.op) dropped)
          then
            // Nothing written; the rebuild below takes it.
            return Error(draft |> List.map _.id, Set.toList keptIds)
          else
            // Rows first, then the names left with nothing live get their newest surviving row back,
            // then the op rows: no reader ever sees a binding no op explains.
            let locs = dropped |> List.choose locationRowOf
            for (o, m, n, h, ts) in locs do
              let! _ =
                exec
                  conn
                  "DELETE FROM locations
                   WHERE owner = @o AND modules = @m AND name = @n
                     AND item_hash = @h AND origin_ts = @ts"
                  [ "@o", box o
                    "@m", box m
                    "@n", box n
                    "@h", box h
                    "@ts", box ts ]
              ()
            step "discard.rows"
            for (o, m, n) in
              locs |> List.map (fun (o, m, n, _, _) -> (o, m, n)) |> List.distinct do
              // A name whose newest surviving row is an unbind's tombstone stays unbound.
              let! _ =
                exec
                  conn
                  "UPDATE locations SET unlisted_at = NULL
                   WHERE location_id = (
                     SELECT location_id FROM locations
                     WHERE owner = @o AND modules = @m AND name = @n
                     ORDER BY origin_ts DESC, rowid DESC
                     LIMIT 1
                   )
                   AND source <> 'unbind'
                   AND NOT EXISTS (
                     SELECT 1 FROM locations
                     WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL
                   )"
                  [ "@o", box o; "@m", box m; "@n", box n ]
              ()
            for d in dropped do
              let! _ =
                exec
                  conn
                  "DELETE FROM package_ops WHERE id = @id"
                  [ "@id", box d.id ]
              ()
            return Ok(Some(List.length dropped))
        })
    match surgical with
    | Ok(Some n) -> return Some(Ok n)
    | Ok None -> return Some(Ok 0)
    | Error(draftIds, keptIds) ->
      let parse (xs : List<string>) =
        xs
        |> List.choose (fun s ->
          match System.Guid.TryParse s with
          | true, g -> Some g
          | _ -> None)
        |> Set.ofList
      let dropped = List.length draftIds - List.length keptIds
      let! rebuilt = Draft.rebuild (parse draftIds) (parse keptIds)
      Caching.invalidateAll ()
      return (if rebuilt then Some(Ok dropped) else None)
  }

/// Rewrite main's draft to what <param what> keeps. Returns how many ops went.
let private rewriteMain (what : DiscardWhat) : Ply<Result<int, Failure>> =
  uply {
    let mutable attempt = 1
    let mutable answer = None
    while Option.isNone answer do
      let! once =
        uply {
          try
            let! r = rewriteMainOnce what
            return r |> Option.map (Result.mapError Failed)
          with e ->
            return Some(Error(Failed e.Message))
        }
      match once with
      | Some r -> answer <- Some r
      // An attempt is short, so ten misses in a row is not ordinary contention.
      | None when attempt >= 10 -> answer <- Some(Error(Refused Moved))
      | None -> attempt <- attempt + 1
    return Option.get answer
  }

/// Drop these ops from a branch: untag the uncommitted ones, then delete the rows nothing else holds.
/// Returns how many left the branch.
let private dropBranchOpsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  (opIds : List<string>)
  : Ply<int> =
  uply {
    let mutable dropped = 0
    // Untag FIRST: the untag is guarded on the op being uncommitted, which it can only check while the
    // row still exists.
    for id in opIds do
      let! n =
        exec
          conn
          "DELETE FROM op_branches WHERE branch_id = @branch AND op_id = @id
             AND op_id IN (SELECT id FROM package_ops WHERE commit_hash IS NULL)"
          [ "@branch", box (string branchId); "@id", box id ]
      dropped <- dropped + n
    step "discard.untag"
    for id in opIds do
      let! _ =
        exec
          conn
          "DELETE FROM package_ops WHERE commit_hash IS NULL AND effective = 0 AND id = @id
             AND id NOT IN (SELECT op_id FROM op_branches)"
          [ "@id", box id ]
      ()
    return dropped
  }

/// A branch's own ops, oldest first, decoded, with whether each is committed.
let private ownOpsOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  : Ply<List<string * bool * PT.PackageOp>> =
  uply {
    let! found =
      rows
        conn
        "SELECT p.id, p.commit_hash IS NOT NULL, p.op_blob
         FROM package_ops p JOIN op_branches ob ON ob.op_id = p.id
         WHERE ob.branch_id = @branch
         ORDER BY p.origin_ts, p.rowid"
        [ "@branch", box (string branchId) ]
        (fun r -> (r.GetString 0, r.GetInt64 1 <> 0L, r.GetFieldValue<byte[]> 2))
    return
      found
      |> List.choose (fun (id, committed, blob) ->
        match System.Guid.TryParse id with
        | true, g ->
          BS.PT.PackageOp.tryDeserialize g blob
          |> Option.map (fun op -> (id, committed, op))
        | _ -> None)
  }

let private discardBranch
  (branchId : PT.BranchId)
  (what : DiscardWhat)
  : Ply<Result<int, Failure>> =
  transact (fun conn ->
    uply {
      let b = box (string branchId)
      match what with
      | Collapse -> return Ok 0
      | All ->
        // How many the draft held, which is what the person was shown.
        let where, ps = draftWhere branchId
        let! n = count conn ("SELECT count(*) FROM package_ops p WHERE " + where) ps
        let! draftIds =
          ids
            conn
            "SELECT id FROM package_ops
             WHERE commit_hash IS NULL AND effective = 0
               AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @branch)"
            [ "@branch", b ]
        // Rows first, and only those no OTHER branch holds; then this branch's tags. One transaction,
        // so an op is never left untagged-but-present, which reads as a main op.
        let! _ =
          exec
            conn
            "DELETE FROM package_ops
             WHERE id IN (SELECT value FROM json_each(@ids))
               AND id NOT IN (SELECT op_id FROM op_branches WHERE branch_id <> @branch)"
            [ "@ids", box (jsonStrings draftIds); "@branch", b ]
        step "discard.rows"
        let! _ =
          exec
            conn
            "DELETE FROM op_branches
             WHERE branch_id = @branch AND op_id IN (SELECT value FROM json_each(@ids))"
            [ "@branch", b; "@ids", box (jsonStrings draftIds) ]
        // Once the branch holds nothing, its fork points describe ops that no longer exist.
        let! remaining =
          count
            conn
            "SELECT count(*) FROM op_branches WHERE branch_id = @branch"
            [ "@branch", b ]
        if remaining = 0L then
          let! _ =
            exec
              conn
              "DELETE FROM branch_name_bases WHERE branch_id = @branch"
              [ "@branch", b ]
          ()
        return Ok(int n)
      | Name loc ->
        let! own = ownOpsOn conn branchId
        let toDrop =
          own
          |> List.filter (fun (_, committed, op) ->
            not committed && branchOpTouches loc op)
          |> List.map (fun (id, _, _) -> id)
        let! n = dropBranchOpsOn conn branchId toDrop
        return Ok n
      | Ops opIds ->
        let! n = dropBranchOpsOn conn branchId opIds
        return Ok n
    })

/// Drop what <param what> names from <param branchId>'s draft. Returns how many ops went.
let discard
  (branchId : PT.BranchId)
  (what : DiscardWhat)
  : Task<Result<int, Failure>> =
  uply {
    if branchId.IsMain then
      return! rewriteMain what
    else
      try
        return! discardBranch branchId what
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
      // A collapse that fails is not a reason to refuse the commit, which is what the Dark commit did
      // too (it dropped `collapseDraft`'s Result): the draft is left uncollapsed and still commits,
      // with every naming in it.
      let! _ = rewriteMain Collapse
      ()
    try
      return!
        transact (fun conn ->
          uply {
            let! parent = parentCommitOn conn branchId
            let where, ps = draftWhere branchId
            let! hashIds =
              match scope with
              | Whole ->
                ids
                  conn
                  ("SELECT p.id FROM package_ops p WHERE "
                   + where
                   + " ORDER BY p.id")
                  ps
              | Only selected -> Ply(sortOrdinal selected)
            let! stamp = nextStampOn conn
            let hash = mintCommitHash message stamp hashIds parent
            do! insertCommit conn hash message stamp parent
            step "commit.row"
            let! stamped =
              uply {
                match scope with
                | Whole ->
                  let! n =
                    exec
                      conn
                      ("UPDATE package_ops SET commit_hash = @hash WHERE id IN
                         (SELECT p.id FROM package_ops p WHERE "
                       + where
                       + ")")
                      (("@hash", box hash) :: ps)
                  return int64 n
                | Only selected ->
                  let mutable n = 0L
                  for id in selected do
                    let! one =
                      exec
                        conn
                        ("UPDATE package_ops SET commit_hash = @hash WHERE id = @id
                           AND id IN (SELECT p.id FROM package_ops p WHERE "
                         + where
                         + ")")
                        (("@hash", box hash) :: ("@id", box id) :: ps)
                    n <- n + int64 one
                  return n
              }
            if stamped = 0L then
              return Error(Refused NothingToCommit)
            else
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

/// ARCHIVE: delete the branch's inert ops no other branch holds, drop its tags and bases, mark it
/// archived, author the event that tells other machines, and commit that event -- all in ONE
/// transaction. A merged branch's ops are its parent's already and untagged, so this only marks it.
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
              let b = box (string branchId)
              let! _ =
                exec
                  conn
                  "DELETE FROM package_ops
                   WHERE effective = 0
                     AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)
                     AND id NOT IN (SELECT op_id FROM op_branches WHERE branch_id <> @b)"
                  [ "@b", b ]
              let! _ =
                exec conn "DELETE FROM op_branches WHERE branch_id = @b" [ "@b", b ]
              let! _ =
                exec
                  conn
                  "DELETE FROM branch_name_bases WHERE branch_id = @b"
                  [ "@b", b ]
              let! _ =
                exec
                  conn
                  "UPDATE branches SET archived_at = datetime('now') WHERE id = @b"
                  [ "@b", b ]
              step "archive.marked"
              let! eventId = authorEventOn conn branchId PT.Archived
              step "archive.event"
              // Committed, for the same reason a merge commits what it lands: a `BranchEvent` binds
              // no name, so left in main's draft it reads clean to `status` and gets swept into some
              // unrelated commit.
              let! _ =
                commitLandedOn
                  conn
                  $"archived branch \"{handle}\""
                  [ string eventId ]
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

/// Can this branch merge into its parent: it holds ops, and nothing unmerged branches off it.
/// Conflicts do not gate a merge; they are recorded.
let private gate (branchId : PT.BranchId) : Ply<Result<unit, Refusal>> =
  uply {
    if branchId.IsMain then
      return Error IsMain
    else
      let! tagged = ScmOps.query (ScmOps.Tagged branchId) 0L
      if tagged.total = 0L then
        return Error NothingToMerge
      else
        let! children = ScmBranches.query (ScmBranches.ChildrenOf(string branchId))
        if List.isEmpty children then return Ok() else return Error HasActiveChildren
  }

let private taggedOn
  (conn : SqliteConnection)
  (branchId : PT.BranchId)
  : Ply<List<string>> =
  ids
    conn
    "SELECT op_id FROM op_branches WHERE branch_id = @b"
    [ "@b", box (string branchId) ]

/// MERGE <param branchId> into its parent.
///
/// Into MAIN, one transaction: flip the branch's ops effective and fold them, mark it merged and drop
/// its tags, author the event that tells other machines, commit what landed, record the conflicts.
/// Into a BRANCH, one transaction: move the tags and bases to the parent (whose own overlay folds them
/// later; folding here would leak the child's work into main), mark merged, author the event, record
/// the conflicts. Values that landed in main are evaluated after the transaction commits.
///
/// The gate and the conflicts are read first, and the frontier is read again inside the transaction:
/// if the branch moved in between, nothing is written and the answer says so.
let merge
  (branchId : PT.BranchId)
  (evaluateValues : unit -> Task<unit>)
  : Task<Result<Reconciled, Failure>> =
  uply {
    match! gate branchId with
    | Error r -> return Error(Refused r)
    | Ok() ->
      let! conflicts = preview branchId
      let! parent = parentOf branchId
      let! name = branchName branchId
      let! frontier = ScmOps.query (ScmOps.Tagged branchId) -1L
      let moved = frontier.ops |> List.map _.id
      let movedGuids =
        moved
        |> List.choose (fun s ->
          match System.Guid.TryParse s with
          | true, g -> Some g
          | _ -> None)
      let b = box (string branchId)
      try
        let! result =
          transact (fun conn ->
            uply {
              let! now = taggedOn conn branchId
              if Set.ofList now <> Set.ofList moved then
                return Error(Refused Moved)
              elif parent.IsMain then
                let! toFlip =
                  count
                    conn
                    "SELECT count(*) FROM package_ops p JOIN op_branches ob ON ob.op_id = p.id
                     WHERE ob.branch_id = @b AND p.effective = 0"
                    [ "@b", b ]
                let! _ =
                  exec
                    conn
                    "UPDATE package_ops SET effective = 1
                     WHERE effective = 0
                       AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @b)"
                    [ "@b", b ]
                step "merge.flipped"
                let! _ = foldPendingOn conn
                step "merge.folded"
                let! _ =
                  exec
                    conn
                    "UPDATE branches SET merged_at = datetime('now') WHERE id = @b"
                    [ "@b", b ]
                let! _ =
                  exec
                    conn
                    "DELETE FROM op_branches WHERE branch_id = @b"
                    [ "@b", b ]
                step "merge.marked"
                let! eventId = authorEventOn conn branchId (PT.Merged movedGuids)
                step "merge.event"
                // A merge into main COMMITS what it landed: a pulled branch's ops arrive uncommitted,
                // and the event is new. Left in main's draft they read as work nobody did.
                let landed = (movedGuids |> List.map string) @ [ string eventId ]
                let! commitHash =
                  commitLandedOn conn $"merged branch \"{name}\"" landed
                step "merge.committed"
                // Filed against the PARENT: the merge folded into it, and the branch is finished.
                do! recordConflictsOn conn parent conflicts
                return
                  Ok
                    { moved = toFlip
                      conflicts = conflicts
                      diverged = []
                      repointed = []
                      commit = commitHash |> Option.defaultValue ""
                      warning = "" }
              else
                let p = box (string parent)
                // The tags move with their provenance, and so do the bases: the parent then carries
                // the child's rebinds, and a name without a base could never conflict again.
                let! _ =
                  exec
                    conn
                    "INSERT OR IGNORE INTO op_branches (op_id, branch_id, source)
                     SELECT op_id, @p, source FROM op_branches WHERE branch_id = @b"
                    [ "@p", p; "@b", b ]
                let! _ =
                  exec
                    conn
                    "DELETE FROM op_branches WHERE branch_id = @b"
                    [ "@b", b ]
                step "merge.retagged"
                let! _ =
                  exec
                    conn
                    "INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
                     SELECT @p, owner, modules, name, base_hash FROM branch_name_bases WHERE branch_id = @b"
                    [ "@p", p; "@b", b ]
                let! _ =
                  exec
                    conn
                    "DELETE FROM branch_name_bases WHERE branch_id = @b"
                    [ "@b", b ]
                let! _ =
                  exec
                    conn
                    "UPDATE branches SET merged_at = datetime('now') WHERE id = @b"
                    [ "@b", b ]
                step "merge.marked"
                let! _ = authorEventOn conn branchId (PT.Merged movedGuids)
                do! recordConflictsOn conn parent conflicts
                return
                  Ok
                    { moved = int64 (List.length moved)
                      conflicts = conflicts
                      diverged = []
                      repointed = []
                      commit = ""
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


/// One caller of a name the parent moved, to repoint at the parent's version. Which callers are
/// outdated is a language question, answered in Dark (`SCM.Constraints`) and handed in.
type Repoint =
  { usage : string
    dependency : PT.PackageLocation
    kind : PT.ItemKind
    fromHash : PT.Hash
    toHash : PT.Hash }

/// What the parent binds now for each name <param branchId> recorded a base for: main's COMMITTED
/// bindings (a branch never sees main's draft), and a parent below main's own words over them. A name
/// the parent's chain ended reads "".
let private parentHashesForBases
  (parent : PT.BranchId)
  (branchId : PT.BranchId)
  : Ply<Map<string, string>> =
  uply {
    let key o m n = o + "\n" + m + "\n" + n
    let! fromMain = ScmBindings.query (ScmBindings.CommittedAtBases(string branchId))
    let fromMain =
      fromMain |> List.map (fun b -> (key b.owner b.modules b.name, b.hash))
    let! overlay =
      uply {
        if parent.IsMain then
          return []
        else
          let! words = chainWords parent
          return
            words
            |> List.map (fun (k, w) ->
              // The chain word's key is `owner/modules/name`; rebuild it from the binding, or, for a
              // name the chain ended, from the key itself.
              match w with
              | Some b -> (key b.owner b.modules b.name, b.hash)
              | None ->
                match k.Split('/') |> List.ofArray with
                | [ o; m; n ] -> (key o m n, "")
                | _ -> (k, ""))
      }
    return
      (fromMain @ overlay) |> List.fold (fun m (k, h) -> Map.add k h m) Map.empty
  }

/// REBASE <param branchId> onto its parent: accept the parent's current state as its new base.
///
/// Reads first: the conflicts the reconciliation records, and which names the parent moved since the
/// fork (both answered against the store as it stands). Then each outdated caller is repointed, a
/// propagation per caller, each a complete edit of its own. Then ONE transaction moves every base and
/// records the conflicts. Stopped before that transaction, the bases still say the old fork point, so
/// running the rebase again redoes what is left and finishes.
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
      let! parent = parentOf branchId
      let! bases = nameBases branchId
      let! parentHashes = parentHashesForBases parent branchId
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
        return!
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
    let! parent = parentOf branchId
    let! hashes = parentHashesForBases parent branchId
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
              |> List.choose (fun (_, _, op) -> bindsKey owner modules name op)
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
              let opId = Branches.opRowId op
              let! _ =
                exec
                  conn
                  "INSERT OR IGNORE INTO package_ops (id, op_blob, op_kind, applied, effective, origin_ts)
                   VALUES (@id, @blob, @kind, 0, 0, @ts)"
                  [ "@id", box (string opId)
                    "@blob", box (BS.PT.PackageOp.serialize opId op)
                    "@kind", box (BS.PT.PackageOp.kind op)
                    "@ts", box stamp ]
              let! _ =
                exec
                  conn
                  "INSERT OR IGNORE INTO op_branches (op_id, branch_id, source)
                   SELECT @id, @b, 'op'
                   WHERE NOT EXISTS (SELECT 1 FROM package_ops WHERE id = @id AND effective = 1)"
                  [ "@id", box (string opId); "@b", box (string branchId) ]
              step "resolve.stored"
              do! setBaseToParentOn conn branchId owner modules name
              return Ok()
          })
    with e ->
      return Error(Failed e.Message)
  }
  |> Ply.toTask

/// TAKE THEIRS: drop every op of the branch's that binds the name, so the overlay falls back to the
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
              |> List.filter (fun (_, _, op) ->
                match op with
                | PT.PackageOp.SetReferenceNames(loc, _, _, _) ->
                  loc.owner = owner && modulesOf loc = modules && loc.name = name
                | _ -> Option.isSome (bindsKey owner modules name op))
              |> List.map (fun (id, _, _) -> id)
            match binding with
            | [] -> return Error(Refused NotBound)
            | opIds ->
              for id in opIds do
                let! _ =
                  exec
                    conn
                    "DELETE FROM op_branches WHERE op_id = @id AND branch_id = @b"
                    [ "@id", box id; "@b", box (string branchId) ]
                ()
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
    let! found =
      rows conn "SELECT id, owner, modules, name, item_type, COALESCE(part, ''), kind, candidates,
                COALESCE(auto_resolved_to, ''), COALESCE(reason, ''), status,
                COALESCE(resolved_by, '')
         FROM conflicts WHERE id = @id" [ "@id", box id ] (fun r ->
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
    return List.tryHead found
  }
  |> Ply.toTask

/// The location a conflict is about.
let locationOfConflict (c : Detect.Conflict) : PT.PackageLocation =
  { owner = c.owner
    modules = if c.modules = "" then [] else c.modules.Split('.') |> List.ofArray
    name = c.name }

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
