/// The DRAFT: the ops you've authored on main and haven't committed yet.
///
/// Membership is `package_ops.commit_hash IS NULL`, restricted to ops that aren't tagged to a branch (a
/// branch's ops are branch-pending, and its own draft lives behind the overlay). Authoring is live-on-write,
/// so a draft op is already folded and already running; "draft" says it hasn't been reviewed and committed,
/// not that it's inert.
///
/// The draft is a REWRITABLE head over an immutable tail: committed ops never change, draft ops can be
/// dropped or collapsed until `commit` takes them. That is what makes it safe to run propagation on every
/// edit without every intermediate version becoming permanent.
///
/// Rewriting means un-doing a fold, and a fold has no general inverse -- `locations` and `deprecations`
/// record the RESULT of the whole sequence. Two ways back:
///
///   - SURGICAL. Delete the rows the dropped ops wrote and re-list what they superseded. O(dropped), never
///     disturbs an untouched row. Sound only for ops whose rows are identifiable afterwards, which
///     `surgicallyUndoable` decides.
///   - REBUILD. Delete every main op and re-insert what survives. Always correct, and expensive enough that
///     every other reader sees a half-empty store while it runs.
///
/// Surgical covers ordinary authoring and propagation. The rebuild is the fallback, not the default.
module LibDB.Draft

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


/// One op in main's draft, with what's needed to undo its effect.
type DraftOp =
  {
    id : System.Guid
    /// The authoring stamp. `locations.origin_ts` carries the stamp of the op that wrote the row, so this
    /// is how a row is traced back to the op that put it there.
    ts : string
    op : PT.PackageOp
  }


/// Main's draft, oldest first.
let read () : Task<List<DraftOp>> =
  Sql.query
    """
    SELECT id, op_blob, origin_ts
    FROM package_ops
    WHERE commit_hash IS NULL
      AND id NOT IN (SELECT op_id FROM op_branches)
    ORDER BY origin_ts, rowid
    """
  |> Sql.executeAsync (fun read ->
    let id = read.uuid "id"
    { id = id
      ts = read.stringOrNone "origin_ts" |> Option.defaultValue ""
      op = BS.PT.PackageOp.deserialize id (read.bytes "op_blob") })


/// The item hashes the draft ADDS. A SetName paired with one of these named a new item rather than renaming
/// an existing one, which is the distinction the fold made when it wrote the binding.
let private addedHashes (ops : List<PT.PackageOp>) : Set<PT.Hash> =
  ops
  |> List.choose (fun op ->
    match op with
    | PT.PackageOp.AddType t -> Some t.hash
    | PT.PackageOp.AddValue v -> Some v.hash
    | PT.PackageOp.AddFn f -> Some f.hash
    | _ -> None)
  |> Set.ofList


/// Can dropping this op be undone by deleting the rows it wrote and re-listing what it superseded?
///
/// Yes for the ops a draft is made of:
///   - `Add*` write content-addressed rows keyed by hash. Leaving them breaks nothing: content is immutable
///     and shared, and an orphan is a GC question.
///   - `SetName` paired with an `Add` in the same draft wrote one `locations` row, identifiable by
///     (name, hash, origin_ts). A STANDALONE SetName is a rename, and the fold also unlists every other row
///     holding that hash -- rows this cannot identify afterwards, since `unlisted_at` is a wall-clock second.
///   - `Resolve` writes one `locations` row, same as above.
///
/// No for the rest: `Deprecate` / `Undeprecate` write `deprecations`, which has no `origin_ts` to trace a
/// row back by, and `Decide` upserts `propagation_policy` without recording what it overwrote.
let private surgicallyUndoable (added : Set<PT.Hash>) (op : PT.PackageOp) : bool =
  match op with
  | PT.PackageOp.AddType _
  | PT.PackageOp.AddValue _
  | PT.PackageOp.AddFn _ -> true
  | PT.PackageOp.SetName(_, target, _) -> Set.contains target.hash added
  | PT.PackageOp.Resolve _ -> true
  | PT.PackageOp.Deprecate _
  | PT.PackageOp.Undeprecate _
  | PT.PackageOp.Decide _ -> false
  // A branch event writes `branches`, which records no origin_ts, so the row it changed cannot be traced
  // back to the op that changed it. Same reason as the three above: rebuild rather than guess.
  | PT.PackageOp.BranchEvent _ -> false


/// The `locations` row an op wrote, as the tuple that identifies it: (owner, modules, name, hash, ts).
let private locationRowOf
  (d : DraftOp)
  : Option<string * string * string * string * string> =
  let row (loc : PT.PackageLocation) (target : PT.Reference) =
    let (PT.Hash h) = target.hash
    Some(loc.owner, String.concat "." loc.modules, loc.name, h, d.ts)

  match d.op with
  | PT.PackageOp.SetName(loc, target, _) -> row loc target
  | PT.PackageOp.Resolve(_, loc, target) -> row loc target
  | _ -> None


/// Drop ops by deleting the rows they wrote and re-listing what they superseded.
///
/// Order matters: `locations` rows first, then names left with nothing live get their most recent surviving
/// binding back, then the ops. Op rows first would leave a window where a reader sees a binding no op
/// explains.
let private surgicalDrop (dropped : List<DraftOp>) : Task<unit> =
  task {
    let rows = dropped |> List.choose locationRowOf

    // Delete by the full tuple, not by `origin_ts` alone: a stamp is unique per process but two instances
    // can mint the same one, and deleting a committed row that happened to collide would silently unbind a
    // name nobody touched.
    for (owner, modules, name, hash, ts) in rows do
      do!
        Sql.query
          """
          DELETE FROM locations
          WHERE owner = @owner AND modules = @modules AND name = @name
            AND item_hash = @hash AND origin_ts = @ts
          """
        |> Sql.parameters
          [ "owner", Sql.string owner
            "modules", Sql.string modules
            "name", Sql.string name
            "hash", Sql.string hash
            "ts", Sql.string ts ]
        |> Sql.executeStatementAsync

    // Re-list what the dropped binding had superseded: the most recent row left for that name. Ordered by
    // `origin_ts` (the authoring stamp) rather than by insertion, so this restores the same binding the
    // fold's timestamp-LWW would pick, rather than whichever row happened to be written last.
    let names = rows |> List.map (fun (o, m, n, _, _) -> (o, m, n)) |> List.distinct

    for (owner, modules, name) in names do
      do!
        Sql.query
          """
          UPDATE locations SET unlisted_at = NULL
          WHERE location_id = (
            SELECT location_id FROM locations
            WHERE owner = @owner AND modules = @modules AND name = @name
            ORDER BY origin_ts DESC, rowid DESC
            LIMIT 1
          )
          AND NOT EXISTS (
            SELECT 1 FROM locations
            WHERE owner = @owner AND modules = @modules AND name = @name
              AND unlisted_at IS NULL
          )
          """
        |> Sql.parameters
          [ "owner", Sql.string owner
            "modules", Sql.string modules
            "name", Sql.string name ]
        |> Sql.executeStatementAsync

    for d in dropped do
      do!
        Sql.query "DELETE FROM package_ops WHERE id = @id"
        |> Sql.parameters [ "id", Sql.uuid d.id ]
        |> Sql.executeStatementAsync

    // This path edits `locations` directly rather than folding, so it has to say so itself.
    Caching.invalidateAll ()
  }


/// Delete every main op and re-insert the ones that survive. The fallback when the surgical path can't
/// identify what a dropped op wrote.
let private rebuild (keptIds : Set<System.Guid>) : Task<unit> =
  task {
    let! ops = Queries.getWipOps ()
    let! preserveTs = Queries.getWipOpOriginTs ()
    let! preserveCommit = Queries.getWipOpCommits ()

    // An op survives if it was committed, or if the caller kept it.
    let surviving =
      ops
      |> List.filter (fun op ->
        let id = Inserts.computeOpHash op
        Map.containsKey id preserveCommit || Set.contains id keptIds)

    match! Inserts.discardWipOps () with
    | Error msg -> Exception.raiseInternal "draft rebuild failed" [ "msg", msg ]
    | Ok _ ->
      let! _ =
        Inserts.insertAndApplyOpsPreservingTs preserveTs preserveCommit surviving
      ()
  }


/// Rewrite the draft to whatever <param keep> returns, and report how many ops went.
///
/// `keep` gets the draft in authoring order and returns the subset that remains: it SELECTS, it does not
/// author. That contract is what makes both callers one operation -- `discard` keeps nothing, `collapse`
/// keeps the last version of each name.
let rewriteBy (keep : List<DraftOp> -> List<DraftOp>) : Task<Result<int64, string>> =
  task {
    try
      let! draft = read ()

      if List.isEmpty draft then
        return Ok 0L
      else
        let keptIds = keep draft |> List.map (fun d -> d.id) |> Set.ofList

        let dropped = draft |> List.filter (fun d -> not (Set.contains d.id keptIds))
        let count = int64 (List.length dropped)

        if count = 0L then
          return Ok 0L
        else
          let added = addedHashes (draft |> List.map (fun d -> d.op))

          if dropped |> List.forall (fun d -> surgicallyUndoable added d.op) then
            do! surgicalDrop dropped
          else
            do! rebuild keptIds

          return Ok count
    with ex ->
      return Error ex.Message
  }


/// `rewriteBy` for callers that only need the op's SHAPE to decide.
///
/// Selecting by shape can't tell two ops apart when they're the same shape, which the pin path needs (a name
/// can hold both an edit you typed and a repoint propagation wrote), so that one uses `rewriteBy` directly.
let rewrite
  (keep : List<PT.PackageOp> -> List<PT.PackageOp>)
  : Task<Result<int64, string>> =
  rewriteBy (fun draft ->
    let keptOps = keep (draft |> List.map (fun d -> d.op)) |> Set.ofList
    draft |> List.filter (fun d -> Set.contains d.op keptOps))


/// Drop main's draft entirely. Returns how many ops went.
let discard () : Task<Result<int64, string>> = rewrite (fun _ -> [])


/// Collapse the draft's superseded NAMINGS: for each name, keep the last binding and drop the earlier ones.
///
/// Five edits to one function otherwise leave five namings in the log, four of them describing a version
/// that stopped being what the name means before anyone saw it. An intermediate draft version is a
/// keystroke, not history.
///
/// Deliberately does NOT drop the `Add` carrying each intermediate version's content: that needs a
/// reachability check -- a pinned dependent may be the only thing still referring to one -- and getting it
/// wrong means a re-fold produces a different store. Keeping them re-folds identically: content present,
/// name bound once.
///
/// Run at commit. Before that every version in the draft is reachable by `undo`.
let collapse () : Task<Result<int64, string>> =
  rewrite (fun ops ->
    let lastBindingIndex =
      ops
      |> List.indexed
      |> List.fold
        (fun acc (i, op) ->
          match op with
          | PT.PackageOp.SetName(loc, _, _) ->
            Map.add (loc.owner, loc.modules, loc.name) i acc
          | _ -> acc)
        Map.empty

    ops
    |> List.indexed
    |> List.filter (fun (i, op) ->
      match op with
      | PT.PackageOp.SetName(loc, _, _) ->
        Map.tryFind (loc.owner, loc.modules, loc.name) lastBindingIndex = Some i
      | _ -> true)
    |> List.map snd)


/// The authoring stamps of the bindings PROPAGATION wrote for this name, live or since superseded.
///
/// Needed because a name can hold BOTH in one draft: an edit you typed and a repoint that followed
/// something else. They are the same shape as ops, so the only thing that tells them apart is which
/// `locations` row each one wrote, and `source` is recorded there per binding.
///
/// Unlisted rows count. An edit after a repoint supersedes it, which unlists the repoint's row while its op
/// is still sitting in the draft -- exactly the case that has to be undone.
let private propagationStampsFor (loc : PT.PackageLocation) : Task<Set<string>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT origin_ts FROM locations
        WHERE owner = @owner AND modules = @modules AND name = @name
          AND source = 'propagation'
        """
      |> Sql.parameters
        [ "owner", Sql.string loc.owner
          "modules", Sql.string (String.concat "." loc.modules)
          "name", Sql.string loc.name ]
      |> Sql.executeAsync (fun read ->
        read.stringOrNone "origin_ts" |> Option.defaultValue "")
    return Set.ofList rows
  }


/// Is this name's live binding a repoint that PROPAGATION staged and no commit has committed?
///
/// Joined on `locations.op_id`, the exact op the fold recorded for this binding. The stamp (`origin_ts`)
/// orders bindings but does not identify one: two instances can mint the same stamp, and after a sync this
/// store holds both -- so matching on it picks whichever came back first and `pin` takes the wrong branch.
let private isStagedRepoint (loc : PT.PackageLocation) : Task<bool> =
  task {
    let! row =
      Sql.query
        """
        SELECT l.source AS source
        FROM locations l
        WHERE l.owner = @owner AND l.modules = @modules AND l.name = @name
          AND l.unlisted_at IS NULL
          AND EXISTS (
            SELECT 1 FROM package_ops p
            WHERE p.id = l.op_id
              AND p.commit_hash IS NULL
              AND p.id NOT IN (SELECT op_id FROM op_branches)
          )
        LIMIT 1
        """
      |> Sql.parameters
        [ "owner", Sql.string loc.owner
          "modules", Sql.string (String.concat "." loc.modules)
          "name", Sql.string loc.name ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "source")

    return row = Some "propagation"
  }


/// Un-stage the repoint the draft holds for one name: a PIN, before commit.
///
/// A pin is retroactive by design: by the time you decide something should not have followed, it already
/// has. Authoring a rebinding op would say "and now put it back", a permanent op recording a decision still
/// being made; dropping the staged repoint says it never happened, and leaves nothing behind.
///
/// Returns 0 when there is nothing staged -- the binding is committed, or you authored it -- and the caller
/// then takes the post-commit path, which rebinds against committed history.
///
/// Only the naming op is dropped, never the `Add`. A surviving `Add` re-folds identically: content present,
/// name not bound to it. Dropping content needs a reachability check.
let unstageRepoint (loc : PT.PackageLocation) : Task<Result<int64, string>> =
  task {
    match! isStagedRepoint loc with
    | false -> return Ok 0L
    | true ->
      // Only the namings PROPAGATION wrote. Dropping every naming for the name silently reverted an edit
      // you had typed yourself, whenever you'd edited the item and then edited something it depends on:
      // the pin reported success and the item went back to its committed version, losing your work.
      let! propagated = propagationStampsFor loc

      return!
        rewriteBy (fun draft ->
          draft
          |> List.filter (fun d ->
            match d.op with
            | PT.PackageOp.SetName(l, _, _) when l = loc ->
              not (Set.contains d.ts propagated)
            | _ -> true))
  }
