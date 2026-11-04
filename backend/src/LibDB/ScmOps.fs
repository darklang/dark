/// Reads over the op log (`package_ops`) and the branch tags on it (`op_branches`), for the Dark SCM
/// code (`SCM.PackageOps`, `SCM.Branches`, `SCM.Commits`, `SCM.Draft`). One function, one query family:
/// `query` takes which ops are wanted and how many, and answers with op rows (or just how many there
/// are). The op blobs are decoded here, once, so Dark gets `PackageOp`s rather than bytes.
///
/// Each case is the SQL Dark used to run itself, moved across as it was: the same statement, the same
/// ordering, the same scope. Nothing here decides anything. Main versus branch is spelled out per case
/// exactly as the Dark side spelled it: main sees and owns the ops no branch has tagged, a branch the
/// ops tagged to it (and, for what it can SEE, to its parent chain).
///
/// A row with no `id` is dropped, which is what every Dark decoder did. A blob this build cannot read
/// (or a row whose id is not a uuid) comes back with no op rather than being dropped, so a listing can
/// still show it; the Dark side drops those where it used to.
module LibDB.ScmOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization


/// Which ops are wanted. One case per question the Dark side asks.
type Filter =
  /// Every op in the log, newest first by when it landed here.
  | Recent
  /// The ops whose id starts with this, newest first.
  | IdPrefix of prefix : string
  /// The ops a commit holds, oldest first.
  | InCommit of hash : string
  /// `InCommit`, ids only.
  | IdsInCommit of hash : string
  /// The ops a branch can SEE (main's, plus its chain's own), newest first by insertion order.
  | Visible of branchId : PT.BranchId
  /// The ids of the ops a branch can see that landed after (or at) a `created_at`, oldest first.
  | LandedSince of branchId : PT.BranchId * since : string * andAtSince : bool
  /// The ids of the ops that landed at exactly this `created_at`.
  | LandedAt of at : string
  /// These ops, oldest first. Ids the log does not hold are skipped.
  | Ids of ids : List<string>
  /// Every `BranchEvent` op.
  | BranchEvents
  /// The draft: on main the uncommitted effective ops no branch has tagged, on a branch its own
  /// uncommitted ops. Oldest first.
  | Draft of branchId : PT.BranchId
  /// `Draft`, ids only, sorted by id.
  | DraftIds of branchId : PT.BranchId
  /// A branch's uncommitted ops that this instance authored (not imported). Ids only.
  | AuthoredDraft of branchId : PT.BranchId
  /// A branch's own tagged ops, oldest first, with what put each one there.
  | Own of branchId : PT.BranchId
  /// A branch's own uncommitted ops. Ids only.
  | OwnUncommitted of branchId : PT.BranchId
  /// Every op id tagged to a branch, from the tags alone (an op row need not exist).
  | Tagged of branchId : PT.BranchId
  /// A branch's own ops that are not yet effective. Ids only.
  | Unmerged of branchId : PT.BranchId
  /// A branch's uncommitted, inert ops: what discarding its draft deletes. Ids only.
  | DiscardableDraft of branchId : PT.BranchId
  /// The ops a branch's chain shows it, oldest first: the branch's own, and what its ancestors below
  /// main committed. An ancestor's draft stays its own.
  | Chain of branchId : PT.BranchId
  /// `Chain`, committed ops only.
  | ChainCommitted of branchId : PT.BranchId
  /// The uncommitted effective ops, tagged or not. Ids only.
  | MainUncommitted
  /// Every effective op, oldest first.
  | Effective
  /// One row whose `ts` is the next commit stamp: the log's latest `origin_ts` plus a second, or now
  /// for an empty log.
  | NextStamp
  /// One row whose `ts` is the log's latest `origin_ts` plus a second, or "" for an empty log.
  | StampPastEverything
  /// One row whose `ts` is the newest `created_at` among landed ops, or "".
  | NewestLanded
  /// One row whose `ts` is the store's clock, in `created_at` form.
  | StoreNow


/// One op row. `ts` is the authoring stamp (`origin_ts`), "" where the case did not read it;
/// `sizeBytes` is the blob's size, 0 where the case did not read the blob; `source` is what put the
/// op on the branch, for `Own` only ("" elsewhere); `op` is the decoded op, None where the case did not
/// read the blob or this build cannot read it.
type Row =
  { id : string
    ts : string
    sizeBytes : int64
    source : string
    op : Option<PT.PackageOp> }

/// `total` is the number of matching ops when the limit was 0 (and `ops` is then empty); otherwise it
/// is the number of rows returned.
type Page = { total : int64; ops : List<Row> }


type private Shape =
  {
    /// Anything before `SELECT`: the chain walk, for the case that reads through it.
    prefix : string
    /// The columns, aliased `id`, `ts`, `op_blob` and `source` where present.
    cols : string
    /// Everything after the columns: the table and the filter.
    from : string
    /// The ORDER BY clause, or "" for none.
    order : string
    parameters : List<string * Microsoft.Data.Sqlite.SqliteParameter>
  }


/// The parent-chain walk, as the Dark side's `chainCteSql` spells it: @p0 = the branch, @p1 = main's
/// id. Main's id is bound, never written as a literal.
let private chainCte =
  "WITH RECURSIVE chain(bid) AS (
     SELECT @p0
     UNION
     SELECT b.parent_id FROM branches b JOIN chain c ON b.id = c.bid
     WHERE b.parent_id != @p1
   )"

let private branchParam (b : PT.BranchId) = "p0", Sql.string (string b)

let private chainParams (b : PT.BranchId) =
  [ branchParam b; "p1", Sql.string (string PT.BranchId.Main) ]

/// Which ops a branch can SEE: main's log, plus its chain's own delta ops.
let private visibleWhere (b : PT.BranchId) =
  if b.IsMain then
    "id NOT IN (SELECT op_id FROM op_branches)", []
  else
    "(id NOT IN (SELECT op_id FROM op_branches)
      OR id IN (SELECT op_id FROM op_branches WHERE branch_id IN ("
    + chainCte
    + " SELECT bid FROM chain)))",
    chainParams b

/// The draft, in one place, as the Dark side's `draftWhere` spells it.
let private draftWhere (b : PT.BranchId) =
  if b.IsMain then
    "p.commit_hash IS NULL AND p.effective = 1
     AND p.id NOT IN (SELECT op_id FROM op_branches)",
    []
  else
    "p.commit_hash IS NULL
     AND p.id IN (SELECT op_id FROM op_branches WHERE branch_id = @p0)",
    [ branchParam b ]

/// An op has landed once it is applied (main's) or tagged onto a branch (a branch's own are inert).
let private landedWhere = "(applied = 1 OR id IN (SELECT op_id FROM op_branches))"

// A JSON array of strings for `json_each` to bind. Written with the writer, never the reflecting
// serializer, which the published (AOT) binary does not have.
let private jsonList (items : List<string>) : string =
  use buffer = new System.IO.MemoryStream()
  (use w = new System.Text.Json.Utf8JsonWriter(buffer)
   w.WriteStartArray()
   items |> List.iter (fun (s : string) -> w.WriteStringValue s)
   w.WriteEndArray())
  System.Text.Encoding.UTF8.GetString(buffer.ToArray())

let private withBlob = "id, origin_ts AS ts, op_blob"
let private withBlobP = "p.id, p.origin_ts AS ts, p.op_blob"


let private shape (f : Filter) : Shape =
  let s cols from order ps =
    { prefix = ""; cols = cols; from = from; order = order; parameters = ps }
  match f with
  | Recent ->
    // `rowid DESC` breaks the tie: `created_at` is second-resolution, so a burst of ops inside one
    // second ties and a LIMIT would take an arbitrary subset.
    s withBlob "FROM package_ops" "ORDER BY created_at DESC, rowid DESC" []

  | IdPrefix prefix ->
    s
      withBlob
      "FROM package_ops WHERE id LIKE @prefix || '%'"
      "ORDER BY rowid DESC"
      [ "prefix", Sql.string prefix ]

  | InCommit hash ->
    s
      withBlob
      "FROM package_ops WHERE commit_hash = @hash"
      "ORDER BY origin_ts, rowid"
      [ "hash", Sql.string hash ]

  | IdsInCommit hash ->
    s
      "id"
      "FROM package_ops WHERE commit_hash = @hash"
      "ORDER BY origin_ts, rowid"
      [ "hash", Sql.string hash ]

  | Visible b ->
    let where, ps = visibleWhere b
    s withBlob ("FROM package_ops WHERE " + where) "ORDER BY rowid DESC" ps

  | LandedSince(b, since, andAtSince) ->
    // APPLIED ops only on main: an op is inserted before it is folded and marked applied after, so
    // reporting it earlier would let a watcher check the old version and never look again. A branch's
    // own ops are inert for good and tagged in the transaction that stores them.
    let where, ps = visibleWhere b
    let complete = if b.IsMain then "applied = 1" else landedWhere
    let comparison = if andAtSince then ">=" else ">"
    s
      "id"
      ("FROM package_ops WHERE "
       + where
       + $" AND {complete} AND created_at {comparison} @since")
      "ORDER BY created_at, rowid"
      (("since", Sql.string since) :: ps)

  | LandedAt at ->
    s
      "id"
      ("FROM package_ops WHERE created_at = @at AND " + landedWhere)
      ""
      [ "at", Sql.string at ]

  | Ids ids ->
    s
      withBlob
      "FROM package_ops WHERE id IN (SELECT value FROM json_each(@ids))"
      "ORDER BY origin_ts, rowid"
      [ "ids", Sql.string (jsonList ids) ]

  | BranchEvents ->
    // Byte 9 of the blob is the op tag; 0x0A is BranchEvent.
    s withBlob "FROM package_ops WHERE substr(op_blob, 9, 1) = X'0A'" "" []

  | Draft b ->
    let where, ps = draftWhere b
    s
      withBlobP
      ("FROM package_ops p WHERE " + where)
      "ORDER BY p.origin_ts, p.rowid"
      ps

  | DraftIds b ->
    let where, ps = draftWhere b
    s "p.id" ("FROM package_ops p WHERE " + where) "ORDER BY p.id" ps

  | AuthoredDraft b ->
    s
      "p.id"
      "FROM package_ops p
       JOIN op_branches ob ON ob.op_id = p.id
       WHERE p.commit_hash IS NULL AND ob.branch_id = @p0 AND ob.source <> 'import'"
      ""
      [ branchParam b ]

  | Own b ->
    s
      "p.id, p.op_blob, p.origin_ts AS ts, ob.source"
      "FROM package_ops p
       JOIN op_branches ob ON ob.op_id = p.id
       WHERE ob.branch_id = @p0"
      "ORDER BY p.origin_ts, p.rowid"
      [ branchParam b ]

  | OwnUncommitted b ->
    s
      "p.id"
      "FROM package_ops p
       JOIN op_branches ob ON ob.op_id = p.id
       WHERE ob.branch_id = @p0 AND p.commit_hash IS NULL"
      ""
      [ branchParam b ]

  | Tagged b ->
    s "op_id AS id" "FROM op_branches WHERE branch_id = @p0" "" [ branchParam b ]

  | Unmerged b ->
    s
      "p.id"
      "FROM package_ops p
       JOIN op_branches ob ON ob.op_id = p.id
       WHERE ob.branch_id = @p0 AND p.effective = 0"
      ""
      [ branchParam b ]

  | DiscardableDraft b ->
    s
      "id"
      "FROM package_ops
       WHERE commit_hash IS NULL AND effective = 0
         AND id IN (SELECT op_id FROM op_branches WHERE branch_id = @p0)"
      ""
      [ branchParam b ]

  | Chain b ->
    { prefix = chainCte
      cols = withBlobP
      from =
        "FROM package_ops p JOIN op_branches ob ON ob.op_id = p.id
         WHERE ob.branch_id IN (SELECT bid FROM chain)
           AND (ob.branch_id = @p0 OR p.commit_hash IS NOT NULL)"
      order = "ORDER BY p.origin_ts, p.rowid"
      parameters = chainParams b }

  | ChainCommitted b ->
    { prefix = chainCte
      cols = withBlobP
      from =
        "FROM package_ops p JOIN op_branches ob ON ob.op_id = p.id
         WHERE ob.branch_id IN (SELECT bid FROM chain) AND p.commit_hash IS NOT NULL"
      order = "ORDER BY p.origin_ts, p.rowid"
      parameters = chainParams b }

  | MainUncommitted ->
    s "id" "FROM package_ops WHERE commit_hash IS NULL AND effective = 1" "" []

  | Effective ->
    s withBlob "FROM package_ops WHERE effective = 1" "ORDER BY origin_ts, rowid" []

  | NextStamp
  | StampPastEverything
  | NewestLanded
  | StoreNow -> Exception.raiseInternal "a stamp case has no row shape" []


/// The one-value cases: the statement, whose single column is `ts`.
let private stampSql (f : Filter) : Option<string> =
  match f with
  | NextStamp ->
    Some
      "SELECT COALESCE(strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second'),
              strftime('%Y-%m-%dT%H:%M:%fZ','now')) AS ts FROM package_ops"
  | StampPastEverything ->
    Some
      "SELECT strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second') AS ts
       FROM package_ops"
  | NewestLanded ->
    Some(
      "SELECT COALESCE(max(created_at), '') AS ts FROM package_ops WHERE "
      + landedWhere
    )
  | StoreNow -> Some "SELECT datetime('now') AS ts"
  | _ -> None


/// Reads one row of a shape's `cols`: `id` always, and `ts`, `op_blob` and `source` when it selects them.
let private readRow (cols : string) (read : RowReader) : Option<Row> =
  let has (col : string) = cols.Contains col
  let textOr col =
    if has col then read.stringOrNone col |> Option.defaultValue "" else ""
  match read.stringOrNone "id" with
  | None -> None
  | Some id ->
    let blob = if has "op_blob" then read.bytesOrNone "op_blob" else None
    let op =
      match blob, System.Guid.TryParse id with
      | Some bytes, (true, guid) -> BS.PT.PackageOp.tryDeserialize guid bytes
      | _ -> None
    Some
      { id = id
        ts = textOr "ts"
        sizeBytes =
          blob |> Option.map (fun b -> int64 b.Length) |> Option.defaultValue 0L
        source = textOr "source"
        op = op }


/// The ops <param f> selects, at most <param limit> of them (a negative limit means all of them).
/// A limit of 0 counts the matches instead. The stamp cases ignore the limit and answer one row.
let query (f : Filter) (limit : int64) : Task<Page> =
  task {
    match stampSql f with
    | Some sql ->
      let! ts =
        Sql.query sql
        |> Sql.executeRowAsync (fun read ->
          read.stringOrNone "ts" |> Option.defaultValue "")
      return
        { total = 1L
          ops = [ { id = ""; ts = ts; sizeBytes = 0L; source = ""; op = None } ] }
    | None ->
      let s = shape f
      if limit = 0L then
        let! n =
          Sql.query (s.prefix + " SELECT count(*) AS n " + s.from)
          |> Sql.parameters s.parameters
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        return { total = n; ops = [] }
      else
        let! rows =
          Sql.query (
            s.prefix
            + " SELECT "
            + s.cols
            + " "
            + s.from
            + " "
            + s.order
            + " LIMIT @limit"
          )
          |> Sql.parameters (("limit", Sql.int64 limit) :: s.parameters)
          |> Sql.executeAsync (readRow s.cols)
        let ops = List.choose identity rows
        return { total = int64 (List.length ops); ops = ops }
  }
