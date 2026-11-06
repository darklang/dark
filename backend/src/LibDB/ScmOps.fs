/// Reads over the op log (`package_ops`), for the Dark SCM code (`SCM.PackageOps`, `SCM.Branches`,
/// `SCM.Commits`, `SCM.Draft`). One function, one query family: `query` takes which ops are wanted
/// and how many, and answers with op rows (or just how many there are). The op blobs are decoded
/// here, once, so Dark gets `PackageOp`s rather than bytes.
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
  /// The ops a branch can SEE (its own and its ancestors', up to main's), newest first.
  | Visible of branchId : PT.BranchId
  /// The ids of the ops a branch can see that landed after (or at) a `created_at`, oldest first.
  | LandedSince of branchId : PT.BranchId * since : string * andAtSince : bool
  /// The ids of the ops that landed at exactly this `created_at`.
  | LandedAt of at : string
  /// These ops, oldest first. Ids the log does not hold are skipped.
  | Ids of ids : List<string>
  /// A branch's draft: its uncommitted ops, oldest first.
  | Draft of branchId : PT.BranchId
  /// `Draft`, ids only, sorted by id.
  | DraftIds of branchId : PT.BranchId
  /// A branch's own ops, committed or not, oldest first, with what wrote each one: what it would bring
  /// to a merge. None for main, which is the trunk and merges into nothing.
  | Own of branchId : PT.BranchId
  /// The ops written on a branch that a merge has since moved to its parent.
  | MergedFrom of branchId : PT.BranchId
  /// Every op of main's, oldest first.
  | Main
  /// One row whose `ts` is the newest `created_at` among landed ops, or "".
  | NewestLanded
  /// One row whose `ts` is the store's clock, in `created_at` form.
  | StoreNow
  /// One row whose `ts` sorts after everything in the log: its latest stamp plus a second, or now
  /// for an empty log.
  | NextStamp


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


let private branchParam (b : PT.BranchId) = "b", Sql.string (string b)

let private withBlobRows = "id, origin_ts AS ts, op_blob, source"

let private withBlob = "id, origin_ts AS ts, op_blob"


let private shape (f : Filter) : Shape =
  let s cols from order ps =
    { prefix = ""; cols = cols; from = from; order = order; parameters = ps }
  match f with
  | Recent -> s withBlob "FROM package_ops" "ORDER BY rowid DESC" []

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

  | Visible b ->
    { prefix = Layers.cte "@"
      cols = withBlob
      from = "FROM package_ops WHERE branch_id IN (SELECT bid FROM chain)"
      order = "ORDER BY rowid DESC"
      parameters = Layers.sqlParams b true }

  | LandedSince(b, since, andAtSince) ->
    // APPLIED ops only: reporting an op before it folds would let a watcher check the old version
    // and never look again.
    let comparison = if andAtSince then ">=" else ">"
    { prefix = Layers.cte "@"
      cols = "id"
      from =
        "FROM package_ops WHERE branch_id IN (SELECT bid FROM chain) AND applied = 1 "
        + $"AND created_at {comparison} @since"
      order = "ORDER BY created_at, rowid"
      parameters = ("since", Sql.string since) :: Layers.sqlParams b true }

  | LandedAt at ->
    s
      "id"
      "FROM package_ops WHERE created_at = @at AND applied = 1"
      ""
      [ "at", Sql.string at ]

  | Ids ids ->
    s
      withBlob
      "FROM package_ops WHERE id IN (SELECT value FROM json_each(@ids))
       GROUP BY id"
      "ORDER BY origin_ts, rowid"
      [ "ids", Sql.string (jsonStrings ids) ]

  | Draft b ->
    s
      withBlob
      "FROM package_ops WHERE branch_id = @b AND commit_hash IS NULL"
      "ORDER BY origin_ts, rowid"
      [ branchParam b ]

  | DraftIds b ->
    s
      "id"
      "FROM package_ops WHERE branch_id = @b AND commit_hash IS NULL"
      "ORDER BY id"
      [ branchParam b ]

  | Own b ->
    s
      withBlobRows
      "FROM package_ops WHERE branch_id = @b AND branch_id <> @main"
      "ORDER BY origin_ts, rowid"
      [ branchParam b; "main", Sql.string (string PT.BranchId.Main) ]

  | MergedFrom b ->
    s
      withBlob
      "FROM package_ops WHERE authored_on = @b AND branch_id <> @b"
      "ORDER BY origin_ts, rowid"
      [ branchParam b ]

  | Main ->
    s
      withBlob
      "FROM package_ops WHERE branch_id = @b"
      "ORDER BY origin_ts, rowid"
      [ branchParam PT.BranchId.Main ]

  | NewestLanded
  | StoreNow
  | NextStamp -> Exception.raiseInternal "a stamp case has no row shape" []


/// The one-value cases: the statement, whose single column is `ts`.
let private stampSql (f : Filter) : Option<string> =
  match f with
  | NewestLanded ->
    Some
      "SELECT COALESCE(max(created_at), '') AS ts FROM package_ops WHERE applied = 1"
  | StoreNow -> Some "SELECT datetime('now') AS ts"
  | NextStamp ->
    Some
      "SELECT COALESCE(strftime('%Y-%m-%dT%H:%M:%fZ', MAX(origin_ts), '+1 second'),
              strftime('%Y-%m-%dT%H:%M:%fZ','now')) AS ts FROM package_ops"
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
