/// Reads over `commits`, for the Dark SCM code (`SCM.Commits`, and the commit listings in
/// `SCM.PackageOps`). One function, one query family: `query` takes which commits are wanted and how
/// many, and answers with commit rows (or just how many there are).
///
/// Each case is one statement over `commits`, and nothing here decides anything; the branch cases
/// filter on `branch_id`.
///
/// A row with no `hash` is dropped, and every other column reads NULL as "".
module LibDB.ScmCommits

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes


/// Which commits are wanted. One case per question the Dark side asks.
type Filter =
  /// Every commit in the store, newest first by when it was written here.
  | Recent
  /// The commits whose ops <param branchId> owns, newest first by insertion order.
  | OnBranch of branchId : PT.BranchId
  /// The commits whose hash starts with this, newest first. LIKE wildcards in the prefix are matched
  /// literally.
  | HashPrefix of prefix : string
  /// The commits that name this one as their parent.
  | ChildrenOf of hash : string


type Commit =
  {
    hash : string
    message : string
    originTs : string
    createdAt : string
    parent : string
    /// A commit that records what happened to a branch rather than work (an archive).
    housekeeping : bool
  }

/// `total` is the number of matching commits when the limit was 0 (and `commits` is then empty);
/// otherwise it is the number of commits returned.
type Page = { total : int64; commits : List<Commit> }


type private Shape =
  {
    /// Everything after `SELECT ...`: the table, its alias `c`, and the filter.
    from : string
    /// The ORDER BY clause, or "" for none.
    order : string
    parameters : List<string * Microsoft.Data.Sqlite.SqliteParameter>
  }


/// `\` is the escape character for the LIKE in `HashPrefix`.
let private escapeLike (s : string) : string =
  s.Replace("\\", "\\\\").Replace("%", "\\%").Replace("_", "\\_")


let private shape (f : Filter) : Shape =
  match f with
  | Recent ->
    { from = "FROM commits c"
      order = "ORDER BY c.created_at DESC, c.rowid DESC"
      parameters = [] }

  | OnBranch branchId ->
    // What the branch OWNS, which is a different question from what it can see: the number behind
    // "2 ahead" and behind a branch's own commit list.
    { from = "FROM commits c WHERE c.branch_id = @p0"
      order = "ORDER BY c.rowid DESC"
      parameters = [ "p0", Sql.string (string branchId) ] }

  | HashPrefix prefix ->
    { from = "FROM commits c WHERE c.hash LIKE @prefix || '%' ESCAPE '\\'"
      order = "ORDER BY c.created_at DESC, c.rowid DESC"
      parameters = [ "prefix", Sql.string (escapeLike prefix) ] }

  | ChildrenOf hash ->
    { from = "FROM commits c WHERE c.parent = @hash"
      order = ""
      parameters = [ "hash", Sql.string hash ] }


/// The commits <param f> selects, at most <param limit> of them (a negative limit means all of them).
/// A limit of 0 counts the matches instead.
let query (f : Filter) (limit : int64) : Task<Page> =
  task {
    let s = shape f
    if limit = 0L then
      let! n =
        Sql.query ("SELECT count(*) AS n " + s.from)
        |> Sql.parameters s.parameters
        |> Sql.executeRowAsync (fun read -> read.int64 "n")
      return { total = n; commits = [] }
    else
      let! rows =
        Sql.query (
          "SELECT c.hash, c.message, c.origin_ts, c.created_at, c.parent, c.housekeeping "
          + s.from
          + " "
          + s.order
          + " LIMIT @limit"
        )
        |> Sql.parameters (("limit", Sql.int64 limit) :: s.parameters)
        |> Sql.executeAsync (fun read ->
          let orEmpty col = read.stringOrNone col |> Option.defaultValue ""
          read.stringOrNone "hash"
          |> Option.map (fun h ->
            { hash = h
              message = orEmpty "message"
              originTs = orEmpty "origin_ts"
              createdAt = orEmpty "created_at"
              parent = orEmpty "parent"
              housekeeping = read.int64 "housekeeping" <> 0L }))
      let commits = List.choose identity rows
      return { total = int64 (List.length commits); commits = commits }
  }
