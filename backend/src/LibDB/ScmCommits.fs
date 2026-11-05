/// Reads over `commits`, for the Dark SCM code (`SCM.Commits`, and the commit listings in
/// `SCM.PackageOps`). One function, one query family: `query` takes which commits are wanted and how
/// many, and answers with commit rows (or just how many there are).
///
/// Each case is the SQL Dark used to run itself, moved across as it was: the same statement, the same
/// ordering, the same scope. Nothing here decides anything. A commit has no branch column, so the
/// branch cases ask the op log which commits a branch's ops sit in, exactly as before: main owns the
/// effective ops no branch has claimed, a branch owns the ops tagged to it.
///
/// A row with no `hash` is dropped, and every other column reads NULL as "", which is what the Dark
/// decoder did.
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
  /// The newest commit among the ops <param branchId> can see (its chain plus main), newest by stamp.
  | ParentFor of branchId : PT.BranchId


type Commit =
  { hash : string
    message : string
    originTs : string
    createdAt : string
    parent : string }

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


/// The parent-chain walk, as the Dark side's `chainCteSql` spells it: @p0 = the branch, @p1 = main's
/// id. Main's id is bound, never written as a literal.
let private chainCte =
  "WITH RECURSIVE chain(bid) AS (
     SELECT @p0
     UNION
     SELECT b.parent_id FROM branches b JOIN chain c ON b.id = c.bid
     WHERE b.parent_id != @p1
   )"

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
    let owned, ps =
      if branchId.IsMain then
        "p.effective = 1 AND p.id NOT IN (SELECT op_id FROM op_branches)", []
      else
        "p.id IN (SELECT op_id FROM op_branches WHERE branch_id = @p0)",
        [ "p0", Sql.string (string branchId) ]
    { from =
        "FROM commits c
         WHERE c.hash IN (SELECT DISTINCT p.commit_hash FROM package_ops p
                          WHERE p.commit_hash IS NOT NULL AND "
        + owned
        + ")"
      order = "ORDER BY c.rowid DESC"
      parameters = ps }

  | HashPrefix prefix ->
    { from = "FROM commits c WHERE c.hash LIKE @prefix || '%' ESCAPE '\\'"
      order = "ORDER BY c.created_at DESC, c.rowid DESC"
      parameters = [ "prefix", Sql.string (escapeLike prefix) ] }

  | ChildrenOf hash ->
    { from = "FROM commits c WHERE c.parent = @hash"
      order = ""
      parameters = [ "hash", Sql.string hash ] }

  | ParentFor branchId ->
    // What this branch can SEE, committed: its own ops plus main's, which is why a fresh branch's
    // first commit follows main's latest rather than nothing. A branch off a branch follows its
    // parent's commits too.
    let where, ps =
      if branchId.IsMain then
        "p.commit_hash IS NOT NULL AND p.effective = 1
         AND p.id NOT IN (SELECT op_id FROM op_branches)",
        []
      else
        "p.commit_hash IS NOT NULL
         AND (p.id IN (SELECT op_id FROM op_branches WHERE branch_id IN ("
        + chainCte
        + " SELECT bid FROM chain))
              OR (p.effective = 1 AND p.id NOT IN (SELECT op_id FROM op_branches)))",
        [ "p0", Sql.string (string branchId)
          "p1", Sql.string (string PT.BranchId.Main) ]
    { from =
        "FROM commits c WHERE c.hash IN (SELECT p.commit_hash FROM package_ops p WHERE "
        + where
        + ")"
      order = "ORDER BY c.origin_ts DESC, c.rowid DESC"
      parameters = ps }


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
          "SELECT c.hash, c.message, c.origin_ts, c.created_at, c.parent "
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
              parent = orEmpty "parent" }))
      let commits = List.choose identity rows
      return { total = int64 (List.length commits); commits = commits }
  }
