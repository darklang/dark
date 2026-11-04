/// Reads over `conflicts`, for the Dark SCM code (`SCM.Conflicts`, and the constraint and override
/// checks that share the table). One function, one query family: `query` takes what is being asked
/// and answers with rows.
///
/// Each case is the SQL Dark used to run itself, moved across as it was: the same statement, the
/// same ordering, the same branch scope. Nothing here decides anything: the superseded sweep, the
/// `#` on a pasted id and the candidate parsing all stay in Dark.
///
/// Writes (recording a conflict, settling one) are not here yet; they stay in Dark for now.
module LibDB.ScmConflicts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite


/// What is being asked. One case per question the Dark side asks.
type Query =
  /// Conflicts awaiting a human on this branch, oldest first.
  | Pending of branchId : uuid
  /// The one conflict on this branch whose id starts with this, or whose name (dotted, or bare) is
  /// this. Pending ones first, then oldest. At most one row.
  | ByRef of reference : string * branchId : uuid
  /// The ids of acked records that are not name conflicts: the constraint acks. Every branch.
  | AckedConstraints
  /// Whether any conflict was overridden, on any branch: at most one row, id only.
  | AnyOverridden


/// One row of `conflicts`. `candidates` is the JSON as stored; Dark parses it. Which fields are
/// filled depends on the query; the rest are "".
type Row =
  { id : string
    owner : string
    modules : string
    name : string
    itemType : string
    part : string
    kind : string
    candidates : string
    autoResolvedTo : string
    reason : string
    status : string
    resolvedBy : string }

let private empty : Row =
  { id = ""
    owner = ""
    modules = ""
    name = ""
    itemType = ""
    part = ""
    kind = ""
    candidates = ""
    autoResolvedTo = ""
    reason = ""
    status = ""
    resolvedBy = "" }


let private run
  (sql : string)
  (parameters : List<string * Microsoft.Data.Sqlite.SqliteParameter>)
  (read : RowReader -> Option<Row>)
  : Task<List<Row>> =
  task {
    let! rows = Sql.query sql |> Sql.parameters parameters |> Sql.executeAsync read
    return List.choose identity rows
  }

let private orEmpty (read : RowReader) (col : string) : string =
  read.stringOrNone col |> Option.defaultValue ""

/// The full row, as the Dark decoder read it: a row without an id is dropped, any other NULL reads
/// as "".
let private fullRow (read : RowReader) : Option<Row> =
  read.stringOrNone "id"
  |> Option.map (fun id ->
    { id = id
      owner = orEmpty read "owner"
      modules = orEmpty read "modules"
      name = orEmpty read "name"
      itemType = orEmpty read "item_type"
      part = orEmpty read "part"
      kind = orEmpty read "kind"
      candidates = orEmpty read "candidates"
      autoResolvedTo = orEmpty read "auto_resolved_to"
      reason = orEmpty read "reason"
      status = orEmpty read "status"
      resolvedBy = orEmpty read "resolved_by" })

let private idOnly (read : RowReader) : Option<Row> =
  read.stringOrNone "id" |> Option.map (fun id -> { empty with id = id })

let private branchParam (branchId : uuid) = Sql.string (string branchId)


let query (q : Query) : Task<List<Row>> =
  match q with
  | Pending branchId ->
    run
      "SELECT id, owner, modules, name, item_type, COALESCE(part,'') AS part, kind, candidates,
              auto_resolved_to, reason, status, COALESCE(resolved_by,'') AS resolved_by
       FROM conflicts WHERE status = 'pending' AND branch_id = @branch ORDER BY created_at"
      [ "branch", branchParam branchId ]
      fullRow

  | ByRef(reference, branchId) ->
    // PENDING FIRST. A name that has conflicted twice matches both rows, and the listing shows only
    // pending ones, so the command acting on the listing has to agree with it. Ordering rather than
    // filtering: `show <id>` of a settled conflict should still find it.
    run
      "SELECT id, owner, modules, name, item_type, COALESCE(part,'') AS part, kind, candidates,
              auto_resolved_to, reason, status, COALESCE(resolved_by,'') AS resolved_by
       FROM conflicts
       WHERE (id LIKE @ref || '%'
              OR (owner || '.' || modules || '.' || name) = @ref
              OR name = @ref)
         AND branch_id = @branch
       ORDER BY CASE WHEN status = 'pending' THEN 0 ELSE 1 END, created_at
       LIMIT 1"
      [ "ref", Sql.string reference; "branch", branchParam branchId ]
      fullRow

  | AckedConstraints ->
    run
      "SELECT id FROM conflicts
       WHERE status = 'acked'
         AND kind NOT IN ('same-name-different-hash', 'independent-creation')"
      []
      idOnly

  | AnyOverridden ->
    run "SELECT id FROM conflicts WHERE status = 'overridden' LIMIT 1" [] idOnly
