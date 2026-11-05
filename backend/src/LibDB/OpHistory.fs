/// Visibility rules shared by reference names and undo history.
module LibDB.OpHistory

open FSharp.Control.Tasks
open Fumble
open LibDB.Sqlite
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization

/// Queries alias package_ops as p. The trusted SQL predicate identifies operations
/// owned by the reader; other operations must have been committed by the cutoff.
let visibility (own : string) (before : Option<string>) : string =
  let committed = $"(({own}) OR p.commit_hash IS NOT NULL)"
  match before with
  | None -> committed
  | Some _ ->
    $"{committed} AND p.origin_ts <= @before AND (({own}) OR EXISTS (SELECT 1 FROM commits c WHERE c.hash = p.commit_hash AND c.origin_ts <= @before))"

/// Read one scope's revisions. Undo chooses the parent cutoff after filtering
/// these operations to the location being undone, so ancestry stays with undo.
let revisionOperations (scope : PT.BranchId) inherited before =
  task {
    // `+` keeps SQLite on the op-kind index: main's ops are nearly the whole log.
    let ownership = "+p.branch_id = @scope"
    // Undo cutoffs describe committed history, including when explicitly applied
    // to the current scope. Live current-scope reads have no cutoff.
    let committedOnly = inherited || Option.isSome before
    let visible = visibility (if committedOnly then "0 = 1" else "1 = 1") before
    let! rows =
      Sql.query
        $"SELECT p.id, p.op_blob, p.origin_ts AS ts FROM package_ops p
          WHERE {ownership} AND {visible}
            AND p.op_kind IN ('set-name', 'unbind', 'rename', 'reference-names', 'decision')
          ORDER BY p.origin_ts, p.rowid"
      |> Sql.parameters (
        [ "scope", Sql.string (string scope) ]
        @ (before
           |> Option.toList
           |> List.map (fun stamp -> "before", Sql.string stamp))
      )
      |> Sql.executeAsync (fun row ->
        let id = row.uuid "id"
        id,
        Option.defaultValue "" (row.stringOrNone "ts"),
        BS.PT.PackageOp.tryDeserialize id (row.bytes "op_blob"))
    return
      rows
      |> List.choose (fun (id, stamp, op) ->
        op |> Option.map (fun op -> string id, stamp, op))
  }
