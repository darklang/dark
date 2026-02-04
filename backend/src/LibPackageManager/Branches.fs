module LibPackageManager.Branches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes


/// Well-known main branch UUID
let mainBranchId : PT.BranchId = PT.mainBranchId


type Branch =
  { id : PT.BranchId
    name : string
    parentBranchId : Option<PT.BranchId>
    baseCommitId : Option<uuid>
    createdAt : NodaTime.Instant
    mergedAt : Option<NodaTime.Instant> }


let private readBranch (read : RowReader) : Branch =
  { id = read.uuid "id"
    name = read.string "name"
    parentBranchId = read.uuidOrNone "parent_branch_id"
    baseCommitId = read.uuidOrNone "base_commit_id"
    createdAt = read.instant "created_at"
    mergedAt = read.instantOrNone "merged_at" }


let create (name : string) (parentBranchId : PT.BranchId) : Task<Branch> =
  task {
    let id = System.Guid.NewGuid()

    // Get parent's latest commit as base
    let! baseCommitId =
      Sql.query
        """
        SELECT id FROM commits
        WHERE branch_id = @parent_id
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters [ "parent_id", Sql.uuid parentBranchId ]
      |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")

    do!
      Sql.query
        """
        INSERT INTO branches (id, name, parent_branch_id, base_commit_id, created_at)
        VALUES (@id, @name, @parent_id, @base_commit_id, datetime('now'))
        """
      |> Sql.parameters
        [ "id", Sql.uuid id
          "name", Sql.string name
          "parent_id", Sql.uuid parentBranchId
          "base_commit_id", Sql.uuidOrNone baseCommitId ]
      |> Sql.executeStatementAsync

    return
      { id = id
        name = name
        parentBranchId = Some parentBranchId
        baseCommitId = baseCommitId
        createdAt = NodaTime.SystemClock.Instance.GetCurrentInstant()
        mergedAt = None }
  }


let get (id : PT.BranchId) : Task<Option<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, parent_branch_id, base_commit_id, created_at, merged_at
        FROM branches
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync readBranch
  }


let getByName (name : string) : Task<Option<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, parent_branch_id, base_commit_id, created_at, merged_at
        FROM branches
        WHERE name = @name
        """
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeRowOptionAsync readBranch
  }


let list () : Task<List<Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, parent_branch_id, base_commit_id, created_at, merged_at
        FROM branches
        WHERE merged_at IS NULL
        ORDER BY created_at ASC
        """
      |> Sql.executeAsync readBranch
  }


let rename (id : PT.BranchId) (newName : string) : Task<Result<unit, string>> =
  task {
    try
      do!
        Sql.query
          """
          UPDATE branches SET name = @name WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid id; "name", Sql.string newName ]
        |> Sql.executeStatementAsync
      return Ok()
    with ex ->
      return Error ex.Message
  }


let delete (id : PT.BranchId) : Task<Result<unit, string>> =
  task {
    if id = mainBranchId then
      return Error "Cannot delete main branch"
    else
      // Check for children
      let! children =
        Sql.query
          """
          SELECT COUNT(*) as cnt FROM branches
          WHERE parent_branch_id = @id AND merged_at IS NULL
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeAsync (fun read -> read.int64 "cnt")

      match children with
      | [ cnt ] when cnt > 0L ->
        return Error "Cannot delete branch with active children"
      | _ ->
        do!
          Sql.query "DELETE FROM branches WHERE id = @id"
          |> Sql.parameters [ "id", Sql.uuid id ]
          |> Sql.executeStatementAsync
        return Ok()
  }


let setMerged (id : PT.BranchId) : Task<unit> =
  task {
    do!
      Sql.query
        """
        UPDATE branches SET merged_at = datetime('now') WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeStatementAsync
  }


/// Returns [current; parent; grandparent; ...; main]
/// Used by name resolution queries to walk the branch chain
let getBranchChain (id : PT.BranchId) : Task<List<PT.BranchId>> =
  task {
    let mutable chain = [ id ]
    let mutable currentId = id

    let mutable keepGoing = true
    while keepGoing do
      let! parentOpt =
        Sql.query
          """
          SELECT parent_branch_id FROM branches WHERE id = @id
          """
        |> Sql.parameters [ "id", Sql.uuid currentId ]
        |> Sql.executeRowOptionAsync (fun read -> read.uuidOrNone "parent_branch_id")

      match parentOpt with
      | Some(Some parentId) ->
        chain <- chain @ [ parentId ]
        currentId <- parentId
      | _ -> keepGoing <- false

    return chain
  }
