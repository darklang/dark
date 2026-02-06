module LibPackageManager.Branches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes


let private readBranch (read : RowReader) : PT.Branch =
  { id = read.uuid "id"
    name = read.string "name"
    parentBranchId = read.uuidOrNone "parent_branch_id"
    baseCommitId = read.uuidOrNone "base_commit_id"
    createdAt = read.instant "created_at"
    mergedAt = read.instantOrNone "merged_at" }


let create (name : string) (parentBranchId : PT.BranchId) : Task<PT.Branch> =
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


let get (id : PT.BranchId) : Task<Option<PT.Branch>> =
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


let getByName (name : string) : Task<Option<PT.Branch>> =
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


let list () : Task<List<PT.Branch>> =
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
    if id = PT.mainBranchId then
      return Error "Cannot delete main branch"
    else
      // Check for children
      let! childCount =
        Sql.query
          """
          SELECT COUNT(*) as cnt FROM branches
          WHERE parent_branch_id = @id AND merged_at IS NULL
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

      if childCount > 0L then
        return Error "Cannot delete branch with active children"
      else
        // Check for dependent data that would cause FK constraint failure
        let! commitCount =
          Sql.query "SELECT COUNT(*) as cnt FROM commits WHERE branch_id = @id"
          |> Sql.parameters [ "id", Sql.uuid id ]
          |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

        let! uncommittedOpCount =
          Sql.query
            "SELECT COUNT(*) as cnt FROM package_ops WHERE branch_id = @id AND commit_id IS NULL"
          |> Sql.parameters [ "id", Sql.uuid id ]
          |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

        let! locationCount =
          Sql.query "SELECT COUNT(*) as cnt FROM locations WHERE branch_id = @id"
          |> Sql.parameters [ "id", Sql.uuid id ]
          |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

        if commitCount > 0L || uncommittedOpCount > 0L || locationCount > 0L then
          let parts =
            [ if commitCount > 0L then $"{commitCount} commit(s)"
              if uncommittedOpCount > 0L then
                $"{uncommittedOpCount} uncommitted change(s)"
              if locationCount > 0L then $"{locationCount} package location(s)" ]
          let details = String.concat ", " parts
          return
            Error
              $"Cannot delete branch: it contains {details}. Merge the branch first."
        else
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
