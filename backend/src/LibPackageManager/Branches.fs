module LibPackageManager.Branches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes


let private readBranch (read : RowReader) : PT.Branch =
  { id = read.uuid "id"
    name = read.string "name"
    parentBranchId = read.uuidOrNone "parent_branch_id"
    baseCommitHash = read.stringOrNone "base_commit_hash" |> Option.map Hash
    createdAt = read.instant "created_at"
    mergedAt = read.instantOrNone "merged_at" }


let create (name : string) (parentBranchId : PT.BranchId) : Task<PT.Branch> =
  task {
    let id = System.Guid.NewGuid()

    // Get parent's latest commit as base
    let! baseCommitHash =
      Sql.query
        """
        SELECT hash FROM commits
        WHERE branch_id = @parent_id
        ORDER BY created_at DESC
        LIMIT 1
        """
      |> Sql.parameters [ "parent_id", Sql.uuid parentBranchId ]
      |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "hash"))

    let baseCommitHashParam =
      match baseCommitHash with
      | Some(Hash h) -> Sql.string h
      | None -> Sql.dbnull

    do!
      Sql.query
        """
        INSERT INTO branches (id, name, parent_branch_id, base_commit_hash, created_at)
        VALUES (@id, @name, @parent_id, @base_commit_hash, datetime('now'))
        """
      |> Sql.parameters
        [ "id", Sql.uuid id
          "name", Sql.string name
          "parent_id", Sql.uuid parentBranchId
          "base_commit_hash", baseCommitHashParam ]
      |> Sql.executeStatementAsync

    // Emit BranchOp
    do!
      BranchOpPlayback.insertOnly (
        PT.BranchOp.CreateBranch(id, name, Some parentBranchId, baseCommitHash)
      )

    return
      { id = id
        name = name
        parentBranchId = Some parentBranchId
        baseCommitHash = baseCommitHash
        createdAt = NodaTime.SystemClock.Instance.GetCurrentInstant()
        mergedAt = None }
  }


let get (id : PT.BranchId) : Task<Option<PT.Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, parent_branch_id, base_commit_hash, created_at, merged_at
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
        SELECT id, name, parent_branch_id, base_commit_hash, created_at, merged_at
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
        SELECT id, name, parent_branch_id, base_commit_hash, created_at, merged_at
        FROM branches
        WHERE merged_at IS NULL AND archived_at IS NULL
        ORDER BY created_at ASC
        """
      |> Sql.executeAsync readBranch
  }


let listAll () : Task<List<PT.Branch>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name, parent_branch_id, base_commit_hash, created_at, merged_at
        FROM branches
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


/// Archive a branch (soft delete). Sets archived_at, emits ArchiveBranch BranchOp.
let archive (id : PT.BranchId) : Task<Result<unit, string>> =
  task {
    if id = PT.mainBranchId then
      return Error "Cannot archive main branch"
    else
      // Check for active children
      let! childCount =
        Sql.query
          """
          SELECT COUNT(*) as cnt FROM branches
          WHERE parent_branch_id = @id AND merged_at IS NULL AND archived_at IS NULL
          """
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeRowAsync (fun read -> read.int64 "cnt")

      if childCount > 0L then
        return Error "Cannot archive branch with active children"
      else
        do!
          Sql.query
            "UPDATE branches SET archived_at = datetime('now') WHERE id = @id"
          |> Sql.parameters [ "id", Sql.uuid id ]
          |> Sql.executeStatementAsync

        // Emit BranchOp
        do! BranchOpPlayback.insertOnly (PT.BranchOp.ArchiveBranch id)

        return Ok()
  }


/// Unarchive a branch (clear archived_at)
let unarchive (id : PT.BranchId) : Task<Result<unit, string>> =
  task {
    try
      do!
        Sql.query "UPDATE branches SET archived_at = NULL WHERE id = @id"
        |> Sql.parameters [ "id", Sql.uuid id ]
        |> Sql.executeStatementAsync
      return Ok()
    with ex ->
      return Error ex.Message
  }


/// Delete a branch — kept for backward compatibility, now calls archive
let delete (id : PT.BranchId) : Task<Result<unit, string>> = archive id


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
/// Used by name resolution queries to walk the branch chain.
/// Uses a recursive CTE to fetch the entire chain in a single query.
let getBranchChain (id : PT.BranchId) : Task<List<PT.BranchId>> =
  Sql.query
    """
    WITH RECURSIVE chain(id) AS (
      SELECT @id
      UNION ALL
      SELECT b.parent_branch_id
      FROM branches b JOIN chain c ON b.id = c.id
      WHERE b.parent_branch_id IS NOT NULL
    )
    SELECT id FROM chain
    """
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeAsync (fun read -> read.uuid "id")
