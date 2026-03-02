module LibPackageManager.Rebase

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes


type RebaseConflict =
  { owner : string; modules : string; name : string; itemType : string }


/// Get location paths modified on a branch since a given commit
let private getLocationPathsModifiedSince
  (branchId : PT.BranchId)
  (sinceCommitHash : Option<ContentHash>)
  : Task<List<RebaseConflict>> =
  task {
    match sinceCommitHash with
    | None ->
      // No base commit - all committed locations on this branch are "modified"
      return!
        Sql.query
          """
          SELECT DISTINCT owner, modules, name, item_type
          FROM locations
          WHERE branch_id = @branch_id
            AND commit_id IS NOT NULL
            AND deprecated_at IS NULL
          """
        |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
        |> Sql.executeAsync (fun read ->
          { owner = read.string "owner"
            modules = read.string "modules"
            name = read.string "name"
            itemType = read.string "item_type" })
    | Some(ContentHash commitHashStr) ->
      // Locations committed after the base commit
      return!
        Sql.query
          """
          SELECT DISTINCT l.owner, l.modules, l.name, l.item_type
          FROM locations l
          JOIN commits c ON l.commit_id = c.id
          WHERE l.branch_id = @branch_id
            AND l.commit_id IS NOT NULL
            AND l.deprecated_at IS NULL
            AND c.created_at > (SELECT created_at FROM commits WHERE id = @since_commit_id)
          """
        |> Sql.parameters
          [ "branch_id", Sql.uuid branchId
            "since_commit_id", Sql.string commitHashStr ]
        |> Sql.executeAsync (fun read ->
          { owner = read.string "owner"
            modules = read.string "modules"
            name = read.string "name"
            itemType = read.string "item_type" })
  }


/// Check for rebase conflicts without performing rebase
let getConflicts (branchId : PT.BranchId) : Task<List<RebaseConflict>> =
  task {
    let! branchOpt = Branches.get branchId
    match branchOpt with
    | None -> return []
    | Some branch ->
      match branch.parentBranchId with
      | None -> return [] // main branch, nothing to rebase
      | Some parentId ->
        let branchLocations =
          getLocationPathsModifiedSince branchId branch.baseCommitHash
        let parentLocations =
          getLocationPathsModifiedSince parentId branch.baseCommitHash

        let! branchLocs = branchLocations
        let! parentLocs = parentLocations

        // Conflict = same (owner, modules, name, itemType) modified on both sides
        let branchSet =
          branchLocs
          |> List.map (fun l -> (l.owner, l.modules, l.name, l.itemType))
          |> Set.ofList

        let conflicts =
          parentLocs
          |> List.filter (fun l ->
            Set.contains (l.owner, l.modules, l.name, l.itemType) branchSet)

        return conflicts
  }


/// Perform rebase: verify no conflicts, update base_commit_id to parent's latest
let rebase (branchId : PT.BranchId) : Task<Result<string, List<RebaseConflict>>> =
  task {
    let! branchOpt = Branches.get branchId
    match branchOpt with
    | None -> return Error []
    | Some branch ->
      match branch.parentBranchId with
      | None -> return Ok "Main branch, nothing to rebase"
      | Some parentId ->
        // Get parent's latest commit
        let! parentLatest =
          Sql.query
            """
            SELECT id FROM commits
            WHERE branch_id = @parent_id
            ORDER BY created_at DESC
            LIMIT 1
            """
          |> Sql.parameters [ "parent_id", Sql.uuid parentId ]
          |> Sql.executeRowOptionAsync (fun read -> ContentHash(read.string "id"))

        if branch.baseCommitHash = parentLatest then
          return Ok "Already up to date"
        else
          // Check for conflicts
          let! conflicts = getConflicts branchId

          if not (List.isEmpty conflicts) then
            return Error conflicts
          else
            // Update base_commit_id
            let baseCommitHashParam =
              match parentLatest with
              | Some(ContentHash h) -> Sql.string h
              | None -> Sql.dbnull

            do!
              Sql.query
                """
                UPDATE branches
                SET base_commit_id = @base_commit_id
                WHERE id = @id
                """
              |> Sql.parameters
                [ "id", Sql.uuid branchId; "base_commit_id", baseCommitHashParam ]
              |> Sql.executeStatementAsync

            return Ok "Rebased successfully"
  }
