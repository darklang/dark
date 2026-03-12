module LibPackageManager.Merge

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes


/// Check if a branch can be merged
let canMerge (branchId : PT.BranchId) : Task<Result<unit, PT.MergeError>> =
  task {
    if branchId = PT.mainBranchId then
      return Error PT.MergeError.IsMainBranch
    else
      let! branchOpt = Branches.get branchId
      match branchOpt with
      | None -> return Error PT.MergeError.NotFound
      | Some branch ->
        match branch.parentBranchId with
        | None -> return Error PT.MergeError.IsMainBranch
        | Some parentId ->
          // Must be rebased
          let! parentLatest =
            Sql.query
              """
              SELECT hash FROM commits
              WHERE branch_id = @parent_id
              ORDER BY created_at DESC
              LIMIT 1
              """
            |> Sql.parameters [ "parent_id", Sql.uuid parentId ]
            |> Sql.executeRowOptionAsync (fun read -> Hash(read.string "hash"))

          if branch.baseCommitHash <> parentLatest then
            return Error PT.MergeError.NotRebased
          else
            // Must have no WIP
            let! wipCount =
              Sql.query
                """
                SELECT COUNT(*) as cnt FROM package_ops
                WHERE branch_id = @branch_id AND commit_hash IS NULL
                """
              |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
              |> Sql.executeAsync (fun read -> read.int64 "cnt")

            match wipCount with
            | [ cnt ] when cnt > 0L -> return Error PT.MergeError.HasWip
            | _ ->
              // Must have no active children
              let! childCount =
                Sql.query
                  """
                  SELECT COUNT(*) as cnt FROM branches
                  WHERE parent_branch_id = @branch_id AND merged_at IS NULL
                  """
                |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
                |> Sql.executeAsync (fun read -> read.int64 "cnt")

              match childCount with
              | [ cnt ] when cnt > 0L -> return Error PT.MergeError.HasChildren
              | _ ->
                // Must have commits
                let! commitCount =
                  Sql.query
                    """
                    SELECT COUNT(*) as cnt FROM commits
                    WHERE branch_id = @branch_id
                    """
                  |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
                  |> Sql.executeAsync (fun read -> read.int64 "cnt")

                match commitCount with
                | [ 0L ] -> return Error PT.MergeError.NothingToMerge
                | _ -> return Ok()
  }


/// Merge a branch into its parent
let merge (branchId : PT.BranchId) : Task<Result<unit, PT.MergeError>> =
  task {
    let! canMergeResult = canMerge branchId
    match canMergeResult with
    | Error e -> return Error e
    | Ok() ->
      let! branchOpt = Branches.get branchId
      match branchOpt with
      | None -> return Error PT.MergeError.NotFound
      | Some branch ->
        let parentId = branch.parentBranchId |> Option.defaultValue PT.mainBranchId

        do!
          BranchOpPlayback.insertAndApply (
            PT.BranchOp.MergeBranch(branchId, parentId)
          )

        return Ok()
  }
