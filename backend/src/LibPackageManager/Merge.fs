module LibPackageManager.Merge

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes


/// Check if a branch can be merged
let canMerge (branchId : PT.BranchId) : Task<Result<unit, PT.MergeError>> =
  task {
    if branchId = Branches.mainBranchId then
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
              SELECT id FROM commits
              WHERE branch_id = @parent_id
              ORDER BY created_at DESC
              LIMIT 1
              """
            |> Sql.parameters [ "parent_id", Sql.uuid parentId ]
            |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")

          if branch.baseCommitId <> parentLatest then
            return Error PT.MergeError.NotRebased
          else
            // Must have no WIP
            let! wipCount =
              Sql.query
                """
                SELECT COUNT(*) as cnt FROM package_ops
                WHERE branch_id = @branch_id AND commit_id IS NULL
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
        let parentId =
          branch.parentBranchId |> Option.defaultValue Branches.mainBranchId

        // For each branch location, deprecate any existing parent location at the same path
        let! branchLocations =
          Sql.query
            """
            SELECT owner, modules, name, item_type
            FROM locations
            WHERE branch_id = @branch_id
              AND deprecated_at IS NULL
            """
          |> Sql.parameters [ "branch_id", Sql.uuid branchId ]
          |> Sql.executeAsync (fun read ->
            (read.string "owner",
             read.string "modules",
             read.string "name",
             read.string "item_type"))

        for (owner, modules, name, itemType) in branchLocations do
          do!
            Sql.query
              """
              UPDATE locations
              SET deprecated_at = datetime('now')
              WHERE branch_id = @parent_id
                AND owner = @owner
                AND modules = @modules
                AND name = @name
                AND item_type = @item_type
                AND deprecated_at IS NULL
              """
            |> Sql.parameters
              [ "parent_id", Sql.uuid parentId
                "owner", Sql.string owner
                "modules", Sql.string modules
                "name", Sql.string name
                "item_type", Sql.string itemType ]
            |> Sql.executeStatementAsync

        // Move commits, ops, and locations to parent
        do!
          Sql.query
            "UPDATE commits SET branch_id = @parent_id WHERE branch_id = @branch_id"
          |> Sql.parameters
            [ "parent_id", Sql.uuid parentId; "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        do!
          Sql.query
            "UPDATE package_ops SET branch_id = @parent_id WHERE branch_id = @branch_id"
          |> Sql.parameters
            [ "parent_id", Sql.uuid parentId; "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        do!
          Sql.query
            """
            UPDATE locations SET branch_id = @parent_id
            WHERE branch_id = @branch_id AND deprecated_at IS NULL
            """
          |> Sql.parameters
            [ "parent_id", Sql.uuid parentId; "branch_id", Sql.uuid branchId ]
          |> Sql.executeStatementAsync

        // Mark branch as merged
        do! Branches.setMerged branchId

        return Ok()
  }
