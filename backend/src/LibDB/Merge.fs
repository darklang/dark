module LibDB.Merge

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

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


/// A merge collision: the merging branch and its parent BOTH have a listed binding for the same FQN,
/// to DIFFERENT content. A `MergeBranch` silently lets the child win (it unlists the parent's
/// binding) — so, exactly like a sync divergence, we detect these and RECORD them in the same
/// reviewable conflict store, rather than dropping the parent's binding without a trace. Two AI
/// agents editing the same item on two branches across two instances converge here: the merge still
/// completes (child-wins stands), but the overwrite is visible via `dark conflicts`, not silent.
///
/// Returns `(location, parentHash, childHash)` per collision, with `location` formatted exactly as
/// `Sync.detectDivergences` formats it, so merge collisions and sync divergences read identically.
let detectMergeCollisions
  (branchId : PT.BranchId)
  (parentId : PT.BranchId)
  : Task<List<string * string * string>> =
  Sql.query
    """
    SELECT ch.owner AS owner, ch.modules AS modules, ch.name AS name,
           p.item_hash AS parent_hash, ch.item_hash AS child_hash
    FROM locations ch
    JOIN locations p
      ON p.owner = ch.owner AND p.modules = ch.modules
         AND p.name = ch.name AND p.item_type = ch.item_type
    WHERE ch.branch_id = @branch_id AND ch.unlisted_at IS NULL
      AND p.branch_id = @parent_id AND p.unlisted_at IS NULL
      AND p.item_hash <> ch.item_hash
    """
  |> Sql.parameters
    [ "branch_id", Sql.uuid branchId; "parent_id", Sql.uuid parentId ]
  |> Sql.executeAsync (fun read ->
    let owner = read.string "owner"
    let modules = read.string "modules"
    let name = read.string "name"
    let locStr =
      if modules = "" then $"{owner}.{name}" else $"{owner}.{modules}.{name}"
    (locStr, read.string "parent_hash", read.string "child_hash"))


/// Merge a branch into its parent.
///
/// TODO (multi-tenant): reads parent's state (`canMerge` queries
/// commits/conflicts), then applies, with no version check or
/// transaction across the read+apply pair. CLI single-process is
/// fine; future-server use will corrupt under concurrent merges
/// against the same parent.
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

        // Detect + record merge collisions BEFORE MergeBranch unlists the parent's binding. The
        // child still wins (the merge semantics are unchanged), but each silently-overwritten parent
        // binding is recorded in the same store sync uses — reviewable via `dark conflicts`, keyed
        // by `merge:<branch>` so its origin is clear. localHash = the parent binding we replaced,
        // incomingHash = the child binding that won (mirrors sync's local-vs-incoming).
        let! collisions = detectMergeCollisions branchId parentId
        for (locStr, parentHash, childHash) in collisions do
          do!
            Conflicts.record
              $"merge:{branch.name}"
              locStr
              parentHash
              childHash
              "MergeChildWins"

        do!
          BranchOpPlayback.insertAndApply (
            PT.BranchOp.MergeBranch(branchId, parentId)
          )

        return Ok()
  }
