module LibPackageManager.AccountContext

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


/// Get the current branch for an account
let getCurrentBranch (accountId : uuid) : Task<Option<uuid>> =
  task {
    return!
      Sql.query
        """
        SELECT current_branch_id
        FROM account_context
        WHERE account_id = @account_id
        """
      |> Sql.parameters [ "account_id", Sql.uuid accountId ]
      |> Sql.executeRowOptionAsync (fun read -> read.uuidOrNone "current_branch_id")
      |> Task.map Option.flatten
  }


/// Set the current branch for an account
let setCurrentBranch (accountId : uuid) (branchId : uuid) : Task<unit> =
  task {
    let now = System.DateTime.UtcNow

    do!
      Sql.query
        """
        INSERT INTO account_context (account_id, current_branch_id, last_updated_at)
        VALUES (@account_id, @branch_id, @last_updated_at)
        ON CONFLICT(account_id) DO UPDATE SET
          current_branch_id = @branch_id,
          last_updated_at = @last_updated_at
        """
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId
          "branch_id", Sql.uuid branchId
          "last_updated_at", Sql.string (now.ToString("o")) ]
      |> Sql.executeNonQueryAsync
      |> Task.map (fun _ -> ())

    return ()
  }


/// Clear the current branch for an account (sets it to NULL)
let clearCurrentBranch (accountId : uuid) : Task<unit> =
  task {
    let now = System.DateTime.UtcNow

    do!
      Sql.query
        """
        INSERT INTO account_context (account_id, current_branch_id, last_updated_at)
        VALUES (@account_id, NULL, @last_updated_at)
        ON CONFLICT(account_id) DO UPDATE SET
          current_branch_id = NULL,
          last_updated_at = @last_updated_at
        """
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId
          "last_updated_at", Sql.string (now.ToString("o")) ]
      |> Sql.executeNonQueryAsync
      |> Task.map (fun _ -> ())

    return ()
  }


/// Get or create the current branch for an account (ensures 'main' exists)
let getOrCreateCurrentBranch (accountId : uuid) : Task<uuid> =
  task {
    let! currentBranch = getCurrentBranch accountId

    match currentBranch with
    | Some branchId -> return branchId
    | None ->
      // No branch set, ensure 'main' exists and set it
      let! mainBranch = Branches.ensureMainBranch (Some accountId)
      do! setCurrentBranch accountId mainBranch.id
      return mainBranch.id
  }
