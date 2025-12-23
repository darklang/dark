/// Account management functions
module LibPackageManager.Accounts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Db

/// Get account UUID by name
let getByName (name : string) : Task<Option<uuid>> =
  task {
    return!
      Sql.query
        """
        SELECT id FROM accounts_v0 WHERE name = @name
        """
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
  }

/// Get account name by UUID
let getName (id : uuid) : Task<Option<string>> =
  task {
    return!
      Sql.query
        """
        SELECT name FROM accounts_v0 WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "name")
  }

/// List all account names
let listNames () : Task<List<string>> =
  task {
    return!
      Sql.query
        """
        SELECT name FROM accounts_v0 ORDER BY name
        """
      |> Sql.executeAsync (fun read -> read.string "name")
  }

/// List all accounts (id, name pairs)
let listAll () : Task<List<uuid * string>> =
  task {
    return!
      Sql.query
        """
        SELECT id, name FROM accounts_v0 ORDER BY name
        """
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.string "name"))
  }

/// Get or create an account by name (returns the UUID)
let getOrCreate (name : string) : Task<uuid> =
  task {
    // Try to find existing
    let! existing = getByName name
    match existing with
    | Some id -> return id
    | None ->
      // Create new account with a random UUID
      let newId = System.Guid.NewGuid()
      do!
        Sql.query
          """
          INSERT INTO accounts_v0 (id, name) VALUES (@id, @name)
          ON CONFLICT (name) DO NOTHING
          """
        |> Sql.parameters [ "id", Sql.uuid newId; "name", Sql.string name ]
        |> Sql.executeStatementAsync
      // Return the id (might be different if there was a race)
      let! finalId = getByName name
      return finalId |> Option.defaultValue newId
  }
