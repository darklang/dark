/// Functions related to Accounts/Users. Accounts have no metadata, not even a
/// username, name, or email. That is left to be handled in the DarkEditor canvas
module LibCloud.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Db

type Account = { id : UserID; name : string }

let getUserByName (name : string) : Task<Option<UserID>> =
  Sql.query "SELECT id FROM accounts_v0 WHERE name = @name"
  |> Sql.parameters [ "name", Sql.string name ]
  |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")

let createUser (name : string) : Task<Result<UserID, string>> =
  task {
    let userID = System.Guid.NewGuid()
    let! rowsAffected =
      Sql.query
        "INSERT INTO accounts_v0 (id, name)
         VALUES (@id, @name)
         ON CONFLICT (name) DO NOTHING"
      |> Sql.parameters [ "id", Sql.uuid userID; "name", Sql.string name ]
      |> Sql.executeNonQueryAsync

    if rowsAffected = 1 then
      return Ok userID
    else
      return Error $"A user with the name '{name}' already exists"
  }
