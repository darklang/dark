/// Functions related to Accounts/Users. Accounts have no metadata, not even a
/// username, name, or email. That is left to be handled in the DarkEditor canvas
module LibCloud.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Db

type Account = { id : UserID; name : string }

let createUser (name : string) : Task<UserID> =
  task {
    let userID = System.Guid.NewGuid()
    do!
      Sql.query
        "INSERT INTO accounts_v0
          (id, name)
          VALUES (@id, @name)"
      |> Sql.parameters [ "id", Sql.uuid userID; "name", Sql.string name ]
      |> Sql.executeStatementAsync

    return userID
  }
