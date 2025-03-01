/// Functions related to Accounts/Users. Accounts have no metadata, not even a
/// username, name, or email. That is left to be handled in the DarkEditor canvas
module LibCloud.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Db

let createUser () : Task<UserID> =
  task {
    let userID = System.Guid.NewGuid()
    do!
      Sql.query
        "INSERT INTO accounts_v0
          (id)
          VALUES (@id)"
      |> Sql.parameters [ "id", Sql.uuid userID ]
      |> Sql.executeStatementAsync

    return userID
  }
