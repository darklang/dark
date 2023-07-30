/// Functions related to Accounts/Users. Accounts have no metadata, not even a
/// username, name, or email. That is left to be handled in the DarkEditor canvas
module LibCloud.Account

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude
open Tablecloth
open Db

module RT = LibExecution.RuntimeTypes

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
