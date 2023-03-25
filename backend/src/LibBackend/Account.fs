/// Functions related to Accounts/Users
module LibBackend.Account

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude
open Tablecloth
open Db

module RT = LibExecution.RuntimeTypes

// **********************
// Types
// **********************

type Account = { username : UserName.T }

type UserInfo = { username : UserName.T; id : UserID }

// **********************
// Adding
// **********************

let insertUser (username : UserName.T) : Task<Result<unit, string>> =
  task {
    match UserName.newUserAllowed (string username) with
    | Ok () ->
      try
        do!
          Sql.query
            "INSERT INTO accounts_v0
             (id, username)
             VALUES (@id, @username)"
          |> Sql.parameters [ "id", Sql.uuid (System.Guid.NewGuid())
                              "username", Sql.string (string username) ]
          |> Sql.executeStatementAsync
        return Ok()
      with
      | _ -> return Error "Username taken"
    | Error e -> return Error e
  }


// **********************
// Querying
// **********************

// used in internalFn
let usernameForUserID (userID : UserID) : Task<Option<UserName.T>> =
  Sql.query
    "SELECT username
     FROM accounts_v0
     WHERE accounts_v0.id = @userid"
  |> Sql.parameters [ "userid", Sql.uuid userID ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "username" |> UserName.create)

let getUser (username : UserName.T) : Task<Option<UserInfo>> =
  Sql.query
    "SELECT id
     FROM accounts_v0
     WHERE accounts_v0.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowOptionAsync (fun read ->
    { username = username; id = read.uuid "id" })



// **********************
// Local/test developement
// **********************

let initTestAccounts () : Task<unit> =
  task {
    do!
      insertUser (UserName.create "test")
      |> Task.map (Exception.unwrapResultInternal [])

    return ()
  }

/// Initialize accounts needed for development and testing
let initializeDevelopmentAccounts (serviceName : string) : Task<unit> =
  task {
    print $"Initing LibBackend.Account in {serviceName}"
    do! initTestAccounts ()
    print $"Inited  LibBackend.Account in {serviceName}"
    return ()
  }
