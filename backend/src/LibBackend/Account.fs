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

let validateAccount (account : Account) : Result<unit, string> =
  UserName.newUserAllowed (string account.username)


// Passwords set here are only valid locally;
// production uses Auth0 to check access
let upsertAccount (admin : bool) (account : Account) : Task<Result<unit, string>> =
  task {
    match validateAccount account with
    | Ok () ->
      return!
        Sql.query
          "INSERT INTO accounts_v0
              (id, username)
              VALUES
              (@id, @username)"
        |> Sql.parameters [ "id", Sql.uuid (System.Guid.NewGuid())
                            "username", Sql.string (string account.username) ]
        |> Sql.executeStatementAsync
        |> Task.map Ok
    | Error _ as result -> return result
  }

let upsertNonAdmin = upsertAccount false

let insertUser (username : UserName.T) : Task<Result<unit, string>> =
  task {
    let account = { username = username }

    match validateAccount account with
    | Ok () ->
      try
        // insert
        do!
          Sql.query
            "INSERT INTO accounts_v0
              (id, username)
              VALUES
              (@id, @username)"
          |> Sql.parameters [ "id", Sql.uuid (System.Guid.NewGuid())
                              "username", Sql.string (string username) ]
          |> Sql.executeStatementAsync

        return Ok()
      with
      | e ->
        // CLEANUP this is a bad error message
        return Error e.Message
    // return Error "Insert failed, probably because the username is already taken."
    | Error e -> return Error e
  }



// **********************
// Querying
// **********************

let userIDForUserName (username : UserName.T) : Task<UserID> =
  Sql.query
    "SELECT id
       FROM accounts_v0
       WHERE accounts_v0.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
  |> Task.map (fun user ->
    match user with
    | Some v -> v
    | None -> Exception.raiseGrandUser "User not found")

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



let ownerNameFromCanvasName (canvasName : CanvasName.T) : OwnerName.T =
  match String.split "-" (string canvasName) with
  | owner :: _ -> OwnerName.create owner
  | _ -> OwnerName.create (string canvasName)


// **********************
// Local/test developement
// **********************

let initTestAccounts () : Task<unit> =
  task {
    do!
      upsertNonAdmin { username = UserName.create "test" }
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
