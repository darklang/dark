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

let validateEmail (email : string) : Result<unit, string> =
  // just checking it's roughly the shape of an email
  let reString = "^.+@.+\\..+$"

  if FsRegEx.isMatch reString email then
    Ok()
  else
    Error($"Invalid email '{email}'")


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

// Any external calls to this should also call Analytics.identifyUser;
// we can't do it here because that sets up a module dependency cycle
// CLEANUP remove the separate user creation functions
let insertUser (username : UserName.T) : Task<Result<unit, string>> =
  task {
    let account =
      // As of the move to auth0, we  no longer store passwords in postgres. We do
      // still use postgres locally, which is why we're not removing the field
      // entirely. Local account creation is done in
      // upsertAccount/upsertAdmin, so using Password.invalid here does
      // not affect that
      { username = username }

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

        // verify insert worked
        let! accountExists =
          // CLEANUP: if this was added with a different email/name/etc this won't pick it up
          Sql.query
            "SELECT TRUE FROM accounts_v0
              WHERE username = @username"
          |> Sql.parameters [ "username", Sql.string (string username) ]
          |> Sql.executeExistsAsync

        if accountExists then
          return Ok()
        else
          return
            Error "Insert failed, probably because the username is already taken."
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
