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

type Account =
  { username : UserName.T
    password : Password.T
    email : string
    name : string }

type UserInfo =
  { username : UserName.T
    name : string
    admin : bool
    email : string
    id : UserID }

type UserInfoAndCreatedAt =
  { username : UserName.T
    name : string
    admin : bool
    email : string
    id : UserID
    createdAt : NodaTime.Instant }

let userInfoToPerson (ui : UserInfo) : LibService.Rollbar.Person =
  Some { id = ui.id; username = Some ui.username }


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
  |> Result.and_ (validateEmail account.email)

// Passwords set here are only valid locally;
// production uses Auth0 to check access
let upsertAccount (admin : bool) (account : Account) : Task<Result<unit, string>> =
  task {
    match validateAccount account with
    | Ok () ->
      return!
        Sql.query
          "INSERT INTO accounts
             (id, username, name, email, admin, password)
             VALUES
             (@id, @username, @name, @email, @admin, @password)
             ON CONFLICT (username)
             DO UPDATE SET name = EXCLUDED.name,
                           email = EXCLUDED.email,
                           password = EXCLUDED.password"
        |> Sql.parameters [ "id", Sql.uuid (System.Guid.NewGuid())
                            "username", Sql.string (string account.username)
                            "admin", Sql.bool admin
                            "name", Sql.string account.name
                            "email", Sql.string account.email
                            ("password", account.password |> string |> Sql.string) ]
        |> Sql.executeStatementAsync
        |> Task.map Ok
    | Error _ as result -> return result
  }

let upsertAdmin = upsertAccount true
let upsertNonAdmin = upsertAccount false //needed for tests

// Any external calls to this should also call Analytics.identifyUser;
// we can't do it here because that sets up a module dependency cycle
// CLEANUP remove the separate user creation functions
let insertUser
  (username : UserName.T)
  (email : string)
  (name : string)
  : Task<Result<unit, string>> =
  task {
    let account =
      // As of the move to auth0, we  no longer store passwords in postgres. We do
      // still use postgres locally, which is why we're not removing the field
      // entirely. Local account creation is done in
      // upsertAccount/upsertAdmin, so using Password.invalid here does
      // not affect that
      { username = username
        password = Password.invalid
        email = email
        name = name }

    match validateAccount account with
    | Ok () ->

      try
        // insert
        do!
          Sql.query
            "INSERT INTO accounts
              (id, username, name, email, admin, password)
              VALUES
              (@id, @username, @name, @email, false, @password)
              ON CONFLICT DO NOTHING"
          |> Sql.parameters [ "id", Sql.uuid (System.Guid.NewGuid())
                              "username", Sql.string (string username)
                              "name", Sql.string name
                              "email", Sql.string email
                              ("password", Sql.string (string Password.invalid)) ]
          |> Sql.executeStatementAsync

        // verify insert worked
        let! accountExists =
          // CLEANUP: if this was added with a different email/name/etc this won't pick it up
          Sql.query
            "SELECT TRUE from ACCOUNTS
              WHERE username = @username
                AND name = @name
                AND email = @email
                AND password = @password"
          |> Sql.parameters [ "username", Sql.string (string username)
                              "name", Sql.string name
                              "email", Sql.string email
                              ("password", Sql.string (string Password.invalid)) ]
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
       FROM accounts
       WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
  |> Task.map (fun user ->
    match user with
    | Some v -> v
    | None -> Exception.raiseGrandUser "User not found")

let usernameForUserID (userID : UserID) : Task<Option<UserName.T>> =
  Sql.query
    "SELECT username
     FROM accounts
     WHERE accounts.id = @userid"
  |> Sql.parameters [ "userid", Sql.uuid userID ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "username" |> UserName.create)

let getUser (username : UserName.T) : Task<Option<UserInfo>> =
  Sql.query
    "SELECT name, email, admin, id
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowOptionAsync (fun read ->
    { username = username
      name = read.string "name"
      email = read.string "email"
      admin = read.bool "admin"
      id = read.uuid "id" })

let getUserCreatedAt (username : UserName.T) : Task<NodaTime.Instant> =
  Sql.query
    "SELECT created_at
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowAsync (fun read -> read.instantWithoutTimeZone "created_at")

let getUserAndCreatedAt
  (username : UserName.T)
  : Task<Option<UserInfoAndCreatedAt>> =
  Sql.query
    "SELECT name, email, admin, created_at, id
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowOptionAsync (fun read ->
    { username = username
      name = read.string "name"
      email = read.string "email"
      admin = read.bool "admin"
      id = read.uuid "id"
      createdAt = read.instantWithoutTimeZone "created_at" })

let getUserByEmail (email : string) : Task<Option<UserInfo>> =
  Sql.query
    "SELECT name, username, admin, id
     FROM accounts
     WHERE accounts.email = @email"
  |> Sql.parameters [ "email", Sql.string email ]
  |> Sql.executeRowOptionAsync (fun read ->
    { username = UserName.create (read.string "username")
      name = read.string "name"
      email = email
      admin = read.bool "admin"
      id = read.uuid "id" })

let getUsers () : Task<List<UserName.T>> =
  Sql.query
    "SELECT username
     FROM accounts"
  |> Sql.executeAsync (fun read -> UserName.create (read.string "username"))

let isAdmin (username : UserName.T) : Task<bool> =
  Sql.query
    "SELECT TRUE
     FROM accounts
     WHERE accounts.username = @username
       AND admin = true"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeExistsAsync


let canAccessOperations (username : UserName.T) : Task<bool> = isAdmin username

// formerly called auth_domain_for
let ownerNameFromCanvasName (canvasName : CanvasName.T) : OwnerName.T =
  match String.split "-" (string canvasName) with
  | owner :: _ -> OwnerName.create owner
  | _ -> OwnerName.create (string canvasName)

// **********************
// What user has access to
// **********************

let ownedCanvases (userID : UserID) : Task<List<CanvasName.T>> =
  Sql.query
    "SELECT name
     FROM canvases
     WHERE account_id = @userID"
  |> Sql.parameters [ "userID", Sql.uuid userID ]
  |> Sql.executeAsync (fun read -> read.string "name" |> CanvasName.createExn)
  |> Task.map List.sort



// **********************
// Local/test developement
// **********************

let initTestAccounts () : Task<unit> =
  task {
    do!
      upsertNonAdmin
        { username = UserName.create "test_unhashed"
          password = Password.fromHash "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+unhashed@darklang.com"
          name = "Dark Backend Tests with Unhashed Password" }
      |> Task.map (Exception.unwrapResultInternal [])

    do!
      upsertNonAdmin
        { username = UserName.create "test"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test@darklang.com"
          name = "Dark Backend Tests" }
      |> Task.map (Exception.unwrapResultInternal [])

    do!
      upsertAdmin
        { username = UserName.create "test_admin"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+admin@darklang.com"
          name = "Dark Backend Test Admin" }
      |> Task.map (Exception.unwrapResultInternal [])

    do!
      upsertNonAdmin
        { username = UserName.create "sample"
          password = Password.invalid
          email = "test+sample@darklang.com"
          name = "Dark Backend Sample owner" }
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
