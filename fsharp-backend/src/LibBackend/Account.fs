module LibBackend.Account

// Functions related to Accounts/Users

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude
open Tablecloth
open Db

module RT = LibExecution.RuntimeTypes
module DvalRepr = LibExecution.DvalRepr

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
    createdAt : System.DateTime }

let userInfoToPerson (ui : UserInfo) : LibService.Rollbar.Person =
  { id = ui.id; email = ui.email; username = ui.username }



// **********************
// Special usernames
// **********************



// **********************
// Adding
// **********************


let validateEmail (email : string) : Result<unit, string> =
  (* just checking it's roughly the shape of an email *)
  let reString = "^.+@.+\\..+$"

  if FsRegEx.isMatch reString email then
    Ok()
  else
    Error($"Invalid email '{email}'")


let validateAccount (account : Account) : Result<unit, string> =
  validateEmail account.email

// Passwords set here are only valid locally, production uses auth0 to check
// access
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
let upsertNonAdmin = upsertAccount false

// Any external calls to this should also call Analytics.identifyUser;
// we can't do it here because that sets up a module dependency cycle
let insertUser
  (username : UserName.T)
  (email : string)
  (name : string)
  (analyticsMetadata : Option<RT.DvalMap>)
  : Task<Result<unit, string>> =
  task {
    let account =
      // As of the move to auth0, we  no longer store passwords in postgres. We do
      // still use postgres locally, which is why we're not removing the field
      // entirely. Local account creation is done in
      // upsert_account_exn/upsert_admin_exn, so using Password.invalid here does
      // not affect that
      { username = username
        password = Password.invalid
        email = email
        name = name }

    match validateAccount account with
    | Ok () ->
      let analyticsMetadata = analyticsMetadata |> Option.unwrap (Map.empty)

      try
        do!
          Sql.query
            "INSERT INTO accounts
              (id, username, name, email, admin, password, segment_metadata)
              VALUES
              (@id, @username, @name, @email, false, @password, @metadata)
              ON CONFLICT DO NOTHING"
          |> Sql.parameters [ "id", Sql.uuid (System.Guid.NewGuid())
                              "username", Sql.string (string username)
                              "name", Sql.string name
                              "email", Sql.string email
                              ("password", Sql.string (string Password.invalid))
                              ("analytics_metadata",
                               Sql.jsonb (
                                 DvalRepr.toInternalQueryableV1 analyticsMetadata
                               )) ]
          |> Sql.executeStatementAsync
        let! exists =
          Sql.query
            "SELECT 1 from ACCOUNTS
              WHERE username = @username
                AND name = @name
                AND email = @email
                AND password = @password"
          |> Sql.parameters [ "username", Sql.string (string username)
                              "name", Sql.string name
                              "email", Sql.string email
                              ("password", Sql.string (string Password.invalid)) ]
          |> Sql.executeExistsAsync
        if exists then
          return Ok()
        else
          return
            Error "Insert failed, probably because the username is already taken."
      with
      | _ ->
        return Error "Insert failed, probably because the username is already taken."
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
  |> Task.map (function
    | Some v -> v
    | None -> Exception.raiseDeveloper "User not found")

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

let getUserCreatedAt (username : UserName.T) : Task<System.DateTime> =
  Sql.query
    "SELECT created_at
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowAsync (fun read -> read.dateTime "created_at")

let getUserAndCreatedAtAndAnalyticsMetadata
  (username : UserName.T)
  : Task<Option<UserInfoAndCreatedAt * string>> =
  Sql.query
    "SELECT name, email, admin, created_at, id, segment_metadata
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeRowOptionAsync (fun read ->
    { username = username
      name = read.string "name"
      email = read.string "email"
      admin = read.bool "admin"
      id = read.uuid "id"
      createdAt = read.dateTime "created_at" },
    read.string "segment_metadata")

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

let setAdmin (admin : bool) (username : UserName.T) : Task<unit> =
  Sql.query
    "UPDATE accounts
        SET admin = @admin where username = @username"
  |> Sql.parameters [ "admin", Sql.bool admin
                      "username", Sql.string (string username) ]
  |> Sql.executeStatementAsync

// Returns None if no valid user, or Some username _from the db_ if valid.
// Note: the input username may also be an email address. We do this because
// users input data this way and it seems silly not to allow it.
//
// No need to detect which and SQL differently; no valid username contains a
// '@', and every valid email address does. [If you say 'uucp bang path', I
// will laugh and then tell you to give me a real email address.]
//
// This function was converted from OCaml. The OCaml Libsodium
// (https://github.com/ahrefs/ocaml-sodium/blob/master/lib/sodium.ml), the F#
// version is libsodium-net
// (https://github.com/tabrath/libsodium-core/blob/master/src/Sodium.Core/PasswordHash.cs).
// The OCaml version uses the argon2i versions under the hood, which we use explicitly in F#.
let authenticate
  (usernameOrEmail : string)
  (givenPassword : string)
  : Task<Option<string>> =
  Sql.query
    "SELECT username, password from accounts
      WHERE accounts.username = @usernameOrEmail OR accounts.email = @usernameOrEmail"
  |> Sql.parameters [ "usernameOrEmail", Sql.string usernameOrEmail ]
  |> Sql.executeRowOptionAsync (fun read ->
    (read.string "username", read.string "password"))
  |> Task.map (
    Option.andThen (fun (username, password) ->
      let dbHash = password |> Base64.decodeFromString |> UTF8.ofBytesWithReplacement

      if Sodium.PasswordHash.ArgonHashStringVerify(dbHash, givenPassword) then
        Some(username)
      else
        None)
  )

let canAccessOperations (username : UserName.T) : Task<bool> = isAdmin username

// formerly called auth_domain_for
let ownerNameFromCanvasName (host : CanvasName.T) : OwnerName.T =
  match String.split "-" (string host) with
  | owner :: _ -> OwnerName.create owner
  | _ -> OwnerName.create (string host)

// **********************
// What user has access to
// **********************

let ownedCanvases (userID : UserID) : Task<List<CanvasName.T>> =
  Sql.query
    "SELECT DISTINCT c.name
     FROM canvases c
     WHERE c.account_id = @userID"
  |> Sql.parameters [ "userID", Sql.uuid userID ]
  |> Sql.executeAsync (fun read -> read.string "name" |> CanvasName.create)
  |> Task.map List.sort


// NB: this returns canvases an account has access to via an organization, not
// the organization(s) themselves
let accessibleCanvases (userID : UserID) : Task<List<CanvasName.T>> =
  Sql.query
    "SELECT c.name
       FROM access
      INNER JOIN accounts as org on access.organization_account = org.id
      INNER JOIN canvases as c on org.id = account_id
      WHERE access.access_account = @userID"
  |> Sql.parameters [ "userID", Sql.uuid userID ]
  |> Sql.executeAsync (fun read -> read.string "name" |> CanvasName.create)
  |> Task.map List.sort

let orgs (userID : UserID) : Task<List<OrgName.T>> =
  Sql.query
    "SELECT org.username
     FROM access
     INNER JOIN accounts as org on access.organization_account = org.id
     WHERE access.access_account = @userID"
  |> Sql.parameters [ "userID", Sql.uuid userID ]
  |> Sql.executeAsync (fun read -> read.string "username" |> OrgName.create)
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
          name = "Dark OCaml Tests with Unhashed Password" }
      |> Task.map (Exception.unwrapResultInternal [])

    do!
      upsertNonAdmin
        { username = UserName.create "test"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test@darklang.com"
          name = "Dark OCaml Tests" }
      |> Task.map (Exception.unwrapResultInternal [])

    do!
      upsertAdmin
        { username = UserName.create "test_admin"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+admin@darklang.com"
          name = "Dark OCaml Test Admin" }
      |> Task.map (Exception.unwrapResultInternal [])

    return ()
  }

let initBannedAccounts () : Task<unit> =
  task {
    do!
      UserName.banned
      |> Set.toList
      |> Task.iterSequentially (fun username ->
        upsertNonAdmin
          { username = UserName.createUnsafe username
            password = Password.invalid
            email = $"ops+{username}@darklang.com"
            name = $"Disallowed account {username}" }
        |> Task.map (Exception.unwrapResultInternal []))

    return ()
  }

let initAdmins () : Task<unit> =
  task {
    let password =
      // "what"
      Password.fromHash
        "JGFyZ29uMmkkdj0xOSRtPTMyNzY4LHQ9NCxwPTEkcEQxWXBLOG1aVStnUUJUYXdKZytkQSR3TWFXb1hHOER1UzVGd2NDYzRXQVc3RlZGN0VYdVpnMndvZEJ0QnY1bkdJAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

    do!
      upsertAdmin
        { username = UserName.create "dark"
          password = password
          email = "ops+darkuser@darklang.com"
          name = "Dark Local Admin user" }
      |> Task.map (Exception.unwrapResultInternal [])

    do!
      upsertAdmin
        { username = UserName.create "paul"
          password = password
          email = "paul@darklang.com"
          name = "Paul Biggar" }
      |> Task.map (Exception.unwrapResultInternal [])

    return ()
  }

// accounts to create namespaces for dark canvases
let initUsefulCanvases () : Task<unit> =
  // Needed for tests
  task {
    do!
      upsertNonAdmin
        { username = UserName.create "sample"
          password = Password.invalid
          email = "opsample@darklang.com"
          name = "Sample owner" }
      |> Task.map (Exception.unwrapResultInternal [])

    return ()
  }


let init (serviceName : string) : Task<unit> =
  task {
    print $"Initing LibBackend in {serviceName}"
    if Config.createAccounts then
      do! initTestAccounts ()
      do! initBannedAccounts ()
      do! initAdmins ()
      do! initUsefulCanvases ()
      return ()
    else
      return ()
  }
