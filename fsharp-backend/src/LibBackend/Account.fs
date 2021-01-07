module LibBackend.Account

// Functions related to Accounts/Users

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp.Tasks
open Npgsql

open Prelude
open Prelude.Tablecloth
open LibExecution.SharedTypes
open Db

// **********************
// Types
// **********************

// since these are all usernames, use types for safety
module UserName =
  type T =
    private
    | UserName of string

    override this.ToString() = let (UserName username) = this in username

  let create (str : string) : T = UserName str

module OwnerName =
  type T =
    private
    | OwnerName of string

    override this.ToString() = let (OwnerName name) = this in name

  let create (str : string) : T = OwnerName str


module CanvasName =
  type T =
    private
    | CanvasName of string

    override this.ToString() = let (CanvasName name) = this in name

  let create (str : string) : T = CanvasName str

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
    id : System.Guid }

type UserInfoAndCreatedAt =
  { username : UserName.T
    name : string
    admin : bool
    email : string
    id : System.Guid
    createdAt : System.DateTime }

type Validate =
  | Validate
  | DontValidate

// **********************
// Special usernames
// **********************

let bannedUserNames : List<UserName.T> =
  // originally from https://ldpreload.com/blog/names-to-reserve
  // we allow www, because we have a canvas there
  [ "abuse"
    "admin"
    "administrator"
    "autoconfig"
    "broadcasthost"
    "ftp"
    "hostmaster"
    "imap"
    "info"
    "is"
    "isatap"
    "it"
    "localdomain"
    "localhost"
    "mail"
    "mailer-daemon"
    "marketing"
    "mis"
    "news"
    "nobody"
    "noc"
    "noreply"
    "no-reply"
    "pop"
    "pop3"
    "postmaster"
    "root"
    "sales"
    "security"
    "smtp"
    "ssladmin"
    "ssladministrator"
    "sslwebmaster"
    "support"
    "sysadmin"
    "usenet"
    "uucp"
    "webmaster"
    "wpad"
    // original to us from here
    "billing"
    "dev"

    // alpha, but not beta, because user beta already exists (with ownership
    // transferred to us)
    "alpha" ]
  |> List.map UserName.create



// **********************
// Adding
// **********************

let validateUserName (username : string) : Result<unit, string> =
  (* rules: no uppercase, ascii only, must start with letter, other letters can
   * be numbers or underscores. 3-20 characters. *)
  let reString = @"^[a-z][a-z0-9_]{2,20}$"

  if FsRegEx.isMatch reString username then
    Ok()
  else
    Error($"Invalid username '{username}', must match /{reString}/")


let validateEmail (email : string) : Result<unit, string> =
  (* just checking it's roughly the shape of an email *)
  let reString = "^.+@.+\\..+$"

  if FsRegEx.isMatch reString email then
    Ok()
  else
    Error($"Invalid email '{email}, must match /{reString}/")


let validateAccount (account : Account) : Result<unit, string> =
  validateUserName (account.username.ToString())
  |> Result.and_ (validateEmail account.email)

// Passwords set here are only valid locally, production uses auth0 to check
// access
let upsertAccount
  (admin : bool)
  (validate : Validate)
  (account : Account)
  : Task<Result<unit, string>> =
  task {
    // FSTODO - this used to be default true
    let result = if validate = Validate then validateAccount account else Ok() in

    match result with
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
                              "username", Sql.string (account.username.ToString())
                              "admin", Sql.bool admin
                              "name", Sql.string account.name
                              "email", Sql.string account.email
                              ("password",
                               account.password |> Password.toString |> Sql.string) ]
          |> Sql.executeStatementAsync
          |> Task.map Ok
    | Error _ -> return result
  }

let upsertAdmin = upsertAccount true
let upsertNonAdmin = upsertAccount false

// **********************
// Querying
// **********************

let userIDForUserName (username : UserName.T) : Task<UserID> =
  if List.contains username bannedUserNames then
    failwith "Banned username"
  else
    Sql.query
      "SELECT id
       FROM accounts
       WHERE accounts.username = @username"
    |> Sql.parameters [ "username", Sql.string (username.ToString()) ]
    |> Sql.executeRowAsync (fun read -> read.uuid "id")


let usernameForUserID (userID : System.Guid) : Task<Option<UserName.T>> =
  Sql.query
    "SELECT username
     FROM accounts
     WHERE accounts.id = @userid"
  |> Sql.parameters [ "userid", Sql.uuid userID ]
  |> Sql.executeRowOptionAsync
       (fun read -> read.string "username" |> UserName.create)

let getUser (username : UserName.T) : Task<Option<UserInfo>> =
  Sql.query
    "SELECT name, email, admin, id
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (username.ToString()) ]
  |> Sql.executeRowOptionAsync
       (fun read ->
         { username = username
           name = read.string "name"
           email = read.string "email"
           admin = read.bool "admin"
           id = read.uuid "id" })

let getUserCreatedAtExn (username : UserName.T) : Task<System.DateTime> =
  Sql.query
    "SELECT created_at
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (username.ToString()) ]
  |> Sql.executeRowAsync (fun read -> read.dateTime "created_at")

let getUserAndCreatedAtAndAnalyticsMetadata
  (username : UserName.T)
  : Task<Option<UserInfoAndCreatedAt * string>> =
  Sql.query
    "SELECT name, email, admin, created_at, id, segment_metadata
     FROM accounts
     WHERE accounts.username = @username"
  |> Sql.parameters [ "username", Sql.string (username.ToString()) ]
  |> Sql.executeRowOptionAsync
       (fun read ->
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
  |> Sql.executeRowOptionAsync
       (fun read ->
         { username = UserName.create (read.string "username")
           name = read.string "name"
           email = email
           admin = read.bool "admin"
           id = read.uuid "id" })

let getUsers : Task<List<UserName.T>> =
  Sql.query
    "SELECT username
     FROM accounts"
  |> Sql.executeAsync (fun read -> UserName.create (read.string "username"))

let isAdmin (username : UserName.T) : Task<bool> =
  Sql.query
    "SELECT 1
     FROM accounts
     WHERE accounts.username = @username
       AND admin = true"
  |> Sql.parameters [ "username", Sql.string (username.ToString()) ]
  |> Sql.executeExistsAsync

let setAdmin (admin : bool) (username : UserName.T) : Task<unit> =
  Sql.query
    "UPDATE accounts
        SET admin = @admin where username = @username"
  |> Sql.parameters [ "admin", Sql.bool admin
                      "username", Sql.string (username.ToString()) ]
  |> Sql.executeStatementAsync

let canAccessOperations (username : UserName.T) : Task<bool> = isAdmin username

let ownerID (username : UserName.T) : Task<Option<System.Guid>> =
  if List.contains username bannedUserNames then
    task { return None }
  else
    Sql.query
      "SELECT id
       FROM accounts
       WHERE accounts.username = @username"
    |> Sql.parameters [ "username", Sql.string (username.ToString()) ]
    |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")

// formerly called auth_domain_for
let ownerNameFromCanvasName (host : CanvasName.T) : OwnerName.T =
  match String.split '-' (host.ToString()) with
  | owner :: _ -> OwnerName.create owner
  | _ -> OwnerName.create (host.ToString())



// **********************
// Testing
// **********************

let initTestAccounts () : Task<unit> =
  task {
    let! test_unhashed =
      upsertNonAdmin
        Validate
        { username = UserName.create "test_unhashed"
          password = Password.fromHash "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+unhashed@darklang.com"
          name = "Dark OCaml Tests with Unhashed Password" }

    Result.okOrRaise test_unhashed

    let! test =
      upsertNonAdmin
        Validate
        { username = UserName.create "test"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test@darklang.com"
          name = "Dark OCaml Tests" }

    Result.okOrRaise test

    let! test_admin =
      upsertAdmin
        Validate
        { username = UserName.create "test_admin"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+admin@darklang.com"
          name = "Dark OCaml Test Admin" }

    Result.okOrRaise test_admin

    return ()
  }

let init () : Task<unit> =
  if Config.createAccounts then initTestAccounts () else task { return () }
// FSTODO
// initBannedAccounts ()
// initAdmins()
// initUsefulCanvases()
