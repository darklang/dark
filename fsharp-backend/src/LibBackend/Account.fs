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

type Account =
  { username : string
    password : LibBackend.Password.T
    email : string
    name : string }

let bannedUsernames : List<string> =
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

let userIDForUsername (user : string) : Task<UserID> =
  let owner = String.toLower user

  if List.contains owner bannedUsernames then
    failwith "Banned username"
  else
    Sql.query
      "SELECT id
       FROM accounts
       WHERE accounts.username = @username"
    |> Sql.parameters [ "username", Sql.string user ]
    |> Sql.executeRowAsync (fun read -> read.uuid "id")


let validateUsername (username : string) : Result<unit, string> =
  (* rules: no uppercase, ascii only, must start with letter, other letters can
   * be numbers or underscores. 3-20 characters. *)
  let reString = @"^[a-z][a-z0-9_]{2,20}$"

  if FsRegEx.isMatch reString username then
    Ok()
  else
    Error($"Invalid username '{username}', must match /{reString}/")


let validateEmail (email : string) : Result<unit, string> =
  (* just checking it's roughly the shape of an email *)
  let reString = @"^.+@.+\\..+$"

  if FsRegEx.isMatch reString email then
    Ok()
  else
    Error($"Invalid email '{email}, must match /{reString}/")


let validateAccount (account : Account) : Result<unit, string> =
  validateUsername account.username |> Result.and_ (validateEmail account.email)

// Passwords set here are only valid locally, production uses auth0 to check
// access
let upsertAccount (admin : bool) (account : Account) : Task<Result<unit, string>> =
  task {
    // FSTODO - this used to be default true
    let result = if true then validateAccount account else Ok() in

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
                              "username", Sql.string account.username
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



let initTestAccounts () : Task<unit> =
  task {
    let! test_unhashed =
      upsertNonAdmin
        { username = "test_unhashed"
          password = Password.fromHash "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+unhashed@darklang.com"
          name = "Dark OCaml Tests with Unhashed Password" }

    Result.okOrRaise test_unhashed

    let! test =
      upsertNonAdmin
        { username = "test"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test@darklang.com"
          name = "Dark OCaml Tests" }

    Result.okOrRaise test

    let! test_admin =
      upsertAdmin
        { username = "test_admin"
          password = Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+admin@darklang.com"
          name = "Dark OCaml Test Admin" }

    Result.okOrRaise test_admin

    return ()
  }
