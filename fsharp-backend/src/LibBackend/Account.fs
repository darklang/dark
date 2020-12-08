module LibBackend.Account

// Functions related to Accounts/Users

open System.Threading.Tasks
open FSharpPlus
open Npgsql.FSharp
open Npgsql

open Prelude
open LibExecution.SharedTypes
open Db


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
    Sql.query "SELECT id
               FROM accounts
               WHERE accounts.username = @username"
    |> Sql.parameters [ "username", Sql.string user ]
    |> Sql.executeRowAsync (fun read -> read.uuid "id")
