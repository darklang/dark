module ExecHost

// A command to run common tasks in production. Note that most tasks could/should be
// run by creating new functions in LibDarkInternal instead. This should be used for
// cases where that is not appropriate.

// Based on https://andrewlock.net/deploying-asp-net-core-applications-to-kubernetes-part-10-creating-an-exec-host-deployment-for-running-one-off-commands/

// Usage:
// exechost run-migrations
// exechost login <user>

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

let runMigrations () =
  print $"Running migrations"
  LibBackend.Migrations.run ()


let emergencyLogin (username : string) : Task<unit> =
  task {
    print $"Generating a cookie for {LibBackend.Config.cookieDomain}"
    // validate the user exists
    let! _username = LibBackend.Account.getUser (UserName.create username)
    let! authData = LibBackend.Session.insert username
    print
      $"See docs/emergency-login.md for instructions. Your values are
  Name = __session
  Value = {authData.sessionKey}
  Domain = {LibBackend.Config.cookieDomain}
  (note: initial dot is _important_)"
    return ()
  }

[<EntryPoint>]
let main args : int =
  let mainTask =
    task {
      try
        LibBackend.Init.init "execHost"
      with
      | e ->
        // FSTODO rollbar
        raise e

      try
        // FSTODO reportToRollbar commands
        match args with
        | [| "emergency-login"; username |] ->
          do! emergencyLogin username
          return 0
        | [| "run-migrations" |] ->
          runMigrations ()
          return 0
        | _ ->
          print (
            "Invalid usage!!\n\nUSAGE: ExecHost emergency-login <user>\n"
            + "USAGE: ExecHost run-migrations"
          )
          return 1
      with
      | e ->
        print e.Message
        print e.StackTrace
        return 1
    }
  mainTask.Result
