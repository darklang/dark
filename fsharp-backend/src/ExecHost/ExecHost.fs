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

module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let runMigrations () =
  print $"Running migrations"
  LibBackend.Migrations.run ()


let emergencyLogin (username : string) : Task<unit> =
  task {
    print $"Generating a cookie for {LibBackend.Config.cookieDomain}"
    // validate the user exists
    let username = UserName.create username
    let! _user = LibBackend.Account.getUser username
    let! authData = LibBackend.Session.insert username
    print
      $"See docs/emergency-login.md for instructions. Your values are
  Name = __session
  Value = {authData.sessionKey}
  Domain = {LibBackend.Config.cookieDomain}
  (note: initial dot is _important_)"
    return ()
  }

let run (args : string []) : Task<int> =
  task {
    let id = (ExecutionID "exechost")
    try
      match args with
      | [| "emergency-login"; username |] ->
        Rollbar.sendAlert "emergencyLogin called" id [ "username", username ]
        do! emergencyLogin username
        return 0
      | [| "run-migrations" |] ->
        runMigrations ()
        return 0
      | _ ->
        Rollbar.sendAlert "execHost called" id [ "args", String.concat "," args ]
        print (
          "Invalid usage!!\n\n"
          + "USAGE: ExecHost emergency-login <user>\n"
          + "USAGE: ExecHost run-migrations"
        )
        return 1
    with
    | :? System.TypeInitializationException as e ->
      print e.Message
      print e.StackTrace
      print e.InnerException.Message
      print e.InnerException.StackTrace
      return 1
    | e ->
      print e.Message
      print e.StackTrace
      return 1
  }

[<EntryPoint>]
let main (args : string []) : int =
  try
    LibService.Init.init "ExecHost"
    Telemetry.Console.loadTelemetry "ExecHost" Telemetry.TraceDBQueries
    (LibBackend.Init.init "ExecHost" false).Result
    (run args).Result
  with
  | e ->
    Rollbar.lastDitchBlocking
      "Error running ExecHost"
      (ExecutionID "execHost")
      [ "args", args ]
      e
    -1
