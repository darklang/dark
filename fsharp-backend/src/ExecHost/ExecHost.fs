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
let main args : int =
  try
    LibService.Init.init "ExecHost"
    Telemetry.Console.loadTelemetry "ExecHost" Telemetry.TraceDBQueries
    LibBackend.Init.init "ExecHost"
    (run args).Result
  with
  | e ->
    LibService.Rollbar.lastDitchBlocking
      "Error running ExecHost"
      (ExecutionID "execHost")
      []
      e
    -1
