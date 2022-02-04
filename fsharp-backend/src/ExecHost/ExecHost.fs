module ExecHost

// A command to run common tasks in production. Note that most tasks could/should be
// run by creating new functions in LibDarkInternal instead. This should be used for
// cases where that is not appropriate.

// Based on https://andrewlock.net/deploying-asp-net-core-applications-to-kubernetes-part-10-creating-an-exec-host-deployment-for-running-one-off-commands/

// Run with `ExecHost --help` for usage

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let runMigrations (executionID : ExecutionID) : unit =
  print $"Running migrations"
  LibBackend.Migrations.run executionID

let listMigrations (executionID : ExecutionID) : unit =
  print "Migrations needed:\n"
  LibBackend.Migrations.migrationsToRun ()
  |> List.iter (fun name -> print $" - {name}")


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

let triggerRollbar (executionID : ExecutionID) : unit =
  let tags = [ "int", 6 :> obj; "string", "string"; "float", -0.6; "bool", true ]
  let prefix = "execHost test: "

  // Send async notification ("info" level in the rollbar UI)
  Rollbar.notify executionID $"{prefix} notify" tags

  // Send async error
  Rollbar.sendError executionID $"{prefix} sendError" tags

  // Send async exception
  let e = new System.Exception($"{prefix} sendException exception")
  Rollbar.sendException executionID $"{prefix} sendException" tags e

  // Send sync exception - do this last to wait for the others
  let e = new System.Exception($"{prefix} last ditch blocking exception")
  Rollbar.lastDitchBlocking executionID $"{prefix} last ditch blocking" tags e



let help () : unit =
  [ "USAGE:"
    "  ExecHost emergency-login <user>"
    "  ExecHost migrations list"
    "  ExecHost migrations run"
    "  ExecHost trigger-rollbar"
    "  ExecHost help" ]
  |> List.join "\n"
  |> print

let run (executionID : ExecutionID) (args : string []) : Task<int> =
  task {
    try
      // Track calls to the this
      use _ = Telemetry.createRoot "ExecHost run"
      Telemetry.addTags [ "args", args ]
      Rollbar.notify executionID "execHost called" [ "args", String.concat "," args ]

      match args with

      | [| "emergency-login"; username |] ->
        Rollbar.notify executionID "emergencyLogin called" [ "username", username ]
        do! emergencyLogin username
        return 0

      | [| "migrations"; "list" |] ->
        listMigrations executionID
        return 0

      | [| "migrations"; "run" |] ->
        runMigrations executionID
        return 0

      | [| "trigger-rollbar" |] ->
        triggerRollbar executionID
        // The async operations go in a queue, and I don't know how to block until
        // the queue is empty. Maybe not necessary due to lastDitchBlocking above,
        // but worth keeping in to be certain it arrives
        do! Task.Delay 5000
        return 0

      | [| "help" |] ->
        help ()
        return 0

      | _ ->
        print "Invalid usage!!\n"
        help ()
        return 1

    with
    // Don't reraise as ExecHost is only run interactively
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
    let executionID = Telemetry.executionID ()
    (LibBackend.Init.init "ExecHost" false).Result
    (run executionID args).Result
  with
  | e ->
    Rollbar.lastDitchBlocking
      (ExecutionID "execHost")
      "Error running ExecHost"
      [ "args", args ]
      e
    -1
