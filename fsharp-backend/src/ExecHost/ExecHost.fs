/// A command to run common tasks in production. Note that most tasks could/should
/// be run by creating new functions in LibDarkInternal instead. This should be
/// used for cases where that is not appropriate.
///
/// Based on https://andrewlock.net/deploying-asp-net-core-applications-to-kubernetes-part-10-creating-an-exec-host-deployment-for-running-one-off-commands/
///
/// Run with `ExecHost --help` for usage
module ExecHost

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let runMigrations () : unit =
  print $"Running migrations"
  LibBackend.Migrations.run ()

let listMigrations () : unit =
  print "Migrations needed:\n"
  LibBackend.Migrations.migrationsToRun ()
  |> List.iter (fun name -> print $" - {name}")

/// Given a username, returns a cookie to use to access Canvases
///
/// Mostly available for when Auth0 is down
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

/// Send multiple messages to Rollbar, to ensure our usage is generally OK
let triggerRollbar () : unit =
  let tags = [ "int", 6 :> obj; "string", "string"; "float", -0.6; "bool", true ]
  let prefix = "execHost test: "

  // Send async notification ("info" level in the rollbar UI)
  Rollbar.notify $"{prefix} notify" tags

  // Send async error
  Rollbar.sendError $"{prefix} sendError" tags

  // Send async exception
  let e = new System.Exception($"{prefix} sendException exception")
  Rollbar.sendException None tags e

/// Send a message to Rollbar that should result in a Page (notification)
/// going out
let triggerPagingRollbar () : int =
  // This one pages
  let prefix = "execHost test: "
  // Send sync exception - do this last to wait for the others
  let e = new System.Exception($"{prefix} last ditch blocking exception")
  Rollbar.lastDitchBlockAndPage $"{prefix} last ditch block and page" e

let help () : unit =
  [ "USAGE:"
    "  ExecHost emergency-login <user>"
    "  ExecHost migrations list"
    "  ExecHost migrations run"
    "  ExecHost trigger-rollbar"
    "  ExecHost trigger-pageable-rollbar"
    "  ExecHost convert-st-to-rt"
    "  ExecHost help" ]
  |> List.join "\n"
  |> print

type Options =
  | EmergencyLogin of string
  | MigrationList
  | MigrationsRun
  | TriggerRollbar
  | TriggerPagingRollbar
  | InvalidUsage
  | ConvertST2RT of string
  | ConvertST2RTAll
  | Help

let parse (args : string []) : Options =
  match args with
  | [| "emergency-login"; username |] -> EmergencyLogin username
  | [| "migrations"; "list" |] -> MigrationList
  | [| "migrations"; "run" |] -> MigrationsRun
  | [| "trigger-rollbar" |] -> TriggerRollbar
  | [| "trigger-paging-rollbar" |] -> TriggerPagingRollbar
  | [| "convert-st-to-rt"; "all" |] -> ConvertST2RTAll
  | [| "convert-st-to-rt"; canvasName |] -> ConvertST2RT canvasName
  | [| "help" |] -> Help
  | _ -> InvalidUsage

let usesDB (options : Options) =
  match options with
  | EmergencyLogin _
  | MigrationList
  | MigrationsRun -> true
  | TriggerRollbar
  | TriggerPagingRollbar
  | InvalidUsage
  | ConvertST2RT _
  | ConvertST2RTAll
  | Help -> false

let convertToRT (canvasName : string) : Task<unit> =
  task {
    let canvasName = CanvasName.createExn canvasName
    let! canvasInfo = LibBackend.Canvas.getMetaExn canvasName
    let! canvas = LibBackend.Canvas.loadAll canvasInfo
    let _program = LibBackend.Canvas.toProgram canvas
    let _handlers =
      canvas.handlers
      |> Map.values
      |> List.map (fun h -> LibExecution.ProgramTypesToRuntimeTypes.Handler.toRT h)
    return ()
  }





let run (options : Options) : Task<int> =
  task {
    // Track calls to this
    use _ = Telemetry.createRoot "ExecHost run"
    Telemetry.addTags [ "options", options ]
    Rollbar.notify "execHost called" [ "options", string options ]

    match options with

    | EmergencyLogin username ->
      Rollbar.notify "emergencyLogin called" [ "username", username ]
      do! emergencyLogin username
      return 0

    | MigrationList ->
      listMigrations ()
      return 0

    | MigrationsRun ->
      runMigrations ()
      return 0

    | TriggerRollbar ->
      triggerRollbar ()
      // The async operations go in a queue, and I don't know how to block until
      // the queue is empty.
      do! Task.Delay 5000
      return 0

    | TriggerPagingRollbar -> return triggerPagingRollbar ()

    | ConvertST2RT canvasName ->
      do! convertToRT canvasName
      return 0

    | ConvertST2RTAll ->
      let! allCanvases = LibBackend.Serialize.currentHosts ()
      do! Task.iterWithConcurrency 25 convertToRT allCanvases
      return 0


    | Help ->
      help ()
      return 0

    | InvalidUsage ->
      print "Invalid usage!!\n"
      help ()
      return 1
  }

[<EntryPoint>]
let main (args : string []) : int =
  let name = "ExecHost"
  try
    LibService.Init.init name
    Telemetry.Console.loadTelemetry name Telemetry.TraceDBQueries
    let options = parse args
    if usesDB options then
      (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    let result = (run options).Result
    LibService.Init.shutdown name
    result
  with
  | e ->
    // Don't reraise or report as ExecHost is only run interactively
    printException "" [] e
    LibService.Init.shutdown name
    1
