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


let emergencyLogin (username : string) =
  print $"Generating a cookie for {LibBackend.Config.cookieDomain}"
  // FSTODO: validate the user exists
  let authData = (LibBackend.Session.insert username).Result
  print
    $"See docs/emergency-login.md for instructions. Your values are
Name = __session
Value = {authData.sessionKey}
Domain = {LibBackend.Config.cookieDomain}
(note: initial dot is _important_)"
  ()

[<EntryPoint>]
let main args : int =
  try
    LibBackend.Init.init "execHost"
  with
  | e ->
    // FSTODO rollbar
    raise e

  try
    // FSTODO reportToRollbar commands
    match args with
    | [| "emergency-login"; username |] -> emergencyLogin username
    | [| "run-migrations" |] -> runMigrations ()
    | _ ->
      print (
        "Invalid usage!!\n\nUSAGE: ExecHost emergency-login <user>\n"
        + "USAGE: ExecHost run-migrations"
      )
    0
  with
  | e ->
    print e.Message
    1
