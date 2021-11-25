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

open Argu

type EmergencyLogin =
    | [<MainCommand; ExactlyOnce>] User of user:string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | User _ -> "The username to create a session for"

and RunMigrations =
    | [<Hidden>] Nothing

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Nothing -> "<no arguments>"

[<RequireSubcommand>]
type Arguments =
    | [<CliPrefix(CliPrefix.None)>] Emergency_Login of ParseResults<EmergencyLogin>
    | [<CliPrefix(CliPrefix.None)>] Run_Migrations of ParseResults<RunMigrations>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Run_Migrations _ -> "Run migrations"
            | Emergency_Login _ -> "Login a user in production"

let parser = ArgumentParser.Create<Arguments>(programName = "ExecHost")

[<EntryPoint>]
let main args : int =
  try
    LibBackend.Init.init "execHost"
    let cliArgs = parser.ParseCommandLine args
    let runMigrations = cliArgs.TryGetResult Run_Migrations
    let login = cliArgs.TryGetResult Emergency_Login


    debuG "login" login
    debuG "runMigrations" runMigrations
    // FSTODO send message to slack
    // (runBenchmark filename iterations warmUpCount).GetAwaiter().GetResult()
    0
  with
  | e ->
    // FSTODO rollbar
    print e.Message
    1
