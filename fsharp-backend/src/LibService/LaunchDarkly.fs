/// LaunchDarkly configuration (feature flags)
module LibService.LaunchDarkly

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

open LaunchDarkly.Sdk
open LaunchDarkly.Sdk.Server

// Flags are defined here to allow static typing
type Flag =
  | RunWorkerForCanvas
  | WorkersPerQueueWorker

  override f.ToString() : string =
    match f with
    | RunWorkerForCanvas -> "run-worker-for-canvas"
    | WorkersPerQueueWorker -> "workers-per-queueworker"

/// Set global testing values here. You can set per-"user" settings in a test, but
/// make sure they don't conflict with other tests
// See https://docs.launchdarkly.com/sdk/features/test-data-sources#net-server-side
let testData =
  let td = Integrations.TestData.DataSource()
  td
    .Update(td.Flag(string RunWorkerForCanvas).ValueForAllUsers(LdValue.Of true))
    .Update(td.Flag(string WorkersPerQueueWorker).ValueForAllUsers(LdValue.Of 1))

let client =
  lazy
    (match Config.launchDarklyApiKey with
     | Some key ->
       let config =
         Configuration
           .Builder(key)
           .StartWaitTime(System.TimeSpan.FromSeconds(1))
           .DiagnosticOptOut(false)
           .Offline(false)
           .Logging(Components.NoLogging)
           .Build()
       new LdClient(config)
     | None ->
       let config =
         Configuration
           .Builder("test")
           .DataSource(testData)
           .StartWaitTime(System.TimeSpan.FromSeconds(0))
           .DiagnosticOptOut(true)
           .Offline(false)
           .Logging(Components.NoLogging)
           .Build()
       new LdClient(config))


/// IntVariation, with a default and no user
let intVar (flag : Flag) (default_ : int) : int =
  let user = User.WithKey("__system-user__")
  client.Force().IntVariation(string flag, user, default_)

/// [someID] here doesn't have to be a darklang account id or username or whatever.
/// We can just use whateever we want to turn a knob on, such as for example
/// canvasname.
let boolUserVar (flag : Flag) (someID : string) (default_ : bool) : bool =
  client.Force().BoolVariation(string flag, User.WithKey(someID), default_)

let flush () : unit = client.Force().Dispose()
