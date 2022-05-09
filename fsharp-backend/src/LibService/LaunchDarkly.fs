/// LaunchDarkly configuration (feature flags)
module LibService.LaunchDarkly

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

open LaunchDarkly.Sdk
open LaunchDarkly.Sdk.Server

module Internal =

  // For testing
  // See https://docs.launchdarkly.com/sdk/features/test-data-sources#net-server-side
  let testData = Integrations.TestData.DataSource()

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

  // Internal functions to call LD. Should only be used from within

  let intVar (flagName : string) (userSignifier : string) (default_ : int) : int =
    client.Force().IntVariation(flagName, User.WithKey userSignifier, default_)

  let boolVar (flagName : string) (userSignifier : string) (default_ : bool) : bool =
    client.Force().BoolVariation(flagName, User.WithKey userSignifier, default_)

  // Functions to create dynamic configuration, which can be set remotely
  let intConfig (name : string) (default_ : int) (testDefault : int) : unit -> int =
    testData.Update(testData.Flag(name).ValueForAllUsers(LdValue.Of testDefault))
    |> ignore<Integrations.TestData>
    fun () -> intVar name "system" default_

  // Functions to set per-canvas values
  let canvasBool
    (name : string)
    (default_ : bool)
    (testDefault : bool)
    : CanvasName.T -> bool =
    testData.Update(testData.Flag(name).ValueForAllUsers(LdValue.Of testDefault))
    |> ignore<Integrations.TestData>
    fun canvasName -> boolVar name $"canvas-{canvasName}" default_

  let canvasInt
    (name : string)
    (default_ : int)
    (testDefault : int)
    : CanvasName.T -> int =
    testData.Update(testData.Flag(name).ValueForAllUsers(LdValue.Of testDefault))
    |> ignore<Integrations.TestData>
    fun canvasName -> intVar name $"canvas-{canvasName}" default_



let flush () : unit = Internal.client.Force().Dispose()


/// Canvas Flags - these are per-canvas settings
let useEventsV2 = Internal.canvasBool "use-events-v2" false false

/// Dynamic configuration - this allows us to change the run-time values of system configuration. This is the preferred way of
let queueAllowedExecutionTime = // seconds
  Internal.intConfig "queue-allowed-execution-time" 300 300

let queueDelayBetweenPulls = // milliseconds
  Internal.intConfig "queue-delay-between-pubsub-pulls" 1000 1000
