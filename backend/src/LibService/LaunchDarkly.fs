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
  let boolSetTestDefault (flagName : string) (default_ : bool) =
    testData.Update(testData.Flag(flagName).ValueForAllUsers(LdValue.Of default_))
    |> ignore<Integrations.TestData>

  let intSetTestDefault (flagName : string) (default_ : int) =
    testData.Update(testData.Flag(flagName).ValueForAllUsers(LdValue.Of default_))
    |> ignore<Integrations.TestData>

  let floatSetTestDefault (flagName : string) (default_ : float) =
    testData.Update(testData.Flag(flagName).ValueForAllUsers(LdValue.Of default_))
    |> ignore<Integrations.TestData>

  let stringSetTestDefault (flagName : string) (default_ : string) =
    testData.Update(testData.Flag(flagName).ValueForAllUsers(LdValue.Of default_))
    |> ignore<Integrations.TestData>

  let boolVar (flagName : string) (userSignifier : string) (default_ : bool) : bool =
    client.Force().BoolVariation(flagName, User.WithKey userSignifier, default_)

  let intVar (flagName : string) (userSignifier : string) (default_ : int) : int =
    client.Force().IntVariation(flagName, User.WithKey userSignifier, default_)

  let floatVar
    (flagName : string)
    (userSignifier : string)
    (default_ : float)
    : float =
    // Note use of name Double here (C# double is an F# float)
    client.Force().DoubleVariation(flagName, User.WithKey userSignifier, default_)

  let stringVar
    (flagName : string)
    (userSignifier : string)
    (default_ : string)
    : string =
    client.Force().StringVariation(flagName, User.WithKey userSignifier, default_)


  // -------------
  // System values
  // Functions to create dynamic configuration, which can be set remotely. These
  // provide knobs to change how the system works remotely, for example, changing the
  // number of queue events to execute simulateously.
  // -------------
  let boolConfig
    (name : string)
    (default_ : bool)
    (testDefault : bool)
    : unit -> bool =
    boolSetTestDefault name testDefault
    fun () -> boolVar name "system" default_

  let intConfig (name : string) (default_ : int) (testDefault : int) : unit -> int =
    intSetTestDefault name testDefault
    fun () -> intVar name "system" default_

  let floatConfig
    (name : string)
    (default_ : float)
    (testDefault : float)
    : unit -> float =
    floatSetTestDefault name testDefault
    fun () -> floatVar name "system" default_

  let stringConfig
    (name : string)
    (default_ : string)
    (testDefault : string)
    : unit -> string =
    stringSetTestDefault name testDefault
    fun () -> stringVar name "system" default_



  // -------------
  // per-handler values
  // -------------

  let flagNameForHandler (canvasID : CanvasID) (tlid : tlid) =
    // Use tlid instead of a desc because a tlid is canonical. A desc would be better
    // because it's user readable but it's not too hard to find a tlid id
    $"handler-{canvasID}-{tlid}"

  let handlerBool
    (name : string)
    (default_ : bool)
    (testDefault : bool)
    : CanvasID -> tlid -> bool =
    boolSetTestDefault name testDefault
    fun canvasName tlid -> boolVar name (flagNameForHandler canvasName tlid) default_

  let handlerInt
    (name : string)
    (default_ : int)
    (testDefault : int)
    : CanvasID -> tlid -> int =
    intSetTestDefault name testDefault
    fun canvasName tlid -> intVar name (flagNameForHandler canvasName tlid) default_

  let handlerFloat
    (name : string)
    (default_ : float)
    (testDefault : float)
    : CanvasID -> tlid -> float =
    floatSetTestDefault name testDefault
    fun canvasName tlid ->
      floatVar name (flagNameForHandler canvasName tlid) default_

  let handlerString
    (name : string)
    (default_ : string)
    (testDefault : string)
    : CanvasID -> tlid -> string =
    stringSetTestDefault name testDefault
    fun canvasName tlid ->
      stringVar name (flagNameForHandler canvasName tlid) default_


  // -------------
  // per-canvas values
  // -------------

  let canvasBool
    (name : string)
    (default_ : bool)
    (testDefault : bool)
    : CanvasID -> bool =
    boolSetTestDefault name testDefault
    fun canvasID -> boolVar name $"canvas-{canvasID}" default_

  let canvasInt
    (name : string)
    (default_ : int)
    (testDefault : int)
    : CanvasID -> int =
    intSetTestDefault name testDefault
    fun canvasID -> intVar name $"canvas-{canvasID}" default_

  let canvasFloat
    (name : string)
    (default_ : float)
    (testDefault : float)
    : CanvasID -> float =
    floatSetTestDefault name testDefault
    fun canvasID -> floatVar name $"canvas-{canvasID}" default_

  let canvasString
    (name : string)
    (default_ : string)
    (testDefault : string)
    : CanvasID -> string =
    stringSetTestDefault name testDefault
    fun canvasID -> stringVar name $"canvas-{canvasID}" default_

  // -------------
  // per-service values
  // -------------
  let serviceBool
    (name : string)
    (default_ : bool)
    (testDefault : bool)
    : string -> bool =
    boolSetTestDefault name testDefault
    fun serviceName -> boolVar name $"service-{serviceName}" default_

  let serviceInt
    (name : string)
    (default_ : int)
    (testDefault : int)
    : string -> int =
    intSetTestDefault name testDefault
    fun serviceName -> intVar name $"service-{serviceName}" default_

  let serviceFloat
    (name : string)
    (default_ : float)
    (testDefault : float)
    : string -> float =
    floatSetTestDefault name testDefault
    fun serviceName -> floatVar name $"service-{serviceName}" default_

  let serviceString
    (name : string)
    (default_ : string)
    (testDefault : string)
    : string -> string =
    stringSetTestDefault name testDefault
    fun serviceName -> stringVar name $"service-{serviceName}" default_



let flush () : unit = Internal.client.Force().Dispose()

// --------------
// Handler Flags - per-canvas, per-handler settings
// --------------

let traceSamplingRule =
  Internal.handlerString
    "traces-sampling-rule"
    Config.traceSamplingRuleDefault
    "sample-all"

// --------------
// Canvas Flags - these are per-canvas settings
// --------------
let knownBroken = Internal.canvasBool "canvas-known-broken" false false


// --------------
// Service Flags - may be different for each service
// --------------
// Whether to record traces
let telemetrySamplePercentage =
  Internal.serviceFloat "telemetry-sample-percentage" 100.0 100.0

// --------------
// System flags - this allows us to change the run-time values of system
// configuration. This is the preferred way of setting arbitrary numbers
// --------------
let healthCheckDomains = Internal.stringConfig "canvas-health-checks" "" ""

let queueAllowedExecutionTimeInSeconds =
  Internal.intConfig "queue-allowed-execution-time-in-seconds" 300 300

/// Limit to the number of events each QueueWorker will run concurrently
let queueMaxConcurrentEventsPerWorker =
  // 4 is conservative, we'll probably set this much higher
  Internal.intConfig "queue-max-concurrent-events-per-worker" 4 4

/// Delay between fetches from the queue when something goes wrong
let queueDelayBetweenPullsInMillis =
  Internal.intConfig "queue-delay-between-pubsub-pulls-in-millis" 1000 1000
