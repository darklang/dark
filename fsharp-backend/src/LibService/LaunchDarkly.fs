/// LaunchDarkly configuration (feature flags)
module LibService.LaunchDarkly

open FSharp.Control.Tasks
open System.Threading.Tasks

open LaunchDarkly.Sdk
open LaunchDarkly.Sdk.Server

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
           .Builder("")
           .StartWaitTime(System.TimeSpan.FromSeconds(0))
           .DiagnosticOptOut(true)
           .Offline(true)
           .Logging(Components.NoLogging)
           .Build()
       new LdClient(config))

/// IntVariation, with a default and no user
let intVar (name : string) (default_ : int) : int =
  client.Force().IntVariation(name, null, default_)

/// [someID] here doesn't have to be a darklang account id or username or whatever.
/// We can just use whateever we want to turn a knob on, such as for example
/// canvasname.
let boolUserVar (name : string) (someID : string) (default_ : bool) : bool =
  client.Force().BoolVariation(name, User.WithKey(someID), default_)

let flush () : unit = client.Force().Dispose()
