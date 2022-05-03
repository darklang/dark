/// LaunchDarkly configuration (feature flags)
module LibService.LaunchDarkly

open FSharp.Control.Tasks
open System.Threading.Tasks

open LaunchDarkly.Sdk
open LaunchDarkly.Sdk.Server

let client = lazy (new LdClient(Config.launchDarklyApiKey))

/// IntVariation, with a default and no user
let intVar (name : string) (default_ : int) : int =
  client.Force().IntVariation(name, null, default_)

let flush () : unit = client.Force().Dispose()
