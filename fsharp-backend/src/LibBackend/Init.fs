module LibBackend.Init

// LibBackend holds the whole framework

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Microsoft.Extensions.Diagnostics.HealthChecks

// Add a startup probe to check if we can check the tier one canvases
let legacyServerCheck : LibService.Kubernetes.HealthCheck =
  { probeTypes = [ LibService.Kubernetes.Startup ]
    name = "legacyServerCheck"
    checkFn =
      fun (_ : System.Threading.CancellationToken) ->
        task {
          do! Canvas.checkTierOneHosts ()
          return HealthCheckResult.Healthy("It's fine")
        } }


let init (serviceName : string) (runSideEffects : bool) : Task<unit> =
  task {

    print $"Initing LibBackend in {serviceName}"

    Json.OCamlCompatible.registerConverter (
      EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
    )

    Json.Vanilla.registerConverter (
      EventQueue.WorkerStates.STJJsonConverter.WorkerStateConverter()
    )

    if runSideEffects then do! Account.init serviceName

    print $" Inited LibBackend in {serviceName}"
  }
