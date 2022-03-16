module LibBackend.Init

// LibBackend holds the whole framework

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Db

open Prelude
open Microsoft.Extensions.Diagnostics.HealthChecks

// Add a startup probe to check if we can check the tier one canvases
let legacyServerCheck : LibService.Kubernetes.HealthCheck =
  { probeTypes = [ LibService.Kubernetes.Startup ]
    name = "legacyServerCheck"
    checkFn =
      fun (_ : System.Threading.CancellationToken) ->
        task {
          try
            // Make sure we can load a canvas
            // Loading all the tier one canvases takes way too long, so just pick a simple one
            let host = CanvasName.create "ian-httpbin"
            let! meta = Canvas.getMeta host
            let! (_ : Canvas.T) = Canvas.loadAll meta
            return HealthCheckResult.Healthy("It's fine")
          with
          | e -> return HealthCheckResult.Unhealthy(e.Message)
        } }



let waitForDB () : Task<unit> =
  task {
    let mutable success = false
    while not success do
      try
        do!
          Sql.query "select current_date"
          |> Sql.parameters []
          |> Sql.executeStatementAsync
        success <- true
      with
      | _ -> do! Task.Delay 1000
      return ()
  }



let init (serviceName : string) (runSideEffects : bool) : Task<unit> =
  task {
    print $"Initing LibBackend in {serviceName}"
    Db.init ()

    Json.OCamlCompatible.registerConverter (
      EventQueue.WorkerStates.JsonConverter.WorkerStateConverter()
    )

    Json.Vanilla.registerConverter (
      EventQueue.WorkerStates.STJJsonConverter.WorkerStateConverter()
    )

    if runSideEffects then do! Account.init serviceName

    print $" Inited LibBackend in {serviceName}"
  }
