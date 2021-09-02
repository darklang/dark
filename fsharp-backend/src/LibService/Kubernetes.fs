module LibService.Kubernetes

// ASP.net healthchecks, mapping to k8s routes
// See https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/health-checks

open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Abstractions
open Microsoft.AspNetCore.Diagnostics.HealthChecks

let url (port : int) : string = $"http://*:{port}"

let livenessTag = "liveness"
let readinessTag = "readiness"
let startupTag = "startup"

let configureServices (services : IServiceCollection) : IServiceCollection =
  // each healthcheck is tagged according to the probes it is used in
  let allProbes = [| livenessTag; readinessTag; startupTag |]
  services
    .AddHealthChecks()
    .AddNpgSql(DBConnection.connectionString, tags = allProbes)
  |> ignore<IHealthChecksBuilder>

  services

let configureApp (port : int) (app : IApplicationBuilder) : IApplicationBuilder =
  let taggedWith (tag : string) =
    HealthCheckOptions(Predicate = fun hcr -> hcr.Tags.Contains(tag))
  app
    .UseHealthChecks(PathString(""), port) // Just filter on port, allow any path
    .UseEndpoints(
      (fun endpoints ->
        endpoints.MapHealthChecks("/k8s/livenessProbe", taggedWith livenessTag)
        |> ignore<IEndpointConventionBuilder>
        endpoints.MapHealthChecks("/k8s/startupProbe", taggedWith startupTag)
        |> ignore<IEndpointConventionBuilder>
        endpoints.MapHealthChecks("/k8s/readinessProbe", taggedWith readinessTag)
        |> ignore<IEndpointConventionBuilder>)
    )

//FSTODO: things to check

// CLEANUP add support for https://devblogs.microsoft.com/dotnet/introducing-dotnet-monitor/

//     | "/pkill" ->
//         if !shutdown (* note: this is a ref, not a boolean `not` *)
//         then (
//           shutdown := true ;
//           Log.infO
//             "shutdown"
//             ~data:"Received shutdown request - shutting down"
//             ~params:[("execution_id", Int63.to_string execution_id)] ;
//           Server.respond_string ~status:`OK ~body:"Terminated" () )
//         else (
//           Log.infO
//             "shutdown"
//             ~data:"Received redundant shutdown request - already shutting down"
//             ~params:[("execution_id", Int63.to_string execution_id)] ;
//           Server.respond_string ~status:`OK ~body:"Terminated" () )
//     | _ ->
//         Server.respond_string ~status:`Not_found ~body:"Not found" ()
//   in
