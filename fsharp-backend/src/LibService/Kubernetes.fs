module LibService.Kubernetes

// Kubernetes configuration, managing ASP.NET healthchecks, and other endpoints from
// the k8s control plane.
// See https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/health-checks

open FSharp.Control.Tasks
open System.Threading.Tasks

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


// https://blog.markvincze.com/graceful-termination-in-kubernetes-with-asp-net-core/
// https://andrewlock.net/deploying-asp-net-core-applications-to-kubernetes-part-11-avoiding-downtime-in-rolling-deployments-by-blocking-sigterm/
let terminateService (ctx : HttpContext) = task { return () } :> Task

let configureApp (port : int) (app : IApplicationBuilder) : IApplicationBuilder =

  app.UseEndpoints(
    (fun endpoints ->
      let taggedWith (tag : string) =
        HealthCheckOptions(Predicate = fun hcr -> hcr.Tags.Contains(tag))
      let addHealthCheck (path : string) (tag : string) =
        endpoints.MapHealthChecks(path, taggedWith tag).RequireHost($"*:{port}")
        |> ignore<IEndpointConventionBuilder>
      addHealthCheck "/k8s/livenessProbe" livenessTag
      addHealthCheck "/k8s/startupProbe" startupTag
      addHealthCheck "/k8s/readinessProbe" readinessTag

      endpoints
        .MapPost("/k8s/preStop", RequestDelegate terminateService)
        .RequireHost($"*:{port}")
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
