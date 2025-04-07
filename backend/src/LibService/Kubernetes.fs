/// Kubernetes configuration, managing ASP.NET healthchecks, and other endpoints from
/// the k8s control plane.
/// See https://docs.microsoft.com/en-us/aspnet/core/host-and-deploy/health-checks
module LibService.Kubernetes

open FSharp.Control.Tasks
open System.Threading.Tasks

open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Diagnostics.HealthChecks
open Microsoft.AspNetCore.Hosting

open Microsoft.Extensions.Diagnostics.HealthChecks

let url (port : int) : string = $"http://*:{port}"

let livenessTag = "liveness"
let readinessTag = "readiness"
let startupTag = "startup"

type ProbeType =
  | Liveness
  | Readiness
  | Startup

  override this.ToString() =
    match this with
    | Liveness -> livenessTag
    | Readiness -> readinessTag
    | Startup -> startupTag

type HealthCheck =
  { name : string
    checkFn : System.Threading.CancellationToken -> Task<HealthCheckResult>
    probeTypes : List<ProbeType> }


let configureServices
  (healthChecks : List<HealthCheck>)
  (services : IServiceCollection)
  : IServiceCollection =
  // each healthcheck is tagged according to the probes it is used in
  // TODO bring these back for sqlite if relevant? or just drop?
  //let allProbes = [| livenessTag; readinessTag; startupTag |]

  let healthChecksBuilder =
    services
      .AddHealthChecks()
      //.AddNpgSql(DBConnection.dataSource.ConnectionString, tags = allProbes)

  healthChecks
  |> List.iter (fun hc ->
    let tags = hc.probeTypes |> List.map string |> List.toArray
    healthChecksBuilder.AddAsyncCheck(hc.name, hc.checkFn, tags = tags)
    |> ignore<IHealthChecksBuilder>)

  services

let livenessPath = "/k8s/livenessProbe"
let startupPath = "/k8s/startupProbe"
let readinessPath = "/k8s/readinessProbe"

let configureApp (port : int) (app : IApplicationBuilder) : IApplicationBuilder =
  app.UseEndpoints(
    (fun endpoints ->
      let taggedWith (tag : string) =
        HealthCheckOptions(Predicate = fun hcr -> hcr.Tags.Contains(tag))
      let addHealthCheck (path : string) (tag : string) =
        endpoints.MapHealthChecks(path, taggedWith tag).RequireHost($"*:{port}")
        |> ignore<IEndpointConventionBuilder>
        // The k8s healthcheck comes in on a port, but the google load balancer
        // health check does not include the port in its request. We manually set the
        // load balancer to set the host header to be "bwdserver-healthcheck" (adding
        // a port was not supported)
        endpoints
          .MapHealthChecks(path, taggedWith tag)
          .RequireHost($"gce-ingress-healthcheck")
        |> ignore<IEndpointConventionBuilder>
      addHealthCheck livenessPath livenessTag
      addHealthCheck startupPath startupTag
      addHealthCheck readinessPath readinessTag)
  )


// When a pod is being replaced, scaled down, etc, k8s needs to kill the old pod
// (which is of course active at this point, receiving HTTP requests). It has 2
// mechanisms, which it triggers together: a SIGTERM signal, and a preStop setting.
// (Since we need to handle the SIGTERM signal either way, we don't use the preStop
// setting). Basic explanation here:
// https://blog.markvincze.com/graceful-termination-in-kubernetes-with-asp-net-core/
// https://cloud.google.com/blog/products/containers-kubernetes/kubernetes-best-practices-terminating-with-grace
//
// After sending these triggers, K8s might keep sending requests though, I'm not
// sure. It certainly did in (old versions of the?) nginx ingress, which we use for
// custom domains.  It kept sending requests until the pod was removed from the load
// balancer, which took moment. So we want to finish existing requests, and also
// possibly new requests. If we don't, we'll get 502s. Discusssed here:
// https://andrewlock.net/deploying-asp-net-core-applications-to-kubernetes-part-11-avoiding-downtime-in-rolling-deployments-by-blocking-sigterm/
//
// Our goal when shutting down is to allow the existing requests to complete, and
// then shutdown. This will prevent requests from dying and the requestor getting
// 502s.
//
// Ideally we would make sure other resources are cleaned up (traces have been
// uploaded, etc), but for now we'll stick to just doing what we've always done,
// which is wait 28 seconds (leaving 2 seconds for the rest of the cleanup).
//
// .NET already knows to shut down when it gets a SIGTERM, after which it will wait
// until all requests are complete before shutting down, subject to timeouts. We want
// to allow an extended timeout of 28 seconds, which is more than the default.
//
// It appears that .NET should support HostOptions.ShutDownTimeout, but that doesn't
// appear to work.
//
// The other way is to do the timeout ourselves. We can register an
// ApplicationStopping callback, and the process will not shutdown until this
// callback returns. Of course, k8s will kill the process after 30s, so it's
// worthwhile to finish a little sooner so that .NET can do other cleanup if needed.
//
// CLEANUP: in the future, we would like to wait:
// - check the rollbars, honeycombs, etc, have been dispatched
// - check the traces have been uploaded
// - shutdown as soon as we're done so the rolling upgrade can complete more quickly.

let registerServerTimeout (b : IWebHostBuilder) : unit =
  b.UseShutdownTimeout(System.TimeSpan.FromSeconds(28.0))
  |> ignore<IWebHostBuilder>

open System.Runtime.InteropServices

let registerShutdownCallback (callback : unit -> unit) : unit =
  PosixSignalRegistration.Create(PosixSignal.SIGTERM, (fun _ -> callback ()))
  |> ignore<PosixSignalRegistration>

  PosixSignalRegistration.Create(PosixSignal.SIGINT, (fun _ -> callback ()))
  |> ignore<PosixSignalRegistration>

  PosixSignalRegistration.Create(PosixSignal.SIGQUIT, (fun _ -> callback ()))
  |> ignore<PosixSignalRegistration>

  ()

/// Run an ASP.NET server that provides the healthcheck and shutdown endpoints. This
/// is for services which need to be managed but do not have HTTP servers of their
/// own.
let runKubernetesServer
  (_serviceName : string)
  (healthChecks : List<HealthCheck>)
  (port : int)
  (shutdownCallback : unit -> unit)
  : Task =
  let builder = WebApplication.CreateBuilder()
  configureServices healthChecks builder.Services |> ignore<IServiceCollection>
  registerServerTimeout builder.WebHost
  builder.WebHost.UseUrls(url port) |> ignore<IWebHostBuilder>
  registerShutdownCallback shutdownCallback

  let app = builder.Build()
  Rollbar.AspNet.addRollbarToApp app (fun _ -> None, []) (Some startupPath)
  |> fun app -> app.UseRouting()
  |> configureApp port
  |> ignore<IApplicationBuilder>
  app.RunAsync()





// CLEANUP add support for https://devblogs.microsoft.com/dotnet/introducing-dotnet-monitor/
