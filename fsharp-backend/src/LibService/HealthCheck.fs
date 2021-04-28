module LibService.HealthCheck

open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Abstractions

let url (port : int) : string = $"http://*:{port}"

let configureServices (services : IServiceCollection) : IServiceCollection =
  let (_ : IHealthChecksBuilder) =
    services.AddHealthChecks().AddNpgSql(DBConnection.connectionString)

  services

let configureApp (port : int) (app : IApplicationBuilder) : IApplicationBuilder =
  app
    .UseHealthChecks(PathString("/"), port)
    .UseEndpoints(
      (fun endpoints ->
        let (_ : IEndpointConventionBuilder) =
          endpoints.MapHealthChecks("/healthz").RequireHost($"*:{port}")

        ())
    )

//FSTODO: things to check

// CLEANUP add support for https://devblogs.microsoft.com/dotnet/introducing-dotnet-monitor/


//FSTODO run a health-check service on another port
// let run ~shutdown ~execution_id =
//   let callback _conn req body =
//     match req |> CRequest.uri |> Uri.path with
//     | "/" ->
//       ( match Dbconnection.status () with
//       | `Healthy ->
//           Server.respond_string
//             ~status:`OK
//             ~body:"Hello internal overlord from qw"
//             ()
//       | `Disconnected ->
//           Server.respond_string
//             ~status:`Service_unavailable
//             ~body:"Sorry internal overlord from qw"
//             () )
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
//   (* As we're intended to be in an async thread, no reason to use a `~stop` variable for
//    * the server when we receive /pkill -- we simply need to tell our parent that it should
//    * begin shutting down *)
//   Server.create
//     ~mode:(`TCP (`Port Config.health_check_port))
//     (Server.make ~callback ())
//
