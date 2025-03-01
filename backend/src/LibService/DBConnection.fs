module LibService.DBConnection

open Prelude

// open Npgsql
// open Npgsql.FSharp

open Microsoft.Extensions.Logging.Console
open Microsoft.Extensions.Logging


// let connectionString () : string =
//   let sb = NpgsqlConnectionStringBuilder()
//   sb.Host <- Config.pgHost
//   sb.Port <- Config.pgPort
//   sb.Username <- Config.pgUser
//   sb.Password <- Config.pgPassword
//   sb.Database <- Config.pgDBName

//   sb.SslMode <- if Config.pgSslRequired then SslMode.VerifyFull else SslMode.Allow

//   sb.LoadBalanceHosts <- true

//   sb.IncludeErrorDetail <- true

//   // Our DB in GCP supports 800 connections at once.
//   // We plan to have 2 BwdServers, 2 QueueWorkers, and 1 CronChecker
//   sb.Pooling <- true
//   sb.MinPoolSize <- int (float Config.pgPoolSize * 0.5)
//   sb.MaxPoolSize <- int (float Config.pgPoolSize * 1.5)

//   match Config.pgRootCertPath with
//   | Some rootCert -> sb.RootCertificate <- rootCert
//   | None -> ()

//   sb.ToString()

// let debugConnectionString () : string =
//   match Config.pgLogLevel with
//   | None -> "debug info disabled"
//   | Some _ ->
//     let cs = connectionString ()
//     let cs = FsRegEx.replace "password=.*?;" "password=***;" cs
//     let cs = FsRegEx.replace "Password=.*?;" "Password=***;" cs
//     cs


// let dataSource : NpgsqlDataSource =
//   let cs = connectionString ()
//   let builder = new NpgsqlDataSourceBuilder(cs)

//   // Logging to debug connection issues
//   match Config.pgLogLevel with
//   | Some level ->
//     let level =
//       match level with
//       | "debug" -> LogLevel.Debug
//       | _ -> LogLevel.Information
//     let loggerFactory =
//       LoggerFactory.Create(fun builder ->
//         builder.AddConsole().SetMinimumLevel(level) |> ignore<_>)
//     builder.UseLoggerFactory(loggerFactory).EnableParameterLogging()
//     |> ignore<NpgsqlDataSourceBuilder>
//   | None -> ()

//   builder.UseNodaTime() |> ignore<TypeMapping.INpgsqlTypeMapper>
//   builder.Build()

let debugConnection () : unit =
  // get the id of the host to check it resolves
  LibService.Config.pgHost
  |> System.Net.Dns.GetHostAddresses
  |> Seq.iter (fun ip -> printTime $"IP is ip {ip}")

  // check it's reachable - using a method which works across vpcs on google cloud
  let url = $"{LibService.Config.pgHost}:{LibService.Config.pgPort}"
  try
    let client = new System.Net.Sockets.TcpClient()
    let connection =
      client.BeginConnect(
        LibService.Config.pgHost,
        LibService.Config.pgPort,
        null,
        null
      )
    client.EndConnect(connection)
    let success = connection.AsyncWaitHandle.WaitOne(5000)
    if not success then
      printTime $"Failed to connect to {url}"
    else
      printTime $"Got connected to {url}"
  with e ->
    Telemetry.addException [] e
    printTime $"Failed to get response from {url}: {e.Message} {e.StackTrace}"
