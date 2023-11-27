module LibService.DBConnection

open Npgsql
open Npgsql.FSharp

open Microsoft.Extensions.Logging.Console
open Microsoft.Extensions.Logging


let connectionString () : string =
  let sb = NpgsqlConnectionStringBuilder()
  sb.Host <- Config.pgHost
  sb.Port <- Config.pgPort
  sb.Username <- Config.pgUser
  sb.Password <- Config.pgPassword
  sb.Database <- Config.pgDBName
  sb.SslMode <- if Config.pgSslRequired then SslMode.VerifyFull else SslMode.Allow
  match Config.pgRootCertPath with
  | Some rootCert -> sb.RootCertificate <- rootCert
  | None -> ()
  sb.IncludeErrorDetail <- true
  // Our DB in GCP supports 800 connections at once. We plan to have 2
  // BwdServers, 2 QueueWorkers, and 1 CronChecker
  sb.Pooling <- true
  sb.MinPoolSize <- int (float Config.pgPoolSize * 0.5)
  sb.MaxPoolSize <- int (float Config.pgPoolSize * 1.5)
  sb.ToString()


let dataSource : NpgsqlDataSource =
  let cs = connectionString ()
  let builder = new NpgsqlDataSourceBuilder(cs)

  // Logging to debug connection issues
  match Config.pgLogLevel with
  | Some level ->
    let level =
      match level with
      | "debug" -> LogLevel.Debug
      | _ -> LogLevel.Information
    let loggerFactory =
      LoggerFactory.Create(fun builder ->
        builder.AddConsole().SetMinimumLevel(level) |> ignore<_>)
    builder.UseLoggerFactory(loggerFactory).EnableParameterLogging()
    |> ignore<NpgsqlDataSourceBuilder>
  | None -> ()

  builder.UseNodaTime() |> ignore<TypeMapping.INpgsqlTypeMapper>
  builder.Build()
