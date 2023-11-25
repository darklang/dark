module LibService.DBConnection

open Npgsql
open Npgsql.FSharp


let connectionString () : string =
  let sb = NpgsqlConnectionStringBuilder()
  sb.Host <- Config.pgHost
  sb.Port <- Config.pgPort
  sb.Username <- Config.pgUser
  sb.Password <- Config.pgPassword
  sb.Database <- Config.pgDBName
  sb.SslMode <- if Config.pgSslRequired then SslMode.VerifyFull else SslMode.Allow
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
  builder.UseNodaTime() |> ignore<TypeMapping.INpgsqlTypeMapper>
  builder.Build()
