module LibService.DBConnection

open Npgsql
open Npgsql.FSharp

let dataSource : NpgsqlDataSource =

  let maxPoolSize = int (float Config.pgPoolSize * 1.5)
  let minPoolSize = int (float Config.pgPoolSize * 0.5)

  let connectionString =

    Sql.host Config.pgHost
    |> Sql.port 5432
    |> Sql.username Config.pgUser
    |> Sql.password Config.pgPassword
    |> Sql.database Config.pgDBName
    // |> Sql.sslMode SslMode.Require

    // Our DB in GCP supports 800 connections at once. We plan to have 2 ApiServers, 2
    // BwdServers, 2 QueueWorkers, and 1 CronChecker, in addition to 1 garbage
    // collector.

    |> Sql.config
      $"Pooling=true;Minimum Pool Size={minPoolSize};Maximum Pool Size={maxPoolSize};Include Error Detail=true"
    |> Sql.formatConnectionString

  let builder = new NpgsqlDataSourceBuilder(connectionString)
  builder.UseNodaTime() |> ignore<TypeMapping.INpgsqlTypeMapper>
  builder.Build()
