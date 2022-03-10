module LibService.DBConnection

open Npgsql
open Npgsql.FSharp

let connectionString =

  let maxPoolSize = int (float Config.pgPoolSize * 1.5)
  let minPoolSize = int (float Config.pgPoolSize * 0.5)

  Sql.host Config.pgHost
  |> Sql.port 5432
  |> Sql.username Config.pgUser
  |> Sql.password Config.pgPassword
  |> Sql.database Config.pgDBName
  // |> Sql.sslMode SslMode.Require

  // Our DB in GCP supports 800 connections at once. We plan to have 3 ApiServers, 4
  // BwdServers, 4-10 QueueWorkers, and 1 CronChecker, in addition to 1 garbage
  // collector, and 1 queue-scheduler.

  |> Sql.config
       $"Pooling=true;Minimum Pool Size={minPoolSize};Maximum Pool Size={maxPoolSize};Include Error Detail=true"
  |> Sql.formatConnectionString

let connect () : Sql.SqlProps = Sql.connect connectionString
