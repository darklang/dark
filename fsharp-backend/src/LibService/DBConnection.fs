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


  // Our DB in GCP supports 800 connections at once. In the current rollout, we
  // have 32 api servers, 32 bwd servers, and 60 queue servers. We also need 6
  // connections for GCP, and room to roll up.
  |> Sql.config
       $"Pooling=true;Minimum Pool Size={minPoolSize};Maximum Pool Size={maxPoolSize};Include Error Detail=true"
  |> Sql.formatConnectionString

let connect () : Sql.SqlProps = Sql.connect connectionString
