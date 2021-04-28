module LibService.DBConnection

open Npgsql
open Npgsql.FSharp.Tasks

let connectionString =
  Sql.host Config.pghost
  |> Sql.port 5432
  |> Sql.username Config.pguser
  |> Sql.password Config.pgpassword
  |> Sql.database Config.pgdbname
  // |> Sql.sslMode SslMode.Require

  // Our DB in GCP supports 800 connections at once. In the current rollout, we
  // have 32 api servers, 32 bwd servers, and 60 queue servers. We also need 6
  // connections for GCP, and room to roll up.
  |> Sql.config "Pooling=true;Maximum Pool Size=4;Include Error Detail=true"
  |> Sql.formatConnectionString

let connect () : Sql.SqlProps = Sql.connect connectionString
