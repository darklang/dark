module LibBackend.Db

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp
open Ply
open Npgsql

let defaultConnection =
  let cs =
    Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "dark"
    |> Sql.password "eapnsdc"
    |> Sql.database "prodclone"
    // |> Sql.sslMode SslMode.Require
    |> Sql.config "Pooling=true"
    |> Sql.formatConnectionString

  let conn = new NpgsqlConnection(cs)
  conn.Open()
  conn

module Sql =
  let throwOrReturn (result : Async<Result<'a, exn>>) =
    task {
      let! result = result |> Async.StartImmediateAsTask

      match result with
      | Ok result -> return result
      | Error exn -> return raise exn
    }
