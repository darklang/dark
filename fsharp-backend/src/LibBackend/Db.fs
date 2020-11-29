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

let throwOrReturn (result : Async<Result<'a, exn>>) =
  task {
    let! result = result |> Async.StartImmediateAsTask

    match result with
    | Ok result -> return result
    | Error exn -> return raise exn
  }

let fetch (sql : string)
          (parameters : List<string * SqlValue>)
          (reader : RowReader -> 't)
          : Task<List<'t>> =
  Sql.existingConnection defaultConnection
  |> Sql.query sql
  |> Sql.parameters parameters
  |> Sql.executeAsync reader
  |> throwOrReturn

let fetchOne (sql : string)
             (parameters : List<string * SqlValue>)
             (reader : RowReader -> 't)
             : Task<'t> =
  Sql.existingConnection defaultConnection
  |> Sql.query sql
  |> Sql.parameters parameters
  |> Sql.executeRowAsync reader
  |> throwOrReturn



module Sql =
  let throwOrReturn (result : Async<Result<'a, exn>>) =
    task {
      let! result = result |> Async.StartImmediateAsTask

      match result with
      | Ok result -> return result
      | Error exn -> return raise exn
    }

  let query (sql : string) : Sql.SqlProps =
    Sql.query sql (Sql.existingConnection defaultConnection)

  let executeRowAsync (reader : RowReader -> 't) (props : Sql.SqlProps) : Task<'t> =
    Sql.executeRowAsync reader props |> throwOrReturn

  let executeAsync (reader : RowReader -> 't) (props : Sql.SqlProps) : Task<List<'t>> =
    Sql.executeAsync reader props |> throwOrReturn

  let tlidArray (tlids : List<LibExecution.Runtime.tlid>) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Array ||| NpgsqlTypes.NpgsqlDbType.Bigint
    let tlidsParam = NpgsqlParameter("tlids", typ)
    tlidsParam.Value <- List.toArray tlids
    Sql.parameter tlidsParam
