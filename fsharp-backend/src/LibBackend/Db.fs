module LibBackend.Db

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp
open Npgsql

// make sure the connection is returned to the pool
let connect () : Sql.SqlProps =
  Sql.host "localhost"
  |> Sql.port 5432
  |> Sql.username "dark"
  |> Sql.password "eapnsdc"
  |> Sql.database "prodclone"
  // |> Sql.sslMode SslMode.Require
  |> Sql.config "Pooling=true;Maximum Pool Size=50;Include Error Detail=true"
  |> Sql.formatConnectionString
  |> Sql.connect


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
  connect ()
  |> Sql.query sql
  |> Sql.parameters parameters
  |> Sql.executeAsync reader
  |> throwOrReturn

let fetchOne (sql : string)
             (parameters : List<string * SqlValue>)
             (reader : RowReader -> 't)
             : Task<'t> =
  connect ()
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

  let convertToOption (result : Task<List<'a>>) : Task<Option<'a>> =
    task {
      match! result with
      | [ a ] -> return Some a
      | [] -> return None
      | list ->
          return failwith "Too many results, expected 0 or 1, got {list.Length}"
    }

  let query (sql : string) : Sql.SqlProps = connect () |> Sql.query sql

  let executeRowOptionAsync (reader : RowReader -> 't)
                            (props : Sql.SqlProps)
                            : Task<Option<'t>> =
    Sql.executeAsync reader props |> throwOrReturn |> convertToOption

  let executeRowAsync (reader : RowReader -> 't) (props : Sql.SqlProps) : Task<'t> =
    Sql.executeRowAsync reader props |> throwOrReturn

  let executeAsync (reader : RowReader -> 't) (props : Sql.SqlProps) : Task<List<'t>> =
    Sql.executeAsync reader props |> throwOrReturn

  let executeNonQueryAsync (props : Sql.SqlProps) : Task<int> =
    Sql.executeNonQueryAsync props |> throwOrReturn

  let tlidArray (tlids : List<int64>) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Array ||| NpgsqlTypes.NpgsqlDbType.Bigint
    let tlidsParam = NpgsqlParameter("tlids", typ)
    tlidsParam.Value <- List.toArray tlids
    Sql.parameter tlidsParam
