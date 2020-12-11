module LibBackend.Db

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Npgsql
open Npgsql.FSharp.Tasks

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

module Sql =
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
    Sql.executeAsync reader props |> convertToOption

  let executeStatementAsync (props : Sql.SqlProps) : Task<unit> =
    task {
      let! (_count : int) = Sql.executeNonQueryAsync props
      return ()
    }


  let tlidArray (tlids : List<int64>) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Array ||| NpgsqlTypes.NpgsqlDbType.Bigint
    let tlidsParam = NpgsqlParameter("tlids", typ)
    tlidsParam.Value <- List.toArray tlids
    Sql.parameter tlidsParam
