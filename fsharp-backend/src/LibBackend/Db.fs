module LibBackend.Db

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Npgsql
open Npgsql.FSharp.Tasks

// make sure the connection is returned to the pool
let connect () : Sql.SqlProps =
  Sql.host LibService.Config.pghost
  |> Sql.port 5432
  |> Sql.username LibService.Config.pguser
  |> Sql.password LibService.Config.pgpassword
  |> Sql.database LibService.Config.pgdbname

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
          return failwith $"Too many results, expected 0 or 1, got {list.Length}"
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

  let id (id : uint64) : SqlValue =
    // In the DB, it's actually an int64
    let typ = NpgsqlTypes.NpgsqlDbType.Bigint
    let idParam = NpgsqlParameter("id", typ)
    idParam.Value <- int64 id
    Sql.parameter idParam


  let idArray (ids : List<uint64>) : SqlValue =
    // In the DB, it's actually an int64
    let typ = NpgsqlTypes.NpgsqlDbType.Array ||| NpgsqlTypes.NpgsqlDbType.Bigint
    let idsParam = NpgsqlParameter("ids", typ)
    idsParam.Value <- ids |> List.map int64 |> List.toArray
    Sql.parameter idsParam
