module LibBackend.Db

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Npgsql
open Npgsql.FSharp.Tasks

let connectionString =
  Sql.host LibService.Config.pghost
  |> Sql.port 5432
  |> Sql.username LibService.Config.pguser
  |> Sql.password LibService.Config.pgpassword
  |> Sql.database LibService.Config.pgdbname

  // |> Sql.sslMode SslMode.Require
  // FSTODO pool size
  |> Sql.config "Pooling=true;Maximum Pool Size=50;Include Error Detail=true"
  |> Sql.formatConnectionString

let connect () : Sql.SqlProps = Sql.connect connectionString

module Sql =

  let query (sql : string) : Sql.SqlProps = connect () |> Sql.query sql

  let executeRowOptionAsync
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Task<Option<'t>> =
    task {
      match! Sql.executeAsync reader props with
      | [ a ] -> return Some a
      | [] -> return None
      | list -> return failwith $"Too many results, expected 0 or 1, got {list}"
    }

  let executeRowOption
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Option<'t> =
    match Sql.execute reader props with
    | [ a ] -> Some a
    | [] -> None
    | list -> failwith $"Too many results, expected 0 or 1, got {list}"

  // FSTODO do a better job of naming these
  let executeExistsAsync (props : Sql.SqlProps) : Task<bool> =
    task {
      match! Sql.executeAsync (fun read -> read.NpgsqlReader.GetBoolean 0) props with
      | [ true ] -> return true
      | [] -> return false
      | result -> return failwith $"Too many results, expected 1, got {result}"
    }

  let executeExists (props : Sql.SqlProps) : bool =
    match Sql.execute (fun read -> read.NpgsqlReader.GetInt32 0) props with
    | [ 1 ] -> true
    | [] -> false
    | result -> failwith $"Too many results, expected 1, got {result}"


  let executeStatementAsync (props : Sql.SqlProps) : Task<unit> =
    task {
      let! (_count : int) = Sql.executeNonQueryAsync props
      return ()
    }

  let executeStatement (props : Sql.SqlProps) : unit =
    let _count = Sql.executeNonQuery props
    ()



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

// Extension methods
type RowReader with

  member this.tlid(name : string) = this.int64 name |> uint64
