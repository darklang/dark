module LibBackend.Db

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql
open Npgsql.FSharp

open Prelude

module RT = LibExecution.RuntimeTypes

module Sql =

  // Open a database [transaction] and run [f],in it
  let withTransaction (f : unit -> Task<'a>) : Task<'a> =
    task {
      let connection = LibService.DBConnection.connect () |> Sql.createConnection
      connection.Open()

      let! transaction = connection.BeginTransactionAsync()
      let! result = f ()
      do! transaction.CommitAsync()
      return result
    }

  let query (sql : string) : Sql.SqlProps =
    LibService.DBConnection.connect () |> Sql.query sql

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
    match Sql.execute (fun read -> read.NpgsqlReader.GetBoolean 0) props with
    | [ true ] -> true
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

  let tlid (tlid : uint64) : SqlValue =
    // In the DB, it's actually an int64
    let typ = NpgsqlTypes.NpgsqlDbType.Bigint
    let idParam = NpgsqlParameter("tlid", typ)
    idParam.Value <- int64 tlid
    Sql.parameter idParam

  let idArray (ids : List<uint64>) : SqlValue =
    // In the DB, it's actually an int64
    let typ = NpgsqlTypes.NpgsqlDbType.Array ||| NpgsqlTypes.NpgsqlDbType.Bigint
    let idsParam = NpgsqlParameter("ids", typ)
    idsParam.Value <- ids |> List.map int64 |> List.toArray
    Sql.parameter idsParam

  let queryableDvalMap (dvalmap : RT.DvalMap) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Jsonb
    let param = NpgsqlParameter("dvalmap", typ)
    param.Value <- LibExecution.DvalRepr.toInternalQueryableV1 dvalmap
    Sql.parameter param

  let roundtrippableDval (dval : RT.Dval) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Jsonb
    let param = NpgsqlParameter("dval", typ)
    param.Value <- LibExecution.DvalRepr.toInternalRoundtrippableV0 dval
    Sql.parameter param

  let roundtrippableDvalMap (dvalmap : RT.DvalMap) : SqlValue =
    let typ = NpgsqlTypes.NpgsqlDbType.Jsonb
    let param = NpgsqlParameter("dvalmap", typ)
    param.Value <- LibExecution.DvalRepr.toInternalRoundtrippableV0 (RT.DObj dvalmap)
    Sql.parameter param

// Extension methods
type RowReader with

  member this.tlid(name : string) : tlid = this.int64 name |> uint64
  member this.id(name : string) : id = this.int64 name |> uint64

// member this.queryableDval(name : string) =
//   this.string name |> LibExecution.DvalRepr.ofInternalQueryableV1
// member this.roundtrippableDval(name : string) =
//   this.string name |> LibExecution.DvalRepr.ofInternalRoundtrippableV0
