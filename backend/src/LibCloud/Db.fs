module LibCloud.Db

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open Prelude

module Telemetry = LibService.Telemetry

let connString = "Data Source=./rundir/data.db"

module Sql =
  let query (sql : string) : Sql.SqlProps = Sql.connect connString |> Sql.query sql

  let executeNonQueryAsync props =
    Sql.executeNonQueryAsync props |> Async.StartAsTask |> Task.map Result.unwrap

  let executeRowAsync (reader : RowReader -> 't) (props : Sql.SqlProps) : Task<'t> =
    task {
      match! Sql.executeAsync reader props with
      | Ok [ a ] -> return a
      | Ok [] -> return Exception.raiseInternal $"No results; expected 1" []
      | Ok list ->
        return
          Exception.raiseInternal $"Too many results, expected 1" [ "actual", list ]
      | Error err -> return Exception.raiseInternal "fail" [ "err", err ]
    }


  let executeRowOptionAsync
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Task<Option<'t>> =
    task {
      match! Sql.executeAsync reader props with
      | Ok [ a ] -> return Some a
      | Ok [] -> return None
      | Ok list ->
        return
          Exception.raiseInternal
            $"Too many results, expected 0 or 1"
            [ "actual", list ]
      | Error err -> return Exception.raiseInternal "fail" [ "err", err ]
    }

  let executeAsync rr props =
    Sql.executeAsync rr props |> Async.StartAsTask |> Task.map Result.unwrap

  //   let executeRowOptionSync
  //     (reader : RowReader -> 't)
  //     (props : Sql.SqlProps)
  //     : Option<'t> =
  //     match Sql.execute reader props with
  //     | [ a ] -> Some a
  //     | [] -> None
  //     | list ->
  //       Exception.raiseInternal $"Too many results, expected 0 or 1" [ "actual", list ]

  //   // // TODO do a better job of naming these
  //   // // NOTE: This does not use SQL `EXISTS` but rather expects the query to return a
  //   // // list of 1/0. We should instead (TODO) make this use SQL `EXISTS` because it returns
  //   // // early and fetches less data
  //   // let executeExistsAsync (props : Sql.SqlProps) : Task<bool> =
  //   //   task {
  //   //     match! Sql.executeAsync (fun read -> read.NpgsqlReader.GetBoolean 0) props with
  //   //     | [ true ] -> return true
  //   //     | [] -> return false
  //   //     | result ->
  //   //       return
  //   //         Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]
  //   //   }

  let executeExistsSync (props : Sql.SqlProps) : bool =
    match Sql.execute (fun read -> read.bool 0) props with
    | Ok [ true ] -> true
    | Ok [] -> false
    | Ok result ->
      Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]
    | Error _ -> Exception.raiseInternal "TODO" []


  let executeStatementAsync (props : Sql.SqlProps) : Task<unit> =
    task {
      match! Sql.executeNonQueryAsync props with
      | Error _ -> return () // CLEANUP
      | Ok _count -> return ()
    }

  let executeStatementSync (props : Sql.SqlProps) : unit =
    let _count = Sql.executeNonQuery props
    // TODO: deal with Result^^
    ()

  let uuid (u : uuid) = u.ToString() |> Sql.string

  // let id (id : uint64) : SqlValue =
  //   // In the DB, it's actually an int64
  //   let typ = NpgsqlTypes.NpgsqlDbType.Bigint
  //   let idParam = NpgsqlParameter("id", typ)
  //   idParam.Value <- int64 id
  //   Sql.parameter idParam

  // TODO what does this do to overflowing IDs? are we OK?
  let id (id : uint64) = Sql.int64 (int64 id)

  let tlid (tlid : uint64) = id tlid

  // let idArray (ids : List<uint64>) : SqlValue =
  //   // In the DB, it's actually an int64
  //   let typ = NpgsqlTypes.NpgsqlDbType.Array ||| NpgsqlTypes.NpgsqlDbType.Bigint
  //   let idsParam = NpgsqlParameter("ids", typ)
  //   idsParam.Value <- ids |> List.map int64 |> List.toArray
  //   Sql.parameter idsParam

  //   let array (npgsqlType) (vs : 'a[]) : SqlValue =
  //     let typ = NpgsqlTypes.NpgsqlDbType.Array ||| npgsqlType
  //     let param = NpgsqlParameter("vals", typ)
  //     param.Value <- vs
  //     Sql.parameter param

  //   let traceID (traceID : LibExecution.AnalysisTypes.TraceID.T) : SqlValue =
  //     let typ = NpgsqlTypes.NpgsqlDbType.Uuid
  //     let idParam =
  //       NpgsqlParameter(
  //         "traceID",
  //         typ,
  //         Value = LibExecution.AnalysisTypes.TraceID.toUUID traceID
  //       )
  //     Sql.parameter idParam

  let instant (i : NodaTime.Instant) = Sql.dateTime (i.ToDateTimeUtc())

  let instantOrNone (i : Option<NodaTime.Instant>) =
    match i with
    | Some i -> Sql.dateTime (i.ToDateTimeUtc())
    | None -> Sql.dbnull




// Extension methods
type RowReader with

  member this.uuid(id : string) : uuid = this.string id |> System.Guid.Parse
  member this.tlid(name : string) : tlid = this.int64 name |> uint64
  member this.id(name : string) : id = this.int64 name |> uint64

  //   member this.idArray(name : string) : List<id> =
  //     let array = this.int64Array (name)
  //     array |> Array.toList |> List.map uint64

  //   member this.traceID(name : string) : LibExecution.AnalysisTypes.TraceID.T =
  //     this.uuid name |> LibExecution.AnalysisTypes.TraceID.fromUUID

  member this.instant(name : string) : NodaTime.Instant =
    let dateTime : System.DateTime = this.dateTime (name)
    NodaTime.Instant.FromDateTimeUtc dateTime

  member this.instantOrNone(name : string) : Option<NodaTime.Instant> =
    this.dateTimeOrNone (name) |> Option.map NodaTime.Instant.FromDateTimeUtc



type TableStatsRow =
  { relation : string
    diskBytes : int64
    rows : int64
    diskHuman : string
    rowsHuman : string }

// let tableStats () : Task<List<TableStatsRow>> =
//   // Sizes from the pg_class table are fast (vs, say, running `SELECT count` on a
//   // large table) but also are approximate, not precise. That's fine for purposes
//   // of "how big are my tables growing to be?"
//   //
//   // Three steps in the query in table_stats:
//   // 1) subquery "sizes" gets the data we want (size in bytes, number of rows)
//   // 2) subquery "with_total_row" appends a row to the resultset that SUM()s the contents of each
//   //    field
//   // 3) the final query provides both raw- and humanized- formatted columns
//   Sql.query
//     "WITH sizes AS (
//       SELECT
//         relname as \"relation\",
//         pg_total_relation_size (C .oid) as disk,
//         reltuples::bigint AS \"rows\"
//       FROM pg_class C
//       LEFT JOIN pg_namespace N ON (N.oid = C .relnamespace)
//       WHERE nspname NOT IN ('pg_catalog', 'information_schema')
//         AND C .relkind <> 'i'
//         AND nspname !~ '^pg_toast'
//       ORDER BY pg_total_relation_size (C .oid) DESC
//     ),

//     -- with_total_row is a subquery that appends a SUM() row to the bottom of our result set
//     with_total_row AS (
//       SELECT relation, disk, \"rows\" FROM sizes
//       UNION ALL
//       SELECT
//         'Total',
//         SUM(disk),
//         SUM(\"rows\")
//       FROM sizes
//     )

//     -- now we actually do our output, including both raw and humanized-number
//     -- columns for \"disk\" and \"rows\"
//     SELECT relation,
//       disk,
//       \"rows\",
//       pg_size_pretty(disk) as disk_human,
//       -- NOTE: below uses pg_size_pretty to get us something human readable
//       -- ('100M' is easier than '100000000', but it's _row count_, not bytes,
//       -- hence trimming those parts off.)
//       --
//       -- Examples for trim(from substring(...)):
//       -- 100 MB -> 100M
//       -- 100 kb -> 100k
//       -- 1 bytes -> 1
//       trim(from
//           substring(
//               pg_size_pretty ( \"rows\"::bigint)
//               from '[0-9]* [^b]?')
//       ) as rows_human
//     FROM with_total_row"
//   |> Sql.executeAsync (fun read ->
//     { relation = read.string "relation"
//       diskBytes = read.int64OrNone "disk" |> Option.unwrap 0
//       rows = read.int64OrNone "rows" |> Option.unwrap 0
//       diskHuman = read.string "disk_human"
//       rowsHuman = read.string "rows_human" })
