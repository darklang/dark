module LibDB.Db

// There's a lot of CLEANUP to do here.
// To be reviewed by someone with more DB expertise.

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open Prelude

let connString =
  $"Data Source={LibConfig.Config.dbPath};Mode=ReadWriteCreate;Cache=Private;Pooling=true"

module Sql =
  // Initialize connection with PRAGMA settings that can't be set in the connection string
  let initializeConnection (props : Sql.SqlProps) : Sql.SqlProps =
    props
    |> Sql.query
      @"
      PRAGMA journal_mode=WAL;
      PRAGMA synchronous=NORMAL;
      PRAGMA busy_timeout=5000;
      "
    |> Sql.executeNonQuery
    |> ignore<Result<int, exn>>

    props

  let connect = Sql.connect connString |> initializeConnection

  let query (sql : string) : Sql.SqlProps = connect |> Sql.query sql

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
    Sql.executeAsync rr props
    |> Async.StartAsTask
    |> Task.map (fun r ->
      match r with
      | Ok v -> v
      | Error err ->
        Exception.raiseInternal $"SQL query failed: {err}" [ "error", err ])

  let executeExistsSync (props : Sql.SqlProps) : bool =
    match Sql.execute (fun read -> read.bool 0) props with
    | Ok [ true ] -> true
    | Ok [] -> false
    | Ok result ->
      Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]
    | Error err ->
      Exception.raiseInternal
        $"Database query failed in executeExistsSync: {err}"
        [ "err", err ]

  let executeStatementAsync (props : Sql.SqlProps) : Task<unit> =
    task {
      match! Sql.executeNonQueryAsync props with
      | Error err ->
        Exception.raiseInternal
          $"Database statement failed in executeStatementAsync: {err}"
          [ "err", err ]
      | Ok _count -> return ()
    }

  let executeStatementSync (props : Sql.SqlProps) : unit =
    match Sql.executeNonQuery props with
    | Ok _count -> ()
    | Error err ->
      Exception.raiseInternal
        $"Database statement failed in executeStatementSync: {err}"
        [ "err", err ]

  /// Execute multiple SQL statements in a transaction synchronously
  let executeTransactionSync
    (statements :
      List<string * List<List<string * Microsoft.Data.Sqlite.SqliteParameter>>>)
    : List<int> =
    match connect |> Sql.executeTransaction statements with
    | Ok counts -> counts
    | Error err ->
      Exception.raiseInternal
        $"Database transaction failed in executeTransactionSync: {err}"
        [ "err", err ]

  let uuid (u : uuid) = u.ToString() |> Sql.string

  let uuidOrNone (u : Option<uuid>) =
    match u with
    | Some u -> uuid u
    | None -> Sql.dbnull

  let id (id : uint64) = Sql.int64 (int64 id)

  let tlid (tlid : uint64) = id tlid


  let instant (i : NodaTime.Instant) = Sql.dateTime (i.ToDateTimeUtc())

  let instantOrNone (i : Option<NodaTime.Instant>) =
    match i with
    | Some i -> instant i
    | None -> Sql.dbnull




// Extension methods
type RowReader with

  member this.uuid(id : string) : uuid = this.string id |> System.Guid.Parse

  member this.uuidOrNone(id : string) : Option<uuid> =
    this.stringOrNone id |> Option.map System.Guid.Parse

  member this.tlid(name : string) : tlid = this.int64 name |> uint64
  member this.id(name : string) : id = this.int64 name |> uint64


  member this.instant(name : string) : NodaTime.Instant =
    let dateTime : System.DateTime = this.dateTime (name)
    // SQLite returns DateTime with Unspecified kind, but we know it's UTC
    // TODO consider if this is what we actually want - this seems risky
    let utcDateTime =
      if dateTime.Kind = System.DateTimeKind.Utc then
        dateTime
      else
        System.DateTime.SpecifyKind(dateTime, System.DateTimeKind.Utc)
    NodaTime.Instant.FromDateTimeUtc utcDateTime

  member this.instantOrNone(name : string) : Option<NodaTime.Instant> =
    this.dateTimeOrNone (name)
    |> Option.map (fun dateTime ->
      // SQLite returns DateTime with Unspecified kind, but we know it's UTC
      // TODO consider if this is what we actually want - this seems risky
      let utcDateTime =
        if dateTime.Kind = System.DateTimeKind.Utc then
          dateTime
        else
          System.DateTime.SpecifyKind(dateTime, System.DateTimeKind.Utc)
      NodaTime.Instant.FromDateTimeUtc utcDateTime)



type TableStatsRow =
  { relation : string
    diskBytes : int64
    rows : int64
    diskHuman : string
    rowsHuman : string }

let tableStats () : Ply<List<TableStatsRow>> =
  uply {
    let! pageCount =
      Sql.query "PRAGMA page_count;"
      |> Sql.executeRowAsync (fun r -> r.int64 "page_count")

    let! pageSize =
      Sql.query "PRAGMA page_size;"
      |> Sql.executeRowAsync (fun r -> r.int64 "page_size")

    let dbSizeBytes = pageCount * pageSize

    let! tables =
      Sql.query
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%';"
      |> Sql.executeAsync (fun read -> read.string "name")

    let humanize (n : int64) =
      if n >= 1_000_000_000L then $"{n / 1_000_000_000L}G"
      elif n >= 1_000_000L then $"{n / 1_000_000L}M"
      elif n >= 1_000L then $"{n / 1_000L}k"
      else string n

    let! rowCounts =
      tables
      |> Ply.List.mapSequentially (fun table ->
        uply {
          let! rows =
            Sql.query $"SELECT COUNT(*) as count FROM \"{table}\";"
            |> Sql.executeRowAsync (fun read -> read.int64 "count")

          return (table, rows)
        })

    let totalRows = rowCounts |> List.sumBy snd |> max 1L // prevent divide-by-zero

    return
      rowCounts
      |> List.map (fun (table, rows) ->
        let diskBytes = dbSizeBytes * rows / totalRows
        { relation = table
          diskBytes = diskBytes
          rows = rows
          diskHuman = humanize diskBytes
          rowsHuman = humanize rows })
  }
