module LibDB.Sqlite

// There's a lot of CLEANUP to do here.
// To be reviewed by someone with more DB expertise.

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open Prelude

let private defaultConnString =
  $"Data Source={LibConfig.Config.dbPath};Mode=ReadWriteCreate;Cache=Private;Pooling=true"

// `mutable` only so tests can repoint LibDB at a fresh store (see `Sql.useStoreForTesting`). Production never
// rebinds it. Both the Fumble `connect` AND the raw-ADO fold path (`applyOps` opens `new SqliteConnection
// connString`) read this, so a test swap redirects ALL of LibDB — inserts, reads, and the fold — at the
// instance store.
let mutable connString = defaultConnString

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

  // `mutable` only so tests can repoint LibDB at a fresh store (see `useStoreForTesting`). Production
  // never rebinds it — it stays the default `connString` store for the process's life.
  let mutable connect = Sql.connect connString |> initializeConnection

  /// Force this module's initialization, and with it the first connection open and the PRAGMA round trip.
  ///
  /// Exists so the cost is attributable. It happens on whatever query runs first, which made it look like
  /// part of `growIfNeeded`'s op check -- a check that is index-covered and takes 0.0 ms against this
  /// store. Calling this first moves the cost into a span of its own rather than removing it.
  let warm () : unit =
    connect
    |> Sql.query "SELECT 1"
    |> Sql.executeNonQuery
    |> ignore<Result<int, exn>>

  /// TEST-ONLY: repoint LibDB at the store file at `path` (created if missing), so a test can run true
  /// multi-instance scenarios — each "instance" is its own store, and you switch the active one by
  /// calling this. Every subsequent LibDB operation hits `path`. Call `resetStoreForTesting` to restore
  /// the default. NOT parallel-safe (it mutates process-global state): callers must be `testSequenced`
  /// and restore the default when done.
  let useStoreForTesting (path : string) : unit =
    connString <-
      $"Data Source={path};Mode=ReadWriteCreate;Cache=Private;Pooling=true"
    connect <- Sql.connect connString |> initializeConnection

  /// TEST-ONLY: restore the default store after `useStoreForTesting`.
  let resetStoreForTesting () : unit =
    connString <- defaultConnString
    connect <- Sql.connect connString |> initializeConnection

  /// Count and time every SQL statement this process runs.
  ///
  /// Instrumenting only package-item loads leaves everything Dark code issues through `Stdlib.Sqlite`,
  /// `pmSearch` and the SCM builtins invisible, which is most of it, and makes SQL time easy to mistake
  /// for interpreter cost.
  ///
  /// Gated, and per-statement rather than per-row: a timestamp costs ~1.27 us on an HPET clocksource.
  let inline private timedTask (name : string) (f : unit -> Task<'a>) : Task<'a> =
    if not (Telemetry.isEnabled ()) then
      f ()
    else
      task {
        Telemetry.count $"sql.{name}"
        let t0 = System.Diagnostics.Stopwatch.GetTimestamp()
        let! r = f ()
        let t1 = System.Diagnostics.Stopwatch.GetTimestamp()
        Telemetry.addUs
          "sql.total"
          ((t1 - t0) * 1_000_000L / System.Diagnostics.Stopwatch.Frequency)
        return r
      }

  let inline private timedSync (name : string) (f : unit -> 'a) : 'a =
    if not (Telemetry.isEnabled ()) then
      f ()
    else
      Telemetry.count $"sql.{name}"
      let t0 = System.Diagnostics.Stopwatch.GetTimestamp()
      let r = f ()
      let t1 = System.Diagnostics.Stopwatch.GetTimestamp()
      Telemetry.addUs
        "sql.total"
        ((t1 - t0) * 1_000_000L / System.Diagnostics.Stopwatch.Frequency)
      r

  let query (sql : string) : Sql.SqlProps = connect |> Sql.query sql

  /// A store that can't be read or written is an ENVIRONMENT, not a bug: a read-only mount, a store owned
  /// by another user, a disk with nothing left on it. SQLite says exactly which, then .NET buries it under
  /// an AggregateException and the callers below stringify it into a message, so the cause reached the
  /// user as a stack trace that named neither the store nor the problem. Worse, writes went through
  /// `Result.unwrap`, which prints the raw exception to stdout and raises "TODO: failed to unwrap".
  ///
  /// So: every query path funnels through here first. Only these three codes are translated, because only
  /// these three are things the person running the command can act on. Anything else keeps its own
  /// exception, stack and all, and is a bug worth seeing in full.
  let private storeCondition (e : exn) : string option =
    // The SqliteException arrives wrapped -- an AggregateException from the async boundary, sometimes an
    // InnerException under that -- so this looks through the chain rather than testing the top of it.
    let rec sqliteCause (ex : exn) : SqliteException option =
      match ex with
      | :? SqliteException as s -> Some s
      | :? System.AggregateException as agg ->
        agg.InnerExceptions |> Seq.tryPick sqliteCause
      | _ -> if isNull ex.InnerException then None else sqliteCause ex.InnerException

    match sqliteCause e with
    // 8 = SQLITE_READONLY, 13 = SQLITE_FULL, 14 = SQLITE_CANTOPEN.
    | Some s ->
      match s.SqliteErrorCode with
      | 8 -> Some "the package store is read-only, so nothing can be written to it"
      | 13 -> Some "the disk holding the package store is full"
      | 14 ->
        Some "the package store could not be opened -- it may be missing, or owned by another user"
      | _ -> None
    | None -> None

  /// Raises the store condition if this is one, and does nothing at all if it isn't -- so callers keep
  /// their own error handling for everything else.
  let internal raiseIfStoreCondition (e : exn) : unit =
    match storeCondition e with
    | Some what ->
      Exception.raiseStoreCondition
        $"Can't use the package store: {what}. Nothing was lost -- fix it and run the same command again."
        [ "dbPath", LibConfig.Config.dbPath ]
    | None -> ()

  let executeNonQueryAsync props =
    timedTask "nonQuery" (fun () ->
      Sql.executeNonQueryAsync props
      |> Async.StartImmediateAsTask
      |> Task.map (function
        | Ok n -> n
        | Error(e : exn) ->
          raiseIfStoreCondition e
          raise e))

  let executeRowAsync (reader : RowReader -> 't) (props : Sql.SqlProps) : Task<'t> =
    task {
      match!
        timedTask "row" (fun () ->
          Sql.executeAsync reader props |> Async.StartImmediateAsTask)
      with
      | Ok [ a ] -> return a
      | Ok [] -> return Exception.raiseInternal $"No results; expected 1" []
      | Ok list ->
        return
          Exception.raiseInternal $"Too many results, expected 1" [ "actual", list ]
      | Error err ->
        raiseIfStoreCondition err
        return
          Exception.raiseInternal
            $"SQL query failed in executeRowAsync: {err.Message}"
            [ "err", err ]
    }

  let executeRowOptionAsync
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Task<Option<'t>> =
    task {
      match!
        timedTask "rowOption" (fun () ->
          Sql.executeAsync reader props |> Async.StartImmediateAsTask)
      with
      | Ok [ a ] -> return Some a
      | Ok [] -> return None
      | Ok list ->
        return
          Exception.raiseInternal
            $"Too many results, expected 0 or 1"
            [ "actual", list ]
      | Error err ->
        raiseIfStoreCondition err
        return
          Exception.raiseInternal
            $"SQL query failed in executeRowOptionAsync: {err.Message}"
            [ "err", err ]
    }

  let executeAsync rr props =
    Sql.executeAsync rr props
    |> Async.StartImmediateAsTask
    |> Task.map (fun r ->
      match r with
      | Ok v -> v
      | Error err ->
        raiseIfStoreCondition err
        Exception.raiseInternal $"SQL query failed: {err}" [ "error", err ])

  let executeExistsSync (props : Sql.SqlProps) : bool =
    match
      timedSync "existsSync" (fun () -> Sql.execute (fun read -> read.bool 0) props)
    with
    | Ok [ true ] -> true
    | Ok [] -> false
    | Ok result ->
      Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]
    | Error err ->
      raiseIfStoreCondition err
      Exception.raiseInternal
        $"Database query failed in executeExistsSync: {err}"
        [ "err", err ]

  let executeStatementAsync (props : Sql.SqlProps) : Task<unit> =
    task {
      match!
        timedTask "statement" (fun () ->
          Sql.executeNonQueryAsync props |> Async.StartImmediateAsTask)
      with
      | Error err ->
        raiseIfStoreCondition err
        Exception.raiseInternal
          $"Database statement failed in executeStatementAsync: {err}"
          [ "err", err ]
      | Ok _count -> return ()
    }

  let executeStatementSync (props : Sql.SqlProps) : unit =
    match timedSync "statementSync" (fun () -> Sql.executeNonQuery props) with
    | Ok _count -> ()
    | Error err ->
      raiseIfStoreCondition err
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
      raiseIfStoreCondition err
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
