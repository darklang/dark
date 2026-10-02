module LibDB.Sqlite

// There's a lot of CLEANUP to do here.
// To be reviewed by someone with more DB expertise.

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open Prelude

let private connStringFor (path : string) : string =
  $"Data Source={path};Mode=ReadWriteCreate;Cache=Private;Pooling=true"

/// A connection to a one-shot FILE beside the store: a backup being written, a seed being cut, a
/// file being restored from.
///
/// Unpooled, which is the whole difference. A pooled connection outlives its `Close`, so a process
/// that touches the same path twice -- a server cutting a seed at one commit, then at another, into
/// a file it deleted in between -- is handed a handle to the file that is no longer there and fails
/// with "attempt to write a readonly database" or a disk I/O error. The live store wants pooling;
/// a file opened once does not.
let private fileConnStringFor (path : string) : string =
  $"Data Source={path};Mode=ReadWriteCreate;Cache=Private;Pooling=False"

let private defaultConnString = connStringFor LibConfig.Config.dbPath

/// The store this process is actually reading and writing, which is `LibConfig.Config.dbPath` except
/// under a test that repointed it.
///
/// Held beside `connString` rather than parsed back out of it, and it is what `Builtin.localDbPath`
/// answers. That matters because Dark reaches the store through `Stdlib.Sqlite`, which opens the path
/// this reports: if it answered the CONFIG path while `useStoreForTesting` moved only the F#
/// connection, a two-instance test would run every Dark-side read -- push, pull, conflict detection,
/// resolve -- against the default store while believing it was on instance B, and the whole Dark half
/// of sync would be unreachable from the harness.
let mutable currentDbPath = LibConfig.Config.dbPath

// `mutable` only so tests can repoint LibDB at a fresh store (see `Sql.useStoreForTesting`). Production
// never rebinds it. Both the Fumble `connect` AND the raw-ADO fold path (`applyOps` opens `new
// SqliteConnection connString`) read this, so a test swap redirects ALL of LibDB -- inserts, reads, and
// the fold -- at the instance store.
let mutable connString = defaultConnString

/// Copy the live store to `target`, and copy a file back over the live store.
///
/// Through SQLite's own online-backup API, never a file copy, for two reasons. The store path is
/// guarded (`HostSecurity.isPackageDbPath`), so Dark's file builtins refuse it -- rightly, since a
/// guest must not read or replace the host's store -- and the store's owner offering the operation
/// beats weakening that guard for everyone.
///
/// And a copy of `data.db` alone is not the store: recent writes sit in `data.db-wal`, and a
/// restore has to land while connections are open. The backup API handles both.
module Backup =
  let private copy (fromConn : string) (toConn : string) : Result<unit, string> =
    try
      use source = new SqliteConnection(fromConn)
      source.Open()
      use destination = new SqliteConnection(toConn)
      destination.Open()
      source.BackupDatabase destination
      Ok()
    with e ->
      Error e.Message

  /// Snapshot the live store into `target`, creating it.
  let toFile (target : string) : Result<unit, string> =
    copy connString (fileConnStringFor target)

  /// Replace the live store's contents with `source`'s.
  ///
  /// Contents, not the file: connections already open keep working and see the
  /// restored data, which swapping the file underneath them could not promise. What
  /// it cannot refresh is anything already read into memory, so the caller still
  /// says to restart.
  let fromFile (source : string) : Result<unit, string> =
    if not (System.IO.File.Exists source) then
      Error $"no file at {source}"
    else
      copy (fileConnStringFor source) connString


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
  // never rebinds it: it stays the default `connString` store for the process's life.
  let mutable connect = Sql.connect connString |> initializeConnection

  /// Force this module's initialization (first connection open + PRAGMA round trip)
  /// so the cost lands in its own span instead of inside whatever query runs first.
  let warm () : unit =
    connect
    |> Sql.query "SELECT 1"
    |> Sql.executeNonQuery
    |> ignore<Result<int, exn>>

  /// TEST-ONLY: repoint every LibDB reader and writer, and `Builtin.localDbPath` with them, at the
  /// store file at <param path> (created if missing). That is what lets a test run true multi-instance
  /// scenarios: each "instance" is its own store, and you switch the active one by calling this.
  /// `resetStoreForTesting` restores the default. NOT parallel-safe, since it mutates process-global
  /// state, so callers must be `testSequenced` and must restore the default when done.
  ///
  /// The caller must invalidate the caches after this (`LibDB.Caching.invalidateAll`), which cannot
  /// happen here because `Caching` compiles after this module. The package manager and the branch
  /// overlay memoize by content hash and branch id, and those are IDENTICAL across two copies of one
  /// store, so a read after the swap otherwise answers with the other instance's rows.
  let useStoreForTesting (path : string) : unit =
    connString <- connStringFor path
    currentDbPath <- path
    connect <- Sql.connect connString |> initializeConnection

  /// TEST-ONLY: restore the default store after `useStoreForTesting`.
  let resetStoreForTesting () : unit =
    connString <- defaultConnString
    currentDbPath <- LibConfig.Config.dbPath
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

  /// A store that can't be read or written is an ENVIRONMENT, not a bug: a read-only
  /// mount, another user's store, a full disk. SQLite says which; .NET buries it
  /// under an AggregateException. Every query path funnels through here first. Only
  /// these three codes are translated, because only these three are actionable by
  /// the person running the command; anything else keeps its exception, stack and
  /// all.
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
        Some
          "the package store could not be opened -- it may be missing, or owned by another user"
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

  /// Unwrap a query result: an Error that is a store condition raises as one, anything
  /// else raises internal. Per call site: how the message renders the error (each
  /// wrapper names itself, and some show `err.Message` where most show the whole exn).
  let private unwrapDb (msg : exn -> string) (r : Result<'a, exn>) : 'a =
    match r with
    | Ok v -> v
    | Error err ->
      raiseIfStoreCondition err
      Exception.raiseInternal (msg err) [ "err", err ]

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
      let! r =
        timedTask "row" (fun () ->
          Sql.executeAsync reader props |> Async.StartImmediateAsTask)
      match
        unwrapDb (fun err -> $"SQL query failed in executeRowAsync: {err.Message}") r
      with
      | [ a ] -> return a
      | [] -> return Exception.raiseInternal $"No results; expected 1" []
      | list ->
        return
          Exception.raiseInternal $"Too many results, expected 1" [ "actual", list ]
    }

  let executeRowOptionAsync
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Task<Option<'t>> =
    task {
      let! r =
        timedTask "rowOption" (fun () ->
          Sql.executeAsync reader props |> Async.StartImmediateAsTask)
      match
        unwrapDb
          (fun err -> $"SQL query failed in executeRowOptionAsync: {err.Message}")
          r
      with
      | [ a ] -> return Some a
      | [] -> return None
      | list ->
        return
          Exception.raiseInternal
            $"Too many results, expected 0 or 1"
            [ "actual", list ]
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
      |> unwrapDb (fun err -> $"Database query failed in executeExistsSync: {err}")
    with
    | [ true ] -> true
    | [] -> false
    | result ->
      Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]

  let executeStatementAsync (props : Sql.SqlProps) : Task<unit> =
    task {
      let! r =
        timedTask "statement" (fun () ->
          Sql.executeNonQueryAsync props |> Async.StartImmediateAsTask)
      r
      |> unwrapDb (fun err ->
        $"Database statement failed in executeStatementAsync: {err}")
      |> ignore<int>
    }

  let executeStatementSync (props : Sql.SqlProps) : unit =
    timedSync "statementSync" (fun () -> Sql.executeNonQuery props)
    |> unwrapDb (fun err ->
      $"Database statement failed in executeStatementSync: {err}")
    |> ignore<int>

  /// Execute multiple SQL statements in a transaction synchronously
  let executeTransactionSync
    (statements :
      List<string * List<List<string * Microsoft.Data.Sqlite.SqliteParameter>>>)
    : List<int> =
    connect
    |> Sql.executeTransaction statements
    |> unwrapDb (fun err ->
      $"Database transaction failed in executeTransactionSync: {err}")

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




// SQLite returns DateTime with Unspecified kind, but we know it's UTC
// TODO consider if this is what we actually want - this seems risky
let private toUtcInstant (dateTime : System.DateTime) : NodaTime.Instant =
  let utcDateTime =
    if dateTime.Kind = System.DateTimeKind.Utc then
      dateTime
    else
      System.DateTime.SpecifyKind(dateTime, System.DateTimeKind.Utc)
  NodaTime.Instant.FromDateTimeUtc utcDateTime

// Extension methods
type RowReader with

  member this.uuid(id : string) : uuid = this.string id |> System.Guid.Parse

  member this.uuidOrNone(id : string) : Option<uuid> =
    this.stringOrNone id |> Option.map System.Guid.Parse

  member this.tlid(name : string) : tlid = this.int64 name |> uint64
  member this.id(name : string) : id = this.int64 name |> uint64


  member this.instant(name : string) : NodaTime.Instant =
    toUtcInstant (this.dateTime (name))

  member this.instantOrNone(name : string) : Option<NodaTime.Instant> =
    this.dateTimeOrNone (name) |> Option.map toUtcInstant



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
