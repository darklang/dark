module LibDB.Sqlite

open System.Threading.Tasks
open Microsoft.Data.Sqlite
open Fumble

open Prelude

let private connStringFor (path : string) : string =
  $"Data Source={path};Mode=ReadWriteCreate;Cache=Private;Pooling=true"

let private defaultConnString = connStringFor LibConfig.Config.dbPath

/// A test store made by `scripts/testing/layer-store` (see "Test stores are layers" in
/// docs/internals.md): a hard link to a file other stores share, with its own changes kept in its
/// WAL.
let isLayer (path : string) : bool = System.IO.File.Exists(path + "-layer")

/// The file each layer this process opened links to, and that layer.
let private layersOpen =
  System.Collections.Concurrent.ConcurrentDictionary<string, string>()

/// No checkpoint from <param conn>, automatic or on close: what a layer's connection needs.
let neverCheckpoint (conn : SqliteConnection) : unit =
  SQLitePCL.raw.sqlite3_wal_autocheckpoint (conn.Handle, 0) |> ignore<int>
  // SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE
  SQLitePCL.raw.sqlite3_db_config (conn.Handle, 1006, 1) |> ignore<int * int>

/// Every connection LibDB opens, opened. Open a store any other way and a layer can break.
///
/// A layer's file is shared by every other layer over it, so a checkpoint, which folds the WAL
/// into the file, would hand this store's writes to all of them. So a layer's connection never
/// checkpoints: not automatically (SQLite resets that at every open), and not on close. And a
/// process opens one layer over a file at most: SQLite shares a file's WAL index by inode, so a
/// second link to the same file would read the first's WAL. Its `-layer` marker names the file.
let openConnection (connectionString : string) : SqliteConnection =
  let conn = new SqliteConnection(connectionString)
  conn.Open()
  let path = System.IO.Path.GetFullPath conn.DataSource
  if isLayer path then
    let named = System.IO.File.ReadAllText(path + "-layer")
    let file = if named = "" then path else System.IO.Path.GetFullPath named
    let opened = layersOpen.GetOrAdd(file, path)
    if opened <> path then
      conn.Dispose()
      Exception.raiseInternal
        "A process opened two layer stores over one file, and SQLite would mix their writes (docs/internals.md, \"Test stores are layers\")"
        [ "path", path; "alreadyOpen", opened; "file", file ]
    neverCheckpoint conn
  conn

/// The store this process is actually reading and writing, which is `LibConfig.Config.dbPath` except
/// under a test that repointed it.
///
/// Held beside `connString` rather than parsed back out of it, and it is what `Builtin.localDbPath`
/// answers, so a test that repoints the store with `useStoreForTesting` sees the same path from Dark
/// as the F# connection uses.
let mutable currentDbPath = LibConfig.Config.dbPath

// `mutable` only so tests can repoint LibDB at a fresh store (see `Sql.useStoreForTesting`). Production
// never rebinds it. Both the Fumble `connect` AND the raw-ADO fold path (`applyOps` opens `new
// SqliteConnection connString`) read this, so a test swap redirects ALL of LibDB -- inserts, reads, and
// the fold -- at the instance store.
let mutable connString = defaultConnString

/// this process's writers to a store queue here, not in SQLite. SQLite's waiters poll (busy handler
/// up to 100 ms apart, Microsoft.Data.Sqlite's retry 150 ms) and a newcomer tries at once, so under
/// the parallel tests' stream of short writes a waiter could lose every round for 30 s and fail
/// with "database is locked". reentrant, so a write inside a turn goes ahead
module WriteGate =
  let private gates =
    System.Collections.Concurrent.ConcurrentDictionary<string, obj>()

  let hold (f : unit -> 'a) : 'a =
    lock (gates.GetOrAdd(connString, (fun _ -> obj ()))) f

/// The test runner's copy of the live store. Every other whole-store copy (backups, snapshots, the
/// upgrade path's) is `StoreBackups`.
module Backup =
  /// Create a database file for tests from the live store: a test run's shared snapshot, or a
  /// nested test worker's own store. It holds every table's rows but the traces', read in one
  /// transaction, so it is about the size of the code however large the store has grown: the
  /// traces and the free pages they leave stay behind. The live store is only read.
  let toTestBaseline (target : string) : Result<unit, string> =
    try
      use connection =
        new SqliteConnection(
          $"Data Source={target};Mode=ReadWriteCreate;Pooling=false"
        )
      connection.Open()
      // the live store, attached below, may be a layer
      neverCheckpoint connection
      let command (sql : string) =
        let command = connection.CreateCommand()
        command.CommandText <- sql
        command
      let run (sql : string) =
        use command = command sql
        command.ExecuteNonQuery() |> ignore<int>
      run "PRAGMA journal_mode=OFF; PRAGMA synchronous=OFF"
      do
        use attach = command "ATTACH @path AS live"
        attach.Parameters.AddWithValue("@path", currentDbPath)
        |> ignore<SqliteParameter>
        attach.ExecuteNonQuery() |> ignore<int>
      run "BEGIN"
      let tables, indexes =
        use read =
          command
            "SELECT type, name, sql FROM live.sqlite_master
             WHERE sql IS NOT NULL AND name NOT LIKE 'sqlite_%'"
        use reader = read.ExecuteReader()
        [ while reader.Read() do
            (reader.GetString 0, reader.GetString 1, reader.GetString 2) ]
        |> List.partition (fun (kind, _, _) -> kind = "table")
      for _, _, sql in tables do
        run sql
      for _, name, _ in tables do
        if not (name.StartsWith("trace", System.StringComparison.Ordinal)) then
          run $"INSERT INTO main.\"{name}\" SELECT * FROM live.\"{name}\""
      run
        "DELETE FROM main.sqlite_sequence;
         INSERT INTO main.sqlite_sequence SELECT * FROM live.sqlite_sequence"
      // after the rows: one build of an index is faster than updating it on every insert
      for _, _, sql in indexes do
        run sql
      run "COMMIT"
      Ok()
    with e ->
      Error e.Message


/// "Did the store change since I last asked?", for a process that stays up.
///
/// `PRAGMA data_version` is a per-connection counter: it moves when a commit lands through any OTHER
/// connection, in this process or in another one. Fumble opens a pooled connection per query, and which
/// pooled connection a query lands on is not ours to pick, so two reads of the pragma through it would
/// not be comparable. This module holds one connection of its own for the life of the process and asks it
/// nothing but this. A store at another path gets a connection of its own.
///
/// The number means nothing on its own: only "same as last time" or "moved" is information, and only
/// within one process. A host loop keeps the last value and polls (`LanguageTools.Live.poll`).
module DataVersion =
  let private gate = obj ()

  // One per store, kept for the life of the process. A test that swaps stores and swaps back
  // must get the SAME connection back, or the counter restarts and a change made in between reads
  // as "no change".
  let private held =
    System.Collections.Generic.Dictionary<string, SqliteConnection>()

  let private connection () : SqliteConnection =
    match held.TryGetValue connString with
    | true, conn -> conn
    | false, _ ->
      let conn = openConnection connString
      held[connString] <- conn
      conn

  // The command is held with the connection, not built per call. This runs on a timer for the
  // life of every live view, and building one cost more than the pragma did: 13.4 microseconds
  // a call against 7.6 reusing this, measured over 100,000 calls on the real store. The rest is
  // SQLite taking a read transaction and looking at the WAL index, which is the part that
  // actually answers the question.
  let private commands =
    System.Collections.Generic.Dictionary<string, SqliteCommand>()

  let private command () : SqliteCommand =
    match commands.TryGetValue connString with
    | true, cmd -> cmd
    | false, _ ->
      let cmd = (connection ()).CreateCommand()
      cmd.CommandText <- "PRAGMA data_version"
      cmd.Prepare()
      commands[connString] <- cmd
      cmd

  let current () : int64 =
    Telemetry.count "sql.dataVersion"
    lock gate (fun () -> (command ()).ExecuteScalar() |> unbox<int64>)


/// A store file this process may not write.
let isReadOnly (path : string) : bool =
  not (System.OperatingSystem.IsWindows())
  && System.IO.File.Exists path
  && not (
    (System.IO.File.GetUnixFileMode path).HasFlag System.IO.UnixFileMode.UserWrite
  )

module Sql =
  // Initialize connection with PRAGMA settings that can't be set in the connection string.
  // A read-only store keeps its journal mode: switching it to WAL is a write.
  let initializeConnection (path : string) (props : Sql.SqlProps) : Sql.SqlProps =
    let readOnly = isReadOnly path
    Telemetry.count "sql.init"
    props
    |> Sql.query (
      (if readOnly then "" else "PRAGMA journal_mode=WAL;")
      + "PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000;"
    )
    |> Sql.executeNonQuery
    |> ignore<Result<int, exn>>

    props

  /// A layer's queries all go through one connection that `openConnection` opened, since Fumble
  /// would open its own. One query at a time on it (`timedSync`).
  let private connectTo (path : string) : Sql.SqlProps =
    if isLayer path then
      Sql.existingConnection (openConnection (connStringFor path))
    else
      Sql.connect (connStringFor path) |> initializeConnection path

  let private layered = lazy (isLayer LibConfig.Config.dbPath)
  let private layerGate = obj ()

  // `mutable` only so tests can repoint LibDB at a fresh store (see `useStoreForTesting`). Production
  // never rebinds it: it stays the default `connString` store for the process's life.
  let mutable connect = connectTo currentDbPath

  /// A connection to the store that is opened once and never closed.
  ///
  /// When the last connection to a WAL store closes, SQLite checkpoints it -- syncs the WAL, copies
  /// it into the database, syncs that -- and deletes the WAL, which the next write then recreates and
  /// syncs along with the directory. In a CLI that is every exit: up to four fsyncs, most of a short
  /// command's wall time on a busy disk. While this one is open no other close is the last. The WAL
  /// is still folded in as it grows, by `wal_autocheckpoint` inside a write, and exiting with it
  /// open is what a crash looks like to SQLite: the next open reads the WAL.
  let private heldOpen : Lazy<SqliteConnection> =
    lazy
      (let conn =
        openConnection (
          SqliteConnectionStringBuilder(connString, Pooling = false).ToString()
        )
       // in WAL mode the shared lock is taken by the first read and kept until close
       use cmd = conn.CreateCommand()
       cmd.CommandText <- "PRAGMA user_version"
       cmd.ExecuteScalar() |> ignore<obj>
       conn)

  /// Force this module's initialization (first connection open + PRAGMA round trip)
  /// so the cost lands in its own span instead of inside whatever query runs first.
  let warm () : unit =
    Telemetry.count "sql.init"
    connect
    |> Sql.query "SELECT 1"
    |> Sql.executeNonQuery
    |> ignore<Result<int, exn>>
    heldOpen.Force() |> ignore<SqliteConnection>

  /// TEST-ONLY: repoint every LibDB reader and writer, and `Builtin.localDbPath` with them, at the
  /// store file at <param path> (created if missing). That is what lets a test run true multi-instance
  /// scenarios: each "instance" is its own store, and you switch the active one by calling this.
  /// `resetStoreForTesting` restores the default. NOT parallel-safe, since it mutates process-global
  /// state, so callers must be `testSequenced` and must restore the default when done.
  ///
  /// The caller must invalidate the caches after this (`LibDB.Caching.invalidateAll`), which cannot
  /// happen here because `Caching` compiles after this module. The package managers, one per
  /// branch, memoize by content hash and branch id, and those are IDENTICAL across two copies of one
  /// store, so a read after the swap otherwise answers with the other instance's rows.
  let useStoreForTesting (path : string) : unit =
    connString <- connStringFor path
    currentDbPath <- path
    connect <- connectTo path

  /// TEST-ONLY: restore the default store after `useStoreForTesting`.
  let resetStoreForTesting () : unit =
    connString <- defaultConnString
    currentDbPath <- LibConfig.Config.dbPath
    connect <- connectTo currentDbPath

  /// Count and time every SQL statement this process runs. Each path counts under a `sql.` name
  /// of its own; together they are every statement (`scripts/perf/_ui.py` sums them).
  ///
  /// Instrumenting only package-item loads leaves everything issued through `pmSearch` and the SCM
  /// builtins invisible, which is most of it, and makes SQL time easy to mistake for interpreter cost.
  ///
  /// Gated, and per-statement rather than per-row: a timestamp costs ~1.27 us on an HPET clocksource.
  let inline private timedSync (name : string) (g : unit -> 'a) : 'a =
    let f () = if layered.Value then lock layerGate g else g ()
    if not (Telemetry.isEnabled ()) then
      f ()
    else
      Telemetry.count name
      let t0 = System.Diagnostics.Stopwatch.GetTimestamp()
      let r = f ()
      let t1 = System.Diagnostics.Stopwatch.GetTimestamp()
      Telemetry.addUs
        "sql.total"
        ((t1 - t0) * 1_000_000L / System.Diagnostics.Stopwatch.Frequency)
      r

  let query (sql : string) : Sql.SqlProps = connect |> Sql.query sql

  /// Fumble's `Sql.execute`, counted. Same name, so a caller that opens this module after Fumble
  /// gets this one.
  let execute reader props =
    timedSync "sql.rowsSync" (fun () -> Sql.execute reader props)

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

  /// A statement's count of rows changed.
  let executeCount props : int =
    timedSync "sql.nonQuery" (fun () ->
      WriteGate.hold (fun () -> Sql.executeNonQuery props))
    |> unwrapDb (fun err -> $"Database statement failed: {err}")

  let executeRow (reader : RowReader -> 't) (props : Sql.SqlProps) : 't =
    let r = timedSync "sql.row" (fun () -> Sql.execute reader props)
    match
      unwrapDb (fun err -> $"SQL query failed in executeRow: {err.Message}") r
    with
    | [ a ] -> a
    | [] -> Exception.raiseInternal $"No results; expected 1" []
    | list ->
      Exception.raiseInternal $"Too many results, expected 1" [ "actual", list ]

  let executeRowOption
    (reader : RowReader -> 't)
    (props : Sql.SqlProps)
    : Option<'t> =
    let r = timedSync "sql.rowOption" (fun () -> Sql.execute reader props)
    match
      unwrapDb (fun err -> $"SQL query failed in executeRowOption: {err.Message}") r
    with
    | [ a ] -> Some a
    | [] -> None
    | list ->
      Exception.raiseInternal $"Too many results, expected 0 or 1" [ "actual", list ]

  let executeRows rr props =
    timedSync "sql.rows" (fun () -> Sql.execute rr props)
    |> unwrapDb (fun err -> $"SQL query failed: {err}")

  let executeExists (props : Sql.SqlProps) : bool =
    match
      timedSync "sql.exists" (fun () -> Sql.execute (fun read -> read.bool 0) props)
      |> unwrapDb (fun err -> $"Database query failed in executeExists: {err}")
    with
    | [ true ] -> true
    | [] -> false
    | result ->
      Exception.raiseInternal "Too many results, expected 1" [ "actual", result ]

  let executeStatement (props : Sql.SqlProps) : unit =
    timedSync "sql.statement" (fun () ->
      WriteGate.hold (fun () -> Sql.executeNonQuery props))
    |> unwrapDb (fun err -> $"Database statement failed in executeStatement: {err}")
    |> ignore<int>

  /// Execute multiple SQL statements in one write transaction synchronously: each statement
  /// once per parameter set (once with none), returning each statement's rows changed.
  ///
  /// On a connection of its own with `busy_timeout` set, rather than through Fumble. A pooled
  /// connection has no `busy_timeout` unless something set it, and then a write that meets
  /// another process's lock falls to Microsoft.Data.Sqlite's own retry, which sleeps 150 ms at a
  /// time: under a handful of writers, short writes waited in ~165 ms steps. SQLite's handler
  /// polls in milliseconds. `BEGIN IMMEDIATE` takes the write lock up front, where waiting works.
  let executeTransactionSync
    (statements :
      List<string * List<List<string * Microsoft.Data.Sqlite.SqliteParameter>>>)
    : List<int> =
    Telemetry.countBy
      "sql.transaction"
      (statements |> List.sumBy (fun (_, ps) -> max 1 ps.Length) |> int64)
    try
      timedSync "sql.transactionSync" (fun () ->
        WriteGate.hold (fun () ->
          use conn = openConnection connString
          use pragma = conn.CreateCommand()
          pragma.CommandText <- "PRAGMA busy_timeout=5000;"
          pragma.ExecuteNonQuery() |> ignore<int>
          use tx = conn.BeginTransaction(deferred = false)
          let run (sql : string) (ps : List<string * SqliteParameter>) : int =
            use cmd = conn.CreateCommand()
            cmd.Transaction <- tx
            cmd.CommandText <- sql
            for (name, p) in ps do
              let copy = SqliteParameter(name, p.Value)
              copy.SqliteType <- p.SqliteType
              cmd.Parameters.Add copy |> ignore<SqliteParameter>
            cmd.ExecuteNonQuery()
          let counts =
            statements
            |> List.map (fun (sql, paramSets) ->
              match paramSets with
              | [] -> run sql []
              | sets -> sets |> List.sumBy (run sql))
          tx.Commit()
          counts))
    with e ->
      raiseIfStoreCondition e
      Exception.raiseInternal
        $"Database transaction failed in executeTransactionSync: {e.Message}"
        [ "err", e ]

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




// SQLite returns DateTime with Unspecified kind. We only ever store UTC
// (`datetime('now')` and `DateTime.UtcNow`), so mark it as UTC.
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


/// JSON for `json_each` to bind a list as one parameter. Written with the writer, never the
/// reflecting serializer, which the published (AOT) binary does not have.
let private writeJson (f : System.Text.Json.Utf8JsonWriter -> unit) : string =
  use buffer = new System.IO.MemoryStream()
  (use w = new System.Text.Json.Utf8JsonWriter(buffer)
   f w)
  System.Text.Encoding.UTF8.GetString(buffer.ToArray())

let private writeStrings
  (w : System.Text.Json.Utf8JsonWriter)
  (items : List<string>)
  =
  w.WriteStartArray()
  for s in items do
    w.WriteStringValue s
  w.WriteEndArray()

/// A JSON array of strings.
let jsonStrings (items : List<string>) : string =
  writeJson (fun w -> writeStrings w items)

/// A JSON array of string arrays: one per row, read back with `json_extract(value, '$[i]')`.
let jsonRows (rows : List<List<string>>) : string =
  writeJson (fun w ->
    w.WriteStartArray()
    for row in rows do
      writeStrings w row
    w.WriteEndArray())

/// Every remaining row of <param reader>, each read with <param read>, in order.
let readRows (read : SqliteDataReader -> 'a) (reader : SqliteDataReader) : List<'a> =
  let rec go (acc : List<'a>) =
    if reader.Read() then go (read reader :: acc) else List.rev acc
  go []
