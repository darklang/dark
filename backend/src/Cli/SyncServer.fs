/// `dark sync serve --listen <addr>`: a sync server that runs until stopped, on this machine's store.
///
/// It speaks plain HTTP. TLS is the platform's job in front of it (Fly, Cloud Run, Caddy, `tailscale
/// serve`), so the server never holds a certificate. Two paths:
///
///   GET /health   how the server stands, as JSON, for a platform's health check
///   GET /sync     a WebSocket carrying one sync session (`LibDB.SyncLink`), opened with a token
///                 (`Authorization: Bearer ...`) that says whose machine it is
///
/// Every connection is a session of its own, all on the one store; imports take turns
/// (`SyncLink.importing`). What it does is logged as one JSON object a line on stdout. On SIGTERM or
/// SIGINT it stops taking connections, ends the sessions it has (each import is one transaction, so
/// none is half-applied), and exits; the machines it served reconnect and resume where they were.
module Cli.SyncServer

open System.IO
open System.Net

open Prelude

module SyncLink = LibDB.SyncLink
module SyncTokens = LibDB.SyncTokens

type Options =
  {
    /// `host:port`, `:port` or a port alone.
    listen : string
    /// Where scheduled backups go, if anywhere.
    backupDir : Option<string>
    backupEvery : System.TimeSpan
    /// How many scheduled backups to keep.
    keep : int
    /// The largest frame a session takes.
    maxFrame : int
  }

let private writing = obj ()

/// One JSON object on stdout: the time, what happened, and its details.
let log (event : string) (fields : List<string * obj>) : unit =
  use buffer = new MemoryStream()
  (use w = new System.Text.Json.Utf8JsonWriter(buffer)
   w.WriteStartObject()
   w.WriteString("at", System.DateTime.UtcNow.ToString("o"))
   w.WriteString("event", event)
   for (key, value) in fields do
     match value with
     | :? string as v -> w.WriteString(key, v)
     | :? int as v -> w.WriteNumber(key, v)
     | :? int64 as v -> w.WriteNumber(key, v)
     | :? float as v -> w.WriteNumber(key, v)
     | :? bool as v -> w.WriteBoolean(key, v)
     | v -> w.WriteString(key, string v)
   w.WriteEndObject())
  let line = System.Text.Encoding.UTF8.GetString(buffer.ToArray())
  lock writing (fun () ->
    System.Console.Out.WriteLine line
    System.Console.Out.Flush())

/// The HttpListener prefix for <param listen>.
let prefixOf (listen : string) : Result<string, string> =
  let host, port =
    match listen.LastIndexOf ':' with
    | -1 -> "", listen
    | i -> listen.Substring(0, i), listen.Substring(i + 1)
  match System.Int32.TryParse port with
  | true, p when p > 0 && p < 65536 ->
    let host =
      match host with
      | ""
      | "0.0.0.0"
      | "*"
      | "[::]" -> "+"
      | h -> h
    Ok $"http://{host}:{p}/"
  | _ ->
    Error $"`{listen}` isn't an address to listen on; say `host:port` or `:port`"

/// A copy of the store at <param storePath> at <param target>, taken while it is in use: SQLite's
/// `VACUUM INTO` reads one moment of it.
let backup (storePath : string) (target : string) : unit =
  Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath target))
  |> ignore<DirectoryInfo>
  if File.Exists target then File.Delete target
  use conn =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={storePath};Mode=ReadOnly;Pooling=false"
    )
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- "VACUUM INTO $target"
  cmd.Parameters.AddWithValue("$target", target)
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  cmd.ExecuteNonQuery() |> ignore<int>

let private scheduledBackups (dir : string) : List<FileInfo> =
  if Directory.Exists dir then
    DirectoryInfo(dir).GetFiles("dark-sync-*.db")
    |> Array.sortByDescending (fun f -> f.Name)
    |> Array.toList
  else
    []

/// Take a scheduled backup into <param dir>, and keep only the newest <param keep>.
let private backupNow (storePath : string) (dir : string) (keep : int) : unit =
  let stamp = System.DateTime.UtcNow.ToString "yyyyMMdd-HHmmss"
  let target = Path.Combine(dir, $"dark-sync-{stamp}.db")
  let clock = System.Diagnostics.Stopwatch.StartNew()
  try
    backup storePath target
    log
      "backup"
      [ "path", target
        "bytes", FileInfo(target).Length
        "ms", clock.ElapsedMilliseconds ]
    for old in
      scheduledBackups dir
      |> List.skip (min keep (List.length (scheduledBackups dir))) do
      old.Delete()
      log "backup-pruned" [ "path", old.FullName ]
  with e ->
    log "backup-failed" [ "path", target; "error", e.Message ]

/// A JSON reply.
let private reply
  (ctx : HttpListenerContext)
  (status : int)
  (headers : List<string * string>)
  (body : List<string * obj>)
  : unit =
  try
    use buffer = new MemoryStream()
    (use w = new System.Text.Json.Utf8JsonWriter(buffer)
     w.WriteStartObject()
     for (key, value) in body do
       match value with
       | :? string as v -> w.WriteString(key, v)
       | :? int as v -> w.WriteNumber(key, v)
       | :? int64 as v -> w.WriteNumber(key, v)
       | :? bool as v -> w.WriteBoolean(key, v)
       | v -> w.WriteString(key, string v)
     w.WriteEndObject())
    let bytes = buffer.ToArray()
    ctx.Response.StatusCode <- status
    ctx.Response.ContentType <- "application/json"
    for (name, value) in headers do
      ctx.Response.Headers.Add(name, value)
    ctx.Response.ContentLength64 <- int64 bytes.Length
    ctx.Response.OutputStream.Write(bytes, 0, bytes.Length)
    ctx.Response.Close()
  with _ ->
    ()

/// What the server is doing: its sessions by connection, and whether it is stopping.
type private Running =
  { sessions :
      System.Collections.Concurrent.ConcurrentDictionary<System.Guid, string>
    stopping : System.Threading.ManualResetEventSlim
    started : System.DateTime }

let private health
  (storePath : string)
  (running : Running)
  (ctx : HttpListenerContext)
  =
  try
    let (id, name) = LibDB.OpSync.identityOf storePath
    let cursor = LibDB.OpSync.cursorOf storePath
    reply
      ctx
      (if running.stopping.IsSet then 503 else 200)
      []
      [ "status", (if running.stopping.IsSet then "stopping" else "ok")
        "format", LibDB.OpSync.formatVersion
        "build", LibConfig.Config.buildHash
        "store", id
        "name", name
        "journal", cursor.journal
        "sessions", running.sessions.Count
        "uptimeSeconds",
        int64 (System.DateTime.UtcNow - running.started).TotalSeconds ]
  with e ->
    reply ctx 503 [] [ "status", "store-unreadable"; "error", e.Message ]

let private bearer (ctx : HttpListenerContext) : string =
  match ctx.Request.Headers["Authorization"] with
  | null -> ""
  | h when h.StartsWith "Bearer " -> h.Substring("Bearer ".Length).Trim()
  | _ -> ""

let private session
  (storePath : string)
  (makeSide : SyncTokens.Account -> string -> (unit -> bool) -> SyncLink.Side)
  (running : Running)
  (ctx : HttpListenerContext)
  : unit =
  let remote = string ctx.Request.RemoteEndPoint
  if not ctx.Request.IsWebSocketRequest then
    reply ctx 400 [] [ "error", "websocket-required" ]
  elif ctx.Request.Headers[SyncLink.formatHeader] <> LibDB.OpSync.formatVersion then
    log
      "refused"
      [ "remote", remote
        "why", "format"
        "theirs",
        box (
          ctx.Request.Headers[SyncLink.formatHeader]
          |> Option.ofObj
          |> Option.defaultValue ""
        ) ]
    reply
      ctx
      426
      [ SyncLink.refusalHeader, "format" ]
      [ "error", "format"; "format", LibDB.OpSync.formatVersion ]
  elif running.stopping.IsSet then
    reply ctx 503 [] [ "error", "stopping" ]
  else
    match SyncTokens.check storePath (bearer ctx) with
    | Error refusal ->
      let why, person =
        match refusal with
        | SyncTokens.Unknown -> "unknown-token", ""
        | SyncTokens.Revoked person -> "revoked", person
      log "refused" [ "remote", remote; "why", why; "person", person ]
      reply ctx 401 [ SyncLink.refusalHeader, why ] [ "error", why ]
    | Ok account ->
      let id = System.Guid.NewGuid()
      let workDir = Path.Combine(SyncLink.dir (), "sessions", string id)
      let ws =
        ctx
          .AcceptWebSocketAsync(null, 64 * 1024, System.TimeSpan.FromSeconds 15.0)
          .Result.WebSocket
      running.sessions[id] <- account.person
      log
        "session-start"
        [ "session", string id
          "person", account.person
          "token", account.tokenId
          "readOnly", account.readOnly
          "remote", remote ]
      let clock = System.Diagnostics.Stopwatch.StartNew()
      let ended =
        try
          use stream =
            System.Net.WebSockets.WebSocketStream.Create(
              ws,
              System.Net.WebSockets.WebSocketMessageType.Binary,
              true
            )
          // A token revoked while its session is open ends the session within half a minute.
          let lastCheck = ref System.DateTime.UtcNow
          let stillValid = ref true
          let keepGoing () =
            if
              System.DateTime.UtcNow - lastCheck.Value > System.TimeSpan.FromSeconds
                30.0
            then
              lastCheck.Value <- System.DateTime.UtcNow
              stillValid.Value <- SyncTokens.isLive storePath account.tokenId
            not running.stopping.IsSet && stillValid.Value
          let side = makeSide account workDir keepGoing
          let ended = SyncLink.run side stream stream false
          // A closing handshake, so the other end reads an end rather than a broken stream.
          (try
            use cts = new System.Threading.CancellationTokenSource(2000)
            ws
              .CloseOutputAsync(
                System.Net.WebSockets.WebSocketCloseStatus.NormalClosure,
                "",
                cts.Token
              )
              .Wait()
           with _ ->
             ())
          ended
        with e ->
          SyncLink.PeerGone e.Message
      running.sessions.TryRemove id |> ignore<bool * string>
      try
        Directory.Delete(workDir, true)
      with _ ->
        ()
      log
        "session-end"
        [ "session", string id
          "person", account.person
          "ended", string ended
          "seconds", clock.Elapsed.TotalSeconds ]

/// Run the server until SIGTERM or SIGINT. <param makeSide> makes one session's end, for a person,
/// in a directory of its own, that stops when told.
let run
  (storePath : string)
  (options : Options)
  (makeSide : SyncTokens.Account -> string -> (unit -> bool) -> SyncLink.Side)
  : int =
  match prefixOf options.listen with
  | Error reason ->
    System.Console.Error.WriteLine reason
    2
  | Ok prefix ->
    let lockPath = Path.Combine(SyncLink.dir (), "serve.lock")
    Directory.CreateDirectory(SyncLink.dir ()) |> ignore<DirectoryInfo>
    let held =
      try
        Some(
          new FileStream(
            lockPath,
            FileMode.OpenOrCreate,
            FileAccess.ReadWrite,
            FileShare.None
          )
        )
      with :? IOException ->
        None
    match held with
    | None ->
      System.Console.Error.WriteLine
        "a sync server is already running on this store; one store takes one server."
      1
    | Some held ->
      use _held = held
      let running =
        { sessions = System.Collections.Concurrent.ConcurrentDictionary()
          stopping = new System.Threading.ManualResetEventSlim(false)
          started = System.DateTime.UtcNow }
      let listener = new HttpListener()
      listener.Prefixes.Add prefix
      let startedListening =
        try
          listener.Start()
          Ok()
        with e ->
          Error e.Message
      match startedListening with
      | Error reason ->
        System.Console.Error.WriteLine $"can't listen on {options.listen}: {reason}"
        1
      | Ok() ->
        let stop (signal : System.Runtime.InteropServices.PosixSignalContext) =
          signal.Cancel <- true
          if not running.stopping.IsSet then
            log
              "stopping"
              [ "signal", string signal.Signal; "sessions", running.sessions.Count ]
            running.stopping.Set()
        use _term =
          System.Runtime.InteropServices.PosixSignalRegistration.Create(
            System.Runtime.InteropServices.PosixSignal.SIGTERM,
            stop
          )
        use _int =
          System.Runtime.InteropServices.PosixSignalRegistration.Create(
            System.Runtime.InteropServices.PosixSignal.SIGINT,
            stop
          )
        let (id, name) = LibDB.OpSync.identityOf storePath
        log
          "listening"
          [ "prefix", prefix
            "store", id
            "name", name
            "format", LibDB.OpSync.formatVersion
            "build", LibConfig.Config.buildHash
            "tokens",
            List.length (
              SyncTokens.list storePath
              |> List.filter (fun t -> Option.isNone t.revokedAt)
            )
            "backupDir", box (options.backupDir |> Option.defaultValue "") ]
        match options.backupDir with
        | Some dir ->
          let backups =
            System.Threading.Thread(
              (fun () ->
                while not (running.stopping.Wait options.backupEvery) do
                  backupNow storePath dir options.keep),
              IsBackground = true
            )
          backups.Start()
        | None -> ()
        let accept () =
          while not running.stopping.IsSet do
            match
              (try
                Some(listener.GetContext())
               with _ ->
                 None)
            with
            | None -> ()
            | Some ctx ->
              let handle () =
                try
                  match ctx.Request.Url.AbsolutePath with
                  | "/health" -> health storePath running ctx
                  | path when path = SyncLink.sessionPath ->
                    session storePath makeSide running ctx
                  | _ -> reply ctx 404 [] [ "error", "not-found" ]
                with e ->
                  log "request-failed" [ "error", e.Message ]
              System.Threading.Thread(handle, IsBackground = true).Start()
        let acceptor = System.Threading.Thread(accept, IsBackground = true)
        acceptor.Start()
        running.stopping.Wait()
        // Each session sees `stopping` within a poll and ends between imports.
        let deadline = System.DateTime.UtcNow.AddSeconds 25.0
        while running.sessions.Count > 0 && System.DateTime.UtcNow < deadline do
          System.Threading.Thread.Sleep 50
        // Only now: stopping the listener drops the connections it still holds, mid-frame.
        (try
          listener.Stop()
         with _ ->
           ())
        (try
          use conn =
            new Microsoft.Data.Sqlite.SqliteConnection(
              $"Data Source={storePath};Pooling=false"
            )
          conn.Open()
          use cmd = conn.CreateCommand()
          cmd.CommandText <- "PRAGMA wal_checkpoint(TRUNCATE)"
          cmd.ExecuteNonQuery() |> ignore<int>
         with _ ->
           ())
        log "stopped" [ "sessions", running.sessions.Count ]
        0
