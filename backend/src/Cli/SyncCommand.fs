/// `dark sync`: keep this store in step with a sync server, or with one machine you own over ssh.
///
/// A host verb, run here and never by package code: turning it on sends the whole store elsewhere,
/// so only the person at this shell can do it. The settings it keeps (`sync.peer`, `sync.command`)
/// are not config keys Dark code may write, and a server's token is a file only this user can read.
///
///   dark sync add <url> [--token <token> | --token -]   a sync server, with the token it gave you
///   dark sync add <host> [--command <dark there>]       a machine of yours, over ssh
///   dark sync off                                       stop; nothing goes or comes after this
///   dark sync                                           how the link stands
///
/// On the server:
///
///   dark sync serve --listen <addr> [--backup-dir <dir>] [--backup-every <hours>] [--keep <n>]
///                   [--max-frame-mb <n>]
///   dark sync token new <person> [--read-only] [--label <text>]
///   dark sync token add <person> [--read-only] [--label <text>]   the token itself on stdin
///   dark sync token list
///   dark sync token revoke <id or person>
///   dark sync backup <file>
///   dark sync restore <file>
///
/// Two verbs no person types: `dark sync serve`, which ssh runs on the other machine, and
/// `dark sync watch`, the background process that holds the connection open (`LibDB.SyncLink`).
module Cli.Sync

open System.IO

open Prelude

module SyncLink = LibDB.SyncLink
module SyncTokens = LibDB.SyncTokens
module SyncServer = LibDB.SyncServer

let private peerKey = SyncLink.peerKey
let private commandKey = "sync.command"

/// The key the boot reads, with the other startup settings, to know sync is on.
let settingKey = peerKey

/// How long the first command after a restart waits for the other machine's changes.
let private catchUpBound = System.TimeSpan.FromSeconds 1.5

let private noticesPath () = Path.Combine(SyncLink.dir (), "notices")
let private logPath () = Path.Combine(LibConfig.Config.logDir, "sync.log")

/// The token for the server in `sync.peer`. Readable by this user alone, and never printed.
let private tokenPath () = Path.Combine(SyncLink.dir (), "token")

let private appendLine (path : string) (line : string) : unit =
  try
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore<DirectoryInfo>
    File.AppendAllText(path, line + "\n")
  with _ ->
    ()

/// A line for the person, shown on stderr by the next command that starts on this machine.
let private say (line : string) : unit = appendLine (noticesPath ()) line

/// A line for the log, `logs/sync.log`, kept under about a megabyte.
let private log (line : string) : unit =
  let path = logPath ()
  try
    if File.Exists path && FileInfo(path).Length > 1_000_000L then
      File.Move(path, path + ".old", true)
  with _ ->
    ()
  let at = System.DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss.fff"
  appendLine path $"[{at}] {line}"

/// Record how the link stands, for `dark sync`, `dark status` and the first command after a restart.
let private note (change : SyncLink.Standing -> SyncLink.Standing) : unit =
  SyncLink.Standing.update (SyncLink.standingPath ()) change

let private config (key : string) : Option<string> =
  try
    LibDB.Config.get key
  with _ ->
    None

let private err (line : string) : unit = System.Console.Error.WriteLine line

/// Print what the watcher left for the person, and clear it. One stat when there is nothing.
let showNotices () : unit =
  let path = noticesPath ()
  if File.Exists path then
    let lines =
      try
        File.ReadAllLines path
      with _ ->
        [||]
    try
      File.Delete path
    with _ ->
      ()
    lines |> Array.distinct |> Array.iter System.Console.Error.WriteLine

/// Start `dark sync watch` in a session of its own, detached from this terminal, and return at
/// once. Its stdio is /dev/null: nothing it does is printed.
let private startWatcher () : unit =
  try
    let self = LibExecution.HostProcess.currentExecutablePath ()
    let psi = System.Diagnostics.ProcessStartInfo("/bin/sh")
    psi.ArgumentList.Add "-c"
    psi.ArgumentList.Add
      "exec \"$0\" --process-launch \"$0\" sync watch </dev/null >/dev/null 2>&1"
    psi.ArgumentList.Add self
    psi.UseShellExecute <- false
    (System.Diagnostics.Process.Start psi).Dispose()
  with e ->
    log $"could not start the watcher: {e.Message}"

/// A line that says something and asks nothing: dim, on stderr.
let private dim (line : string) : unit =
  NonBlockingConsole.writeErrLine $"\u001b[2m{line}\u001b[0m"

/// What every other command does about sync as it starts: show what is waiting for the person, and
/// start the watcher if sync is on and none is running. A watcher that was not running means this
/// machine was off or asleep, so the command waits a moment for the other machine's changes before
/// it reads anything, and says in one dim line when they did not come. With sync off, one stat.
let atStartup (peer : Option<string>) : unit =
  showNotices ()
  match peer with
  | Some host when not (SyncLink.watcherRunning ()) ->
    let started = System.DateTime.UtcNow
    startWatcher ()
    match
      SyncLink.awaitCatchUp
        (fun () -> SyncLink.Standing.read (SyncLink.standingPath ()))
        started
        catchUpBound
    with
    | SyncLink.CaughtUp -> LibDB.Caching.invalidateAll ()
    | SyncLink.OutOfReach _ ->
      dim $"sync: can't reach {host} right now; this is this machine's copy."
    | SyncLink.StillGoing ->
      dim
        $"sync: still catching up with {host}; what is newer there arrives in the background."
  | _ -> ()

/// Make sure the store has its own `instance.id` before it syncs: that is who it is to the other
/// end, and where that end resumes from. A store that is a copy of another takes a new one.
let private ensureIdentity () : unit =
  let before = config "instance.id"
  LibDB.Config.ensureInstance ()
  match before, config "instance.id" with
  | Some old, Some now when old <> now ->
    log
      $"this store is a copy of {old}, made elsewhere; it syncs as {now} from now on"
  | _ -> ()

/// One end of a session on this machine's store.
let private side
  (fold : unit -> unit)
  (keepGoing : unit -> bool)
  (record : bool)
  (notices : string -> unit)
  : SyncLink.Side =
  ensureIdentity ()
  { store = LibConfig.Config.dbPath
    workDir = SyncLink.dir ()
    afterImport = fun _ -> fold ()
    say = notices
    note = if record then note else (fun _ -> ())
    log = log
    keepGoing = keepGoing
    pollMs = 50
    quietMs = 20000
    maxFrame = SyncLink.defaultMaxFrame
    account = None }

/// `dark sync serve`: the other machine's end, on stdin and stdout, until they close. It records
/// nothing about the link, and shows nobody anything: that is the watcher's, on the machine that
/// connected.
let private serveStdio (fold : unit -> unit) : int =
  let input = System.Console.OpenStandardInput()
  let output = System.Console.OpenStandardOutput()
  let ended = SyncLink.run (side fold (fun () -> true) false log) input output false
  log $"serve ended: {ended}"
  0

/// What a token file says, if anything.
let private storedToken () : Option<string> =
  try
    let text = (File.ReadAllText(tokenPath ())).Trim()
    if text = "" then None else Some text
  with _ ->
    None

/// Keep <param token> where only this user can read it: the file is made with that mode, never
/// widened after.
let private storeToken (token : string) : unit =
  let path = tokenPath ()
  Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore<DirectoryInfo>
  let temporary = path + ".new"
  if File.Exists temporary then File.Delete temporary
  let options = FileStreamOptions()
  options.Mode <- FileMode.CreateNew
  options.Access <- FileAccess.Write
  if not (System.OperatingSystem.IsWindows()) then
    options.UnixCreateMode <- UnixFileMode.UserRead ||| UnixFileMode.UserWrite
  (use stream = new FileStream(temporary, options)
   let bytes = System.Text.Encoding.UTF8.GetBytes(token + "\n")
   stream.Write(bytes, 0, bytes.Length))
  File.Move(temporary, path, true)

let private forgetToken () : unit =
  try
    File.Delete(tokenPath ())
  with _ ->
    ()

/// How to reach the peer named <param peer>: a server's address, or an ssh host.
let private transportFor (peer : string) : Option<SyncLink.Transport> =
  match SyncLink.serverUrl peer with
  | Some url -> storedToken () |> Option.map (fun t -> SyncLink.Server(url, t))
  | None ->
    let command = config commandKey |> Option.defaultValue "dark"
    Some(SyncLink.ssh peer command)

/// `dark sync watch`: hold the connection to the peer until sync is turned off.
let private watch (fold : unit -> unit) : int =
  // A watcher started while a command was looking at the lock retries for a moment.
  let held =
    [ 0..4 ]
    |> List.tryPick (fun attempt ->
      if attempt > 0 then System.Threading.Thread.Sleep 200
      SyncLink.takeLock ())
  match held, config peerKey with
  | Some lock, Some host ->
    use _lock = lock
    match transportFor host with
    | None ->
      say
        $"sync: there is no token for {host} on this machine. `dark sync add {host} --token -` to give it one."
      0
    | Some transport ->
      // A new token (`dark sync add` again) ends this watcher; the next one uses it.
      let token = storedToken ()
      let keepGoing () = config peerKey = Some host && storedToken () = token
      log $"watching {host}"
      SyncLink.watch
        { side = side fold keepGoing true say
          transport = transport
          peer = host
          patience = System.TimeSpan.FromMinutes 10.0
          mention = fun line -> say $"\u001b[2m{line}\u001b[0m" }
      log "watcher stopped"
      0
  | Some lock, None ->
    lock.Dispose()
    0
  | None, _ -> 0

let private failureLine
  (host : string)
  (failure : Option<SyncLink.Failure>)
  : string =
  match failure with
  | Some(SyncLink.NotAllowed reason) ->
    $"{host} refused this machine's ssh key ({reason}). `ssh {host}` has to work without a prompt."
  | Some(SyncLink.TokenRefused true) ->
    $"{host} refused the token: it was revoked. Ask whoever runs the server for a new one."
  | Some(SyncLink.TokenRefused false) ->
    $"{host} refused the token: it isn't one the server knows. Check it was copied whole."
  | Some(SyncLink.NoDark reason) ->
    $"there is no dark to run on {host} ({reason}). Say where it is: `dark sync add {host} --command <path>`."
  | Some(SyncLink.NotSpoken reason) ->
    if reason.EndsWith "." || reason.EndsWith "?" then reason else $"{reason}."
  | Some(SyncLink.Unreachable reason) -> $"couldn't reach {host}: {reason}."
  | None -> $"couldn't finish catching up with {host}."

/// The token as the person gave it: on the command line, `-` for stdin, or asked for when there is
/// a terminal to ask at (not echoed).
let private readToken (given : Option<string>) (url : string) : Option<string> =
  let trimmed (s : string) =
    match s with
    | null -> None
    | s when s.Trim() = "" -> None
    | s -> Some(s.Trim())
  match given with
  | Some "-" -> trimmed (System.Console.In.ReadLine())
  | Some token -> trimmed token
  | None when System.Console.IsInputRedirected -> None
  | None ->
    System.Console.Error.Write $"Token for {url}: "
    let rec read (acc : List<char>) =
      let key = System.Console.ReadKey true
      match key.Key with
      | System.ConsoleKey.Enter -> acc
      | System.ConsoleKey.Backspace ->
        read (
          match acc with
          | _ :: rest -> rest
          | [] -> []
        )
      | _ -> read (key.KeyChar :: acc)
    let typed = read [] |> List.rev |> Array.ofList |> System.String
    System.Console.Error.WriteLine()
    trimmed typed

/// Catch up both ways once, here in the foreground, and only then turn sync on, so a peer that
/// cannot be reached, or a token it refuses, is said now and changes nothing.
let private connectFirst
  (fold : unit -> unit)
  (peer : string)
  (transport : SyncLink.Transport)
  (keep : unit -> unit)
  : int =
  match config peerKey with
  | Some current when current <> peer ->
    err $"already syncing with {current}. `dark sync off` first; one at a time."
    1
  | _ ->
    let clock = System.Diagnostics.Stopwatch.StartNew()
    match SyncLink.connect (side fold (fun () -> true) true say) transport true with
    | SyncLink.Exchanged, _, _ ->
      keep ()
      LibDB.Config.set peerKey peer
      startWatcher ()
      let seconds = float clock.ElapsedMilliseconds / 1000.0
      err
        $"syncing with {peer}. Caught up in {seconds:F1} s; from now on, changes go both ways as you make them."
      0
    | _, failure, _ ->
      err (failureLine peer failure)
      err "sync stays off."
      1

let private addServer
  (fold : unit -> unit)
  (url : System.Uri)
  (text : string)
  (token : Option<string>)
  : int =
  if url.Scheme = "http" && not (SyncLink.plainHttpAllowed url) then
    err
      $"{text} is plain http on the open internet, and the token would cross it in the clear. Use https://."
    2
  else
    match readToken token text with
    | None ->
      err
        $"a server needs a token: `dark sync add {text} --token <token>`, or `--token -` to read it from stdin."
      2
    | Some token ->
      // Kept only once the server takes it.
      connectFirst fold text (SyncLink.Server(url, token)) (fun () ->
        storeToken token)

let private addHost
  (fold : unit -> unit)
  (host : string)
  (command : Option<string>)
  : int =
  let remote =
    command |> Option.orElse (config commandKey) |> Option.defaultValue "dark"
  connectFirst fold host (SyncLink.ssh host remote) (fun () ->
    forgetToken ()
    match command with
    | Some c -> LibDB.Config.set commandKey c
    | None -> ())

/// `dark sync off`: forget the peer and wait for the watcher to let go, so nothing travels after
/// this returns.
let private off () : int =
  match config peerKey with
  | None ->
    err "sync is already off."
    0
  | Some host ->
    LibDB.Config.unset peerKey
    LibDB.Config.unset commandKey
    forgetToken ()
    let deadline = System.DateTime.UtcNow.AddSeconds 15.0
    let rec wait () =
      if SyncLink.watcherRunning () && System.DateTime.UtcNow < deadline then
        System.Threading.Thread.Sleep 50
        wait ()
    wait ()
    note (SyncLink.Standing.into "off" "")
    err
      $"sync is off. Nothing goes to or comes from {host} now; your store keeps everything it has."
    0

let private status () : int =
  match config peerKey with
  | None ->
    err
      "sync is off. `dark sync add <url> --token <token>` keeps this store in step with a sync server; `dark sync add <host>`, with a machine of yours over ssh."
    0
  | Some _ ->
    err (SyncLink.statusNow ())
    0


// ------------------
// The server's verbs
// ------------------

let private parseServe (args : List<string>) : Result<SyncServer.Options, string> =
  let rec go (o : SyncServer.Options) (rest : List<string>) =
    let positive (flag : string) (text : string) (f : int -> SyncServer.Options) =
      match System.Int32.TryParse text with
      | true, n when n > 0 -> go (f n)
      | _ -> fun _ -> Error $"{flag} takes a whole number above 0, not `{text}`"
    match rest with
    | [] when o.listen = "" ->
      Error "say where to listen: `dark sync serve --listen :8080`"
    | [] -> Ok o
    | "--listen" :: addr :: rest -> go { o with listen = addr } rest
    | "--backup-dir" :: dir :: rest -> go { o with backupDir = Some dir } rest
    | "--backup-every" :: hours :: rest ->
      positive
        "--backup-every"
        hours
        (fun n -> { o with backupEvery = System.TimeSpan.FromHours(float n) })
        rest
    | "--keep" :: n :: rest ->
      positive "--keep" n (fun n -> { o with keep = n }) rest
    | "--max-frame-mb" :: n :: rest ->
      positive
        "--max-frame-mb"
        n
        (fun n -> { o with maxFrame = n * 1024 * 1024 })
        rest
    | flag :: _ -> Error $"`{flag}` isn't an option of `dark sync serve`"
  go
    { listen = ""
      backupDir = None
      backupEvery = System.TimeSpan.FromHours 6.0
      keep = 28
      maxFrame = 256 * 1024 * 1024 }
    args

let private serveListening (fold : unit -> unit) (args : List<string>) : int =
  match parseServe args with
  | Error reason ->
    err reason
    2
  | Ok options ->
    ensureIdentity ()
    let store = LibConfig.Config.dbPath
    let makeSide
      (account : SyncTokens.Account)
      (workDir : string)
      keepGoing
      : SyncLink.Side =
      let fields (line : string) : List<string * obj> =
        [ "person", account.person; "line", line ]
      { store = store
        workDir = workDir
        afterImport = fun _ -> fold ()
        // Nobody reads a server's notices; its conflicts are its clients' to see.
        say = fun line -> SyncServer.log "notice" (fields line)
        note = fun _ -> ()
        log = fun line -> SyncServer.log "session" (fields line)
        keepGoing = keepGoing
        pollMs = 50
        quietMs = 20000
        maxFrame = options.maxFrame
        account = Some account }
    SyncServer.run store options makeSide

let private parseTokenOptions (args : List<string>) : Result<bool * string, string> =
  let rec go (readOnly : bool) (label : string) (rest : List<string>) =
    match rest with
    | [] -> Ok(readOnly, label)
    | "--read-only" :: rest -> go true label rest
    | "--label" :: text :: rest -> go readOnly text rest
    | flag :: _ ->
      Error $"`{flag}` isn't an option here; `--read-only` and `--label <text>` are"
  go false "" args

let private tokenVerb (args : List<string>) : int =
  let store = LibConfig.Config.dbPath
  let checkPerson (person : string) (then' : unit -> int) =
    if SyncTokens.validPerson person then
      then' ()
    else
      err
        $"`{person}` can't name a person: letters, digits, `.`, `_` and `-`, up to 64."
      2
  match args with
  | "new" :: person :: rest ->
    checkPerson person (fun () ->
      match parseTokenOptions rest with
      | Error reason ->
        err reason
        2
      | Ok(readOnly, label) ->
        let token = SyncTokens.generate ()
        let id = SyncTokens.add store person token readOnly label
        let kind = if readOnly then "read-only token" else "token"
        // The token alone on stdout, so it can be piped; this is the only time it is shown.
        System.Console.Out.WriteLine token
        err
          $"a {kind} for {person} (id {id}). Only its hash is kept, so it won't be shown again. On their machine: dark sync add <this server's url> --token -"
        0)
  | "add" :: person :: rest ->
    checkPerson person (fun () ->
      match parseTokenOptions rest, System.Console.In.ReadLine() with
      | Error reason, _ ->
        err reason
        2
      | Ok _, null ->
        err "`dark sync token add` reads the token from stdin, and there was none."
        2
      | Ok _, token when token.Trim().Length < 16 ->
        err
          "that token is too short to keep; one from `dark sync token new` is 49 characters."
        2
      | Ok(readOnly, label), token ->
        let id = SyncTokens.add store person (token.Trim()) readOnly label
        err $"kept a token for {person} (id {id})."
        0)
  | [ "list" ] ->
    match SyncTokens.list store with
    | [] ->
      err "no tokens yet. `dark sync token new <person>` makes one."
      0
    | tokens ->
      let rows =
        tokens
        |> List.map (fun t ->
          let state =
            match t.revokedAt with
            | Some at -> $"revoked {at}"
            | None -> if t.readOnly then "read-only" else "read-write"
          let used = t.lastUsedAt |> Option.defaultValue "never"
          [ t.id; t.person; state; t.createdAt; used; t.label ])
      let header = [ "ID"; "PERSON"; "ACCESS"; "CREATED"; "LAST USED"; "LABEL" ]
      let widths =
        header
        |> List.mapi (fun i h ->
          (h :: (rows |> List.map (fun r -> r[i])))
          |> List.map String.length
          |> List.max)
      let line (cells : List<string>) =
        List.zip widths cells
        |> List.map (fun (w, c) -> c.PadRight w)
        |> String.concat "  "
        |> fun s -> s.TrimEnd()
      System.Console.Out.WriteLine(line header)
      rows |> List.iter (line >> System.Console.Out.WriteLine)
      0
  | [ "revoke"; idOrPerson ] ->
    match SyncTokens.revoke store idOrPerson with
    | [] ->
      err
        $"no live token has the id or person `{idOrPerson}`. `dark sync token list` shows them."
      1
    | revoked ->
      for t in revoked do
        err
          $"revoked {t.id} ({t.person}). Its open sessions end within half a minute."
      0
  | _ ->
    err
      "  dark sync token new <person> [--read-only] [--label <text>]\n  dark sync token add <person> [--read-only] [--label <text>]   (token on stdin)\n  dark sync token list\n  dark sync token revoke <id or person>"
    2

let private backupVerb (target : string) : int =
  let clock = System.Diagnostics.Stopwatch.StartNew()
  try
    SyncServer.backup LibConfig.Config.dbPath target
    let mb = float (FileInfo(target).Length) / 1048576.0
    err $"backed up to {target} ({mb:F1} MB, {clock.ElapsedMilliseconds} ms)."
    0
  with e ->
    err $"couldn't back up to {target}: {e.Message}"
    1

/// Put the store back as <param source> holds it. The server must be stopped. The store takes a new
/// `instance.id`, so every machine compares whole stores with it on its next sync rather than
/// trusting where it thought the two stood, and sends back whatever the backup lacks.
let private restoreVerb (source : string) : int =
  let store = LibConfig.Config.dbPath
  let serving =
    try
      let path = Path.Combine(SyncLink.dir (), "serve.lock")
      Directory.CreateDirectory(SyncLink.dir ()) |> ignore<DirectoryInfo>
      use _ =
        new FileStream(
          path,
          FileMode.OpenOrCreate,
          FileAccess.ReadWrite,
          FileShare.None
        )
      false
    with :? IOException ->
      true
  let check () : Result<unit, string> =
    try
      use conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={source};Mode=ReadOnly;Pooling=false"
        )
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <-
        "SELECT (SELECT integrity_check FROM pragma_integrity_check LIMIT 1),
                (SELECT COUNT(*) FROM sqlite_master WHERE name = 'sync_journal')"
      use reader = cmd.ExecuteReader()
      if reader.Read() && reader.GetString 0 = "ok" && reader.GetInt64 1 = 1L then
        Ok()
      else
        Error "it isn't a whole dark store"
    with e ->
      Error e.Message
  if serving then
    err "a sync server is running on this store. Stop it first."
    1
  elif not (File.Exists source) then
    err $"no file at {source}."
    2
  else
    match check () with
    | Error reason ->
      err $"won't restore from {source}: {reason}."
      1
    | Ok() ->
      let aside =
        store
        + ".before-restore-"
        + System.DateTime.UtcNow.ToString "yyyyMMdd-HHmmss"
      SyncServer.backup store aside
      // The new store is made whole beside the old one, with its new identity, then moved over it
      // in one rename: this process has the old one open, and nothing writes the new one in place.
      let incoming = store + ".restoring"
      if File.Exists incoming then File.Delete incoming
      File.Copy(source, incoming)
      (use conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={incoming};Pooling=false"
        )
       conn.Open()
       use cmd = conn.CreateCommand()
       cmd.CommandText <-
         "INSERT INTO config_v0 (key, value) VALUES ('instance.id', $id)
          ON CONFLICT(key) DO UPDATE SET value = excluded.value;
          PRAGMA wal_checkpoint(TRUNCATE);"
       cmd.Parameters.AddWithValue("$id", string (System.Guid.NewGuid()))
       |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
       cmd.ExecuteNonQuery() |> ignore<int>)
      Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools()
      for suffix in [ "-wal"; "-shm" ] do
        if File.Exists(store + suffix) then File.Delete(store + suffix)
      File.Move(incoming, store, true)
      err
        $"restored from {source}. The store as it was is at {aside}. Machines that sync with it compare whole stores once and send back what the backup lacks."
      0

/// The forms of `dark sync` handled here, or None for Dark to answer `help`.
let execute (fold : unit -> unit) (args : List<string>) : Option<int> =
  match args with
  | [ "help" ]
  | [ "--help" ]
  | [ "-h" ] -> None
  | []
  | [ "status" ] -> Some(status ())
  | ("add" | "on") :: target :: rest when not (target.StartsWith "-") ->
    match SyncLink.serverUrl target, rest with
    | Some url, [] -> Some(addServer fold url target None)
    | Some url, [ "--token"; token ] -> Some(addServer fold url target (Some token))
    | None, [] -> Some(addHost fold target None)
    | None, [ "--command"; command ] -> Some(addHost fold target (Some command))
    | Some _, _ ->
      err $"  dark sync add {target} [--token <token> | --token -]"
      Some 2
    | None, _ ->
      err $"  dark sync add {target} [--command <dark there>]"
      Some 2
  | [ "off" ] -> Some(off ())
  | [ "serve" ] -> Some(serveStdio fold)
  | "serve" :: rest -> Some(serveListening fold rest)
  | [ "watch" ] -> Some(watch fold)
  | "token" :: rest -> Some(tokenVerb rest)
  | [ "backup"; target ] -> Some(backupVerb target)
  | [ "restore"; source ] -> Some(restoreVerb source)
  | _ ->
    let form = String.concat " " args
    err $"`dark sync {form}` isn't a form of this command."
    err
      "  dark sync | dark sync add <url> --token <token> | dark sync add <host> | dark sync off"
    Some 2
