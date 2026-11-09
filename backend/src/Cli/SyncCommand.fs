/// `dark sync`: keep this store in step with one machine you own, over ssh.
///
/// A host verb, run here and never by package code: turning it on sends the whole store to another
/// machine, so only the person at this shell can do it. The settings it keeps (`sync.peer`,
/// `sync.command`) are not config keys Dark code may write.
///
///   dark sync add <host> [--command <dark there>]   catch up both ways once, then keep in step
///   dark sync off                                   stop; nothing goes or comes after this returns
///   dark sync                                       how the link stands
///
/// Two verbs no person types: `dark sync serve`, which ssh runs on the other machine, and
/// `dark sync watch`, the background process that holds the connection open (`LibDB.SyncLink`).
module Cli.Sync

open System.IO

open Prelude

module SyncLink = LibDB.SyncLink

let private peerKey = "sync.peer"
let private commandKey = "sync.command"

/// The key the boot reads, with the other startup settings, to know sync is on.
let settingKey = peerKey

let private dir () = Path.Combine(LibConfig.Config.runDir, "sync")
let private noticesPath () = Path.Combine(dir (), "notices")
let private statePath () = Path.Combine(dir (), "state")
let private lockPath () = Path.Combine(dir (), "watch.lock")
let private logPath () = Path.Combine(LibConfig.Config.logDir, "sync.log")

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

let private writeState (state : string) : unit =
  try
    Directory.CreateDirectory(dir ()) |> ignore<DirectoryInfo>
    let temporary = statePath () + ".new"
    File.WriteAllText(temporary, state)
    File.Move(temporary, statePath (), true)
  with _ ->
    ()

let private config (key : string) : Option<string> =
  try
    (LibDB.Config.get key).Result
  with _ ->
    None

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

/// The lock a watcher holds while it runs, or None when another process holds it.
let private takeLock () : Option<FileStream> =
  try
    Directory.CreateDirectory(dir ()) |> ignore<DirectoryInfo>
    Some(
      new FileStream(
        lockPath (),
        FileMode.OpenOrCreate,
        FileAccess.ReadWrite,
        FileShare.None
      )
    )
  with :? IOException ->
    None

let private watcherRunning () : bool =
  match takeLock () with
  | Some held ->
    held.Dispose()
    false
  | None -> true

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

/// What every other command does about sync as it starts: show what is waiting for the person,
/// and start the watcher if sync is on and none is running. With sync off, one stat.
let atStartup (peer : Option<string>) : unit =
  showNotices ()
  match peer with
  | Some _ when not (watcherRunning ()) -> startWatcher ()
  | _ -> ()

let private side (fold : unit -> unit) (keepGoing : unit -> bool) : SyncLink.Side =
  { store = LibConfig.Config.dbPath
    workDir = dir ()
    afterImport = fun _ -> fold ()
    say = say
    connected = fun name -> log $"connected to {name}"
    log = log
    keepGoing = keepGoing
    pollMs = 50 }

/// `dark sync serve`: the other machine's end, on stdin and stdout, until they close.
let private serve (fold : unit -> unit) : int =
  let input = System.Console.OpenStandardInput()
  let output = System.Console.OpenStandardOutput()
  let ended = SyncLink.run (side fold (fun () -> true)) input output false
  log $"serve ended: {ended}"
  0

/// `dark sync watch`: hold the connection to the peer until sync is turned off.
let private watch (fold : unit -> unit) : int =
  // A watcher started while a command was looking at the lock retries for a moment.
  let held =
    [ 0..4 ]
    |> List.tryPick (fun attempt ->
      if attempt > 0 then System.Threading.Thread.Sleep 200
      takeLock ())
  match held, config peerKey with
  | Some lock, Some host ->
    use _lock = lock
    let command = config commandKey |> Option.defaultValue "dark"
    let keepGoing () = config peerKey = Some host
    log $"watching {host}"
    SyncLink.watch
      { side =
          { (side fold keepGoing) with
              connected =
                fun name ->
                  log $"connected to {name}"
                  writeState
                    $"connected to {host} since {SyncLink.clock System.DateTime.UtcNow}" }
        transport = SyncLink.ssh host command
        peer = host
        patience = System.TimeSpan.FromMinutes 10.0
        writeState = writeState }
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
  | Some(SyncLink.NoDark reason) ->
    $"there is no dark to run on {host} ({reason}). Say where it is: `dark sync add {host} --command <path>`."
  | Some(SyncLink.NotSpoken reason) -> $"{reason}."
  | Some(SyncLink.Unreachable reason) -> $"couldn't reach {host}: {reason}."
  | None -> $"couldn't finish catching up with {host}."

/// `dark sync add <host>`: catch up both ways once, here in the foreground, and only then turn sync
/// on, so a host that cannot be reached is said now and changes nothing.
let private add
  (fold : unit -> unit)
  (host : string)
  (command : Option<string>)
  : int =
  match config peerKey with
  | Some current when current <> host ->
    System.Console.Error.WriteLine
      $"already syncing with {current}. `dark sync off` first; one machine at a time."
    1
  | _ ->
    let remote =
      command |> Option.orElse (config commandKey) |> Option.defaultValue "dark"
    let clock = System.Diagnostics.Stopwatch.StartNew()
    match
      SyncLink.connect (side fold (fun () -> true)) (SyncLink.ssh host remote) true
    with
    | SyncLink.Exchanged, _ ->
      (LibDB.Config.set peerKey host).Wait()
      match command with
      | Some c -> (LibDB.Config.set commandKey c).Wait()
      | None -> ()
      startWatcher ()
      let seconds = float clock.ElapsedMilliseconds / 1000.0
      System.Console.Error.WriteLine
        $"syncing with {host}. Caught up in {seconds:F1} s; from now on, changes go both ways as you make them."
      0
    | _, failure ->
      System.Console.Error.WriteLine(failureLine host failure)
      System.Console.Error.WriteLine "sync stays off."
      1

/// `dark sync off`: forget the peer and wait for the watcher to let go, so nothing travels after
/// this returns.
let private off () : int =
  match config peerKey with
  | None ->
    System.Console.Error.WriteLine "sync is already off."
    0
  | Some host ->
    (LibDB.Config.unset peerKey).Wait()
    (LibDB.Config.unset commandKey).Wait()
    let deadline = System.DateTime.UtcNow.AddSeconds 15.0
    let rec wait () =
      if watcherRunning () && System.DateTime.UtcNow < deadline then
        System.Threading.Thread.Sleep 50
        wait ()
    wait ()
    writeState "off"
    System.Console.Error.WriteLine
      $"sync is off. Nothing goes to or comes from {host} now; your store keeps everything it has."
    0

let private status () : int =
  match config peerKey with
  | None ->
    System.Console.Error.WriteLine
      "sync is off. `dark sync add <host>` keeps this store in step with a machine you own, over ssh."
    0
  | Some host ->
    let state =
      try
        File.ReadAllText(statePath ()).Trim()
      with _ ->
        ""
    let state = if state = "" || state = "off" then "starting" else state
    System.Console.Error.WriteLine $"syncing with {host}: {state}."
    0

/// The forms of `dark sync` handled here, or None for Dark to answer `help`.
let execute (fold : unit -> unit) (args : List<string>) : Option<int> =
  match args with
  | [ "help" ]
  | [ "--help" ]
  | [ "-h" ] -> None
  | []
  | [ "status" ] -> Some(status ())
  | [ "add"; host ]
  | [ "on"; host ] when not (host.StartsWith "-") -> Some(add fold host None)
  | [ "add"; host; "--command"; command ]
  | [ "on"; host; "--command"; command ]
  | [ "add"; "--command"; command; host ]
  | [ "on"; "--command"; command; host ] -> Some(add fold host (Some command))
  | [ "off" ] -> Some(off ())
  | [ "serve" ] -> Some(serve fold)
  | [ "watch" ] -> Some(watch fold)
  | _ ->
    let form = String.concat " " args
    System.Console.Error.WriteLine
      $"`dark sync {form}` isn't a form of this command."
    System.Console.Error.WriteLine
      "  dark sync | dark sync add <host> [--command <dark>] | dark sync off"
    Some 1
