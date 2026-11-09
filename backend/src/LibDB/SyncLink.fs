/// Keeping two stores in step over one pipe: the session both ends run, and the watcher that holds a
/// session open to a machine you own over ssh.
///
/// The two ends are the same code. Each says hello, sends a have file of what it holds, answers the
/// other's have file with a bundle of everything the other lacks (`OpSync.export`), and from then on
/// watches its own store: whenever its journal grows, it sends what changed (`OpSync.exportSince`).
/// What arrives is imported (`OpSync.import`) and folded. So a save on either machine reaches the
/// other within one poll of the store and one trip over the pipe, in both directions, over one
/// connection, with no server: the other end is `dark sync serve` started by ssh.
///
/// Nothing here waits on the network for a person. The watcher is its own process; a command only
/// checks that one is running. When the other machine cannot be reached the journal keeps growing,
/// and the next connection's have file catches up whatever the cursor missed.
module LibDB.SyncLink

open System.IO
open System.Threading.Tasks

open Microsoft.Data.Sqlite

open Prelude


/// Step <param step> from <param start> until it says it is done. A loop with constant stack and no
/// mutable state: a session runs for days, twenty steps a second.
let private iterate (step : 's -> Choice<'s, 'r>) (start : 's) : 'r =
  Seq.unfold
    (fun (c : Choice<'s, 'r>) ->
      match c with
      | Choice1Of2 s ->
        let next = step s
        Some(next, next)
      | Choice2Of2 _ -> None)
    (Choice1Of2 start)
  |> Seq.pick (fun c ->
    match c with
    | Choice2Of2 r -> Some r
    | Choice1Of2 _ -> None)


// ------------------
// Frames
// ------------------

/// What travels over the pipe, one frame at a time: a kind byte, a 4-byte big-endian length, then
/// the payload. Have and bundle frames carry the SQLite files `OpSync` writes, whole.
type Frame =
  /// The format both ends must speak, and what this end calls itself.
  | Hello of version : string * name : string
  | Have of byte[]
  | Bundle of byte[]
  /// Nothing to say; sent when idle, so a dead pipe is found by writing to it.
  | Ping
  /// Not sent: what reading says when the other end has gone, and why.
  | Closed of reason : string

/// A frame larger than this is refused rather than allocated: no honest bundle comes near it.
let private maxPayload = 1 <<< 30

let writeFrame (output : Stream) (frame : Frame) : unit =
  let kind, payload =
    match frame with
    | Hello(version, name) ->
      'H', System.Text.Encoding.UTF8.GetBytes($"{version}\n{name}")
    | Have bytes -> 'V', bytes
    | Bundle bytes -> 'B', bytes
    | Ping -> 'P', [||]
    | Closed _ -> 'P', [||]
  let header = Array.zeroCreate<byte> 5
  header[0] <- byte kind
  System.Buffers.Binary.BinaryPrimitives.WriteInt32BigEndian(
    System.Span<byte>(header, 1, 4),
    payload.Length
  )
  output.Write(header, 0, 5)
  output.Write(payload, 0, payload.Length)
  output.Flush()

/// Read exactly <param count> bytes, or None at the end of the stream.
let private readExactly (input : Stream) (count : int) : Option<byte[]> =
  let buffer = Array.zeroCreate<byte> count
  let rec fill (at : int) =
    if at = count then
      true
    else
      let n = input.Read(buffer, at, count - at)
      if n <= 0 then false else fill (at + n)
  if fill 0 then Some buffer else None

/// The next frame, blocking until it is whole. A truncated or unknown frame closes the session: a
/// half-delivered bundle must never count as delivered.
let readFrame (input : Stream) : Frame =
  try
    match readExactly input 5 with
    | None -> Closed "the other end closed the connection"
    | Some header ->
      let length =
        System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(
          System.ReadOnlySpan<byte>(header, 1, 4)
        )
      if length < 0 || length > maxPayload then
        Closed "the other end sent a frame too large to be a bundle"
      else
        match readExactly input length with
        | None -> Closed "the connection ended in the middle of a frame"
        | Some payload ->
          match char header[0] with
          | 'H' ->
            match System.Text.Encoding.UTF8.GetString(payload).Split('\n', 2) with
            | [| version; name |] -> Hello(version, name)
            | _ -> Closed "the other end's hello did not parse"
          | 'V' -> Have payload
          | 'B' -> Bundle payload
          | 'P' -> Ping
          | _ -> Closed "the other end does not speak dark sync"
  with e ->
    Closed e.Message


// ------------------
// The session
// ------------------

/// One end of a session: the store it keeps in step, and what it does around each exchange.
type Side =
  {
    store : string
    /// Where this end writes the have and bundle files it sends and receives.
    workDir : string
    /// After a bundle added something: fold it, so the next command finds it applied.
    afterImport : OpSync.Counts -> unit
    /// A line for the person (a conflict, a refusal). Not printed here.
    say : string -> unit
    /// The other end said hello, with its name.
    connected : string -> unit
    /// A line for the log, for whoever is measuring or debugging.
    log : string -> unit
    /// False ends the session: sync was turned off.
    keepGoing : unit -> bool
    /// How often to look at the store for a change.
    pollMs : int
  }

/// How a session ended.
type Ended =
  /// The other end went away, or the pipe broke.
  | PeerGone of reason : string
  /// `keepGoing` said to stop.
  | Stopped
  /// One full exchange each way is done (`once`).
  | Exchanged
  /// The other end speaks another format, or sent something unreadable.
  | Refused of reason : string

type private State =
  { cursor : Option<OpSync.Cursor>
    peerName : string
    gotFull : bool
    lastSent : System.DateTime }

let private fileIn (side : Side) (name : string) : string =
  Path.Combine(side.workDir, $"{System.Environment.ProcessId}-{name}.db")

let private removeFile (path : string) : unit =
  for suffix in [ ""; "-wal"; "-shm"; "-journal" ] do
    try
      File.Delete(path + suffix)
    with _ ->
      ()

/// Anything worth applying: the import counts nothing when the bundle held nothing new.
let private changed (c : OpSync.Counts) : bool =
  c.ops + c.committed + c.dropped + c.commits + c.blobs + c.journal + c.branches > 0L

/// Where the store stands, on a connection held for the session: one indexed read per poll.
let private standing (conn : SqliteConnection) : OpSync.Cursor =
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "SELECT (SELECT COALESCE(MAX(seq), 0) FROM sync_journal),
            (SELECT COALESCE(MAX(rowid), 0) FROM package_blobs)"
  use reader = cmd.ExecuteReader()
  if reader.Read() then
    { OpSync.Cursor.journal = reader.GetInt64 0; blobs = reader.GetInt64 1 }
  else
    { OpSync.Cursor.journal = 0L; blobs = 0L }

/// The line a person reads about a conflict the import recorded.
let conflictLine (c : ScmDetect.Conflict) : string =
  let name = ScmDetect.fqn c.owner c.modules c.name
  let kept =
    match
      c.candidates |> List.tryFind (fun cand -> cand.hash = c.autoResolvedTo)
    with
    | Some cand when cand.side = "local" -> "this machine's"
    | Some cand -> $"{cand.author}'s"
    | None -> "one"
  let newer = if c.reason = "later-origin-ts" then " (newer)" else ""
  $"sync: {name} changed on both machines; kept {kept}{newer}. `dark conflicts` to choose."

/// Run one session on <param input> and <param output> until it ends. With <param once>, it ends as
/// soon as each end has sent the other everything it lacked.
let run (side : Side) (input : Stream) (output : Stream) (once : bool) : Ended =
  Directory.CreateDirectory side.workDir |> ignore<DirectoryInfo>
  let haveOut = fileIn side "have-out"
  let haveIn = fileIn side "have-in"
  let bundleOut = fileIn side "bundle-out"
  let bundleIn = fileIn side "bundle-in"
  // Every frame is read as it comes, on a thread of its own, whatever this end is busy with: both
  // ends send their have files at once, and a pipe holds far less than one.
  let frames = System.Threading.Channels.Channel.CreateUnbounded<Frame>()
  let pump () =
    iterate
      (fun () ->
        let frame = readFrame input
        frames.Writer.TryWrite frame |> ignore<bool>
        match frame with
        | Closed _ -> Choice2Of2()
        | _ -> Choice1Of2())
      ()
  use conn =
    new SqliteConnection($"Data Source={side.store};Mode=ReadOnly;Pooling=false")
  conn.Open()

  // A write the other end is not reading blocks, so it runs aside and is waited on in slices: turning
  // sync off still stops the session at once, and an end that stopped reading is given up on.
  let send (frame : Frame) : Option<Ended> =
    // A thread of its own rather than the pool's: a write must start even when every pool thread
    // is blocked elsewhere in the process.
    let written = TaskCompletionSource<Option<string>>()
    let writer =
      System.Threading.Thread(
        (fun () ->
          try
            writeFrame output frame
            written.SetResult None
          with e ->
            written.SetResult(Some e.Message)),
        IsBackground = true
      )
    writer.Start()
    let deadline = System.DateTime.UtcNow.AddSeconds 60.0
    let rec wait () =
      if written.Task.Wait 200 then
        written.Task.Result |> Option.map PeerGone
      elif not (side.keepGoing ()) then
        Some Stopped
      elif System.DateTime.UtcNow > deadline then
        Some(PeerGone "the other end stopped reading")
      else
        wait ()
    wait ()

  let sendBundle (counts : OpSync.Counts) (what : string) : Option<Ended> =
    let bytes = File.ReadAllBytes bundleOut
    side.log
      $"sent {what}: {counts.ops} ops, {counts.journal} entries, {counts.commits} commits, {bytes.Length} bytes"
    send (Bundle bytes)

  let receive (st : State) (bytes : byte[]) : Result<State, Ended> =
    File.WriteAllBytes(bundleIn, bytes)
    let started = System.Diagnostics.Stopwatch.StartNew()
    match (OpSync.import side.store bundleIn).Result with
    | Error reason -> Error(Refused reason)
    | Ok counts ->
      if changed counts then
        side.afterImport counts
        side.log
          $"received: {counts.ops} ops, {counts.dropped} dropped, {counts.committed} committed, {counts.journal} entries, applied in {started.ElapsedMilliseconds} ms"
      for c in counts.conflicts do
        side.say (conflictLine c)
      // What the import added is the other end's own, so the cursor steps over it, unless this
      // store had moved on its own first: then the next export sends both, and the other end
      // ignores the part it holds.
      let cursor =
        match st.cursor with
        | Some c when c.journal = counts.journalBefore ->
          Some { c with journal = counts.upTo.journal }
        | other -> other
      Ok { st with cursor = cursor; gotFull = true }

  let step (st : State) : Choice<State, Ended> =
    if once && st.gotFull && Option.isSome st.cursor then
      Choice2Of2 Exchanged
    else
      match frames.Reader.TryRead() with
      | true, frame ->
        match frame with
        | Closed reason -> Choice2Of2(PeerGone reason)
        | Ping -> Choice1Of2 st
        | Hello(version, name) ->
          if version <> OpSync.formatVersion then
            Choice2Of2(
              Refused
                $"{name} runs a dark that speaks sync format {version}; this one speaks {OpSync.formatVersion}"
            )
          else
            side.log $"hello from {name}"
            side.connected name
            Choice1Of2 { st with peerName = name }
        | Have bytes ->
          File.WriteAllBytes(haveIn, bytes)
          let clock = System.Diagnostics.Stopwatch.StartNew()
          let exported = (OpSync.export side.store haveIn bundleOut).Result
          side.log
            $"have of {bytes.Length} bytes answered in {clock.ElapsedMilliseconds} ms"
          match exported with
          | Error reason -> Choice2Of2(Refused reason)
          | Ok counts ->
            match sendBundle counts "catch-up" with
            | Some ended -> Choice2Of2 ended
            | None ->
              Choice1Of2
                { st with
                    cursor = Some counts.upTo
                    lastSent = System.DateTime.UtcNow }
        | Bundle bytes ->
          match receive st bytes with
          | Error ended -> Choice2Of2 ended
          | Ok st -> Choice1Of2 st
      | _ ->
        if frames.Reader.WaitToReadAsync().AsTask().Wait(side.pollMs) then
          Choice1Of2 st
        elif not (side.keepGoing ()) then
          Choice2Of2 Stopped
        else
          match st.cursor with
          | Some c ->
            let now = standing conn
            if now.journal > c.journal || now.blobs > c.blobs then
              match (OpSync.exportSince side.store c bundleOut).Result with
              | Error reason -> Choice2Of2(Refused reason)
              | Ok counts ->
                let sent =
                  if changed counts then sendBundle counts "change" else None
                match sent with
                | Some ended -> Choice2Of2 ended
                | None ->
                  Choice1Of2
                    { st with
                        cursor = Some counts.upTo
                        lastSent = System.DateTime.UtcNow }
            elif
              System.DateTime.UtcNow - st.lastSent > System.TimeSpan.FromSeconds 15.0
            then
              match send Ping with
              | Some ended -> Choice2Of2 ended
              | None -> Choice1Of2 { st with lastSent = System.DateTime.UtcNow }
            else
              Choice1Of2 st
          | None -> Choice1Of2 st

  try
    // Reading starts before anything is sent.
    Task.Factory.StartNew(pump, TaskCreationOptions.LongRunning) |> ignore<Task>
    let opening =
      match send (Hello(OpSync.formatVersion, OpSync.nameOf side.store)) with
      | Some ended -> Some ended
      | None ->
        let clock = System.Diagnostics.Stopwatch.StartNew()
        match (OpSync.writeHave side.store haveOut).Result with
        | Error reason -> Some(Refused reason)
        | Ok() ->
          side.log $"have written in {clock.ElapsedMilliseconds} ms"
          send (Have(File.ReadAllBytes haveOut))
    match opening with
    | Some ended -> ended
    | None ->
      iterate
        step
        { cursor = None
          peerName = ""
          gotFull = false
          lastSent = System.DateTime.UtcNow }
  finally
    for path in [ haveOut; haveIn; bundleOut; bundleIn ] do
      removeFile path


// ------------------
// The transport
// ------------------

/// The program that reaches the other end and runs `dark sync serve` there, speaking frames on its
/// stdin and stdout.
type Transport = { program : string; arguments : List<string> }

/// ssh to <param host>, with the person's own keys and config and never a prompt, running
/// <param command> (the `dark` on that machine). A connection that stops answering is given up on
/// after about 15 seconds.
let ssh (host : string) (command : string) : Transport =
  { program = "ssh"
    arguments =
      [ "-T"
        "-o"
        "BatchMode=yes"
        // Rides a shared connection the person already has open, but never leaves one behind.
        "-o"
        "ControlMaster=no"
        "-o"
        "ConnectTimeout=10"
        "-o"
        "ServerAliveInterval=5"
        "-o"
        "ServerAliveCountMax=3"
        host
        $"{command} sync serve" ] }

/// Why a connection failed, as far as what ssh said tells.
type Failure =
  /// The key was refused or the host key does not match: only the person can fix that.
  | NotAllowed of reason : string
  /// The other machine has no `dark` where the command says.
  | NoDark of reason : string
  /// Off, asleep, or offline: it will come back.
  | Unreachable of reason : string
  | NotSpoken of reason : string

let private lastLine (text : string) : string =
  text.Split('\n')
  |> Array.map (fun l -> l.Trim())
  |> Array.filter (fun l -> l <> "")
  |> Array.tryLast
  |> Option.defaultValue ""

let classify (exitCode : int) (stderr : string) : Failure =
  let line = lastLine stderr
  if
    stderr.Contains "Permission denied"
    || stderr.Contains "Host key verification failed"
    || stderr.Contains "REMOTE HOST IDENTIFICATION HAS CHANGED"
  then
    NotAllowed line
  elif
    exitCode = 127
    || stderr.Contains "command not found"
    || stderr.Contains "not found"
  then
    NoDark line
  elif line = "" then
    Unreachable $"the connection ended (exit {exitCode})"
  else
    Unreachable line

/// Start <param transport> and run a session over it. The child is always gone when this returns.
let connect
  (side : Side)
  (transport : Transport)
  (once : bool)
  : Ended * Option<Failure> =
  let psi = System.Diagnostics.ProcessStartInfo(transport.program)
  for a in transport.arguments do
    psi.ArgumentList.Add a
  psi.UseShellExecute <- false
  psi.RedirectStandardInput <- true
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  let started =
    try
      Ok(System.Diagnostics.Process.Start psi)
    with e ->
      Error e.Message
  match started with
  | Error reason -> PeerGone reason, Some(Unreachable reason)
  | Ok proc ->
    use proc = proc
    let stderr = proc.StandardError.ReadToEndAsync()
    let ended =
      try
        run side proc.StandardOutput.BaseStream proc.StandardInput.BaseStream once
      with e ->
        PeerGone e.Message
    let kill () =
      try
        proc.Kill true
      with _ ->
        ()
    // Closing its stdin is how the other end learns the session is over; it finishes what it is
    // importing first. Sync turned off does not wait for that, and a write still blocked on a pipe
    // nobody reads ends when the child does.
    if ended = Stopped then kill ()
    (try
      proc.StandardInput.Close()
     with _ ->
       ())
    // After one full exchange the other end may still be importing a big first bundle.
    let patience = if ended = Exchanged then 600000 else 10000
    if not (proc.WaitForExit patience) then kill ()
    proc.WaitForExit()
    let said = if stderr.Wait 1000 then stderr.Result else ""
    match ended with
    | PeerGone _ when proc.ExitCode <> 0 -> ended, Some(classify proc.ExitCode said)
    | PeerGone reason -> ended, Some(Unreachable reason)
    | Refused reason -> ended, Some(NotSpoken reason)
    | _ -> ended, None


// ------------------
// The watcher
// ------------------

/// What the watcher needs besides a session's side.
type Watch =
  {
    side : Side
    transport : Transport
    /// The other machine, as the person named it.
    peer : string
    /// How long the other machine may be out of reach before the person is told, once.
    patience : System.TimeSpan
    /// Where to record how the link stands, for `dark sync` to show.
    writeState : string -> unit
  }

type private Watching =
  { failingSince : Option<System.DateTime>
    lastSaid : Option<System.DateTime>
    backoffMs : int }

/// A moment as the person's clock shows it.
let clock (t : System.DateTime) : string = t.ToLocalTime().ToString "HH:mm"

/// Sleep about <param ms>, waking early when sync is turned off. True when it was.
let private nap (watch : Watch) (ms : int) : bool =
  let deadline = System.DateTime.UtcNow.AddMilliseconds(float ms)
  let rec go () =
    if not (watch.side.keepGoing ()) then
      true
    elif System.DateTime.UtcNow >= deadline then
      false
    else
      System.Threading.Thread.Sleep 200
      go ()
  go ()

/// Keep a session open to the other machine until sync is turned off, reconnecting with backoff.
/// What needs the person (a refused key, no dark there, out of reach for longer than the patience)
/// is said once, and again at most hourly while it lasts.
let watch (watch : Watch) : unit =
  let fresh = { failingSince = None; lastSaid = None; backoffMs = 500 }
  let step (w : Watching) : Choice<Watching, unit> =
    if not (watch.side.keepGoing ()) then
      Choice2Of2()
    else
      let started = System.DateTime.UtcNow
      watch.writeState $"connecting to {watch.peer}"
      let ended, failure = connect watch.side watch.transport false
      let now = System.DateTime.UtcNow
      // A session that ran for a while was a success, whatever ended it.
      let lasted = now - started > System.TimeSpan.FromSeconds 20.0
      match ended, failure with
      | Stopped, _ -> Choice2Of2()
      | _, None ->
        watch.side.log $"session ended: {ended}"
        Choice1Of2 fresh
      | _, Some _ when lasted ->
        watch.side.log
          $"session ended after {int (now - started).TotalSeconds} s: {ended}"
        watch.writeState
          $"offline since {clock now}: lost the connection to {watch.peer}"
        if nap watch 500 then
          Choice2Of2()
        else
          Choice1Of2 { fresh with failingSince = Some now; backoffMs = 1000 }
      | _, Some failure ->
        let since = w.failingSince |> Option.defaultValue now
        let hourly =
          match w.lastSaid with
          | None -> true
          | Some t -> now - t > System.TimeSpan.FromHours 1.0
        let line, reason =
          match failure with
          | NotAllowed reason ->
            Some
              $"sync: {watch.peer} refused this machine's ssh key ({reason}). Nothing syncs until `ssh {watch.peer}` works without a prompt.",
            reason
          | NoDark reason ->
            Some
              $"sync: there is no dark to run on {watch.peer} ({reason}). `dark sync add {watch.peer} --command <path>` says where it is.",
            reason
          | NotSpoken reason ->
            Some $"sync: {reason}. Update dark on both machines.", reason
          | Unreachable reason ->
            (if now - since >= watch.patience then
               Some
                 $"sync: can't reach {watch.peer} since {clock since}; your work is kept here and goes when it's back."
             else
               None),
            reason
        watch.side.log $"connection failed: {reason}"
        watch.writeState $"offline since {clock since}: {reason}"
        let said =
          match line with
          // Out of reach is said once an outage; what only the person can fix, hourly.
          | Some l when
            (match failure with
             | Unreachable _ -> Option.isNone w.lastSaid
             | _ -> hourly)
            ->
            watch.side.say l
            Some now
          | _ -> w.lastSaid
        if nap watch w.backoffMs then
          Choice2Of2()
        else
          Choice1Of2
            { failingSince = Some since
              lastSaid = said
              backoffMs = min 30000 (w.backoffMs * 2) }
  iterate step fresh
  watch.writeState "off"
