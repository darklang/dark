/// Keeping two stores in step over one pipe: the session both ends run, and the watcher that holds a
/// session open to a machine you own over ssh.
///
/// The two ends are the same code. Each says hello with who it is, then asks the other for what it
/// lacks: from the point the two last agreed on, when it remembers one (`OpSync.receivedFrom`), and by
/// sending a have file of what it holds when it does not. From then on each end watches its own store
/// and sends what changed past a cursor as soon as it sees it (`OpSync.exportSince`). What arrives is
/// imported (`OpSync.import`), folded and acknowledged, and the acknowledgement is the point the next
/// session starts from. So a save on either machine reaches the other within one poll of the store
/// and one trip over the pipe, in both directions, over one connection, with no server: the other end
/// is `dark sync serve` started by ssh.
///
/// Nothing here waits on the network for a person. The watcher is its own process; a command only
/// checks that one is running, and the first command after a restart waits a moment for the catch-up
/// (`awaitCatchUp`). When the other machine cannot be reached the journal keeps growing, and the next
/// connection catches up from the last agreed point.
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

/// `iterate` for a step that waits: each step is awaited, so the loop holds no thread between them.
let private iterateTask (step : 's -> Task<Choice<'s, 'r>>) (start : 's) : Task<'r> =
  task {
    let mutable at = Choice1Of2 start
    while (match at with
           | Choice1Of2 _ -> true
           | Choice2Of2 _ -> false) do
      match at with
      | Choice1Of2 s ->
        let! next = step s
        at <- next
      | Choice2Of2 _ -> ()
    match at with
    | Choice2Of2 r -> return r
    | Choice1Of2 _ ->
      return Exception.raiseInternal "the loop ended without a result" []
  }


// ------------------
// How the link stands
// ------------------

/// How the link stands, as the watcher records it: for `dark sync`, the line `dark status` prints,
/// and the first command after a restart, which waits for `caughtUp`.
type Standing =
  {
    /// "connecting", "connected", "offline" or "off".
    state : string
    /// Why it is offline, as far as anyone can tell.
    detail : string
    /// When it came to be in this state.
    since : Option<System.DateTime>
    /// The last time the other end said anything.
    contact : Option<System.DateTime>
    /// When the other end's catch-up last landed here, folded.
    caughtUp : Option<System.DateTime>
    /// The other store's `instance.id`, once it has said hello.
    peer : string
    /// The other end said this machine's token only reads.
    readOnly : bool
  }

module Standing =
  let empty : Standing =
    { state = ""
      detail = ""
      since = None
      contact = None
      caughtUp = None
      peer = ""
      readOnly = false }

  let private stamp (t : Option<System.DateTime>) : string =
    t
    |> Option.map (fun t -> t.ToUniversalTime().ToString("o"))
    |> Option.defaultValue ""

  let private parseStamp (text : string) : Option<System.DateTime> =
    match
      System.DateTime.TryParse(
        text,
        System.Globalization.CultureInfo.InvariantCulture,
        System.Globalization.DateTimeStyles.RoundtripKind
      )
    with
    | true, t -> Some(t.ToUniversalTime())
    | _ -> None

  /// What <param path> says, or `empty` when there is nothing readable there.
  let read (path : string) : Standing =
    let lines =
      try
        File.ReadAllLines path
      with _ ->
        [||]
    let field (key : string) =
      lines
      |> Array.tryPick (fun line ->
        if line.StartsWith(key + " ") then Some(line.Substring(key.Length + 1))
        elif line = key then Some ""
        else None)
      |> Option.defaultValue ""
    { state = field "state"
      detail = field "detail"
      since = parseStamp (field "since")
      contact = parseStamp (field "contact")
      caughtUp = parseStamp (field "caughtUp")
      peer = field "peer"
      readOnly = field "access" = "read" }

  /// Replace what <param path> says, whole, so a reader never sees half of it.
  let write (path : string) (s : Standing) : unit =
    try
      Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore<DirectoryInfo>
      let text =
        [ $"state {s.state}"
          $"detail {s.detail.Replace('\n', ' ')}"
          $"since {stamp s.since}"
          $"contact {stamp s.contact}"
          $"caughtUp {stamp s.caughtUp}"
          $"peer {s.peer}"
          "access " + (if s.readOnly then "read" else "write") ]
        |> String.concat "\n"
      let temporary = path + ".new"
      File.WriteAllText(temporary, text + "\n")
      File.Move(temporary, path, true)
    with _ ->
      ()

  let update (path : string) (change : Standing -> Standing) : unit =
    write path (change (read path))

  /// Into <param state> from now on; `since` stays when it was already in it.
  let into (state : string) (detail : string) (s : Standing) : Standing =
    if s.state = state then
      { s with detail = detail }
    else
      { s with state = state; detail = detail; since = Some System.DateTime.UtcNow }


// ------------------
// Frames
// ------------------

/// What travels over the pipe, one frame at a time: a kind byte, a 4-byte big-endian length, then
/// the payload. Have and bundle frames carry the SQLite files `OpSync` writes, whole; a cursor is
/// two 8-byte big-endian numbers.
type Frame =
  /// The format both ends must speak, who this end is (its name and its `instance.id`), whether the
  /// other end may only read (a server says so to a read-only token), and this end's clock.
  | Hello of Hello
  /// "I hold everything of yours up to here; send what came after."
  | Resume of OpSync.Cursor
  /// "That point is not one I can send from (the store was emptied since); send a have file."
  | Rehave
  | Have of byte[]
  | Bundle of byte[]
  /// "Nothing past your point; I stand here."
  | UpToDate of OpSync.Cursor
  /// "I now hold everything of yours up to here."
  | Ack of OpSync.Cursor
  /// Nothing to say; sent every few seconds, whatever this end is busy with, so the other end
  /// knows it is alive.
  | Ping
  /// "I am ending this session, and why": sent before a refusal, so the other end can say it.
  | Bye of reason : string
  /// Not sent: what reading says when the other end has gone, and why.
  | Closed of reason : string

and Hello =
  { version : string
    name : string
    id : string
    readOnly : bool
    clock : Option<System.DateTime> }

/// A frame larger than this is refused rather than allocated, unless the end says otherwise
/// (`Side.maxFrame`): no honest bundle comes near it.
let defaultMaxFrame = 1 <<< 30

let private cursorBytes (c : OpSync.Cursor) : byte[] =
  let bytes = Array.zeroCreate<byte> 16
  System.Buffers.Binary.BinaryPrimitives.WriteInt64BigEndian(
    System.Span<byte>(bytes, 0, 8),
    c.journal
  )
  System.Buffers.Binary.BinaryPrimitives.WriteInt64BigEndian(
    System.Span<byte>(bytes, 8, 8),
    c.blobs
  )
  bytes

let private cursorOfBytes (bytes : byte[]) : Option<OpSync.Cursor> =
  if bytes.Length <> 16 then
    None
  else
    Some
      { OpSync.Cursor.journal =
          System.Buffers.Binary.BinaryPrimitives.ReadInt64BigEndian(
            System.ReadOnlySpan<byte>(bytes, 0, 8)
          )
        blobs =
          System.Buffers.Binary.BinaryPrimitives.ReadInt64BigEndian(
            System.ReadOnlySpan<byte>(bytes, 8, 8)
          ) }

let writeFrame (output : Stream) (frame : Frame) : unit =
  let kind, payload =
    match frame with
    | Hello h ->
      let mode = if h.readOnly then "read" else "write"
      let clock =
        h.clock |> Option.map (fun t -> t.ToString("o")) |> Option.defaultValue ""
      'H',
      System.Text.Encoding.UTF8.GetBytes(
        $"{h.version}\n{h.name}\n{h.id}\n{mode}\n{clock}"
      )
    | Resume c -> 'R', cursorBytes c
    | Rehave -> 'Q', [||]
    | Have bytes -> 'V', bytes
    | Bundle bytes -> 'B', bytes
    | UpToDate c -> 'U', cursorBytes c
    | Ack c -> 'A', cursorBytes c
    | Ping -> 'P', [||]
    | Bye reason -> 'X', System.Text.Encoding.UTF8.GetBytes reason
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
/// Grows as the bytes arrive, so a header naming a frame of the largest size costs nothing until
/// the other end actually sends that much.
let private readExactly (input : Stream) (count : int) : Option<byte[]> =
  use buffer = new MemoryStream()
  let chunk = Array.zeroCreate<byte> (min count (64 * 1024))
  let rec fill (left : int) =
    if left = 0 then
      true
    else
      let n = input.Read(chunk, 0, min left chunk.Length)
      if n <= 0 then
        false
      else
        buffer.Write(chunk, 0, n)
        fill (left - n)
  if fill count then Some(buffer.ToArray()) else None

/// The next frame, blocking until it is whole. A truncated or unknown frame closes the session: a
/// half-delivered bundle must never count as delivered.
let readFrame (maxPayload : int) (input : Stream) : Frame =
  try
    match readExactly input 5 with
    | None -> Closed "the other end closed the connection"
    | Some header ->
      let length =
        System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(
          System.ReadOnlySpan<byte>(header, 1, 4)
        )
      if length < 0 || length > maxPayload then
        Closed
          $"the other end sent a frame of {length} bytes, over the limit of {maxPayload}"
      else
        match readExactly input length with
        | None -> Closed "the connection ended in the middle of a frame"
        | Some payload ->
          let cursorFrame (make : OpSync.Cursor -> Frame) =
            match cursorOfBytes payload with
            | Some c -> make c
            | None -> Closed "the other end sent a point that did not parse"
          match char header[0] with
          | 'H' ->
            let fields = System.Text.Encoding.UTF8.GetString(payload).Split('\n')
            let field (i : int) = if i < fields.Length then fields[i] else ""
            // Only the version is read from an end that speaks another format: it is refused by it.
            Hello
              { version = field 0
                name = field 1
                id = (if field 2 = "" then field 1 else field 2)
                readOnly = field 3 = "read"
                clock =
                  match
                    System.DateTime.TryParse(
                      field 4,
                      System.Globalization.CultureInfo.InvariantCulture,
                      System.Globalization.DateTimeStyles.RoundtripKind
                    )
                  with
                  | true, t -> Some(t.ToUniversalTime())
                  | _ -> None }
          | 'R' -> cursorFrame Resume
          | 'Q' -> Rehave
          | 'V' -> Have payload
          | 'B' -> Bundle payload
          | 'U' -> cursorFrame UpToDate
          | 'A' -> cursorFrame Ack
          | 'P' -> Ping
          | 'X' -> Bye(System.Text.Encoding.UTF8.GetString payload)
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
    /// After a bundle added something: fold it, so the next command finds it applied. Before the
    /// bundle is acknowledged, so the catch-up counts as landed only once it is folded.
    afterImport : OpSync.Counts -> unit
    /// A line for the person (a conflict, a refusal). Not printed here.
    say : string -> unit
    /// Record how the link stands (`Standing`).
    note : (Standing -> Standing) -> unit
    /// A line for the log, for whoever is measuring or debugging.
    log : string -> unit
    /// False ends the session: sync was turned off.
    keepGoing : unit -> bool
    /// How often to look at the store for a change.
    pollMs : int
    /// How long the other end may say nothing before the session is given up: it is asleep,
    /// stopped, or the network dropped without a word. Each end pings four times as often.
    quietMs : int
    /// The largest frame this end takes.
    maxFrame : int
    /// On a server: whose token the other end came with. Its drafts go only to that person's
    /// stores, and a read-only token sends nothing. None between one person's own machines.
    account : Option<SyncTokens.Account>
  }

/// How a session ended.
type Ended =
  /// The other end went away, or the pipe broke.
  | PeerGone of reason : string
  /// `keepGoing` said to stop.
  | Stopped
  /// One full exchange each way is done, and the other end has said it holds this end's (`once`).
  | Exchanged
  /// The other end speaks another format, or sent something unreadable.
  | Refused of reason : string
  /// Nothing came from the other end for `quietMs`.
  | Unanswered

type private State =
  {
    /// How far into this store the other end has been sent.
    cursor : Option<OpSync.Cursor>
    /// The other store's (id, name), once it said hello.
    peer : Option<string * string>
    /// The other end's catch-up has arrived and been folded here.
    caughtUp : bool
    /// How far into this store's journal the other end has said it holds everything.
    acked : Option<int64>
    /// When the other end last said anything.
    heard : System.DateTime
    /// The other end said this one may only read: nothing of this store's goes to it.
    readOnly : bool
    /// Where the journal's trim goes on from, a window at a time while nothing else is going on
    /// (`OpSync.trimStep`). Started by the session's first acknowledgement.
    trimming : Option<int64>
    /// This end asked with a have file: the two stores are comparing whole, and the catch-up that
    /// answers joins same-named branches (`OpSync.importFirst`).
    fromScratch : bool
  }

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

/// Anything worth sending: the branch registry goes with every bundle, and alone says nothing new.
let private carries (c : OpSync.Counts) : bool =
  c.ops + c.commits + c.blobs + c.journal > 0L

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
  let who (cand : ScmDetect.Candidate) =
    if cand.side = "local" then "this machine" else cand.author
  match c.candidates |> List.partition (fun cand -> cand.removed) with
  | [ ended ], [ edited ] ->
    let how = if ended.text.StartsWith "renamed" then ended.text else "removed"
    let outcome =
      if c.autoResolvedTo = "" then
        if ended.text.StartsWith "renamed" then
          "the rename stands"
        else
          "it stays removed"
      else
        "the change is kept"
    $"sync: {name} was {how} on {who ended} and changed on {who edited}; {outcome}. `dark conflicts` to choose."
  | _ ->
    let kept =
      match
        c.candidates |> List.tryFind (fun cand -> cand.hash = c.autoResolvedTo)
      with
      | Some cand when cand.side = "local" -> "this machine's"
      | Some cand -> $"{cand.author}'s"
      | None -> "one"
    let newer = if c.reason = "later-origin-ts" then " (newer)" else ""
    $"sync: {name} changed on both machines; kept {kept}{newer}. `dark conflicts` to choose."

/// One import and its fold at a time in this process: a server runs a session per connection, all on
/// one store, and the fold works on the process's one view of it.
let private importing = obj ()

/// How far ahead of this end's clock a change may be stamped before a server turns it away. A
/// stamp decides which of two edits made at once wins, so a machine whose clock runs ahead would
/// win every one of them, and pull every other machine's clock forward with it.
let private clockTolerance = System.TimeSpan.FromMinutes 5.0

/// How far apart two ends' clocks may be before the person is told.
let private skewWorthSaying = System.TimeSpan.FromMinutes 2.0

/// Run one session on <param input> and <param output> until it ends, setting <param greeted> when
/// the other end says who it is. With <param once>, it ends as soon as each end has sent the other
/// everything it lacked and the other has said it holds it.
let private session
  (greeted : System.Threading.ManualResetEventSlim)
  (side : Side)
  (input : Stream)
  (output : Stream)
  (once : bool)
  : Task<Ended> =
  Directory.CreateDirectory side.workDir |> ignore<DirectoryInfo>
  let haveOut = fileIn side "have-out"
  let haveIn = fileIn side "have-in"
  let bundleOut = fileIn side "bundle-out"
  let bundleIn = fileIn side "bundle-in"
  let (selfId, selfName) = OpSync.identityOf side.store
  let scope =
    match side.account with
    | Some a -> OpSync.DraftsOf a.person
    | None -> OpSync.AllDrafts
  let readOnlyAccount = side.account |> Option.exists (fun a -> a.readOnly)
  // Every frame is read as it comes, on a thread of its own, whatever this end is busy with: a have
  // file is far bigger than a pipe holds, and both ends may be sending at once.
  let frames = System.Threading.Channels.Channel.CreateUnbounded<Frame>()
  let pump () =
    iterate
      (fun () ->
        let frame = readFrame side.maxFrame input
        frames.Writer.TryWrite frame |> ignore<bool>
        match frame with
        | Closed _ -> Choice2Of2()
        | _ -> Choice1Of2())
      ()
  // Closed when the session ends, in the `finally` below.
  let conn =
    new SqliteConnection($"Data Source={side.store};Mode=ReadOnly;Pooling=false")
  conn.Open()

  // The pings are written from a thread of their own, so each frame is written whole under a lock.
  let writing = obj ()
  let write (frame : Frame) : unit = lock writing (fun () -> writeFrame output frame)

  // A write the other end is not reading blocks, so it runs aside and is waited on in slices: turning
  // sync off still stops the session at once, and an end that stopped reading is given up on.
  let send (frame : Frame) : Task<Option<Ended>> =
    // A thread of its own rather than the pool's: a write must start even when every pool thread
    // is blocked elsewhere in the process.
    let written = TaskCompletionSource<Option<string>>()
    let writer =
      System.Threading.Thread(
        (fun () ->
          try
            write frame
            written.SetResult None
          with e ->
            written.SetResult(Some e.Message)),
        IsBackground = true
      )
    writer.Start()
    let deadline = System.DateTime.UtcNow.AddSeconds 60.0
    let rec wait () =
      task {
        let! first = Task.WhenAny(written.Task :> Task, Task.Delay 200)
        if obj.ReferenceEquals(first, written.Task) then
          let! failed = written.Task
          return failed |> Option.map PeerGone
        elif not (side.keepGoing ()) then
          return Some Stopped
        elif System.DateTime.UtcNow > deadline then
          return Some(PeerGone "the other end stopped reading")
        else
          return! wait ()
      }
    wait ()

  let sendBundle (counts : OpSync.Counts) (what : string) : Task<Option<Ended>> =
    let bytes = File.ReadAllBytes bundleOut
    side.log
      $"sent {what}: {counts.ops} ops, {counts.journal} entries, {counts.commits} commits, {bytes.Length} bytes"
    send (Bundle bytes)

  /// Send what changed after <param since>: a bundle, or that nothing did. Answers the cursor the
  /// other end now stands at.
  let sendSince
    (since : OpSync.Cursor)
    (peerId : string)
    (what : string)
    : Task<Result<Option<OpSync.Cursor>, Ended>> =
    task {
      let now = standing conn
      if now.journal <= since.journal && now.blobs <= since.blobs then
        match! send (UpToDate now) with
        | Some ended -> return Error ended
        | None -> return Ok(Some now)
      else
        match OpSync.exportSinceFor scope side.store since peerId bundleOut with
        | Error reason -> return Error(Refused reason)
        | Ok counts when carries counts ->
          match! sendBundle counts what with
          | Some ended -> return Error ended
          | None -> return Ok(Some counts.upTo)
        | Ok counts ->
          match! send (UpToDate counts.upTo) with
          | Some ended -> return Error ended
          | None -> return Ok(Some counts.upTo)
    }

  let sendHave () : Task<Option<Ended>> =
    let clock = System.Diagnostics.Stopwatch.StartNew()
    match OpSync.writeHave side.store haveOut with
    | Error reason -> Task.FromResult(Some(Refused reason))
    | Ok() ->
      let bytes = File.ReadAllBytes haveOut
      side.log
        $"have of {bytes.Length} bytes written in {clock.ElapsedMilliseconds} ms"
      send (Have bytes)

  /// This end now holds everything of the other's up to <param upTo>: remember it, so the next
  /// session starts there, and say so.
  let holds (st : State) (upTo : OpSync.Cursor) : Task<Result<State, Ended>> =
    task {
      match st.peer with
      | None ->
        return Error(Refused "the other end sent changes before saying who it is")
      | Some(id, name) ->
        OpSync.recordReceived side.store id name upTo
        match! send (Ack upTo) with
        | Some ended -> return Error ended
        | None ->
          if not st.caughtUp then
            side.note (fun s -> { s with caughtUp = Some System.DateTime.UtcNow })
          return Ok { st with caughtUp = true }
    }

  /// On a server, why a bundle is stamped too far past this end's clock to take, if it is.
  let aheadOfClock () : Option<string> =
    match side.account with
    | None -> None
    | Some _ ->
      match
        System.DateTime.TryParse(
          OpSync.newestStampIn bundleIn,
          System.Globalization.CultureInfo.InvariantCulture,
          System.Globalization.DateTimeStyles.AdjustToUniversal
          ||| System.Globalization.DateTimeStyles.AssumeUniversal
        )
      with
      | true, newest when newest > System.DateTime.UtcNow + clockTolerance ->
        let ahead = newest - System.DateTime.UtcNow
        let howFar =
          if ahead.TotalDays >= 2.0 then
            $"{int ahead.TotalDays} days"
          else
            $"{int ahead.TotalMinutes} minutes"
        Some
          $"this machine's changes are stamped about {howFar} ahead of the server's clock; they wait until that time comes. Is this machine's clock right?"
      | _ -> None

  let receive (st : State) (bytes : byte[]) : Task<Result<State, Ended>> =
    File.WriteAllBytes(bundleIn, bytes)
    let started = System.Diagnostics.Stopwatch.StartNew()
    let imported =
      match side.account, aheadOfClock () with
      | Some a, _ when a.readOnly ->
        Error $"{a.person}'s token can only read from this server"
      | Some a, _ when not (SyncTokens.isLive side.store a.tokenId) ->
        Error $"this server no longer takes {a.person}'s token"
      | _, Some reason -> Error reason
      | _, None ->
        lock importing (fun () ->
          let importBundle =
            if st.fromScratch && not st.caughtUp then
              OpSync.importFirst
            else
              OpSync.import
          match importBundle side.store bundleIn with
          | Error reason -> Error reason
          | Ok counts ->
            let importMs = started.ElapsedMilliseconds
            if changed counts then side.afterImport counts
            Ok(counts, importMs))
    match imported with
    | Error reason -> Task.FromResult(Error(Refused reason))
    | Ok(counts, importMs) ->
      if changed counts then
        side.log
          $"received {bytes.Length} bytes: {counts.ops} ops, {counts.dropped} dropped, {counts.committed} committed, {counts.journal} entries, applied in {started.ElapsedMilliseconds} ms ({importMs} ms importing)"
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
      holds { st with cursor = cursor } counts.sentUpTo

  let peerIdOf (st : State) : string =
    st.peer |> Option.map fst |> Option.defaultValue ""

  let answered (st : State) (sent : Result<Option<OpSync.Cursor>, Ended>) =
    match sent with
    | Error ended -> Choice2Of2 ended
    | Ok cursor -> Choice1Of2 { st with cursor = Option.orElse st.cursor cursor }

  let answeredBy
    (st : State)
    (sending : Task<Result<Option<OpSync.Cursor>, Ended>>)
    =
    task {
      let! sent = sending
      return answered st sent
    }

  /// Whether <param sending> ended the session, or the session goes on as <param st>.
  let goOnAfter (st : State) (sending : Task<Option<Ended>>) =
    task {
      match! sending with
      | Some ended -> return Choice2Of2 ended
      | None -> return Choice1Of2 st
    }

  let step (st : State) : Task<Choice<State, Ended>> =
    if
      once
      && st.caughtUp
      && (st.readOnly || (Option.isSome st.cursor && Option.isSome st.acked))
    then
      Task.FromResult(Choice2Of2 Exchanged)
    // Turned off: nothing more is taken, even what already arrived.
    elif not (side.keepGoing ()) then
      Task.FromResult(Choice2Of2 Stopped)
    else
      match frames.Reader.TryRead() with
      | true, frame ->
        let heard =
          match frame with
          | Closed _ -> false
          | _ -> true
        let st =
          if heard then
            side.note (fun s -> { s with contact = Some System.DateTime.UtcNow })
            { st with heard = System.DateTime.UtcNow }
          else
            st
        match frame with
        | Closed reason -> Task.FromResult(Choice2Of2(PeerGone reason))
        | Bye reason -> Task.FromResult(Choice2Of2(Refused reason))
        | Ping -> Task.FromResult(Choice1Of2 st)
        | Hello h when h.version <> OpSync.formatVersion ->
          Task.FromResult(
            Choice2Of2(
              Refused
                $"{h.name} runs a dark that speaks sync format {h.version}; this one speaks {OpSync.formatVersion}"
            )
          )
        | Hello h ->
          let bound =
            match side.account with
            | None -> Ok()
            | Some a ->
              OpSync.bindPerson side.store h.id h.name a.person
              |> Result.mapError (fun owner ->
                $"the store on {h.name} syncs as {owner}; {a.person}'s token can't use it")
          match bound with
          | Error reason -> Task.FromResult(Choice2Of2(Refused reason))
          | Ok() ->
            let who =
              match side.account with
              | Some a -> $"{h.name} ({a.person}, token {a.tokenId})"
              | None -> h.name
            side.log $"hello from {who}"
            greeted.Set()
            side.note (fun s ->
              { Standing.into "connected" "" s with
                  peer = h.id
                  readOnly = h.readOnly })
            match h.clock with
            | Some theirs when Option.isNone side.account ->
              let skew = System.DateTime.UtcNow - theirs
              if skew.Duration() > skewWorthSaying then
                let minutes = int (skew.Duration().TotalMinutes)
                let way =
                  if skew > System.TimeSpan.Zero then "ahead of" else "behind"
                side.say
                  $"sync: this machine's clock is about {minutes} minutes {way} {h.name}'s. Set it right (NTP), or edits made at the same time may settle the wrong way."
            | _ -> ()
            let st = { st with peer = Some(h.id, h.name); readOnly = h.readOnly }
            // From the point the two last agreed on, or from scratch. Nothing is asked of an end
            // whose token only reads.
            if readOnlyAccount then
              Task.FromResult(Choice1Of2 { st with fromScratch = false })
            else
              match OpSync.receivedFrom side.store h.id with
              | Some c ->
                side.log $"resuming from {c.journal}/{c.blobs}"
                goOnAfter { st with fromScratch = false } (send (Resume c))
              | None -> goOnAfter { st with fromScratch = true } (sendHave ())
        | Resume c ->
          if OpSync.reaches side.store c then
            answeredBy st (sendSince c (peerIdOf st) "catch-up")
          else
            side.log
              $"asked to resume from {c.journal}/{c.blobs}, which this store never reached"
            goOnAfter st (send Rehave)
        | Rehave -> goOnAfter { st with fromScratch = true } (sendHave ())
        | Have bytes ->
          File.WriteAllBytes(haveIn, bytes)
          let clock = System.Diagnostics.Stopwatch.StartNew()
          let exported = OpSync.exportFor scope side.store haveIn bundleOut
          side.log
            $"have of {bytes.Length} bytes answered in {clock.ElapsedMilliseconds} ms"
          match exported with
          | Error reason -> Task.FromResult(Choice2Of2(Refused reason))
          | Ok counts ->
            let sending =
              if carries counts then
                sendBundle counts "catch-up"
              else
                send (UpToDate counts.upTo)
            task {
              match! sending with
              | Some ended -> return answered st (Error ended)
              | None -> return answered st (Ok(Some counts.upTo))
            }
        | Bundle bytes ->
          task {
            match! receive st bytes with
            | Error ended -> return Choice2Of2 ended
            | Ok st -> return Choice1Of2 st
          }
        | UpToDate c ->
          task {
            match! holds st c with
            | Error ended -> return Choice2Of2 ended
            | Ok st -> return Choice1Of2 st
          }
        | Ack c ->
          match st.peer with
          | None ->
            Task.FromResult(
              Choice2Of2(
                Refused "the other end acknowledged before saying who it is"
              )
            )
          | Some(id, name) ->
            OpSync.recordAcked side.store id name c.journal false |> ignore<int64>
            Task.FromResult(
              Choice1Of2
                { st with
                    acked = Some c.journal
                    trimming =
                      if Option.isNone st.acked then Some 0L else st.trimming }
            )
      | _ ->
        match st.trimming with
        // Nothing to read: the trim goes on, a window at a time.
        | Some after ->
          let dropped, next = OpSync.trimStep side.store after
          if dropped > 0L then
            side.log $"trimmed {dropped} journal entries every peer holds"
          Task.FromResult(Choice1Of2 { st with trimming = next })
        | _ ->
          task {
            let ready = frames.Reader.WaitToReadAsync().AsTask()
            let! first = Task.WhenAny(ready :> Task, Task.Delay side.pollMs)
            if obj.ReferenceEquals(first, ready) then
              return Choice1Of2 st
            elif not (side.keepGoing ()) then
              return Choice2Of2 Stopped
            elif
              (System.DateTime.UtcNow - st.heard).TotalMilliseconds > float
                side.quietMs
            then
              return Choice2Of2 Unanswered
            else
              match st.cursor with
              | Some c when not st.readOnly ->
                let now = standing conn
                if now.journal > c.journal || now.blobs > c.blobs then
                  match
                    OpSync.exportSinceFor scope side.store c (peerIdOf st) bundleOut
                  with
                  | Error reason -> return Choice2Of2(Refused reason)
                  | Ok counts ->
                    let! sent =
                      if carries counts then
                        sendBundle counts "change"
                      else
                        Task.FromResult None
                    match sent with
                    | Some ended -> return Choice2Of2 ended
                    | None -> return Choice1Of2 { st with cursor = Some counts.upTo }
                else
                  return Choice1Of2 st
              | _ -> return Choice1Of2 st
          }

  // Pings go out on a timer of their own, not between steps: a step can be a long import, and the
  // other end must not take that for silence. Off the session's thread (`ConfigureAwait(false)`),
  // since a ping to an end that stopped reading blocks its writer.
  let pinging (stop : System.Threading.CancellationToken) : Task =
    task {
      try
        while true do
          do! Task.Delay(side.quietMs / 4, stop).ConfigureAwait(false)
          try
            write Ping
          with _ ->
            ()
      with :? System.OperationCanceledException ->
        ()
    }

  task {
    use stopPinging = new System.Threading.CancellationTokenSource()
    try
      // Reading starts before anything is sent.
      Task.Factory.StartNew(pump, TaskCreationOptions.LongRunning) |> ignore<Task>
      let hello =
        { version = OpSync.formatVersion
          name = selfName
          id = selfId
          readOnly = readOnlyAccount
          clock = Some System.DateTime.UtcNow }
      match! send (Hello hello) with
      | Some ended -> return ended
      | None ->
        Task.Run(fun () -> pinging stopPinging.Token) |> ignore<Task>
        let! ended, why =
          task {
            try
              let! ended =
                iterateTask
                  step
                  { cursor = None
                    peer = None
                    caughtUp = false
                    acked = None
                    heard = System.DateTime.UtcNow
                    readOnly = false
                    trimming = None
                    fromScratch = false }
              match ended with
              | Refused reason -> return ended, Some reason
              | _ -> return ended, None
            with e ->
              return PeerGone e.Message, Some $"{selfName} failed: {e.Message}"
          }
        // Say why, so the other end can tell its person rather than retry in silence.
        match why with
        | Some reason ->
          try
            write (Bye reason)
          with _ ->
            ()
        | None -> ()
        return ended
    finally
      stopPinging.Cancel()
      conn.Dispose()
      for path in [ haveOut; haveIn; bundleOut; bundleIn ] do
        removeFile path
  }


let run
  (side : Side)
  (input : Stream)
  (output : Stream)
  (once : bool)
  : Task<Ended> =
  task {
    use greeted = new System.Threading.ManualResetEventSlim(false)
    return! session greeted side input output once
  }


// ------------------
// The transport
// ------------------

/// How to reach the other end: a program that runs `dark sync serve` there and speaks frames on its
/// stdin and stdout (ssh), or a sync server's address and the token it knows this person by.
type Transport =
  | Program of program : string * arguments : List<string>
  | Server of url : System.Uri * token : string

/// A server's address as a person may type it: `https://host[:port][/path]`, or `http://` to a
/// machine that is this one or on a private network (a token must not cross the internet in the
/// clear). None when it is not one.
let serverUrl (text : string) : Option<System.Uri> =
  match System.Uri.TryCreate(text, System.UriKind.Absolute) with
  | true, uri when uri.Scheme = "https" || uri.Scheme = "http" -> Some uri
  | _ -> None

/// Whether <param uri> may be spoken to without TLS: this machine, a private network, or a tailnet
/// (WireGuard already encrypts it).
let plainHttpAllowed (uri : System.Uri) : bool =
  let host = uri.IdnHost
  match System.Net.IPAddress.TryParse host with
  | true, ip ->
    System.Net.IPAddress.IsLoopback ip
    || (let b = ip.GetAddressBytes()
        b.Length = 4
        && (b[0] = 10uy
            || (b[0] = 172uy && b[1] >= 16uy && b[1] < 32uy)
            || (b[0] = 192uy && b[1] = 168uy)
            || (b[0] = 100uy && b[1] >= 64uy && b[1] < 128uy)))
  | _ ->
    host = "localhost"
    || host.EndsWith ".localhost"
    || host.EndsWith ".ts.net"
    || host.EndsWith ".internal"
    || not (host.Contains ".")

/// ssh to <param host>, with the person's own keys and config and never a prompt, running
/// <param command> (the `dark` on that machine). A connection that stops answering is given up on
/// after about 15 seconds.
let ssh (host : string) (command : string) : Transport =
  Program(
    "ssh",
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
      $"{command} sync serve" ]
  )

/// What the link's standing says when a server refused this machine's token.
let tokenRefusedDetail = "token refused"

/// Why a connection failed, as far as what ssh said tells.
type Failure =
  /// The key was refused or the host key does not match: only the person can fix that.
  | NotAllowed of reason : string
  /// The server does not take this token: it never knew it, or it was revoked.
  | TokenRefused of revoked : bool
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

/// Start <param program> and run a session over its stdin and stdout. The child is always gone when
/// this returns.
let private connectProgram
  (side : Side)
  (program : string)
  (arguments : List<string>)
  (once : bool)
  : Task<Ended * Option<Failure> * bool> =
  let psi = System.Diagnostics.ProcessStartInfo(program)
  for a in arguments do
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
  | Error reason -> Task.FromResult(PeerGone reason, Some(Unreachable reason), false)
  | Ok proc ->
    task {
      use proc = proc
      use greeted = new System.Threading.ManualResetEventSlim(false)
      let stderr = proc.StandardError.ReadToEndAsync()
      let! ended =
        task {
          try
            return!
              session
                greeted
                side
                proc.StandardOutput.BaseStream
                proc.StandardInput.BaseStream
                once
          with e ->
            return PeerGone e.Message
        }
      let kill () =
        try
          proc.Kill true
        with _ ->
          ()
      // Closing its stdin is how the other end learns the session is over. Sync turned off does not
      // wait for that, nor does an end that stopped answering, which would never close its side; a
      // write still blocked on a pipe nobody reads ends when the child does.
      if ended = Stopped || ended = Unanswered then kill ()
      try
        proc.StandardInput.Close()
      with _ ->
        ()
      let exited = proc.WaitForExitAsync()
      let! first = Task.WhenAny(exited, Task.Delay 10000)
      if not (obj.ReferenceEquals(first, exited)) then kill ()
      do! exited
      let! first = Task.WhenAny(stderr :> Task, Task.Delay 1000)
      let! said =
        if obj.ReferenceEquals(first, stderr) then stderr else Task.FromResult ""
      let failure =
        match ended with
        | PeerGone _ when proc.ExitCode <> 0 -> Some(classify proc.ExitCode said)
        | PeerGone reason -> Some(Unreachable reason)
        | Unanswered -> Some(Unreachable "the other end stopped answering")
        | Refused reason -> Some(NotSpoken reason)
        | _ -> None
      return ended, failure, greeted.IsSet
    }

/// The header a server answers a refused connection with: why, as a word (`ServerRefusal`).
let refusalHeader = "Dark-Sync-Refusal"

/// The header a client names the sync format it speaks in, so a server can refuse it before
/// anything else is said.
let formatHeader = "Dark-Sync-Format"

/// Where on a server sessions are opened.
let sessionPath = "/sync"

let rec private innermost (e : exn) : string =
  match e.InnerException with
  | null -> e.Message
  | inner -> innermost inner

/// Say goodbye on <param ws>, waiting at most 2 s for the other end to take it.
let closeQuickly (ws : System.Net.WebSockets.WebSocket) : Task =
  task {
    try
      use cts = new System.Threading.CancellationTokenSource(2000)
      do!
        ws.CloseOutputAsync(
          System.Net.WebSockets.WebSocketCloseStatus.NormalClosure,
          "",
          cts.Token
        )
    with _ ->
      ()
  }

/// Open a session to the sync server at <param url> with <param token>, over a WebSocket, and run it.
let private connectServer
  (side : Side)
  (url : System.Uri)
  (token : string)
  (once : bool)
  : Task<Ended * Option<Failure> * bool> =
  task {
    let target =
      let b = System.UriBuilder(url)
      b.Scheme <- if url.Scheme = "https" then "wss" else "ws"
      b.Path <- b.Path.TrimEnd('/') + sessionPath
      b.Uri
    let ws = new System.Net.WebSockets.ClientWebSocket()
    ws.Options.SetRequestHeader("Authorization", $"Bearer {token}")
    ws.Options.SetRequestHeader(formatHeader, OpSync.formatVersion)
    ws.Options.CollectHttpResponseDetails <- true
    ws.Options.KeepAliveInterval <- System.TimeSpan.FromSeconds 15.0
    let! opened =
      task {
        try
          use cts =
            new System.Threading.CancellationTokenSource(
              System.TimeSpan.FromSeconds 15.0
            )
          do! ws.ConnectAsync(target, cts.Token)
          return Ok()
        with e ->
          let refusal =
            match ws.HttpResponseHeaders with
            | null -> ""
            | headers ->
              match headers.TryGetValue refusalHeader with
              | true, values -> values |> Seq.tryHead |> Option.defaultValue ""
              | _ -> ""
          return
            match int ws.HttpStatusCode, refusal with
            | _, "revoked" -> Error(TokenRefused true)
            | 401, _
            | _, "unknown-token" -> Error(TokenRefused false)
            | _, "format" ->
              Error(
                NotSpoken "the server runs a dark that speaks another sync format"
              )
            | 0, _ -> Error(Unreachable(innermost e))
            | 404, _ ->
              Error(NotSpoken $"{url} answers, but isn't a dark sync server")
            | code, _ -> Error(Unreachable $"the server answered HTTP {code}")
      }
    match opened with
    | Error failure ->
      ws.Dispose()
      return PeerGone "not connected", Some failure, false
    | Ok() ->
      use stream =
        System.Net.WebSockets.WebSocketStream.Create(
          ws,
          System.Net.WebSockets.WebSocketMessageType.Binary,
          true
        )
      use greeted = new System.Threading.ManualResetEventSlim(false)
      let! ended =
        task {
          try
            return! session greeted side stream stream once
          with e ->
            return PeerGone e.Message
        }
      // A closing handshake the other end may never answer is not waited on for long.
      do! closeQuickly ws
      let failure =
        match ended with
        | PeerGone reason -> Some(Unreachable reason)
        | Unanswered -> Some(Unreachable "the server stopped answering")
        | Refused reason -> Some(NotSpoken reason)
        | _ -> None
      return ended, failure, greeted.IsSet
  }

/// Reach the other end over <param transport> and run a session: how it ended, why it failed if it
/// did, and whether the other end said hello first.
let connect
  (side : Side)
  (transport : Transport)
  (once : bool)
  : Task<Ended * Option<Failure> * bool> =
  match transport with
  | Program(program, arguments) -> connectProgram side program arguments once
  | Server(url, token) -> connectServer side url token once


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
    /// A line that asks nothing of the person, shown dim: only that the other machine has been out
    /// of reach for a long time.
    mention : string -> unit
  }

type private Watching =
  { failingSince : Option<System.DateTime>
    lastSaid : Option<System.DateTime>
    backoffMs : int }

/// A moment as the person's clock shows it.
let clock (t : System.DateTime) : string = t.ToLocalTime().ToString "HH:mm"

/// Sleep about <param ms>, waking early when sync is turned off. True when it was.
let private nap (watch : Watch) (ms : int) : Task<bool> =
  let deadline = System.DateTime.UtcNow.AddMilliseconds(float ms)
  let rec go () =
    task {
      if not (watch.side.keepGoing ()) then
        return true
      elif System.DateTime.UtcNow >= deadline then
        return false
      else
        do! Task.Delay 200
        return! go ()
    }
  go ()

/// Keep a session open to the other machine until sync is turned off, reconnecting with backoff, at
/// most 10 s apart. Waits are on the wall clock, so the first try after this machine wakes is at
/// once. What needs the person (a refused key, no dark there) is said, and again at most hourly
/// while it lasts; out of reach for longer than the patience is mentioned once an outage.
let watch (watch : Watch) : Task<unit> =
  let fresh = { failingSince = None; lastSaid = None; backoffMs = 500 }
  let offline (reason : string) = watch.side.note (Standing.into "offline" reason)
  let step (w : Watching) : Task<Choice<Watching, unit>> =
    task {
      if not (watch.side.keepGoing ()) then
        return Choice2Of2()
      else
        let started = System.DateTime.UtcNow
        if Option.isNone w.failingSince then
          watch.side.note (Standing.into "connecting" "")
        let! ended, failure, greeted = connect watch.side watch.transport false
        let now = System.DateTime.UtcNow
        // A session that got going was a success, whatever ended it: the connection dropped, or
        // the other end went away. Try again soon.
        let lasted =
          match failure with
          | Some(Unreachable _) ->
            greeted || now - started > System.TimeSpan.FromSeconds 20.0
          | _ -> false
        match ended, failure with
        | Stopped, _ -> return Choice2Of2()
        | _, None ->
          watch.side.log $"session ended: {ended}"
          return Choice1Of2 fresh
        | _, Some _ when lasted ->
          watch.side.log
            $"session ended after {int (now - started).TotalSeconds} s: {ended}"
          offline $"lost the connection to {watch.peer}"
          match! nap watch 100 with
          | true -> return Choice2Of2()
          | false ->
            return
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
            | TokenRefused revoked ->
              let what = if revoked then "was revoked" else "isn't one it knows"
              Some
                $"sync: {watch.peer} refused this machine's token: it {what}. Nothing syncs until `dark sync add {watch.peer} --token <new token>`.",
              tokenRefusedDetail
            | NoDark reason ->
              Some
                $"sync: there is no dark to run on {watch.peer} ({reason}). `dark sync add {watch.peer} --command <path>` says where it is.",
              reason
            | NotSpoken reason ->
              let advice =
                if reason.Contains "sync format" then " Update dark on both ends."
                elif reason.EndsWith "." || reason.EndsWith "?" then ""
                else "."
              Some $"sync: {reason}{advice}", reason
            | Unreachable reason ->
              (if now - since >= watch.patience then
                 Some
                   $"sync: can't reach {watch.peer} since {clock since}; your work is kept here and goes when it's back."
               else
                 None),
              reason
          watch.side.log $"connection failed: {reason}"
          offline reason
          let said =
            match line, failure with
            | Some l, Unreachable _ when Option.isNone w.lastSaid ->
              watch.mention l
              Some now
            | Some l, NotAllowed _
            | Some l, TokenRefused _
            | Some l, NoDark _
            | Some l, NotSpoken _ when hourly ->
              watch.side.say l
              Some now
            | _ -> w.lastSaid
          match! nap watch w.backoffMs with
          | true -> return Choice2Of2()
          | false ->
            return
              Choice1Of2
                { failingSince = Some since
                  lastSaid = said
                  backoffMs = min 10000 (w.backoffMs * 2) }
    }
  task {
    do! iterateTask step fresh
    watch.side.note (Standing.into "off" "")
  }


// ------------------
// What a command sees
// ------------------

/// What the first command after a restart found, having started the watcher and waited for it.
type CatchUp =
  /// The other machine's changes are here, folded.
  | CaughtUp
  /// The other machine could not be reached, and why.
  | OutOfReach of reason : string
  /// Still going when the wait ran out; the rest arrives in the background.
  | StillGoing

/// Wait up to <param bound> for a watcher started at <param started> to land the other machine's
/// catch-up, reading how the link stands with <param read>. Ends early when the link says it could
/// not connect.
let awaitCatchUp
  (read : unit -> Standing)
  (started : System.DateTime)
  (bound : System.TimeSpan)
  : CatchUp =
  let rec go () =
    let s = read ()
    match s.caughtUp with
    | Some t when t >= started -> CaughtUp
    | _ ->
      match s.since with
      | Some t when s.state = "offline" && t >= started -> OutOfReach s.detail
      | _ ->
        if System.DateTime.UtcNow - started >= bound then
          StillGoing
        else
          System.Threading.Thread.Sleep 20
          go ()
  go ()

/// `dark status`'s line about sync, for the link to <param host>: whether the other machine has
/// everything this one made (<param behind> is how many changes it has not said it holds), and when
/// it was last heard from.
let statusLine
  (host : string)
  (s : Standing)
  (behind : int64)
  (now : System.DateTime)
  : string =
  let at (t : System.DateTime) =
    if now - t < System.TimeSpan.FromMinutes 1.0 then "just now"
    elif t.ToLocalTime().Date = now.ToLocalTime().Date then clock t
    else t.ToLocalTime().ToString "MMM d HH:mm"
  let contact =
    match s.contact with
    | Some t -> $"last contact {at t}"
    | None -> "no contact yet"
  let changes = if behind = 1L then "1 change" else $"{behind} changes"
  match s.state with
  | "connected" when s.readOnly ->
    $"sync: up to date with {host}, read-only: what you change stays on this machine ({contact})."
  | "connected" when behind = 0L -> $"sync: up to date with {host} ({contact})."
  | "connected" -> $"sync: {changes} on their way to {host} ({contact})."
  | "offline" when s.detail = tokenRefusedDetail ->
    $"sync: {host} refused this machine's token. `dark sync add {host} --token -` with a new one."
  | "offline" ->
    let since =
      match s.since with
      | Some t -> $" since {at t}"
      | None -> ""
    let held =
      if behind = 0L then
        $"nothing here that {host} lacks"
      else
        $"{changes} not on {host} yet"
    $"sync: can't reach {host}{since}; {held} ({contact})."
  | _ when behind = 0L -> $"sync: connecting to {host} ({contact})."
  | _ -> $"sync: connecting to {host}; {changes} not on it yet ({contact})."


// ------------------
// This machine's files
// ------------------

/// The setting that says sync is on, and with which machine.
let peerKey = "sync.peer"

/// Where sync keeps its files: how the link stands, the lines waiting for the person, the watcher's
/// lock, and the have and bundle files of a session.
let dir () : string = Path.Combine(LibConfig.Config.runDir, "sync")

let standingPath () : string = Path.Combine(dir (), "state")

let private lockPath () : string = Path.Combine(dir (), "watch.lock")

/// The lock a watcher holds while it runs, or None when another process holds it.
let takeLock () : Option<FileStream> =
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

let watcherRunning () : bool =
  match takeLock () with
  | Some held ->
    held.Dispose()
    false
  | None -> true

/// How the link stands now: the peer, the watcher's record, and how many changes it lacks. None
/// when sync is off.
let standingNow () : Option<string * Standing * int64> =
  match Config.get peerKey with
  | None -> None
  | Some host ->
    let s = Standing.read (standingPath ())
    // A watcher that has stopped holds no connection, whatever it last recorded; the next
    // command starts another.
    let s =
      if s.state = "connected" && not (watcherRunning ()) then
        Standing.into "connecting" "" s
      else
        s
    Some(host, s, OpSync.notYetAcked Sqlite.currentDbPath s.peer)

/// `dark sync`'s line about the link, or "" when sync is off.
let statusNow () : string =
  match standingNow () with
  | None -> ""
  | Some(host, s, behind) -> statusLine host s behind System.DateTime.UtcNow
