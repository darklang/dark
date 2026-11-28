module NonBlockingConsole

type Stream =
  | Out
  | Err

/// SGR escape sequences (`ESC [ ... m`): colour and text style.
module Ansi =
  // Lazy: built the first time something needs stripping, not on every start.
  let private sgr =
    lazy
      System.Text.RegularExpressions.Regex(
        "\u001b\\[([0-9;]*)m",
        System.Text.RegularExpressions.RegexOptions.Compiled
      )

  let withoutSgr (text : string) : string =
    if text.IndexOf '\u001b' < 0 then text else sgr.Value.Replace(text, "")

  /// Foreground, background, and their bright variants; the rest is style or a reset.
  let private isColour (n : int) : bool =
    (n >= 30 && n <= 49) || (n >= 90 && n <= 107)

  /// For `NO_COLOR`, which asks for no colour, not no style: bold and the like stay.
  /// A reset that would only end a colour dropped earlier in the same text goes too.
  let withoutColour (text : string) : string =
    if text.IndexOf '\u001b' < 0 then
      text
    else
      let rec keep (ps : string list) : string list =
        match ps with
        | ("38" | "48") :: "5" :: _ :: rest -> keep rest
        | ("38" | "48") :: "2" :: _ :: _ :: _ :: rest -> keep rest
        | p :: rest ->
          match System.Int32.TryParse p with
          | true, n when isColour n -> keep rest
          | _ -> p :: keep rest
        | [] -> []
      // Since the last reset (or the start): was a style kept, and was a colour dropped? A reset
      // with no style to end and only a dropped colour behind it has nothing left to undo.
      let (pieces, last, _) =
        sgr.Value.Matches text
        |> Seq.fold
          (fun
               (pieces, at, (styled, dropped))
               (m : System.Text.RegularExpressions.Match) ->
            let before = text.Substring(at, m.Index - at)
            let ps =
              if m.Groups[1].Value = "" then
                [ "0" ]
              else
                List.ofArray (m.Groups[1].Value.Split ';')
            let kept = keep ps
            let code, state =
              match kept with
              | [] -> "", (styled, true)
              | _ when List.forall (fun p -> p = "0") kept ->
                (if dropped && not styled then "" else "\u001b[0m"), (false, false)
              | _ -> "\u001b[" + String.concat ";" kept + "m", (true, dropped)
            (code :: before :: pieces, m.Index + m.Length, state))
          ([], 0, (false, false))
      String.concat "" (List.rev (text.Substring last :: pieces))

  /// A pipe, a file or `TERM=dumb` gets no escape sequences; `CLICOLOR_FORCE` keeps them anyway.
  let decide (redirected : bool) : string -> string =
    let env (name : string) =
      match System.Environment.GetEnvironmentVariable name with
      | null -> ""
      | v -> v
    let force = env "CLICOLOR_FORCE"
    if force <> "" && force <> "0" then id
    elif redirected || env "TERM" = "dumb" then withoutSgr
    elif env "NO_COLOR" <> "" then withoutColour
    else id

/// Where a stream's text goes. A pipe or a terminal gets a stream of its own on the fd:
/// `Console.Out` ignores a broken pipe, so a run piped into `head` went on long after `head` had
/// exited, and the first write through `Console` to a terminal switches it to keypad mode
/// (`ESC[?1h ESC=`), which only the interactive loop wants. Not a file: a stream on a file would
/// write at its own offset, over what anything else sharing the fd wrote.
///
/// Also returns that stream of its own, when it made one, for writing bytes. The writer flushes
/// on every write, so text and bytes written in turn stay in order.
let private writerFor
  (fd : int)
  (console : unit -> System.IO.TextWriter)
  : System.IO.TextWriter * Option<System.IO.Stream> =
  if System.OperatingSystem.IsWindows() then
    console (), None
  else
    let stream =
      new System.IO.FileStream(
        new Microsoft.Win32.SafeHandles.SafeFileHandle(nativeint fd, false),
        System.IO.FileAccess.Write,
        1
      )
    if stream.CanSeek then
      stream.Dispose()
      console (), None
    else
      new System.IO.StreamWriter(
        stream,
        System.Text.UTF8Encoding false,
        AutoFlush = true
      ),
      Some stream

/// `write` failing with EPIPE: whoever read stdout has gone.
let private isBrokenPipe (e : exn) : bool =
  match e with
  | :? System.IO.IOException as e -> e.HResult = 32
  | _ -> false

/// One thing waiting to be written.
type private Queued =
  | Text of Stream * string
  /// Bytes for stdout, written exactly as they are.
  | Bytes of byte[]

type private BlockingCollection =
  System.Collections.Concurrent.BlockingCollection<Queued>

type private Capture() =
  member val All = System.Text.StringBuilder()
  member val Out = System.Text.StringBuilder()
  member val Err = System.Text.StringBuilder()
  member val Active = true with get, set

// It seems like printing on the Console can cause a deadlock. I observed that all
// the tasks in the threadpool were blocking on Console.WriteLine, and that the
// logging thread in the background was blocked on one of those threads. This is
// like a known issue with a known solution:
// https://stackoverflow.com/a/3670628/104021.

// Note that there are sometimes other loggers, such as in IHosts, which may also
// need to move off the console logger.

// This adds a collection which receives all output from WriteLine. Then, a
// background thread writes the output to Console.
//
// Both streams go through the one queue. A stderr write that went straight to the console
// would overtake stdout lines still waiting in the queue, so an error could print above the
// output that led to it; it would also escape a capture window and the browser sink below.
[<NoEquality; NoComparison>]
type private State =
  { isWasm : bool

    // Where output goes in the browser, when the host has said. `System.Console.Out` there is a
    // SyncTextWriter, and a write through it from inside a resumed task continuation left the
    // interpreter's next await unable to suspend (a blocking wait, fatal on the one browser
    // thread). The browser host hands in a plain buffer instead and drains it from JS.
    mutable browserSink : (string -> unit) option

    queue : BlockingCollection

    out : Lazy<System.IO.TextWriter>
    // Where bytes for stdout go: the same place as `out`'s text.
    outBytes : Lazy<System.IO.Stream>
    err : Lazy<System.IO.TextWriter>

    // Decided per stream, here, because the colouring helpers can't know where a string will land.
    forOut : Lazy<string -> string>
    forErr : Lazy<string -> string>

    // When capturing, writes go to a buffer instead of the console queue. Used by the CLI to run a
    // command and show its output in-frame (the workbench's inline command bar) rather than to
    // stdout, and by the CLI test harness to read what a command printed.
    //
    // AsyncLocal, not a plain static: the capture belongs to the flow that started it. A static
    // means one capture window swallows everything the whole process prints, which is why the CLI
    // tests had to be sequenced against the entire suite -- two of them capturing at once would
    // read each other's output, and anything else printing would land in whichever window was
    // open. AsyncLocal flows across `await`, so a command that resumes on another thread still
    // captures, which thread affinity would not give.
    //
    // StringBuilder is not thread-safe and a capturing flow can fan out, so every touch of the
    // buffer still goes through `captureLock`.
    captureLock : obj
    captureBuffer : System.Threading.AsyncLocal<Capture>

    // Use a lock so that wait() doesn't return until the thread has actually printed
    // (it would finish once it was removed from the queue)
    printLock : obj }

/// The printer thread. It is handed the state rather than reading it back from a static, so it
/// cannot see the state half set up.
let private printLoop (s : State) () : unit =
  while true do
    let wrote =
      lock s.printLock (fun () ->
        try
          let mutable v = Text(Out, "")
          // Don't block (eg with `Take`) while holding the lock
          if s.queue.TryTake(&v) then
            match v with
            | Text(Out, text) -> s.out.Force().Write(s.forOut.Force () text)
            | Text(Err, text) -> s.err.Force().Write(s.forErr.Force () text)
            | Bytes bytes ->
              let stream = s.outBytes.Force()
              stream.Write(bytes, 0, bytes.Length)
              stream.Flush()
            true
          else
            false
        with
        | e when isBrokenPipe e ->
          // Nobody reads the answer any more, so stop, quietly, as `yes | head` does. 141 is
          // what a shell reports for a process SIGPIPE ended.
          exit 141
        | e ->
          System.Console.Error.WriteLine(
            $"Exception in blocking queue thread: {e.Message}"
          )
          false)

    // Sleep OUTSIDE the lock. Sleeping inside means holding `printLock` for essentially the whole
    // millisecond of every idle iteration, and `wait()` spins acquiring the same lock; .NET's Monitor
    // isn't fair, so `wait()` loses that race repeatedly and a two-line command can spend ~100 ms in
    // it. Taking and writing an item stays inside the lock, so `wait()` still cannot return between a
    // value leaving the queue and reaching the console, which is the invariant the lock exists for.
    if not wrote then System.Threading.Thread.Sleep 1

/// The state is built by whichever thread first prints, captures or waits, not by F# static
/// initialisation. F# runs a class's `static let`s from the whole file's initialiser, which the
/// CLR does not tie to the class's own: a thread that entered the file another way (say through
/// `Ansi` above) could leave the class marked ready while its statics were still being set, and a
/// second thread printing at that moment failed with "static data being accessed recursively".
/// A field with no initialiser has nothing to race: it is null until one complete `State` is
/// stored, and the thread that stores it is the one that starts the printer.
type private Instance =
  [<DefaultValue>]
  static val mutable private state : State

  static member Get() : State =
    let current = Instance.state
    if not (isNull (box current)) then
      current
    else
      // The lock is the type itself because a lock object of our own would be a static needing
      // initialisation, which is the thing this avoids. Nobody else can see this private type.
      lock typeof<Instance> (fun () ->
        if isNull (box Instance.state) then
          let out = lazy (writerFor 1 (fun () -> System.Console.Out))
          let fresh =
            { isWasm = System.OperatingSystem.IsBrowser()
              browserSink = None
              queue = new BlockingCollection()
              out = lazy (fst (out.Force()))
              outBytes =
                lazy
                  (match snd (out.Force()) with
                   | Some stream -> stream
                   | None -> System.Console.OpenStandardOutput())
              err = lazy (fst (writerFor 2 (fun () -> System.Console.Error)))
              forOut = lazy (Ansi.decide System.Console.IsOutputRedirected)
              forErr = lazy (Ansi.decide System.Console.IsErrorRedirected)
              captureLock = obj ()
              captureBuffer = System.Threading.AsyncLocal<Capture>()
              printLock = obj () }
          Instance.state <- fresh
          // Background threads aren't supported in Blazor
          if not fresh.isWasm then
            let thread = System.Threading.Thread(printLoop fresh)
            thread.IsBackground <- true
            thread.Name <- "Prelude.NonBlockingConsole printer"
            thread.Start()
        Instance.state)

type private Private =

  static member wait() : unit =
    let s = Instance.Get()
    while lock s.printLock (fun () -> s.queue.Count > 0) do
      ()

  static member SetBrowserSink(sink : string -> unit) : unit =
    (Instance.Get()).browserSink <- Some sink

  /// Append to this flow's capture window, if one is open, and say whether it was. The decision
  /// and the append are one step, so a concurrent Stop can't leave a write appended to a buffer
  /// nobody will read, or tear the StringBuilder. A capture holds text, so bytes are read as UTF-8;
  /// they are passed as they are and decoded only when a window is open.
  static member private Capture
    (
      s : State,
      stream : Stream,
      text : string,
      bytes : byte[]
    ) : bool =
    lock s.captureLock (fun () ->
      let c = s.captureBuffer.Value
      if isNull (box c) || not c.Active then
        false
      else
        let value =
          if isNull bytes then text else System.Text.Encoding.UTF8.GetString bytes
        c.All.Append(value) |> ignore
        (match stream with
         | Out -> c.Out
         | Err -> c.Err)
          .Append(value)
        |> ignore
        true)

  static member Write(stream : Stream, value : string) : unit =
    let s = Instance.Get()
    if s.isWasm then
      // The browser's terminal is the person's screen for both streams. Its `Console.Error` is
      // the devtools console, where a refusal would never be seen.
      match s.browserSink, stream with
      | Some sink, _ -> sink value
      | None, Out -> System.Console.Out.Write value
      | None, Err -> System.Console.Error.Write value
    elif not (Private.Capture(s, stream, value, null)) then
      s.queue.Add(Text(stream, value))

  /// Bytes for stdout, exactly as they are: nothing is removed, whatever stdout is. The browser
  /// shows text, so there they are read as UTF-8.
  static member WriteBytes(value : byte[]) : unit =
    let s = Instance.Get()
    if s.isWasm then Private.Write(Out, System.Text.Encoding.UTF8.GetString value)
    elif not (Private.Capture(s, Out, null, value)) then s.queue.Add(Bytes value)

  /// Begin a capture window for THIS flow. Returns false if one was already open here, in which case
  /// nothing changes: the caller must not assume it owns the buffer. Nesting isn't supported;
  /// refusing is better than silently discarding the outer capture's output.
  static member StartCapture() : bool =
    let s = Instance.Get()
    lock s.captureLock (fun () ->
      let c = s.captureBuffer.Value
      if isNull (box c) || not c.Active then
        s.captureBuffer.Value <- Capture()
        true
      else
        false)

  static member StopCapture() : string * string * string =
    let s = Instance.Get()
    lock s.captureLock (fun () ->
      let c = s.captureBuffer.Value
      s.captureBuffer.Value <- Unchecked.defaultof<Capture>
      if isNull (box c) then
        ("", "", "")
      else
        // Child tasks retain the AsyncLocal reference after their parent stops.
        // Closing the buffer makes their later writes visible on the console.
        c.Active <- false
        (c.All.ToString(), c.Out.ToString(), c.Err.ToString()))


let wait () : unit = Private.wait ()

let writeInline (value : string) : unit = Private.Write(Out, value)

/// Bytes to stdout exactly as they are, in order with everything already queued.
let writeBytes (value : byte[]) : unit = Private.WriteBytes value

let writeLine (value : string) : unit = Private.Write(Out, value + "\n")

let writeErrInline (value : string) : unit = Private.Write(Err, value)

/// In order with everything already queued for stdout; see the queue's comment.
let writeErrLine (value : string) : unit = Private.Write(Err, value + "\n")

/// Route subsequent output on BOTH streams into an in-memory buffer instead of the console.
/// Returns false if a capture window was already open (the existing one is left untouched).
let startCapture () : bool = Private.StartCapture()

/// Stop capturing and return everything written since `startCapture`, both streams in order.
let stopCapture () : string =
  let (all, _, _) = Private.StopCapture()
  all

/// `(both, stdout, stderr)`.
let stopCaptureEach () : string * string * string = Private.StopCapture()

/// Browser host only: route every write to <param sink> instead of `System.Console`.
let setBrowserSink (sink : string -> unit) : unit = Private.SetBrowserSink sink
