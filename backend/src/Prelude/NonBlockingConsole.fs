module NonBlockingConsole

type Stream =
  | Out
  | Err

type BlockingCollection =
  System.Collections.Concurrent.BlockingCollection<struct (Stream * string)>

type private Capture() =
  member val All = System.Text.StringBuilder()
  member val Out = System.Text.StringBuilder()
  member val Err = System.Text.StringBuilder()
  member val Active = true with get, set

type private Private() =

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
  static let isWasm = System.OperatingSystem.IsBrowser()

  // Where output goes in the browser, when the host has said. `System.Console.Out` there is a
  // SyncTextWriter, and a write through it from inside a resumed task continuation left the
  // interpreter's next await unable to suspend (a blocking wait, fatal on the one browser
  // thread). The browser host hands in a plain buffer instead and drains it from JS.
  static let mutable browserSink : (string -> unit) option = None


  static let mQueue : BlockingCollection = new BlockingCollection()

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
  static let captureLock : obj = obj ()

  static let captureBuffer = new System.Threading.AsyncLocal<Capture>()

  // Use a lock so that wait() doesn't return until the thread has actually printed
  // (it would finish once it was removed from the queue)
  static let mLock : obj = obj ()

  static do
    let f () =
      while true do
        let mutable wrote = false

        lock mLock (fun () ->
          try
            let mutable v = struct (Out, null)
            // Don't block (eg with `Take`) while holding the lock
            if mQueue.TryTake(&v) then
              match v with
              | struct (Out, text) -> System.Console.Out.Write(text)
              | struct (Err, text) -> System.Console.Error.Write(text)
              wrote <- true
          with e ->
            System.Console.Error.WriteLine(
              $"Exception in blocking queue thread: {e.Message}"
            ))

        // Sleep OUTSIDE the lock. Sleeping inside means holding `mLock` for essentially the whole
        // millisecond of every idle iteration, and `wait()` spins acquiring the same lock; .NET's Monitor
        // isn't fair, so `wait()` loses that race repeatedly and a two-line command can spend ~100 ms in
        // it. Taking and writing an item stays inside the lock, so `wait()` still cannot return between a
        // value leaving the queue and reaching the console, which is the invariant the lock exists for.
        if not wrote then System.Threading.Thread.Sleep 1


    // Background threads aren't supported in Blazor
    if not isWasm then
      let thread = System.Threading.Thread(f)
      thread.IsBackground <- true
      thread.Name <- "Prelude.NonBlockingConsole printer"
      thread.Start()

  static member wait() : unit =
    let mutable shouldWait = true
    while shouldWait do
      lock mLock (fun () -> shouldWait <- mQueue.Count > 0)

  static member SetBrowserSink(sink : string -> unit) : unit =
    browserSink <- Some sink

  static member Write(stream : Stream, value : string) : unit =
    if isWasm then
      // The browser's terminal is the person's screen for both streams. Its `Console.Error` is
      // the devtools console, where a refusal would never be seen.
      match browserSink, stream with
      | Some sink, _ -> sink value
      | None, Out -> System.Console.Out.Write value
      | None, Err -> System.Console.Error.Write value
    else
      // Take the capture decision and the append atomically, so a concurrent Stop can't leave a write
      // appended to a buffer nobody will read, or tear the StringBuilder.
      let captured =
        lock captureLock (fun () ->
          let c = captureBuffer.Value
          if isNull (box c) || not c.Active then
            false
          else
            c.All.Append(value) |> ignore
            (match stream with
             | Out -> c.Out
             | Err -> c.Err)
              .Append(value)
            |> ignore
            true)

      if not captured then mQueue.Add(struct (stream, value))

  /// Begin a capture window for THIS flow. Returns false if one was already open here, in which case
  /// nothing changes: the caller must not assume it owns the buffer. Nesting isn't supported;
  /// refusing is better than silently discarding the outer capture's output.
  static member StartCapture() : bool =
    lock captureLock (fun () ->
      let c = captureBuffer.Value
      if isNull (box c) || not c.Active then
        captureBuffer.Value <- Capture()
        true
      else
        false)

  static member StopCapture() : string * string * string =
    lock captureLock (fun () ->
      let c = captureBuffer.Value
      captureBuffer.Value <- Unchecked.defaultof<Capture>
      if isNull (box c) then
        ("", "", "")
      else
        // Child tasks retain the AsyncLocal reference after their parent stops.
        // Closing the buffer makes their later writes visible on the console.
        c.Active <- false
        (c.All.ToString(), c.Out.ToString(), c.Err.ToString()))


let wait () : unit = Private.wait ()

let writeInline (value : string) : unit = Private.Write(Out, value)

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
