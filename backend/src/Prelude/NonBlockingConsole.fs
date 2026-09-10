module NonBlockingConsole

type BlockingCollection = System.Collections.Concurrent.BlockingCollection<string>

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
  static let isWasm = System.OperatingSystem.IsBrowser()


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

  static let captureBuffer =
    new System.Threading.AsyncLocal<System.Text.StringBuilder>()

  // Use a lock so that wait() doesn't return until the thread has actually printed
  // (it would finish once it was removed from the queue)
  static let mLock : obj = obj ()

  static do
    let f () =
      while true do
        let mutable wrote = false

        lock mLock (fun () ->
          try
            let mutable v = null
            // Don't block (eg with `Take`) while holding the lock
            if mQueue.TryTake(&v) then
              System.Console.Write(v)
              wrote <- true
          with e ->
            System.Console.WriteLine(
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

  static member Write(value : string) : unit =
    if isWasm then
      System.Console.Write value
    else
      // Take the capture decision and the append atomically, so a concurrent Stop can't leave a write
      // appended to a buffer nobody will read, or tear the StringBuilder.
      let captured =
        lock captureLock (fun () ->
          let cb = captureBuffer.Value
          if isNull cb then
            false
          else
            cb.Append(value) |> ignore
            true)

      if not captured then mQueue.Add(value)

  /// Begin a capture window for THIS flow. Returns false if one was already open here, in which case
  /// nothing changes: the caller must not assume it owns the buffer. Nesting isn't supported;
  /// refusing is better than silently discarding the outer capture's output.
  static member StartCapture() : bool =
    lock captureLock (fun () ->
      if isNull captureBuffer.Value then
        captureBuffer.Value <- System.Text.StringBuilder()
        true
      else
        false)

  static member StopCapture() : string =
    lock captureLock (fun () ->
      let sb = captureBuffer.Value
      captureBuffer.Value <- null
      if isNull sb then "" else sb.ToString())


let wait () : unit = Private.wait ()

let writeInline (value : string) : unit = Private.Write value

let writeLine (value : string) : unit = Private.Write(value + "\n")

/// Route subsequent `print`/`printLine` output into an in-memory buffer instead of the console.
/// Returns false if a capture window was already open (the existing one is left untouched).
let startCapture () : bool = Private.StartCapture()

/// Stop capturing and return everything written since `startCapture`.
let stopCapture () : string = Private.StopCapture()
