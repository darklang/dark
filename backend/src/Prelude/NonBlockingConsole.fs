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

  // When capturing (non-null), writes go to this buffer instead of the console queue. Used by the CLI to run
  // a command and show its output in-frame (the workbench's inline command bar) rather than to stdout. Capture
  // is started and stopped synchronously from the interactive loop's key handler.
  //
  // The *driver* is single-threaded, but `Write` is not: a daemon, sync, or telemetry thread can call it while
  // a capture window is open. StringBuilder is not thread-safe, so every touch of the buffer goes through
  // `captureLock`. This does not give capture thread affinity: any stdout written by another thread during the
  // window still lands in the captured string rather than on screen. That's a known wart, kept because the
  // alternative (thread-affine capture) would silently drop output from a command that resumes on a different
  // thread after async work.
  static let captureLock : obj = obj ()
  static let mutable captureBuffer : System.Text.StringBuilder = null

  // Use a lock so that wait() doesn't return until the thread has actually printed
  // (it would finish once it was removed from the queue)
  static let mLock : obj = obj ()

  static do
    let f () =
      while true do
        lock mLock (fun () ->
          try
            let mutable v = null
            // Don't block (eg with `Take`) while holding the lock
            if mQueue.TryTake(&v) then
              System.Console.Write(v)
            else
              System.Threading.Thread.Sleep 1 // 1ms
          with e ->
            System.Console.WriteLine(
              $"Exception in blocking queue thread: {e.Message}"
            ))


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
          let cb = captureBuffer
          if isNull cb then
            false
          else
            cb.Append(value) |> ignore
            true)

      if not captured then mQueue.Add(value)

  /// Begin a capture window. Returns false if one was already open, in which case nothing changes: the
  /// caller must not assume it owns the buffer. Nesting isn't supported (there is exactly one caller,
  /// `Workbench.captureOutput`); refusing is better than silently discarding the outer capture's output.
  static member StartCapture() : bool =
    lock captureLock (fun () ->
      if isNull captureBuffer then
        captureBuffer <- System.Text.StringBuilder()
        true
      else
        false)

  static member StopCapture() : string =
    lock captureLock (fun () ->
      let sb = captureBuffer
      captureBuffer <- null
      if isNull sb then "" else sb.ToString())


let wait () : unit = Private.wait ()

let writeInline (value : string) : unit = Private.Write value

let writeLine (value : string) : unit = Private.Write(value + "\n")

/// Route subsequent `print`/`printLine` output into an in-memory buffer instead of the console.
/// Returns false if a capture window was already open (the existing one is left untouched).
let startCapture () : bool = Private.StartCapture()

/// Stop capturing and return everything written since `startCapture`.
let stopCapture () : string = Private.StopCapture()
