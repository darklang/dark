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
              System.Console.WriteLine(v)
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

  static member WriteLine(value : string) : unit =
    if isWasm then System.Console.WriteLine value else mQueue.Add(value)


let wait () : unit = Private.wait ()

let writeLine (value : string) : unit = Private.WriteLine value
