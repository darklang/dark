/// The event queue a scheduler feeds its processes from, and the sources that post to it.
///
/// One queue per scheduler. Everything that happens outside a process (a key pressed, a timer
/// firing, the store changing, a parked await completing) becomes a `HostEvent` on this queue,
/// and the scheduler thread is the only consumer. Sources post from wherever they run: the stdin
/// reader thread, `System.Threading.Timer` callbacks, the store poll, thread-pool continuations.
/// None of them ever touch a VM; that is the whole discipline (`docs/processes.md`).
///
/// Several schedulers can live in one OS process (the CLI's root scheduler and its workers, one
/// per core), and there is still one console and one store. So the reader thread and the store
/// poll are process-wide, below, and deliver to whichever queue asked: a key goes to the queue
/// that requested it, a store change to every queue watching.
///
/// This module is the queues and the sources only. It has no idea what a process is.
/// `Scheduler.fs` owns subscriptions (which process wants which event) and matching.
module LibExecution.HostEvents

open System.Collections.Concurrent
open System.Threading

open Prelude

module RT = RuntimeTypes

type ProcessId = System.Guid

type TimerId = int64

/// What a process can wait for: `Stdlib.Host.EventSpec`, as F#.
type EventSpec =
  /// The next key press.
  | Key
  /// The package store changed (any op landed, in this instance or pulled).
  | StoreChanged
  /// Fires once, after this many milliseconds.
  | Timer of ms : int64
  /// A process finished.
  | ExecDone of ProcessId

/// What arrives on the queue.
type HostEvent =
  /// A key, already in its Dark shape (`Stdlib.Cli.Stdin.KeyRead`). The reader thread builds it;
  /// the scheduler hands it over untouched.
  | Key of keyRead : RT.Dval
  /// The store's data version moved. Carries nothing: which ops landed is Dark's question,
  /// answered by `Stdlib.Host.await` before the loop sees the event.
  | StoreChanged
  /// A timer registered by a subscription fired.
  | Timer of TimerId
  /// The task a process was parked on finished (well or badly). Internal: the scheduler resumes
  /// the process on its own thread.
  | Completed of ProcessId
  /// A process finished, well or badly. For Dark subscribers (`ExecDone id`), who ask
  /// `Exec.await` how; F# callers await the process directly. Posted to the schedulers with a
  /// subscriber for it (`Scheduler.Finish`), which may not be the one that ran it.
  | ExecDone of ProcessId
  /// Nothing to route: a process became runnable from outside the loop (a spawn from another
  /// thread), or the loop was asked to stop, and the loop, blocked on the queue with nothing
  /// runnable, has to look again.
  | Wake


/// A source of one kind of event, installed by the host that has it.
///
/// `LibExecution` can read neither the console nor the store, so the host (the CLI's `main`)
/// installs what it has, once per OS process. A queue without a source for some spec still works:
/// a process parked on it waits forever, exactly as a blocking read with nobody typing would.
type Sources =
  {
    /// Block until one key is available and return it as `KeyRead`, or `None` when stdin is not a
    /// terminal (the reader thread then never starts; `readKey` answers as it always did).
    mutable readKey : Option<unit -> RT.Dval>
    /// The store's current data version, cheap enough to call five times a second.
    mutable storeVersion : Option<unit -> int64>
  }

let sources : Sources = { readKey = None; storeVersion = None }


/// One scheduler's queue, plus the one-shot timers it arms.
type Queue() =
  let events = new BlockingCollection<HostEvent>(new ConcurrentQueue<HostEvent>())

  /// A queue outlives its scheduler's loop: a late source posting after `Stop` reaches
  /// nobody, and that is fine.
  member _.Post(ev : HostEvent) : unit = events.Add ev

  /// Take the next event, blocking until there is one.
  member _.Take() : HostEvent = events.Take()

  member _.TryTake(ev : byref<HostEvent>) : bool = events.TryTake(&ev)

  /// Arm a one-shot timer that posts `Timer id` after `ms`. The returned disposable cancels it; a
  /// timer that fires after its subscription was satisfied by something else posts an id nobody
  /// wants any more, and the scheduler drops it.
  member this.ArmTimer(id : TimerId, ms : int64) : System.IDisposable =
    let ms = max 0L ms
    let mutable timer : Timer = null
    timer <-
      new Timer(
        (fun _ ->
          this.Post(HostEvent.Timer id)
          if not (isNull timer) then timer.Dispose()),
        null,
        ms,
        int64 Timeout.Infinite
      )
    { new System.IDisposable with
        member _.Dispose() = timer.Dispose() }



/// The stdin reader thread, for one `readKey` source: reads one key per request and posts it to
/// the queue that asked. "Demand-driven" is load-bearing: a thread that read continuously would
/// eat keys meant for a `readLine` after the TUI has quit, and would hold `TreatControlCAsInput`
/// while nothing is listening.
///
/// At most one read in flight. A `[Key; Timer]` wait satisfied by the timer leaves its read
/// outstanding; the next request, from any queue, takes that key rather than queueing a second
/// read, so the thread never holds more than one key's worth of the console. Requests are served
/// oldest first.
type private KeyReader(readKey : unit -> RT.Dval) =
  let requests = new SemaphoreSlim(0)
  let waiting = ConcurrentQueue<Queue>()
  let mutable inFlight = 0

  do
    let thread =
      Thread(
        (fun () ->
          while true do
            requests.Wait()
            let key =
              try
                Some(readKey ())
              with _ ->
                // The console went away (stdin closed under us). Stop reading; the subscriber
                // stays parked, as it would have blocked before.
                None
            Volatile.Write(&inFlight, 0)
            match key with
            | Some k ->
              match waiting.TryDequeue() with
              | true, q -> q.Post(HostEvent.Key k)
              | false, _ -> ()
            | None -> ()
            // Another queue asked while this read was in flight: read for it now.
            if
              not waiting.IsEmpty
              && Interlocked.CompareExchange(&inFlight, 1, 0) = 0
            then
              requests.Release() |> ignore<int>),
        IsBackground = true,
        Name = "dark-stdin-reader"
      )
    thread.Start()

  member _.Request(q : Queue) : unit =
    waiting.Enqueue q
    if Interlocked.CompareExchange(&inFlight, 1, 0) = 0 then
      requests.Release() |> ignore<int>


/// The store poll: `PRAGMA data_version` on a timer, posting `StoreChanged` to every watching
/// queue when it moves. One per `storeVersion` source.
type private StorePoll(version : unit -> int64, intervalMs : int) =
  let watchers = ConcurrentDictionary<Queue, unit>()
  let mutable lastVersion =
    try
      version ()
    with _ ->
      -1L

  let timer =
    new Timer(
      (fun _ ->
        let now =
          try
            version ()
          with _ ->
            lastVersion
        if now <> lastVersion then
          lastVersion <- now
          for q in watchers.Keys do
            q.Post HostEvent.StoreChanged),
      null,
      intervalMs,
      intervalMs
    )

  member _.Watch(q : Queue) : unit = watchers.TryAdd(q, ()) |> ignore<bool>

  member _.Unwatch(q : Queue) : unit = watchers.TryRemove q |> ignore<bool * unit>

  interface System.IDisposable with
    member _.Dispose() = timer.Dispose()


/// The process-wide sources, started lazily on the first request that needs them, so a run that
/// never waits on a key never reads the console and one that never waits on the store never polls.
///
/// Keyed on the installed source function: a test that installs a fake console gets a reader of
/// its own, and one left blocked in an earlier test's read cannot starve it.
module Shared =
  let private sync = obj ()
  let mutable private reader : Option<(unit -> RT.Dval) * KeyReader> = None
  let mutable private poll : Option<(unit -> int64) * StorePoll> = None

  /// Ask for one key, delivered to `q`. No-op when stdin is not a terminal (no source installed),
  /// in which case nothing will ever post `Key`.
  let requestKey (q : Queue) : unit =
    match sources.readKey with
    | None -> ()
    | Some readKey ->
      let r =
        lock sync (fun () ->
          match reader with
          | Some(source, r) when obj.ReferenceEquals(source, readKey) -> r
          | _ ->
            let r = KeyReader readKey
            reader <- Some(readKey, r)
            r)
      r.Request q

  /// Start delivering `StoreChanged` to `q` every time the store's data version moves, polling
  /// every `intervalMs`. Idempotent. No-op without a version source.
  let watchStore (q : Queue) (intervalMs : int) : unit =
    match sources.storeVersion with
    | None -> ()
    | Some version ->
      let p =
        lock sync (fun () ->
          match poll with
          | Some(source, p) when obj.ReferenceEquals(source, version) -> p
          | _ ->
            poll
            |> Option.iter (fun (_, old) -> (old :> System.IDisposable).Dispose())
            let p = new StorePoll(version, intervalMs)
            poll <- Some(version, p)
            p)
      p.Watch q

  /// Stop delivering store changes to `q` (a scheduler that has stopped).
  let unwatchStore (q : Queue) : unit =
    lock sync (fun () -> poll |> Option.iter (fun (_, p) -> p.Unwatch q))
