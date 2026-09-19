/// The event queue a scheduler feeds its processes from, and the sources that post to it.
///
/// One queue per scheduler. Everything that happens outside a process (a key pressed, a timer
/// firing, the store changing, a parked await completing) becomes a `HostEvent` on this queue,
/// and the scheduler thread is the only consumer. Sources post from wherever they run: the stdin
/// reader thread, `System.Threading.Timer` callbacks, the store poll, thread-pool continuations.
/// None of them ever touch a VM; that is the whole discipline (`docs/processes.md`).
///
/// This module is the queue and the source registry only. It has no idea what a process is.
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
  /// The store's data version moved. `change` is `Stdlib.Host.Change`: `Unknown` until live's
  /// `scmOpsSince` lands and the poll can say what changed.
  | StoreChanged of change : RT.Dval
  /// A timer registered by a subscription fired.
  | Timer of TimerId
  /// The task a process was parked on finished (well or badly). Internal: the scheduler resumes
  /// the process on its own thread.
  | Completed of ProcessId
  /// A process finished with this value. For Dark subscribers (`ExecDone id`); F# callers await the
  /// process directly.
  | ExecDone of ProcessId * RT.Dval


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
    /// How `StoreChanged` describes what changed. `Unknown` today.
    mutable storeChange : Option<unit -> RT.Dval>
  }

let sources : Sources = { readKey = None; storeVersion = None; storeChange = None }


/// The queue, plus the two long-lived sources it owns: the stdin reader thread and the store poll.
/// Both start lazily, on the first subscription that needs them, so a run that never waits on a
/// key never reads the console and a run that never waits on the store never polls it.
type Queue() =
  let events = new BlockingCollection<HostEvent>(new ConcurrentQueue<HostEvent>())

  // The reader thread reads one key per request. "Demand-driven" is load-bearing: a thread that
  // read continuously would eat keys meant for a `readLine` after the TUI has quit, and would
  // hold `TreatControlCAsInput` while nothing is listening.
  let keyRequests = new SemaphoreSlim(0)
  let mutable readerStarted = 0

  let mutable pollStarted = 0
  let mutable pollTimer : Timer = null
  let mutable lastVersion = -1L

  member _.Post(ev : HostEvent) : unit = events.Add ev

  /// Take the next event, blocking until there is one.
  member _.Take() : HostEvent = events.Take()

  member _.TryTake(ev : byref<HostEvent>) : bool = events.TryTake(&ev)

  /// Ask the reader thread for one key. Starts the thread on the first call; no-op when stdin is
  /// not a terminal (no source installed), in which case nothing will ever post `Key`.
  member this.RequestKey() : unit =
    match sources.readKey with
    | None -> ()
    | Some readKey ->
      if Interlocked.Exchange(&readerStarted, 1) = 0 then
        let thread =
          Thread(
            (fun () ->
              while true do
                keyRequests.Wait()
                let key =
                  try
                    Some(readKey ())
                  with _ ->
                    // The console went away (stdin closed under us). Stop reading; the
                    // subscriber stays parked, as it would have blocked before.
                    None
                match key with
                | Some k -> this.Post(HostEvent.Key k)
                | None -> ()),
            IsBackground = true,
            Name = "dark-stdin-reader"
          )
        thread.Start()
      keyRequests.Release() |> ignore<int>

  /// Start polling the store's data version every `intervalMs`, posting `StoreChanged` when it
  /// moves. Idempotent. No-op without a version source.
  member this.EnsureStorePoll(intervalMs : int) : unit =
    match sources.storeVersion with
    | None -> ()
    | Some version ->
      if Interlocked.Exchange(&pollStarted, 1) = 0 then
        lastVersion <-
          (try
            version ()
           with _ ->
             -1L)
        pollTimer <-
          new Timer(
            (fun _ ->
              let now =
                (try
                  version ()
                 with _ ->
                   lastVersion)
              if now <> lastVersion then
                lastVersion <- now
                let change =
                  match sources.storeChange with
                  | Some describe -> describe ()
                  | None -> RT.DUnit
                this.Post(HostEvent.StoreChanged change)),
            null,
            intervalMs,
            intervalMs
          )

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

  interface System.IDisposable with
    member _.Dispose() =
      if not (isNull pollTimer) then pollTimer.Dispose()
      events.Dispose()
