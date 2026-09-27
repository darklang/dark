/// Processes over the VM: a running computation as a value the runtime can step, park, resume and
/// inspect, and a cooperative scheduler that runs many of them on one thread.
///
/// A process is a `VMState` plus the `ExecutionState` it runs under plus a status. The scheduler
/// steps a process with `Interpreter.executeSync`, which runs it until it finishes, has to wait
/// for something, or spends its instruction budget. A waiting process is parked on the task it is
/// waiting for, and the task's continuation posts `Completed` to the event queue; the scheduler
/// thread resumes it from there. A preempted process goes to the back of the runnable queue.
///
/// Only the scheduler thread steps. Continuations and event sources run on other threads and only
/// ever post to the queue; the one exception (a rare opcode's deferred completion writes the VM it
/// belongs to, while that process is parked and nobody else looks at it) is documented at
/// `Interpreter.StepOutcome`.
///
/// Cores: a scheduler is one thread, and a `Workers` group is N of them (one per core by default),
/// each with its own queue and loop. A process spawned on a worker runs there for its whole life;
/// the only thing it shares with processes elsewhere is its `ExecutionState`, whose caches are
/// concurrent and content-keyed and whose tracer is asked for a per-process view at spawn
/// (`docs/processes.md`, "What a process shares and what it owns").
///
/// The one edge (`docs/processes.md`, "Edges"): a callable the HTTP server's handler path or a
/// preview runs still gets a VM of its own through `Execution.executeApplicable`, outside any
/// process.
module LibExecution.Scheduler

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = RuntimeTypes
module RTE = RT.RuntimeError
module HE = HostEvents

type ProcessId = HE.ProcessId

/// What a parked process is waiting for. For `ps`; the mechanism is always a task.
type Parked =
  /// A host operation the interpreter is performing for a builtin (a file read, an HTTP
  /// request, a process run): the one thing a process is most often waiting on.
  | OnHost of HostTypes.Operation
  /// A builtin that had to wait (`sleep`, a script `eval` runs, ...).
  | OnBuiltin of RT.FQFnName.Builtin
  /// A package function call that had to wait.
  | OnPackageFn of RT.FQFnName.Package
  /// A lambda application that had to wait.
  | OnLambda
  /// One of the rare opcodes, or a deferred return-type check, waiting on the store.
  | OnRareOpcode
  /// `Host.await`: one of these events.
  | OnEvent of HE.EventSpec list
  /// `Exec.await`: another process finishing.
  | OnProcess of ProcessId

/// What a process was started on.
type Entry =
  | EntryFunction of RT.FQFnName.FQFnName
  | EntryExpr

type Status =
  | Runnable
  | Parked of Parked
  | Done of RT.Dval
  | Failed of RTE.Error * RT.CallStack

type Process =
  {
    id : ProcessId
    vm : RT.VMState
    exeState : RT.ExecutionState
    /// What was spawned, for a person reading `ps`.
    entry : Entry
    parent : Option<ProcessId>
    started : System.DateTime
    mutable status : Status
    /// Slices run so far: how many times the budget was refilled. Internal; `ps` shows the
    /// instructions, which is the number that means something to a person.
    mutable slices : int64
    /// Instructions this process has run, summed over its slices (the budget it spent).
    mutable instructionsTaken : int64
    /// Bytes this process allocated on its scheduler's thread, summed over its slices.
    mutable allocated : int64
    /// Why the process was asked to stop (`Kill`), or null; the next step finishes it with
    /// this message instead of running it.
    mutable stopReason : string
    /// True when the stop was a `Kill` (abandon what it waits on) rather than a `Cancel`.
    mutable stopHard : bool
    /// A detached process outlives its parent; the rest die with theirs (`Finish`).
    mutable detached : bool
    /// Completes when the process finishes, for F# callers (`Await`).
    completion : TaskCompletionSource<RT.ExecutionResult>
    /// Set at park, run on the scheduler thread right before the next step: the register write.
    mutable pendingResume : Option<unit -> unit>
    /// The task the process is parked on, so a fault is seen before `pendingResume` runs.
    mutable parkedTask : Task
    /// Set by a builtin that parks the process on events, so `status` can say so rather than
    /// "awaiting hostAwait". Read once at park and cleared.
    mutable parkHint : Parked voption
    /// Store-change generation this process has been told about (see `Subscribe`).
    mutable storeGenSeen : int64
  }

/// One `Host.await` (or `readKey`) in flight: who is waiting, for what, and how to wake them.
type private Subscription =
  {
    proc : Process
    specs : HE.EventSpec list
    wake : TaskCompletionSource<HE.HostEvent>
    /// Timers armed for `Timer ms` specs, disposed when anything satisfies the subscription.
    mutable timers : list<HE.TimerId * System.IDisposable>
  }


/// What `ps` shows for a process. A copy, taken on the scheduler thread; never a reference into a VM.
type ProcessSummary =
  {
    id : ProcessId
    entry : Entry
    status : Status
    parent : Option<ProcessId>
    started : System.DateTime
    /// Instructions run so far (`ps`'s `instructions` column).
    instructionsTaken : int64
    /// Bytes allocated on its scheduler's thread, summed over its slices.
    allocated : int64
    /// Reads the process handed back as promises that have not landed.
    inflight : int
    /// The call stack at the moment of the snapshot, outermost first.
    frames : RT.CallStack
  }


/// Instructions a process may run per slice. The BEAM's reductions, sized so a tight loop yields
/// several hundred times a second and an ordinary program almost never does.
let defaultQuantum = 10_000L

/// How often the store is polled for a change (`PRAGMA data_version`) while a process waits on
/// `StoreChanged`: the ceiling on how long a saved edit takes to reach a live view.
let storePollMs = 200

/// How many worker schedulers a group starts: one per core unless the host says otherwise
/// (`exec.workers` in the store's config; `Cli.fs` reads it).
let mutable defaultWorkers : int = max 1 System.Environment.ProcessorCount

/// Per-process caps, expert settings (`exec.maxInstructions`, `exec.maxBytes`); 0 is no cap. A
/// process over either is finished with a plain reason before its next slice, the way a cancel
/// ends it, so a runaway loop or an allocation storm cannot take the box; `ps` shows what each
/// has used.
let mutable maxInstructions : int64 = 0L
let mutable maxBytes : int64 = 0L


/// Which runnable process a scheduler steps next.
type Policy =
  /// The one that has waited longest. The default, and what every scheduler runs unless told.
  | RoundRobin
  /// Ask: given the runnable processes, oldest first, the id of the one to step. An answer that
  /// names none of them, or none at all, falls back to the oldest. `Cli.fs` installs one that
  /// calls a Dark function (`exec.policy`); a test installs its own. Asked only when there is
  /// a choice (two or more runnable), so an ordinary run never pays for it.
  | Chooser of (list<ProcessSummary> -> Option<ProcessId>)

/// The policy every scheduler consults. Process-wide, set by the host before it starts running.
let mutable policy : Policy = RoundRobin


/// The `ExecutionState` a process runs under: the spawner's, with the pieces that are one
/// process's own replaced. Today that is the tracer (one call stack per process, and the process
/// id on every event); the caches, the policy and the denial lists are shared, the last two under
/// a lock. A record copy, so a spawn costs one allocation here.
let stateForProcess
  (state : RT.ExecutionState)
  (pid : ProcessId)
  : RT.ExecutionState =
  if state.tracing.skipTracing && not state.tracing.traceEffects then
    state
  else
    { state with tracing = state.tracing.forProcess pid }


type Scheduler(quantum : int64) =
  /// Who is waiting on `ExecDone` for which process, across every scheduler in the process:
  /// registered at subscribe time, taken at finish. So a finish posts to the schedulers that
  /// asked, not to every worker in the group: with one worker per core that was fifty queue
  /// posts per finished process, most of a server's per-request cost.
  static let execDoneWatchers =
    System.Collections.Concurrent.ConcurrentDictionary<ProcessId, Scheduler list>()

  /// How many live children each parent has, across the group. A finish cascades to children
  /// by scanning every scheduler's table; for a leaf (an HTTP request, a spawned read) that scan
  /// was most of the per-process cost, so a parent with no entry here is skipped.
  static let childCounts =
    System.Collections.Concurrent.ConcurrentDictionary<ProcessId, int>()

  let queue = new HE.Queue()
  let processes = System.Collections.Generic.Dictionary<ProcessId, Process>()
  let runnable = System.Collections.Generic.Queue<Process>()
  // Touched by the scheduler thread (dispatch) and by builtins subscribing, which run on the
  // scheduler thread in the ordinary case but on a pool thread when the process is inside a
  // builtin that re-entered the interpreter and is continuing on a Ply continuation.
  let sync = obj ()
  let subscriptions = ResizeArray<Subscription>()
  /// Keys that arrived while nobody was waiting for one, for the next `Key` subscriber.
  let pendingKeys = System.Collections.Generic.Queue<RT.Dval>()
  /// How many store changes have been seen. A process subscribing to `StoreChanged` that has not
  /// been told about the latest one is woken at once: a change that lands while a host loop is
  /// rendering is not lost.
  let mutable storeGen = 0L
  let mutable nextTimerId = 0L
  let mutable thread = -1
  /// Set by `Stop`; the loop leaves at its next turn.
  let mutable stopping = false
  /// VMs of processes that finished clean, for the next spawn to reuse (`VMState.reuseFor`): a
  /// fresh VM is five dictionaries and an empty frame pool, and a server spawning a process per
  /// request pays for every frame push again without this. Under `sync`; a handful is enough.
  let spareVMs = System.Collections.Generic.Stack<RT.VMState>()
  /// Finished processes, oldest first, so the table does not grow without bound: `ps` keeps the
  /// last `keepFinished` for a person to look at, and a server's requests do not pile up in it.
  let finished = System.Collections.Generic.Queue<ProcessId>()
  let keepFinished = 64

  /// What a finished process finished WITH, kept until somebody takes it.
  ///
  /// `processes` is the display table and is capped, which is fine for `ps` and wrong for
  /// `await`: `List.parallelMap` over five hundred elements spawns five hundred children, and
  /// the first of them are long forgotten by the time the awaits reach them. So the answer
  /// outlives the row. An entry goes when it is taken; one nobody ever takes (a dropped handle,
  /// a detached child) is evicted oldest-first past `keepResults`, which is where the old
  /// "no process has this handle" came from and is now far enough out to be a real leak rather
  /// than an ordinary list length.
  let finishedResults =
    System.Collections.Concurrent.ConcurrentDictionary<ProcessId, RT.ExecutionResult>()
  let resultOrder = System.Collections.Generic.Queue<ProcessId>()
  let keepResults = 100_000
  /// The group this scheduler belongs to, when it is a root with workers or a worker itself.
  /// `ps` and `kill` answer for the whole group.
  let mutable group : Option<Workers> = None
  /// Live processes (runnable or parked), for placement.
  let mutable live = 0

  static let current = AsyncLocal<Option<Scheduler>>()
  static let currentProcess = AsyncLocal<Option<Process>>()

  static let shared : Lazy<Scheduler> =
    lazy
      (let s = Scheduler(defaultQuantum)
       let thread =
         Thread(
           (fun () -> s.RunUntilStopped()),
           IsBackground = true,
           Name = "dark-shared"
         )
       thread.Start()
       s)

  /// The scheduler running on this thread (or the one this continuation descends from), if any.
  static member Current : Option<Scheduler> =
    // `AsyncLocal` hands back the default (null) where nothing was set; `None` is null too.
    match box current.Value with
    | null -> None
    | _ -> current.Value

  /// The process being stepped, for a builtin that wants to park it on events.
  static member CurrentProcess : Option<Process> =
    match box currentProcess.Value with
    | null -> None
    | _ -> currentProcess.Value

  /// The scheduler on this thread, or the process-wide one for a run nobody scheduled (a test's
  /// `execute`, the LSP, an HTTP handler): its loop runs on a background thread of its own, and
  /// `Exec.spawn` from such a run places its process on that scheduler's workers. Started on
  /// first use.
  static member CurrentOrShared : Scheduler =
    match Scheduler.Current with
    | Some s -> s
    | None -> shared.Value

  member _.Queue = queue

  /// Runnable or parked processes on this scheduler right now.
  member _.Live : int = Volatile.Read &live

  member _.Group
    with get () = group
    and internal set (g : Option<Workers>) = group <- g

  /// The workers this scheduler can hand processes to, started on first use with
  /// `defaultWorkers` of them. A worker asked for its workers answers with its own group.
  member this.Workers : Workers =
    match group with
    | Some g -> g
    | None ->
      lock sync (fun () ->
        match group with
        | Some g -> g
        | None ->
          let g = Workers(this, quantum, defaultWorkers)
          g.Start()
          g)

  // -- Spawning --

  /// A new process that runs `instrs` under `exeState`, from its access. Runnable at once, on
  /// this scheduler.
  member this.Spawn
    (
      exeState : RT.ExecutionState,
      instrs : Option<tlid> * RT.Instructions,
      entry : Entry,
      parent : Option<ProcessId>
    ) : Process =
    let vm =
      match
        lock sync (fun () ->
          if spareVMs.Count > 0 then ValueSome(spareVMs.Pop()) else ValueNone)
      with
      | ValueSome spare ->
        let tlid, program = instrs
        let instrData : RT.InstrData =
          { instructions = List.toArray program.instructions
            resultReg = program.resultIn }
        RT.VMState.reuseFor (spare, tlid, instrData, program.registerCount)
      | ValueNone -> RT.VMState.create instrs
    Interpreter.seedRootAccess exeState.access vm
    let id = System.Guid.NewGuid()
    let p =
      { id = id
        vm = vm
        exeState = stateForProcess exeState id
        entry = entry
        parent = parent
        started = System.DateTime.UtcNow
        status = Runnable
        slices = 0L
        instructionsTaken = 0L
        allocated = 0L
        stopReason = null
        stopHard = false
        detached = false
        completion =
          TaskCompletionSource<RT.ExecutionResult>(
            TaskCreationOptions.RunContinuationsAsynchronously
          )
        pendingResume = None
        parkedTask = null
        parkHint = ValueNone
        storeGenSeen = Volatile.Read &storeGen }
    lock sync (fun () ->
      processes[p.id] <- p
      runnable.Enqueue p)
    Interlocked.Increment &live |> ignore<int>
    match parent with
    | Some par -> childCounts.AddOrUpdate(par, 1, (fun _ n -> n + 1)) |> ignore<int>
    | None -> ()
    // The loop blocks on the queue when nothing is runnable; a spawn from another thread (a
    // test, an F# host, a process on another scheduler) has to wake it. From the scheduler
    // thread it is a harmless no-op.
    if Thread.CurrentThread.ManagedThreadId <> thread then
      queue.Post HE.HostEvent.Wake
    p

  /// Spawn on a worker rather than here: the process runs on another core for its whole life.
  /// The least loaded worker takes it. `Await` and `ps` work the same either way.
  member this.SpawnOn
    (
      exeState : RT.ExecutionState,
      instrs : Option<tlid> * RT.Instructions,
      entry : Entry,
      parent : Option<ProcessId>
    ) : Process =
    this.Workers.Spawn(exeState, instrs, entry, parent)

  /// A new process that applies `applicable` to `arg` (`Exec.spawn f` runs `f ()`), starting from
  /// `access` rather than the state's own: the spawner's, at the moment of the spawn, exactly as
  /// a closure captures it. On a worker.
  member this.SpawnApply
    (
      exeState : RT.ExecutionState,
      applicable : RT.Applicable,
      arg : RT.Dval,
      parent : Option<ProcessId>,
      access : Permissions.Access
    ) : Process =
    let entry =
      match applicable with
      | RT.AppNamedFn named -> EntryFunction named.name
      | RT.AppLambda _ -> EntryExpr
    let instrs = Execution.instructionsForApply applicable arg
    let p =
      this.SpawnOn({ exeState with access = access }, (None, instrs), entry, parent)
    p

  /// The process with this id, anywhere in the group.
  member this.Find(pid : ProcessId) : Option<Process> =
    match group with
    | Some g -> g.All |> List.tryPick (fun s -> s.FindHere pid)
    | None -> this.FindHere pid

  /// What a finished process finished with, taken (so it is not kept twice). `Find` first: a
  /// process still in the table has its own completion task, which is what an awaiter parks on.
  member this.TakeResult(pid : ProcessId) : Option<RT.ExecutionResult> =
    match group with
    | Some g -> g.All |> List.tryPick (fun s -> s.TakeResultHere pid)
    | None -> this.TakeResultHere pid

  member internal _.TakeResultHere(pid : ProcessId) : Option<RT.ExecutionResult> =
    match finishedResults.TryRemove pid with
    | true, r -> Some r
    | false, _ -> None

  /// `Find`, on this scheduler's own table.
  member internal _.FindHere(pid : ProcessId) : Option<Process> =
    match lock sync (fun () -> processes.TryGetValue pid) with
    | true, p -> Some p
    | false, _ -> None

  /// Spawn a call to a named function: the program `Execution.executeFunction` builds, as a process.
  member this.SpawnFunction
    (
      exeState : RT.ExecutionState,
      name : RT.FQFnName.FQFnName,
      typeArgs : list<RT.TypeReference>,
      args : NEList<RT.Dval>,
      parent : Option<ProcessId>
    ) : Process =
    let instrs = Execution.instructionsForFunctionCall name typeArgs args
    this.Spawn(exeState, (None, instrs), EntryFunction name, parent)

  /// The process's result, when it has one.
  member _.Await(p : Process) : Task<RT.ExecutionResult> = p.completion.Task

  /// `Await`, giving up after `ms`: `None` then, and the process keeps running. The timer is
  /// dropped as soon as the process wins, so a short wait inside a loop does not leave a timer
  /// per iteration ticking.
  member _.AwaitWithin(p : Process, ms : int) : Task<Option<RT.ExecutionResult>> =
    let completion = p.completion.Task
    if completion.IsCompleted then
      Task.FromResult(Some completion.Result)
    else
      task {
        use cts = new CancellationTokenSource()
        let delay = Task.Delay(ms, cts.Token)
        let! first = Task.WhenAny(completion :> Task, delay)
        if obj.ReferenceEquals(first, delay) then
          return None
        else
          cts.Cancel()
          return Some completion.Result
      }

  /// Post an event as a source would. What a test harness calls to press a key.
  member _.PushEvent(ev : HE.HostEvent) : unit = queue.Post ev

  // -- Events --

  /// Park `p` on the first of `specs` to happen. Returns the task the builtin hands back to the
  /// interpreter; the scheduler completes it from `Dispatch`. Delivered at once when it can be: a
  /// key that arrived early, a store change `p` has not seen.
  member this.Subscribe
    (
      p : Process,
      specs : HE.EventSpec list
    ) : Task<HE.HostEvent> =
    let wake =
      TaskCompletionSource<HE.HostEvent>(
        TaskCreationOptions.RunContinuationsAsynchronously
      )
    // Asked BEFORE this scheduler's lock is taken. `Find` walks the group and takes every
    // other scheduler's lock, so asking it from inside ours is a lock-order inversion: two
    // workers each awaiting a process on the other deadlock, and both worker loops are gone
    // with no diagnostic. A process that finishes between this and the subscription is
    // covered by the watcher being registered first.
    let alreadyOver =
      specs
      |> List.choose (fun spec ->
        match spec with
        | HE.EventSpec.ExecDone pid ->
          match this.Find pid with
          | Some { status = Done _ }
          | Some { status = Failed _ }
          | None -> Some pid
          | Some _ -> None
        | _ -> None)
      |> Set.ofList
    lock sync (fun () ->
      let wantsKey = List.contains HE.EventSpec.Key specs
      let wantsStore = List.contains HE.EventSpec.StoreChanged specs
      if wantsKey && pendingKeys.Count > 0 then
        wake.SetResult(HE.HostEvent.Key(pendingKeys.Dequeue()))
      elif wantsStore && p.storeGenSeen < storeGen then
        p.storeGenSeen <- storeGen
        wake.SetResult HE.HostEvent.StoreChanged
      else
        // Only when it will actually park: a wake delivered above completes the builtin
        // synchronously and the process never parks, so a hint set then would describe the
        // next, unrelated park.
        p.parkHint <- ValueSome(OnEvent specs)
        let sub = { proc = p; specs = specs; wake = wake; timers = [] }
        subscriptions.Add sub
        for spec in specs do
          match spec with
          | HE.EventSpec.Key -> HE.Shared.requestKey queue
          | HE.EventSpec.StoreChanged -> HE.Shared.watchStore queue storePollMs
          | HE.EventSpec.Timer ms ->
            let id = Interlocked.Increment &nextTimerId
            sub.timers <- (id, queue.ArmTimer(id, ms)) :: sub.timers
          | HE.EventSpec.ExecDone pid ->
            execDoneWatchers.AddOrUpdate(pid, [ this ], (fun _ ws -> this :: ws))
            |> ignore<Scheduler list>
            // Already over (or forgotten): nothing will post, so answer now. The watcher is
            // registered first, so a finish racing this sees it either way.
            if Set.contains pid alreadyOver then
              this.Satisfy(sub, HE.HostEvent.ExecDone pid))
    wake.Task

  /// Wake a subscription and drop it.
  member private _.Satisfy(sub : Subscription, ev : HE.HostEvent) : unit =
    subscriptions.Remove sub |> ignore<bool>
    for (_, timer) in sub.timers do
      timer.Dispose()
    sub.wake.TrySetResult ev |> ignore<bool>

  /// Route one event. Scheduler thread only.
  member private this.Dispatch(ev : HE.HostEvent) : unit =
    match ev with
    | HE.HostEvent.Completed pid ->
      match this.FindHere pid with
      | Some p ->
        match p.status with
        | Parked _ ->
          p.status <- Runnable
          lock sync (fun () -> runnable.Enqueue p)
        | _ -> ()
      | None -> ()
    | HE.HostEvent.Key k ->
      lock sync (fun () ->
        let waiting =
          subscriptions
          |> Seq.tryFind (fun s -> List.contains HE.EventSpec.Key s.specs)
        match waiting with
        | Some sub -> this.Satisfy(sub, ev)
        | None -> pendingKeys.Enqueue k)
    | HE.HostEvent.Timer id ->
      lock sync (fun () ->
        let waiting =
          subscriptions
          |> Seq.tryFind (fun s ->
            s.timers |> List.exists (fun (tid, _) -> tid = id))
        match waiting with
        | Some sub -> this.Satisfy(sub, ev)
        | None -> ())
    | HE.HostEvent.StoreChanged ->
      lock sync (fun () ->
        storeGen <- storeGen + 1L
        let waiting =
          subscriptions
          |> Seq.filter (fun s -> List.contains HE.EventSpec.StoreChanged s.specs)
          |> List.ofSeq
        for sub in waiting do
          sub.proc.storeGenSeen <- storeGen
          this.Satisfy(sub, ev))
    | HE.HostEvent.Wake -> ()
    | HE.HostEvent.ExecDone pid ->
      lock sync (fun () ->
        let waiting =
          subscriptions
          |> Seq.filter (fun s -> List.contains (HE.EventSpec.ExecDone pid) s.specs)
          |> List.ofSeq
        for sub in waiting do
          this.Satisfy(sub, ev))

  // -- Stepping --

  member private this.Finish(p : Process, result : RT.ExecutionResult) : unit =
    p.status <-
      match result with
      | Ok dv -> Done dv
      | Error(rte, stack) -> Failed(rte, stack)
    p.pendingResume <- None
    p.parkedTask <- null
    Interlocked.Decrement &live |> ignore<int>
    // Does nothing in non-tests.
    p.exeState.test.postTestExecutionHook p.exeState.test
    lock sync (fun () ->
      // Only a VM that ran to completion has popped every frame, which is what `reuseFor`
      // needs; one with a read still in flight would have that read's landing count against
      // the next process.
      match result with
      | Ok _ when spareVMs.Count < 8 && Volatile.Read &p.vm.inflight = 0 ->
        spareVMs.Push p.vm
      | _ -> ()
      finished.Enqueue p.id
      while finished.Count > keepFinished do
        processes.Remove(finished.Dequeue()) |> ignore<bool>
      finishedResults[p.id] <- result
      resultOrder.Enqueue p.id
      while resultOrder.Count > keepResults do
        let old = resultOrder.Dequeue()
        finishedResults.TryRemove old |> ignore<bool * RT.ExecutionResult>)
    // To the schedulers with a Dark subscriber waiting on this process, wherever they are;
    // how it ended is the subscriber's to ask (`Exec.await`).
    match execDoneWatchers.TryRemove p.id with
    | true, watchers ->
      for s in List.distinct watchers do
        s.Queue.Post(HE.HostEvent.ExecDone p.id)
    | false, _ -> ()
    p.completion.TrySetResult result |> ignore<bool>
    match p.parent with
    | Some par ->
      match childCounts.AddOrUpdate(par, 0, (fun _ n -> n - 1)) with
      // Removed only if it is STILL zero: a bare `TryRemove` takes whatever the value is now,
      // so a parent that spawned again between the decrement and the remove loses its entry,
      // and its new child then outlives it -- the one thing this table exists to prevent.
      | n when n <= 0 ->
        childCounts.TryRemove(System.Collections.Generic.KeyValuePair(par, 0))
        |> ignore<bool>
      | _ -> ()
    | None -> ()
    // Its children go with it, wherever in the group they run, unless spawned detached: a
    // process that spawned and never awaited leaves nothing running behind it, and a stop
    // of a parent reaches everything under it, as hard or as politely as the parent's was.
    // A leaf has no entry in `childCounts`, so the group-wide scan is skipped for it.
    if childCounts.ContainsKey p.id then
      this.StopChildrenOf(p.id, "its parent finished", p.stopHard)

  member private this.Fail(p : Process, ex : exn) : unit =
    match ex with
    | RT.RuntimeErrorException(_, rte) ->
      this.Finish(p, Error(rte, Execution.callStackFromVM p.vm))
    | ex ->
      let metadata : Metadata =
        Exception.toMetadata ex |> List.map (fun (k, v) -> k, string v)
      try
        p.exeState.reportException p.exeState p.vm metadata ex
        |> Ply.toTask
        |> ignore<Task<unit>>
      with _ ->
        ()
      let metadata = metadata |> List.map (fun (k, v) -> k, RT.DString(string v))
      this.Finish(
        p,
        Error(
          RTE.UncaughtException(ex.Message, metadata),
          Execution.callStackFromVM p.vm
        )
      )

  /// What the current frame is applying, for `ps`. Best effort: the instruction under the counter
  /// is an `Apply` whose callee register still holds the callable; anything else is a rare opcode.
  member private _.Describe(p : Process) : Parked =
    try
      let op = p.vm.hostInflight
      if not (obj.ReferenceEquals(op, null)) then
        OnHost op
      else
        let frame = p.vm.callFrames[p.vm.currentFrameID]
        match frame.instrData.instructions[frame.programCounter] with
        | RT.Apply(_, calleeReg, _, _) ->
          match frame.registers[calleeReg] with
          | RT.DApplicable(RT.AppNamedFn fn) ->
            match fn.name with
            | RT.FQFnName.Builtin b -> OnBuiltin b
            | RT.FQFnName.Package h -> OnPackageFn h
          | RT.DApplicable(RT.AppLambda _) -> OnLambda
          | _ -> OnRareOpcode
        | _ -> OnRareOpcode
    with _ ->
      OnRareOpcode

  /// One slice of `p`. Scheduler thread only.
  member private this.Step(p : Process) : unit =
    // The one rule, checked: only this thread ever steps.
    if thread <> -1 && Thread.CurrentThread.ManagedThreadId <> thread then
      Exception.raiseInternal
        "Scheduler.Step off the scheduler thread"
        [ "thread", Thread.CurrentThread.ManagedThreadId; "scheduler", thread ]
    // A stop asked for (`Cancel`, `Kill`) or a cap crossed: the process ends here with the
    // reason, before its slice.
    if isNull p.stopReason then
      p.stopReason <-
        if maxInstructions > 0L && p.instructionsTaken >= maxInstructions then
          $"over {maxInstructions} instructions (exec.maxInstructions)"
        elif maxBytes > 0L && p.allocated >= maxBytes then
          $"over {maxBytes} bytes allocated (exec.maxBytes)"
        else
          null
    if not (isNull p.stopReason) then
      this.Finish(
        p,
        Error(
          RTE.UncaughtException(p.stopReason, []),
          Execution.callStackFromVM p.vm
        )
      )
    else
      currentProcess.Value <- Some p
      try
        try
          // A fault on the task the process was parked on is the process's failure, before the
          // register write that would read the fault out as a result.
          let parked = p.parkedTask
          if not (isNull parked) && parked.IsFaulted then
            let inner =
              match parked.Exception with
              | null -> System.Exception "parked task faulted"
              | agg -> agg.GetBaseException()
            raise inner
          match p.pendingResume with
          | Some resume ->
            p.pendingResume <- None
            p.parkedTask <- null
            resume ()
          | None -> ()

          p.vm.budget <- quantum
          p.slices <- p.slices + 1L
          let allocBefore = System.GC.GetAllocatedBytesForCurrentThread()
          let outcome = Interpreter.executeSync p.exeState p.vm
          // What the slice actually spent: the budget counts down per instruction, and a
          // process that waits or finishes leaves the rest of it.
          p.instructionsTaken <- p.instructionsTaken + (quantum - p.vm.budget)
          p.allocated <-
            p.allocated
            + (System.GC.GetAllocatedBytesForCurrentThread() - allocBefore)
          match outcome with
          | Interpreter.StepDone dv -> this.Finish(p, Ok dv)
          | Interpreter.StepBudget ->
            p.status <- Runnable
            lock sync (fun () -> runnable.Enqueue p)
          | Interpreter.StepAwait(wait, resume) ->
            if wait.IsCompletedSuccessfully then
              resume ()
              p.status <- Runnable
              lock sync (fun () -> runnable.Enqueue p)
            elif wait.IsFaulted then
              raise (wait.Exception.GetBaseException())
            else
              let parkedOn =
                match p.parkHint with
                | ValueSome hint -> hint
                | ValueNone -> this.Describe p
              p.parkHint <- ValueNone
              p.status <- Parked parkedOn
              p.pendingResume <- Some resume
              p.parkedTask <- wait
              wait.ContinueWith(
                (fun (_ : Task) -> queue.Post(HE.HostEvent.Completed p.id)),
                TaskContinuationOptions.ExecuteSynchronously
              )
              |> ignore<Task>
        with ex ->
          this.Fail(p, ex)
      finally
        currentProcess.Value <- None

  /// The loop: round robin over the runnable processes, draining the event queue between slices
  /// and blocking on it when nothing can run, until `finished ()` or `Stop`.
  member private this.Run(finished : unit -> bool) : unit =
    thread <- Thread.CurrentThread.ManagedThreadId
    current.Value <- Some this
    try
      let mutable ev = Unchecked.defaultof<HE.HostEvent>
      while not (finished ()) && not (Volatile.Read &stopping) do
        while queue.TryTake(&ev) do
          this.Dispatch ev
        // A chooser is asked only when there is a choice, and outside the lock: it is Dark code
        // that may well call `Exec.list`, which takes it. It sees copies, in queue order.
        let asked =
          match policy with
          | RoundRobin -> None
          | Chooser choose ->
            let summaries =
              lock sync (fun () ->
                if runnable.Count > 1 then
                  runnable |> Seq.map this.SummaryOf |> List.ofSeq
                else
                  [])
            if summaries.IsEmpty then None else choose summaries
        let next =
          lock sync (fun () ->
            match asked with
            | Some id when runnable |> Seq.exists (fun p -> p.id = id) ->
              // The pick leaves the queue; the rest keep their order.
              let rest = List.ofSeq runnable
              runnable.Clear()
              let mutable chosen = None
              for p in rest do
                if p.id = id && chosen.IsNone then
                  chosen <- Some p
                else
                  runnable.Enqueue p
              chosen
            | _ -> if runnable.Count = 0 then None else Some(runnable.Dequeue()))
        match next with
        | Some p -> this.Step p
        | None ->
          if not (finished ()) && not (Volatile.Read &stopping) then
            this.Dispatch(queue.Take())
    finally
      current.Value <- None
      thread <- -1

  /// Run the scheduler on this thread until `until` is done.
  member this.RunUntil(until : Process) : RT.ExecutionResult =
    this.Run(fun () -> until.completion.Task.IsCompleted)
    until.completion.Task.Result

  /// Run the scheduler on this thread until `Stop`. What a worker's thread does.
  member this.RunUntilStopped() : unit = this.Run(fun () -> false)

  /// Ask the loop to leave at its next turn, from any thread. Processes still on it stay where
  /// they are; a group stops its workers only when the root is done.
  member _.Stop() : unit =
    Volatile.Write(&stopping, true)
    HE.Shared.unwatchStore queue
    queue.Post HE.HostEvent.Wake

  // -- ps --

  /// Stop a process. It finishes `Failed(reason)` at its next turn. `hard` (`ps kill`) gives a
  /// parked process that turn at once: whatever it was waiting for is abandoned (the task's
  /// late completion posts for a process that is no longer parked, and is dropped). Not hard
  /// (`Exec.cancel`, `ps cancel`) lets a wait on the host or on another process complete
  /// first, so a write in flight finishes; a wait on events (`Host.await`, `readKey`) is cut
  /// either way, since nothing is in flight there. A running process finishes its slice
  /// first; one that completes within it completes. Any scheduler in the group finds it. Its
  /// undetached children are stopped the same way, now (so a parent waiting on one is not
  /// kept waiting) and again when it finishes (for any spawned in between).
  member private this.StopProcess
    (
      pid : ProcessId,
      reason : string,
      hard : bool
    ) : bool =
    let found =
      match group with
      | Some g -> g.All |> List.exists (fun s -> s.StopHere(pid, reason, hard))
      | None -> this.StopHere(pid, reason, hard)
    if found then this.StopChildrenOf(pid, reason, hard)
    found

  /// `ps kill`.
  member this.Kill(pid : ProcessId) : bool =
    this.StopProcess(pid, "killed from ps", true)

  /// `Exec.cancel`, `ps cancel`.
  member this.Cancel(pid : ProcessId) : bool =
    this.StopProcess(pid, "cancelled", false)

  /// Every unfinished, undetached child of `pid`, anywhere in the group, is stopped.
  member this.StopChildrenOf(pid : ProcessId, reason : string, hard : bool) : unit =
    let schedulers =
      match group with
      | Some g -> g.All
      | None -> [ this ]
    for s in schedulers do
      s.StopChildrenHere(pid, reason, hard)

  member internal this.StopChildrenHere
    (
      pid : ProcessId,
      reason : string,
      hard : bool
    ) : unit =
    let children =
      lock sync (fun () ->
        processes.Values
        |> Seq.filter (fun c ->
          c.parent = Some pid
          && not c.detached
          && isNull c.stopReason
          && (match c.status with
              | Done _
              | Failed _ -> false
              | _ -> true))
        |> List.ofSeq)
    for c in children do
      this.StopHere(c.id, reason, hard) |> ignore<bool>
      // Grandchildren, wherever they run.
      this.StopChildrenOf(c.id, reason, hard)

  /// `Stop`, on this scheduler's own table.
  member internal this.StopHere
    (
      pid : ProcessId,
      reason : string,
      hard : bool
    ) : bool =
    match lock sync (fun () -> processes.TryGetValue pid) with
    | true, p ->
      p.stopReason <- reason
      p.stopHard <- hard
      lock sync (fun () ->
        let mine =
          subscriptions |> Seq.filter (fun s -> s.proc.id = pid) |> List.ofSeq
        for sub in mine do
          subscriptions.Remove sub |> ignore<bool>
          for (_, timer) in sub.timers do
            timer.Dispose()
          sub.wake.TrySetCanceled() |> ignore<bool>)
      // A hard stop abandons the wait: the turn is posted now. A soft one lets it land; a
      // cut event subscription lands on its own (the cancelled wake posts like any completion).
      match p.status with
      | Parked _ when hard -> queue.Post(HE.HostEvent.Completed pid)
      | _ -> ()
      true
    | false, _ -> false

  /// Every process the group knows, as copies.
  member this.Snapshot() : list<ProcessSummary> =
    match group with
    | Some g -> g.All |> List.collect (fun s -> s.SnapshotHere())
    | None -> this.SnapshotHere()

  /// A copy of `p` for `ps` and the policy. The frames are read off a VM another thread may be
  /// stepping: `callStackFromVM` walks the parent chain, and a frame popped under it is caught
  /// and read as no frames, never as a fault.
  member _.SummaryOf(p : Process) : ProcessSummary =
    { id = p.id
      entry = p.entry
      status = p.status
      parent = p.parent
      started = p.started
      instructionsTaken = p.instructionsTaken
      allocated = p.allocated
      inflight =
        (match p.status with
         | Done _
         | Failed _ -> 0
         | _ -> Volatile.Read &p.vm.inflight)
      frames =
        match p.status with
        | Done _
        | Failed _ -> []
        | _ ->
          try
            Execution.callStackFromVM p.vm
          with _ ->
            [] }

  /// `Snapshot`, for this scheduler's own table.
  member this.SnapshotHere() : list<ProcessSummary> =
    lock sync (fun () -> processes.Values |> Seq.map this.SummaryOf |> List.ofSeq)


/// A root scheduler and its workers: N more schedulers, each looping on a thread of its own, so
/// processes spawned on them run on N cores. Started lazily by the root's `Workers`; every one is
/// a background thread, so a CLI that exits does not wait on them, and `Stop` ends them for a
/// host that wants to (tests).
and Workers(root : Scheduler, quantum : int64, count : int) =
  let workers = List.init (max 1 count) (fun _ -> Scheduler(quantum))

  /// Join the root and the workers into one group and start the worker threads. Once, by the
  /// root's `Workers` property.
  member this.Start() : unit =
    root.Group <- Some this
    workers
    |> List.iteri (fun i w ->
      w.Group <- Some this
      let thread =
        Thread(
          (fun () -> w.RunUntilStopped()),
          IsBackground = true,
          Name = $"dark-worker-{i}"
        )
      thread.Start())

  /// The workers, without the root.
  member _.Members : Scheduler list = workers

  /// Root first, then the workers.
  member _.All : Scheduler list = root :: workers

  /// Spawn on the least loaded worker.
  member _.Spawn
    (
      exeState : RT.ExecutionState,
      instrs : Option<tlid> * RT.Instructions,
      entry : Entry,
      parent : Option<ProcessId>
    ) : Process =
    let w = workers |> List.minBy (fun w -> w.Live)
    w.Spawn(exeState, instrs, entry, parent)

  /// End every worker's loop. Processes still on them are left as they are.
  member _.Stop() : unit =
    for w in workers do
      w.Stop()


/// Run a named function as the root process of a fresh scheduler on the calling thread, and return
/// its result when it finishes. What the CLI's `main` calls instead of `executeFunction`.
let executeFunction
  (exeState : RT.ExecutionState)
  (name : RT.FQFnName.FQFnName)
  (typeArgs : list<RT.TypeReference>)
  (args : NEList<RT.Dval>)
  : RT.ExecutionResult =
  let s = Scheduler(defaultQuantum)
  let p = s.SpawnFunction(exeState, name, typeArgs, args, None)
  s.RunUntil p
