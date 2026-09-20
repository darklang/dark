/// Processes over the VM: a running computation as a value the runtime can step, park, resume and
/// inspect, and a cooperative scheduler that runs many of them on one thread.
///
/// A process is a `VMState` plus the `ExecutionState` it runs under plus a status. The scheduler
/// steps a process with `Interpreter.stepScheduled`, which runs it until it finishes, has to wait
/// for something, or spends its instruction budget. A waiting process is parked on the task it is
/// waiting for, and the task's continuation posts `Completed` to the event queue; the scheduler
/// thread resumes it from there. A preempted process goes to the back of the runnable queue.
///
/// Only the scheduler thread steps. Continuations and event sources run on other threads and only
/// ever post to the queue; the one exception (a rare opcode's deferred completion writes the VM it
/// belongs to, while that process is parked and nobody else looks at it) is documented at
/// `Interpreter.StepOutcome`.
///
/// Not here yet, deliberately (`docs/processes.md` lists them with the follow-up that brings each):
/// a process parked inside a higher-order builtin (`List.map f` where `f` awaits) is parked as one
/// Ply and `ps` sees the outer frame only; no cores; no user-level `spawn`; no record/replay.
module LibExecution.Scheduler

open System.Threading
open System.Threading.Tasks

open Prelude

module RT = RuntimeTypes
module RTE = RT.RuntimeError
module HE = HostEvents

type ProcessId = HE.ProcessId

/// What a parked process is waiting for. For `ps`; the mechanism is always a task.
type Parked =
  /// A builtin that had to wait (`sleep`, an HTTP call, a script `eval` runs, ...).
  | OnBuiltin of RT.FQFnName.Builtin
  /// A package function call that had to wait.
  | OnPackageFn of RT.FQFnName.Package
  /// A lambda application that had to wait.
  | OnLambda
  /// One of the rare opcodes, or a deferred return-type check, waiting on the store.
  | OnRareOpcode
  /// `Host.await`: one of these events.
  | OnEvent of HE.EventSpec list

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
    /// Slices run so far: how many times the budget was refilled.
    mutable slices : int64
    /// True once `Kill` was asked for; the next step finishes it instead of running it.
    mutable cancelRequested : bool
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
    slices : int64
    /// The call stack at the moment of the snapshot, outermost first.
    frames : RT.CallStack
  }


/// Instructions a process may run per slice. The BEAM's reductions, sized so a tight loop yields
/// several hundred times a second and an ordinary program almost never does.
let defaultQuantum = 10_000L


type Scheduler(quantum : int64) =
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
  let mutable latestChange = RT.DUnit
  let mutable nextTimerId = 0L
  let mutable thread = -1

  static let current = AsyncLocal<Option<Scheduler>>()
  static let currentProcess = AsyncLocal<Option<Process>>()

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

  member _.Quantum = quantum

  member _.Queue = queue

  // -- Spawning --

  /// A new process that runs `instrs` under `exeState`, from its access. Runnable at once.
  member this.Spawn
    (
      exeState : RT.ExecutionState,
      instrs : Option<tlid> * RT.Instructions,
      entry : Entry,
      parent : Option<ProcessId>
    ) : Process =
    let vm = RT.VMState.create instrs
    Interpreter.seedRootAccess exeState.access vm
    let p =
      { id = System.Guid.NewGuid()
        vm = vm
        exeState = exeState
        entry = entry
        parent = parent
        started = System.DateTime.UtcNow
        status = Runnable
        slices = 0L
        cancelRequested = false
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
    // The loop blocks on the queue when nothing is runnable; a spawn from another thread (a
    // test, an F# host) has to wake it. From the scheduler thread it is a harmless no-op.
    if Thread.CurrentThread.ManagedThreadId <> thread then
      queue.Post HE.HostEvent.Wake
    p

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
    lock sync (fun () ->
      let wantsKey = List.contains HE.EventSpec.Key specs
      let wantsStore = List.contains HE.EventSpec.StoreChanged specs
      if wantsKey && pendingKeys.Count > 0 then
        wake.SetResult(HE.HostEvent.Key(pendingKeys.Dequeue()))
      elif wantsStore && p.storeGenSeen < storeGen then
        p.storeGenSeen <- storeGen
        wake.SetResult(HE.HostEvent.StoreChanged latestChange)
      else
        // Only when it will actually park: a wake delivered above completes the builtin
        // synchronously and the process never parks, so a hint set then would describe the
        // next, unrelated park.
        p.parkHint <- ValueSome(OnEvent specs)
        let sub = { proc = p; specs = specs; wake = wake; timers = [] }
        subscriptions.Add sub
        for spec in specs do
          match spec with
          | HE.EventSpec.Key -> queue.RequestKey()
          | HE.EventSpec.StoreChanged -> queue.EnsureStorePoll 200
          | HE.EventSpec.Timer ms ->
            let id = Interlocked.Increment &nextTimerId
            sub.timers <- (id, queue.ArmTimer(id, ms)) :: sub.timers
          | HE.EventSpec.ExecDone _ -> ())
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
      match processes.TryGetValue pid with
      | true, p ->
        match p.status with
        | Parked _ ->
          p.status <- Runnable
          lock sync (fun () -> runnable.Enqueue p)
        | _ -> ()
      | false, _ -> ()
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
    | HE.HostEvent.StoreChanged change ->
      lock sync (fun () ->
        storeGen <- storeGen + 1L
        latestChange <- change
        let waiting =
          subscriptions
          |> Seq.filter (fun s -> List.contains HE.EventSpec.StoreChanged s.specs)
          |> List.ofSeq
        for sub in waiting do
          sub.proc.storeGenSeen <- storeGen
          this.Satisfy(sub, ev))
    | HE.HostEvent.Wake -> ()
    | HE.HostEvent.ExecDone(pid, _) ->
      lock sync (fun () ->
        let waiting =
          subscriptions
          |> Seq.filter (fun s -> List.contains (HE.EventSpec.ExecDone pid) s.specs)
          |> List.ofSeq
        for sub in waiting do
          this.Satisfy(sub, ev))

  // -- Stepping --

  member private _.Finish(p : Process, result : RT.ExecutionResult) : unit =
    p.status <-
      match result with
      | Ok dv -> Done dv
      | Error(rte, stack) -> Failed(rte, stack)
    p.pendingResume <- None
    p.parkedTask <- null
    // Does nothing in non-tests.
    p.exeState.test.postTestExecutionHook p.exeState.test
    match result with
    | Ok dv -> queue.Post(HE.HostEvent.ExecDone(p.id, dv))
    | Error _ -> ()
    p.completion.TrySetResult result |> ignore<bool>

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
    if p.cancelRequested then
      this.Finish(
        p,
        Error(RTE.UncaughtException("cancelled", []), Execution.callStackFromVM p.vm)
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
          match Interpreter.stepScheduled p.exeState p.vm with
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

  /// Run the scheduler on this thread until `until` is done. Round robin over the runnable
  /// processes, draining the event queue between slices and blocking on it when nothing can run.
  member this.RunUntil(until : Process) : RT.ExecutionResult =
    thread <- Thread.CurrentThread.ManagedThreadId
    current.Value <- Some this
    try
      let mutable ev = Unchecked.defaultof<HE.HostEvent>
      while not until.completion.Task.IsCompleted do
        while queue.TryTake(&ev) do
          this.Dispatch ev
        let next =
          lock sync (fun () ->
            if runnable.Count > 0 then Some(runnable.Dequeue()) else None)
        match next with
        | Some p -> this.Step p
        | None ->
          if not until.completion.Task.IsCompleted then this.Dispatch(queue.Take())
      until.completion.Task.Result
    finally
      current.Value <- None
      thread <- -1

  // -- ps --

  /// Ask a process to stop. It finishes `Failed("cancelled")` at its next turn, which a parked
  /// process is given at once: whatever it was waiting for is abandoned (the task's late
  /// completion posts for a process that is no longer parked, and is dropped). A running
  /// process finishes its slice first; one that completes within it completes.
  member this.Kill(pid : ProcessId) : bool =
    match processes.TryGetValue pid with
    | true, p ->
      p.cancelRequested <- true
      lock sync (fun () ->
        let mine =
          subscriptions |> Seq.filter (fun s -> s.proc.id = pid) |> List.ofSeq
        for sub in mine do
          subscriptions.Remove sub |> ignore<bool>
          for (_, timer) in sub.timers do
            timer.Dispose()
          sub.wake.TrySetCanceled() |> ignore<bool>)
      match p.status with
      | Parked _ -> queue.Post(HE.HostEvent.Completed pid)
      | _ -> ()
      true
    | false, _ -> false

  /// Every process this scheduler knows, as copies.
  member _.Snapshot() : list<ProcessSummary> =
    lock sync (fun () ->
      processes.Values
      |> Seq.map (fun p ->
        { id = p.id
          entry = p.entry
          status = p.status
          parent = p.parent
          started = p.started
          slices = p.slices
          frames =
            match p.status with
            | Done _
            | Failed _ -> []
            | _ ->
              try
                Execution.callStackFromVM p.vm
              with _ ->
                [] })
      |> List.ofSeq)


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
