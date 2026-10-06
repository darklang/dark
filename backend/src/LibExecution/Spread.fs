/// `List.map` and its siblings deciding for themselves to spread across cores.
///
/// Nobody asks for this. A list op starts serially, as it always did, and after a few elements
/// it asks whether the rest is worth spreading: the interpreted instructions the elements so far
/// took, times how many are left, against `crossover`. So the decision depends on the body's cost
/// as well as the count, and a short map or a cheap body never spreads.
///
/// A spread hands the rest of the list out in contiguous chunks, each to a process of its own
/// running the same builtin serially over its chunk, and collects the chunks in order.
///
/// What keeps this from changing any program:
///
/// - **Only computation runs in a chunk.** A chunk's process runs with `spreadChild`, under which
///   any call that would take an effect ordinal (`Interpreter.isLogged`) is refused before it does
///   anything. Purity is therefore not predicted, it is observed, over the whole call graph the
///   callable reaches, trait dispatch and callbacks included.
/// - **The first chunk that did not finish is run again, here.** Chunks are collected in input
///   order. At the first one that failed, for any reason (it reached an effect, or the body raised),
///   the later chunks are cancelled and the original process carries on serially from that chunk's
///   first element. Everything before it was pure, so its results are what a serial run would
///   have computed; everything from it on runs exactly as it always did. So an effect happens in
///   its place in the order, and an error is raised by the element that raises it serially, with
///   the frames it raises under serially.
/// - **Nothing spreads while somebody is watching.** A recorded or viewed run keeps every pass of
///   every loop in its own frame tree, as before.
/// - **A chunk does not spread again.** A map inside a spread map runs serially in its chunk, so
///   nesting cannot multiply out.
module LibExecution.Spread

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Sched = LibExecution.Scheduler


/// Interpreted instructions the REST of a list must be projected to take before it is worth
/// spreading. Measured against a grid of body cost by element count
/// (`scripts/perf/workloads/spread.dark`, AOT; `docs/perf/history.md`): forced spreading lost at
/// about 420 instructions of work, first won at about 1,700 (1.24x), and won 1.6 to 2.6x from
/// 6,000 to 13,000 up. 10,000 sits clear of where it starts to pay. Negative turns spreading off, 0 spreads
/// anything with at least `minRemaining` left (for tests). The CLI host sets it from
/// `exec.spreadCrossover`.
///
/// Instructions, not time, so the decision is the same on a loaded box as on an idle one, and so
/// is what a run allocates: a clock-based probe let one GC pause during the elements it timed make
/// a cheap map look expensive, and the perf gate's reading moved by 0.9 MB between runs.
let mutable crossover : int64 = 10_000L

/// Fewer elements left than this and the rest runs where it is.
let mutable minRemaining = 2

/// Chunks per worker. More than one so a chunk of expensive elements does not leave every other
/// core waiting on it.
let mutable chunksPerWorker = 2

/// The least work, in interpreted instructions, worth a chunk of its own. A chunk is a process,
/// and starting one costs about what a few hundred interpreted operations do, so a chunk smaller
/// than this spends more starting than it saves. Measured: a chunk costs 3.3 to 5 KB to start,
/// and serial work allocates about 90 B an instruction, so a 2,000-instruction chunk spends about
/// 3% of its own allocation on starting. The CLI host sets it from `exec.spreadMinChunk`.
let mutable minChunk : int64 = 2_000L

/// How many spreads have started in this process, for tests: a result that is the same either
/// way says nothing about whether the work was spread.
let mutable spreads = 0L

/// How many spreads fell back to running serially from some chunk on.
let mutable fallbacks = 0L

/// Callables a spread has fallen back from once. Not tried again in this process: a body that
/// reached an effect will very likely reach it again, and a spread that falls back has paid for
/// its chunks and gained nothing.
let private fellBack =
  System.Collections.Concurrent.ConcurrentDictionary<struct (bool * int64 * string), byte>()

/// For tests: forget which callables have fallen back.
let forgetFallbacks () = fellBack.Clear()

let private keyOf (app : Applicable) : struct (bool * int64 * string) =
  match app with
  | AppLambda l -> struct (true, int64 l.exprId, null)
  | AppNamedFn n -> struct (false, 0L, string n.name)

/// Whether a list op over `app` may spread at all, under this state. Cheap: called once per list
/// op with more than one element.
let eligible (state : ExecutionState) (app : Applicable) : bool =
  crossover >= 0L
  && not state.spreadChild
  && not state.tracing.collectFrames
  && Option.isNone state.tracing.viewEffect
  && Sched.defaultWorkers > 1
  && not (fellBack.ContainsKey(keyOf app))


/// Instructions this VM has run, as of now: its budget counts down one per instruction and is
/// refilled to `Scheduler.defaultQuantum` at every slice, so the slices so far make up the rest.
/// A VM nobody schedules starts at -1 and only counts down. Read where a frame has just returned,
/// which is where a list op's continuation runs, so the budget has been written back.
let private instructionsSoFar (vm : VMState) : struct (int64 * int64) =
  let slices =
    match Sched.Scheduler.CurrentProcess with
    | Some p -> p.slices
    | None -> 0L
  struct (slices, vm.budget)

let private instructionsSince
  (vm : VMState)
  (struct (slices0, budget0) : struct (int64 * int64))
  : int64 =
  let struct (slices1, budget1) = instructionsSoFar vm
  (slices1 - slices0) * Sched.defaultQuantum + budget0 - budget1


/// The question a list op asks as it goes: is the rest worth spreading yet? Asked after every
/// element, answered only at powers of two, so a cheap body pays a counter increment per element
/// and a compare.
///
/// The count starts when the FIRST element is done, not before it: the first call of a body is
/// the one that loads and converts what it calls. So the earliest answer is after two measured
/// elements.
[<AllowNullLiteral>]
type Probe(vm : VMState) =
  let mutable start = struct (0L, 0L)
  let mutable finished = -1
  let mutable off = false
  let mutable perElement = 0L

  /// After an element: `remaining` is the elements still to go.
  member _.Ask(remaining : List<'a>) : bool =
    finished <- finished + 1
    if finished = 0 then
      start <- instructionsSoFar vm
      false
    elif off || finished < 2 || (finished &&& (finished - 1)) <> 0 then
      false
    else
      // `List.length` walks the rest; at a power of two only, so the walks sum to about twice
      // the list.
      let left = List.length remaining
      if left < minRemaining then
        off <- true
        false
      else
        let ran = instructionsSince vm start
        perElement <- ran / int64 finished
        // ran / finished * left >= crossover, without the division.
        ran * int64 left >= crossover * int64 finished

  /// A list op that has spread does not ask again.
  member _.Stop() = off <- true

  /// What an element has cost so far, in instructions, as of the last answer.
  member _.PerElement = perElement

let probe (state : ExecutionState) (vm : VMState) (app : Applicable) : Probe =
  if eligible state app then Probe vm else null


/// What a spread came back with: the results of the chunks that finished, in order and
/// concatenated, and the input from the first chunk that did not, to be run serially here.
/// `leftoverIndex` is where in the spread input `leftover` begins.
type Outcome = { results : List<Dval>; leftover : List<Dval>; leftoverIndex : int }

/// Spread `items` over chunk processes. Each chunk applies the builtin `builtinName` to
/// `chunkArgs offset chunk` and then to `app`; it must answer a `DList`. `offset` is where the
/// chunk begins in `items`.
let run
  (state : ExecutionState)
  (vm : VMState)
  (builtinName : string)
  (chunkArgs : int -> List<Dval> -> List<Dval>)
  (app : Applicable)
  (perElement : int64)
  (items : List<Dval>)
  : Ply<Outcome> =
  let count = List.length items
  // As many chunks as there are cores to keep busy, but none smaller than `minChunk` of work.
  let byWork =
    if minChunk <= 0L then
      count
    else
      int (min (int64 count) (int64 count * perElement / minChunk))
  let chunkCount =
    max 1 (min byWork (min count (Sched.defaultWorkers * chunksPerWorker)))
  // Contiguous, as even as integer division allows: the first `count % chunkCount` take one more.
  let chunks =
    let baseSize = count / chunkCount
    let extra = count % chunkCount
    let mutable rest = items
    let mutable offset = 0
    [ for i in 0 .. chunkCount - 1 do
        let size = baseSize + (if i < extra then 1 else 0)
        let chunk, after = List.splitAt size rest
        yield struct (offset, chunk)
        offset <- offset + size
        rest <- after ]

  let sched = Sched.Scheduler.CurrentOrShared
  let me = Sched.Scheduler.CurrentProcess
  let parent = me |> Option.map (fun p -> p.id)
  let childState = { state with spreadChild = true }
  let builtin = FQFnName.Builtin(FQFnName.builtin builtinName 0)

  // Already asked to stop: no chunk is worth starting.
  match me with
  | Some m when not (isNull m.stopReason) ->
    RuntimeError.UncaughtException(m.stopReason, []) |> raiseUntargetedRTE
  | _ -> ()

  System.Threading.Interlocked.Increment &spreads |> ignore<int64>

  let procs =
    chunks
    |> List.map (fun (struct (offset, chunk)) ->
      let entry =
        AppNamedFn
          { name = builtin
            typeSymbolTable = TST.empty
            typeArgs = []
            access = None
            argsSoFar = chunkArgs offset chunk
            boundImpls = [] }
      let p =
        sched.SpawnApply(
          childState,
          entry,
          DApplicable app,
          parent,
          vm.activeAccess
        )
      struct (offset, chunk, p))
    |> Array.ofList

  // Its answer is kept for an `Exec.await` until taken; nobody else will take it.
  let forget (p : Sched.Process) =
    p.completion.Task.ContinueWith(
      (fun (_ : Task<ExecutionResult>) ->
        sched.TakeResult p.id |> ignore<Option<ExecutionResult>>),
      TaskContinuationOptions.ExecuteSynchronously
    )
    |> ignore<Task>

  uply {
    let results = ResizeArray<List<Dval>>()
    let mutable i = 0
    let mutable failedAt = -1
    while failedAt < 0 && i < procs.Length do
      let struct (_, _, p) = procs[i]
      match me with
      | Some m -> m.parkHint <- ValueSome(Sched.OnProcess p.id)
      | None -> ()
      let! result = p.completion.Task
      sched.TakeResult p.id |> ignore<Option<ExecutionResult>>
      // What the chunk spent counts against the process that asked for it, so a per-process cap
      // (`exec.maxInstructions`, `exec.maxBytes`) caps a spread map as it caps a serial one.
      match me with
      | Some m ->
        m.instructionsTaken <- m.instructionsTaken + p.instructionsTaken
        m.allocated <- m.allocated + p.allocated
      | None -> ()
      match result with
      | Ok(DList(_, xs)) ->
        results.Add xs
        i <- i + 1
      | _ -> failedAt <- i

    // Asked to stop while it waited (`Cancel`, `Kill`, a parent finishing): the chunks failed
    // because they were stopped with it, and running the rest here would be doing exactly the
    // work the stop was for. So the stop is what this answers.
    match me with
    | Some m when failedAt >= 0 && not (isNull m.stopReason) ->
      for j in failedAt + 1 .. procs.Length - 1 do
        let struct (_, _, p) = procs[j]
        sched.Cancel p.id |> ignore<bool>
        forget p
      RuntimeError.UncaughtException(m.stopReason, []) |> raiseUntargetedRTE
    | _ -> ()

    if failedAt < 0 then
      return { results = List.concat results; leftover = []; leftoverIndex = count }
    else
      System.Threading.Interlocked.Increment &fallbacks |> ignore<int64>
      fellBack.TryAdd(keyOf app, 0uy) |> ignore<bool>
      for j in failedAt + 1 .. procs.Length - 1 do
        let struct (_, _, p) = procs[j]
        sched.Cancel p.id |> ignore<bool>
        forget p
      let struct (offset, _, _) = procs[failedAt]
      let leftover = items |> List.skip offset
      return
        { results = List.concat results
          leftover = leftover
          leftoverIndex = offset }
  }
