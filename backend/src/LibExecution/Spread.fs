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
///
///   A chunk's failure is therefore a signal to this module, and nothing in the chunk may turn
///   it into a value: a builtin that catches its callee's failure re-raises under `spreadChild`
///   (`atRestCheckGuarded`, `applicableTryApply`).
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

/// Which callable a spread fell back from: a lambda by its expression, a named fn by its name.
[<Struct>]
type CallableKey =
  | Lambda of exprId : int64
  | Named of name : FQFnName.FQFnName

/// Callables a spread has fallen back from once. Not tried again in this process: a body that
/// reached an effect will very likely reach it again, and a spread that falls back has paid for
/// its chunks and gained nothing.
///
/// Keyed by the name itself rather than by a string of it. `string` on an `FQFnName` has no
/// override to call, so F# prints the union through reflection, and `eligible` asks on every
/// list op over a named fn: that one `string` was most of the reflection in a whole-tree
/// `dark typecheck`.
let private fellBack =
  System.Collections.Concurrent.ConcurrentDictionary<CallableKey, byte>()

/// For tests: forget which callables have fallen back.
let forgetFallbacks () = fellBack.Clear()

let private keyOf (app : Applicable) : CallableKey =
  match app with
  | AppLambda l -> CallableKey.Lambda(int64 l.exprId)
  | AppNamedFn n -> CallableKey.Named n.name

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


// -- Prediction --
//
// Before a spread starts, the callable is asked whether anything it can reach would be refused in
// a chunk. Predicted impure, it runs serially and no chunk is started. Predicted pure, or unknown,
// it spreads, and the refusal underneath is what keeps a wrong prediction from mattering: a pure
// body spreads either way, and a wrong `Pure` costs one wasted spread (measured: about 15 ms and
// 2.2 MB over 1,024 elements, once per callable per process), never a reordered effect. So the
// prediction buys skipping that waste for code KNOWN to be impure, and an answer knowable before
// the call. It does not make pure code faster: observing was already free there.

/// Off (the default), a list op does not predict: it tries to run across cores and falls back if a
/// chunk finds an effect. On, a callable known to reach an effect is not tried at all. Off by
/// default because, measured on one AOT binary, predicting cost `Canvas.compose` about 6 points of
/// allocation for no speed, and kept bodies that CAN reach an effect but do not serial, while
/// saving only the one wasted spread an impure callable pays per process
/// (`docs/perf/history.md`). The analysis stays: its real use is answering a question about code
/// (can this run across cores, and if not, why) rather than deciding at a call. The CLI host sets
/// it from `exec.spreadPredict`, once per run.
let mutable predicting = false

/// How many spreads were asked about, by answer. For `DARK_SPREAD_REPORT`.
let mutable predictedPure = 0L
let mutable predictedImpure = 0L
let mutable predictedUnknown = 0L

/// Analyses that raised and were answered `Unknown` (`LibDB.PackagePermissions.purity`). Safe,
/// since unknown means observe; counted so a store that cannot be read shows up as a number
/// rather than as spreading that quietly stopped being predicted.
let mutable predictionFailures = 0L

let notePredictionFailure () =
  System.Threading.Interlocked.Increment &predictionFailures |> ignore<int64>

/// `Dval.isInertData`, for a check made on every spread decision: the same answer, without the
/// enumerator `Map.values |> Seq.forall` allocates per record. A frame of `Canvas.compose` closes
/// over a dictionary of a few hundred span records, and checking it that way cost more than
/// starting the chunks did.
let rec private holdsNoCode (dv : Dval) : bool =
  match dv with
  | DApplicable _
  | DDB _
  | DStream _
  | DPromise _ -> false
  | DList(_, items) -> listHoldsNoCode items
  | DTuple(a, b, rest) -> holdsNoCode a && holdsNoCode b && listHoldsNoCode rest
  | DDict(_, _, entries) -> mapHoldsNoCode entries
  | DRecord(_, _, _, fields) -> mapHoldsNoCode fields
  | DEnum(_, _, _, _, fields) -> listHoldsNoCode fields
  | _ -> LibExecution.Dval.isInertData dv

and private listHoldsNoCode (items : List<Dval>) : bool =
  match items with
  | [] -> true
  | x :: rest -> holdsNoCode x && listHoldsNoCode rest

and private mapHoldsNoCode<'k when 'k : comparison> (m : Map<'k, Dval>) : bool =
  let mutable ok = true
  let mutable e = (m :> seq<_>).GetEnumerator()
  while ok && e.MoveNext() do
    let kv = e.Current
    ok <-
      holdsNoCode kv.Value
      && (match box kv.Key with
          | :? DictKey as k -> holdsNoCode k.Dval
          | _ -> true)
  ok

let private both (a : Purity) (b : Purity) : Purity =
  match a, b with
  | Purity.Impure, _
  | _, Purity.Impure -> Purity.Impure
  | Purity.Unknown, _
  | _, Purity.Unknown -> Purity.Unknown
  | Purity.Pure, Purity.Pure -> Purity.Pure

let rec private letPatternRegisters (p : LetPattern) : List<Register> =
  match p with
  | LPVariable r -> [ r ]
  | LPTuple(a, b, rest) -> List.collect letPatternRegisters (a :: b :: rest)
  | LPWildcard
  | LPUnit -> []

let rec private matchPatternRegisters (p : MatchPattern) : List<Register> =
  match p with
  | MPVariable r -> [ r ]
  | MPList ps
  | MPEnum(_, ps) -> List.collect matchPatternRegisters ps
  | MPListCons(h, t) -> matchPatternRegisters h @ matchPatternRegisters t
  | MPTuple(a, b, rest) -> List.collect matchPatternRegisters (a :: b :: rest)
  | MPOr ps -> ps |> NEList.toList |> List.collect matchPatternRegisters
  | _ -> []

/// The registers an instruction writes, other than by loading a callable (`LoadVal` of an
/// applicable, `CreateLambda`), which the walk below tracks itself.
let private writesOther (i : Instruction) : List<Register> =
  match i with
  | LoadVal(_, DApplicable _)
  | CreateLambda(_, _) -> []
  | LoadVal(r, _)
  | CopyVal(r, _)
  | Or(r, _, _)
  | And(r, _, _)
  | CreateString(r, _)
  | CreateTuple(r, _, _, _)
  | CreateList(r, _)
  | CreateDict(r, _)
  | CreateRecord(r, _, _, _)
  | CloneRecordWithUpdates(r, _, _)
  | GetRecordField(r, _, _)
  | CreateEnum(r, _, _, _, _)
  | LoadValue(r, _)
  | Apply(r, _, _, _)
  | VarNotFound(r, _)
  | Unwrap(r, _, _) -> [ r ]
  | CheckLetPatternAndExtractVars(_, pat) -> letPatternRegisters pat
  | CheckMatchPatternAndExtractVars(_, pat, _) -> matchPatternRegisters pat
  | JumpByIfFalse _
  | JumpBy _
  | MatchUnmatched _
  | RaiseNRE _
  | CheckIfFirstExprIsUnit _
  | TraceExpr _ -> []

/// A trait call whose implementation is decided at run time (code nobody saved, like an `eval`'s
/// `a + x`, or a generic deferring to its caller): pure when EVERY implementation of that method
/// the store knows is, since any of them could be the one that runs. One impure candidate makes
/// it unknown rather than impure, for the same reason. Memoised; an implementation authored later
/// is not seen, which the refusal under every chunk makes a cost and never a reordered effect.
type private Memos() =
  member val Traits =
    System.Collections.Concurrent.ConcurrentDictionary<struct (FQTraitName.Package *
    string), Purity>()
  member val Lambdas =
    System.Collections.Concurrent.ConcurrentDictionary<id, Purity>()

/// The memos belong to the oracle that answered them: two states with different `fnPurity` (a
/// test's, the CLI host's) must not share answers. Keyed by the function itself, which a record
/// copy of the state keeps, and weakly, so a state that is gone takes its memos with it.
let private memoTable =
  System.Runtime.CompilerServices.ConditionalWeakTable<obj, Memos>()

let private memosFor (state : ExecutionState) : Memos =
  memoTable.GetValue(box state.fnPurity, (fun _ -> Memos()))

let private ofUndecidedTraitCall
  (state : ExecutionState)
  (tm : FQFnName.TraitMethod)
  : Ply<Purity> =
  let key = struct (tm.trait_, tm.method_)
  let traitMemo = (memosFor state).Traits
  match traitMemo.TryGetValue key with
  | true, known -> Ply known
  | false, _ ->
    uply {
      let! candidates = state.fns.implCandidates state.branchId tm.trait_
      let methods =
        candidates |> List.choose (fun c -> Map.tryFind tm.method_ c.methods)
      let mutable answer =
        if List.isEmpty methods then Purity.Unknown else Purity.Pure
      for m in methods do
        if answer = Purity.Pure then
          let! one = state.fnPurity m
          if one <> Purity.Pure then answer <- Purity.Unknown
      traitMemo[key] <- answer
      return answer
    }

/// Whether `app`, applied to arguments already known to hold no code, can reach anything a chunk
/// would refuse. `depth` bounds the walk through nested lambdas and captured callables.
let rec private ofApplicable
  (state : ExecutionState)
  (depth : int)
  (app : Applicable)
  : Ply<Purity> =
  if depth > 16 then
    Ply Purity.Unknown
  else
    uply {
      match app with
      | AppNamedFn named ->
        let! own =
          match named.name with
          | FQFnName.Builtin b ->
            let mutable found = Unchecked.defaultof<BuiltInFn>
            if state.fns.builtIn.TryGetValue(b, &found) then
              Ply(
                if Interpreter.spreadRefuses found then
                  Purity.Impure
                else
                  Purity.Pure
              )
            else
              Ply Purity.Unknown
          // A bound implementation the caller supplies is not something the store can answer for.
          | FQFnName.Package p when List.isEmpty named.boundImpls -> state.fnPurity p
          | FQFnName.TraitMethod { implFn = FQFnName.Chosen p } -> state.fnPurity p
          | FQFnName.TraitMethod tm -> ofUndecidedTraitCall state tm
          | FQFnName.Package _ -> Ply Purity.Unknown
        let! args = ofValues state depth named.argsSoFar
        return both own args
      | AppLambda lambda ->
        // What its own instructions reach, before its captured values: memoised per lambda, since
        // the instructions and every package fn's answer are fixed for the life of the oracle.
        let lambdaMemo = (memosFor state).Lambdas
        let! own =
          match lambdaMemo.TryGetValue lambda.exprId with
          | true, known -> Ply known
          | false, _ ->
            match state.lambdaInstrCache.TryGetValue lambda.exprId with
            | true, impl ->
              uply {
                let! answer = ofLambdaBody state depth impl
                lambdaMemo[lambda.exprId] <- answer
                return answer
              }
            | false, _ -> Ply Purity.Unknown
        // What it closed over and what it was partly applied to are concrete by now, so a captured
        // callback is judged by what it IS, which no reading of the source could know.
        let! closed =
          ofValues
            state
            depth
            (lambda.closedRegisters |> Captures.toList |> List.map snd)
        let! args = ofValues state depth lambda.argsSoFar
        return both own (both closed args)
    }

and private ofValues
  (state : ExecutionState)
  (depth : int)
  (values : List<Dval>)
  : Ply<Purity> =
  uply {
    let mutable answer = Purity.Pure
    for v in values do
      if answer <> Purity.Impure then
        let! one =
          match v with
          | DApplicable a -> ofApplicable state (depth + 1) a
          | v when holdsNoCode v -> Ply Purity.Pure
          // Code inside a structure: a record of callbacks, say. Not followed.
          | _ -> Ply Purity.Unknown
        answer <- both answer one
    return answer
  }

/// A lambda's body: every callable it loads or creates, and every call it makes. A call through a
/// register that is not a loaded or created callable (a parameter, a field, a call's result, a
/// captured value) is a call the walk cannot see the target of: unknown, unless the captured
/// value, judged separately by `ofApplicable`, covers it.
and private ofLambdaBody
  (state : ExecutionState)
  (depth : int)
  (impl : LambdaImpl)
  : Ply<Purity> =
  uply {
    let instrs = impl.instructions.instructions
    let otherwise = instrs |> List.collect writesOther |> Set.ofList
    let loaded =
      instrs
      |> List.choose (fun i ->
        match i with
        | LoadVal(r, DApplicable _)
        | CreateLambda(r, _) -> Some r
        | _ -> None)
      |> Set.ofList
    // Captured registers are filled at the frame's start, with values `ofApplicable` judges.
    let captured = impl.registersToCloseOver |> List.map snd |> Set.ofList
    let mutable answer = Purity.Pure
    for i in instrs do
      if answer <> Purity.Impure then
        let! one =
          match i with
          | LoadVal(_, DApplicable a) -> ofApplicable state (depth + 1) a
          | LoadVal(_, v) when LibExecution.Dval.isInertData v -> Ply Purity.Pure
          | LoadVal _ -> Ply Purity.Unknown
          | CreateLambda(_, inner) -> ofLambdaBody state (depth + 1) inner
          | LoadValue(_, FQValueName.Package v) ->
            uply {
              match! state.values.package v with
              | Some pv ->
                match pv.body with
                | DApplicable a -> return! ofApplicable state (depth + 1) a
                | body when LibExecution.Dval.isInertData body -> return Purity.Pure
                | _ -> return Purity.Unknown
              | None -> return Purity.Unknown
            }
          | LoadValue _ -> Ply Purity.Unknown
          | Apply(_, target, _, _) when
            (Set.contains target loaded || Set.contains target captured)
            && not (Set.contains target otherwise)
            ->
            Ply Purity.Pure
          | Apply _
          | VarNotFound _ -> Ply Purity.Unknown
          | _ -> Ply Purity.Pure
        answer <- both answer one
    return answer
  }

/// The prediction for spreading `app` over `items`. The items are its arguments, so they are
/// checked too: an element that is itself a callable makes the body's calls through its
/// parameter unknowable.
let predict
  (state : ExecutionState)
  (app : Applicable)
  (items : List<Dval>)
  : Ply<Purity> =
  uply {
    let answer =
      if not predicting then Some Purity.Unknown
      elif listHoldsNoCode items then None
      else Some Purity.Unknown
    let! answer =
      match answer with
      | Some known -> Ply known
      | None -> ofApplicable state 0 app
    match answer with
    | Purity.Pure -> System.Threading.Interlocked.Increment &predictedPure
    | Purity.Impure -> System.Threading.Interlocked.Increment &predictedImpure
    | Purity.Unknown -> System.Threading.Interlocked.Increment &predictedUnknown
    |> ignore<int64>
    return answer
  }


/// What a spread came back with: the results of the chunks that finished, in order and
/// concatenated, and the input from the first chunk that did not, to be run serially here.
/// `leftoverIndex` is where in the spread input `leftover` begins.
type Outcome = { results : List<Dval>; leftover : List<Dval>; leftoverIndex : int }

/// Spread `items` over chunk processes. Each chunk applies the builtin `builtinName` to
/// `chunkArgs offset chunk` and then to `app`; it must answer a `DList`. `offset` is where the
/// chunk begins in `items`.
let rec run
  (state : ExecutionState)
  (vm : VMState)
  (builtinName : string)
  (chunkArgs : int -> List<Dval> -> List<Dval>)
  (app : Applicable)
  (perElement : int64)
  (items : List<Dval>)
  : Ply<Outcome> =
  uply {
    match! predict state app items with
    | Purity.Impure ->
      // Known to reach something a chunk would refuse: no chunk is started, and the list op runs
      // the whole of it here, as it always did. Not asked again for this callable.
      fellBack.TryAdd(keyOf app, 0uy) |> ignore<bool>
      return { results = []; leftover = items; leftoverIndex = 0 }
    | Purity.Pure
    | Purity.Unknown ->
      return! spreadChunks state vm builtinName chunkArgs app perElement items
  }

/// The spread itself, once the prediction has allowed it.
and private spreadChunks
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
