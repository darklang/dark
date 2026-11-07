/// `List.map`, `Dict.map` and their siblings deciding for themselves to spread across cores.
///
/// Nobody asks for this. A list op starts serially, as it always did, and after a few elements
/// it asks whether the rest is worth spreading: the interpreted instructions the elements so far
/// took, times how many are left, against `crossover` (`SpreadSettings`). So the decision depends
/// on the body's cost as well as the count, and a short map or a cheap body never spreads.
///
/// A spread hands the rest out in contiguous chunks, as many as there are idle workers to take
/// them, each a process running the same builtin serially over its stretch, and collects them in
/// order. What it may change is time and CPU, never what the program does (`docs/processes.md`):
///
/// - **Only computation runs in a chunk.** A chunk's process runs with `spreadChild`, under which
///   any call `Interpreter.spreadRefuses` is refused before it does anything. Purity is observed,
///   not predicted, over everything the callable reaches.
/// - **The first chunk that did not finish runs again, here.** At the first chunk that failed for
///   any reason, the later ones are cancelled and the spreading process carries on serially from
///   that chunk's first element, so an effect happens in its place and an error is raised by the
///   element and frames that raise it serially. Nothing in a chunk may turn its failure into a
///   value: a builtin that catches its callee's failure re-raises under `spreadChild`.
/// - **A chunk checks the callable's results as serially** (`Interpreter.checksInChunk`), and
///   what its copies of the checks learn comes back in order (`Interpreter.learnFromChunk`).
/// - **A recorded chunk records apart** (`Tracing.forChunk`), and what it recorded is added to
///   the spreading process's trace when the chunk is taken, so the trace is a serial run's. A
///   chunk that is not taken leaves nothing in it.
/// - **A chunk is charged only for what is taken**, so `exec.maxInstructions` and `exec.maxBytes`
///   give a serial run's verdict, and a chunk has caps of its own, past which it stops and runs
///   again here.
/// - **A chunk does not spread again**, so nesting cannot multiply out.
module LibExecution.Spread

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Sched = LibExecution.Scheduler


/// Fewer elements left than this and the rest runs where it is.
let minRemaining = 2

/// Whether a list op may spread at all, under this state. A view runs serially: it keeps every
/// value of every pass, which a chunk's buffer does not carry. So does a run on a virtual
/// clock, which keeps to one scheduler thread.
let eligible (state : ExecutionState) : bool =
  state.spread.on
  && not state.clock.IsVirtual
  && not state.spreadChild
  && not state.tracing.collectExprValues
  && Option.isNone state.tracing.viewEffect
  && state.exec.workers > 1

let private keyOf (app : Applicable) : SpreadCallable =
  match app with
  | AppLambda l -> SpreadLambda(int64 l.exprId)
  | AppNamedFn n -> SpreadNamed n.name

/// Remembered on the spreading process: a callable that fell back there is not tried again
/// there. Another process, a later request on `serve` say, tries it afresh.
let private fellBackIn (proc : Option<Sched.Process>) (app : Applicable) : bool =
  match proc with
  | Some p -> p.spreadFellBack.Contains(keyOf app)
  | None -> false


/// Instructions this VM has run, as of now: its budget counts down one per instruction and is
/// refilled to `Scheduler.defaultQuantum` at every slice, so the slices so far make up the rest.
/// A VM nobody schedules (`proc` is `None`) starts at -1 and only counts down. Read where a frame
/// has just returned, which is where a list op's continuation runs, so the budget has been
/// written back.
let private instructionsSoFar
  (proc : Option<Sched.Process>)
  (vm : VMState)
  : struct (int64 * int64) =
  let slices =
    match proc with
    | Some p -> p.slices
    | None -> 0L
  struct (slices, vm.budget)

let private instructionsSince
  (proc : Option<Sched.Process>)
  (vm : VMState)
  (struct (slices0, budget0) : struct (int64 * int64))
  : int64 =
  let struct (slices1, budget1) = instructionsSoFar proc vm
  (slices1 - slices0) * Sched.defaultQuantum + budget0 - budget1


/// The question a list op asks as it goes: is the rest worth spreading yet? Asked after every
/// element, answered only at powers of two, so a cheap body pays a counter increment per element
/// and a compare.
///
/// The count starts when the FIRST element is done, not before it: the first call of a body is
/// the one that loads and converts what it calls. So the earliest answer is after two measured
/// elements.
[<AllowNullLiteral>]
type Probe(vm : VMState, settings : SpreadSettings, app : Applicable) =
  // The process stepping this VM, read once: it is the same one for the whole list op, and
  // finding it is an `AsyncLocal` lookup.
  let proc = Sched.Scheduler.CurrentProcess
  // Where the count started, and the bytes this thread had allocated then.
  let mutable start = struct (struct (0L, 0L), 0L)
  let mutable finished = -1
  let mutable off = false
  let mutable perElement = 0L

  /// Counts an element, and says whether this is one the rest is weighed at.
  member private _.Due() : bool =
    finished <- finished + 1
    if finished = 0 then
      start <-
        struct (instructionsSoFar proc vm,
                System.GC.GetAllocatedBytesForCurrentThread())
      false
    else
      not (off || finished < 2 || (finished &&& (finished - 1)) <> 0)

  member private _.Weigh(left : int) : bool =
    if left < minRemaining then
      off <- true
      false
    else
      let struct (from, _) = start
      let ran = instructionsSince proc vm from
      perElement <- ran / int64 finished
      // ran / finished * left >= crossover, without the division.
      if ran * int64 left < settings.crossover * int64 finished then
        false
      elif perElement < settings.minPerElement && settings.crossover > 0L then
        false
      elif fellBackIn proc app then
        off <- true
        false
      else
        true

  /// After an element: `left` is how many are still to go.
  member this.Ask(left : int) : bool = this.Due() && this.Weigh left

  /// A list op that has spread does not ask again.
  member _.Stop() = off <- true

  /// What an element has cost so far, in instructions, as of the last answer.
  member _.PerElement = perElement

  /// What an element has allocated so far, on this thread.
  member _.BytesPerElement() : int64 =
    let struct (_, bytes) = start
    (System.GC.GetAllocatedBytesForCurrentThread() - bytes) / int64 (max 1 finished)


// -- Prediction --
//
// Before a spread starts, the callable is asked whether anything it can reach would be refused in
// a chunk. Predicted impure, it runs serially and no chunk is started. Predicted pure, or unknown,
// it spreads, and the refusal underneath is what keeps a wrong prediction from mattering: a wrong
// `Pure` costs one wasted spread, never a reordered effect. Off by default (`SpreadSettings.predict`):
// measured, predicting cost `Canvas.compose` about 6 points of allocation for no speed, and kept
// bodies that CAN reach an effect but do not serial (`docs/perf/history.md`). Kept for answering
// whether code can run across cores, and why not.

/// `Dval.isInertData`, for a check made on every spread decision: the same answer, without the
/// enumerator `Map.values |> Seq.forall` allocates per record. A frame of `Canvas.compose` closes
/// over a dictionary of a few hundred span records, and checking it that way cost more than
/// starting the chunks did.
let rec private holdsNoCode (dv : Dval) : bool =
  match dv with
  | DApplicable _
  | DStream _
  | DPromise _ -> false
  | DList(_, items) -> not (DvalList.exists (holdsNoCode >> not) items)
  | DTuple(a, b, rest) -> holdsNoCode a && holdsNoCode b && listHoldsNoCode rest
  | DDict(_, _, entries) ->
    DictMap.forall (fun k v -> holdsNoCode k && holdsNoCode v) entries
  | DSet(_, elements) -> DictMap.forall (fun k _ -> holdsNoCode k) elements
  | DRecord(_, _, _, fields) -> RecordFields.forall holdsNoCode fields
  | DEnum(_, _, _, _, fields) -> listHoldsNoCode fields
  | _ -> LibExecution.Dval.isInertData dv

and private listHoldsNoCode (items : List<Dval>) : bool =
  match items with
  | [] -> true
  | x :: rest -> holdsNoCode x && listHoldsNoCode rest

let private both (a : Purity) (b : Purity) : Purity =
  match a, b with
  | Purity.Impure, _
  | _, Purity.Impure -> Purity.Impure
  | Purity.Unknown, _
  | _, Purity.Unknown -> Purity.Unknown
  | Purity.Pure, Purity.Pure -> Purity.Pure

let rec private letPatternRegisters (p : LetPattern) : List<Register> =
  match p with
  | LPVariable(r, _) -> [ r ]
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
  | MPAs(p, r) -> r :: matchPatternRegisters p
  | MPRecord fields -> fields |> List.collect (snd >> matchPatternRegisters)
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
  | CreateSet(r, _)
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
  /// With the impl each answer was read from: a new version of a fn keeps its lambdas'
  /// expression ids, so an answer is only good for the body it was read from.
  member val Lambdas =
    System.Collections.Concurrent.ConcurrentDictionary<id, struct (LambdaImpl *
    Purity)>()

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
  : Purity =
  let key = struct (tm.trait_, tm.method_)
  let traitMemo = (memosFor state).Traits
  match traitMemo.TryGetValue key with
  | true, known -> known
  | false, _ ->
    let candidates = state.fns.implCandidates state.branchId tm.trait_
    let methods =
      candidates |> List.choose (fun c -> Map.tryFind tm.method_ c.methods)
    let answer =
      methods
      |> List.fold
        (fun answer m ->
          if answer <> Purity.Pure then
            answer
          else
            let one = state.fnPurity m
            if one = Purity.Pure then Purity.Pure else Purity.Unknown)
        (if List.isEmpty methods then Purity.Unknown else Purity.Pure)
    traitMemo[key] <- answer
    answer

/// Whether `app`, applied to arguments already known to hold no code, can reach anything a chunk
/// would refuse. `depth` bounds the walk through nested lambdas and captured callables.
let rec private ofApplicable
  (state : ExecutionState)
  (depth : int)
  (app : Applicable)
  : Purity =
  if depth > 16 then
    Purity.Unknown
  else
    match app with
    | AppNamedFn named ->
      let own =
        match named.name with
        | FQFnName.Builtin b ->
          match Lookup.tryFindV b state.fns.builtIn with
          | ValueSome found ->
            (if Interpreter.spreadRefuses found then Purity.Impure else Purity.Pure)
          | ValueNone -> Purity.Unknown
        // A bound implementation the caller supplies is not something the store can answer for.
        | FQFnName.Package p when List.isEmpty named.boundImpls -> state.fnPurity p
        | FQFnName.TraitMethod { implFn = FQFnName.Chosen p } -> state.fnPurity p
        | FQFnName.TraitMethod tm -> ofUndecidedTraitCall state tm
        | FQFnName.Package _ -> Purity.Unknown
      let args = ofValues state depth named.argsSoFar
      both own args
    | AppLambda lambda ->
      // What its own instructions reach, before its captured values: memoised per lambda, since
      // the instructions and every package fn's answer are fixed for the life of the oracle.
      let lambdaMemo = (memosFor state).Lambdas
      let own =
        match lambda.impl with
        | NoCode -> Purity.Unknown
        | HasCode impl ->
          match lambdaMemo.TryGetValue lambda.exprId with
          | true, struct (readFrom, known) when obj.ReferenceEquals(readFrom, impl) ->
            known
          | _ ->
            let answer = ofLambdaBody state depth impl
            lambdaMemo[lambda.exprId] <- struct (impl, answer)
            answer
      // What it closed over is concrete by now, so a captured callback is judged by what it
      // IS, which no reading of the source could know.
      let closed =
        ofValues
          state
          depth
          (lambda.closedRegisters |> Captures.toList |> List.map snd)
      both own closed

and private ofValues
  (state : ExecutionState)
  (depth : int)
  (values : List<Dval>)
  : Purity =
  values
  |> List.fold
    (fun answer v ->
      if answer = Purity.Impure then
        answer
      else
        let one =
          match v with
          | DApplicable a -> ofApplicable state (depth + 1) a
          | v when holdsNoCode v -> Purity.Pure
          // Code inside a structure: a record of callbacks, say. Not followed.
          | _ -> Purity.Unknown
        both answer one)
    Purity.Pure

/// A lambda's body: every callable it loads or creates, and every call it makes. A call through a
/// register that is not a loaded or created callable (a parameter, a field, a call's result, a
/// captured value) is a call the walk cannot see the target of: unknown, unless the captured
/// value, judged separately by `ofApplicable`, covers it.
and private ofLambdaBody
  (state : ExecutionState)
  (depth : int)
  (impl : LambdaImpl)
  : Purity =
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
  instrs
  |> List.fold
    (fun answer i ->
      if answer = Purity.Impure then
        answer
      else
        let one =
          match i with
          | LoadVal(_, DApplicable a) -> ofApplicable state (depth + 1) a
          | LoadVal(_, v) when LibExecution.Dval.isInertData v -> Purity.Pure
          | LoadVal _ -> Purity.Unknown
          | CreateLambda(_, inner) -> ofLambdaBody state (depth + 1) inner
          | LoadValue(_, FQValueName.Package v) ->
            match state.values.package v with
            | Some pv ->
              match pv.body with
              | DApplicable a -> ofApplicable state (depth + 1) a
              | _ when pv.isInert -> Purity.Pure
              | _ -> Purity.Unknown
            | None -> Purity.Unknown
          | LoadValue _ -> Purity.Unknown
          | Apply(_, target, _, _) when
            (Set.contains target loaded || Set.contains target captured)
            && not (Set.contains target otherwise)
            ->
            Purity.Pure
          | Apply _
          | VarNotFound _ -> Purity.Unknown
          | _ -> Purity.Pure
        both answer one)
    Purity.Pure

/// The prediction for spreading `app` over `items`. The items are its arguments, so they are
/// checked too: an element that is itself a callable makes the body's calls through its
/// parameter unknowable.
let predict (state : ExecutionState) (app : Applicable) (items : DvalList) : Purity =
  let answer =
    if not state.spread.predict then Some Purity.Unknown
    elif not (DvalList.exists (holdsNoCode >> not) items) then None
    else Some Purity.Unknown
  let answer =
    match answer with
    | Some known -> known
    | None -> ofApplicable state 0 app
  state.spread.counts.Add(
    match answer with
    | Purity.Pure -> SpreadCount.PredictedPure
    | Purity.Impure -> SpreadCount.PredictedImpure
    | Purity.Unknown -> SpreadCount.PredictedUnknown
  )
  answer


// -- The spread --

/// What a spread came back with: the results of the chunks that finished, in order and
/// concatenated, and `leftoverIndex`, where in the spread input the first chunk that did not
/// finish begins, to be run serially here. For a search, `found` is the answer of the first
/// chunk, in input order, that found what it looked for.
type Outcome = { results : DvalList; leftoverIndex : int; found : Dval voption }

/// What a chunk's answer means to the spread collecting it.
[<Struct>]
type ChunkAnswer =
  /// It finished its stretch, with these results.
  | Finished of results : DvalList
  /// It found what it looked for: the spread stops here.
  | Found of answer : Dval
  /// Not an answer this op gives: run its stretch again here.
  | NotAnAnswer

/// Where collecting a spread's chunks, in order, stopped.
[<Struct>]
type private Stopped =
  | AllFinished
  | FoundAt of found : int * answer : Dval
  | FailedAt of failed : int

/// A list op's chunk: a list of results.
let private listAnswer (answer : Dval) : ChunkAnswer =
  match answer with
  | DList(_, xs) -> Finished xs
  | _ -> NotAnAnswer

let private holdsRead (args : List<Dval>) : bool =
  args
  |> List.exists (fun arg ->
    match arg with
    | DList(_, xs) -> DvalList.exists Pending.isPromise xs
    | other -> Pending.isPromise other)

/// What a chunk may spend before it is stopped and its stretch runs here: several times what its
/// elements cost so far, and never more than the spreading process has left under its own caps.
let private capsFor
  (me : Option<Sched.Process>)
  (perElement : int64)
  (bytesPerElement : int64)
  (size : int)
  : struct (int64 * int64) =
  let left (cap : int64) (used : int64) (own : int64) =
    if cap > 0L then min own (max 1L (cap - used)) else own
  let instructions = 8L * perElement * int64 size + 100_000L
  let bytes = 8L * bytesPerElement * int64 size + 1_000_000L
  match me with
  | Some m ->
    struct (left m.exeState.exec.maxInstructions m.instructionsTaken instructions,
            left m.exeState.exec.maxBytes m.allocated bytes)
  | None -> struct (instructions, bytes)

let private runChunks
  (state : ExecutionState)
  (vm : VMState)
  (builtinName : string)
  (answerOf : Dval -> ChunkAnswer)
  (app : Applicable)
  (probe : Probe)
  (items : DvalList)
  (chunks : List<struct (int * DvalList * List<Dval>)>)
  : Task<Outcome> =
  let count = DvalList.length items
  let sched = Sched.Scheduler.CurrentOrShared
  let slots = sched.WorkersFor(state.exec).ChunkSlots
  let me = Sched.Scheduler.CurrentProcess
  let parent = me |> Option.map (fun p -> p.id)
  let builtin = FQFnName.Builtin(FQFnName.builtin builtinName 0)
  let recording = state.tracing.collectFrames
  let bytesPerElement = probe.BytesPerElement()

  // Already asked to stop: no chunk is worth starting.
  match me with
  | Some m when m.stop.IsSome ->
    slots.Release(List.length chunks) |> ignore<int>
    RuntimeError.UncaughtException(m.stop.Value.reason, []) |> raiseRTE
  | _ -> ()

  state.spread.counts.Add SpreadCount.Spreads
  let entryInstrs = (Execution.instructionsForApply app DUnit).instructions
  // What a chunk's budget spent on its elements: less the entry's instructions and the turns of
  // its root frame on the way in and out, which a serial run does not spend.
  let ownWork (p : Sched.Process) =
    max 0L (p.instructionsTaken - int64 (List.length entryInstrs) - 2L)

  // The callback checks an enclosing fn set up go with every chunk, each its own copy, and
  // what they learn comes back in order (`Interpreter.learnFromChunk`).
  let checks = Interpreter.checksForChunk vm app
  let atSpawn = checks |> List.map (fun x -> x.typeSymbolTable)

  let procs =
    chunks
    |> List.map (fun (struct (offset, chunk, args)) ->
      let entry =
        AppNamedFn
          { name = builtin
            typeSymbolTable = TST.empty
            typeArgs = []
            approvalName = None
            access = None
            argsSoFar = args
            boundImpls = [] }
      // Its run id taken here, so the checks it is handed can name its run.
      let runId = VMRuns.next ()
      let struct (chunkApp, copies) = Interpreter.checksInChunk runId checks app
      let trace =
        if recording then ValueSome(state.tracing.forChunk ()) else ValueNone
      let childState =
        { state with
            spreadChild = true
            tracing =
              match trace with
              | ValueSome t -> t.tracing
              | ValueNone -> state.tracing }
      // Counts of its own, added to this VM's when the chunk is collected below: the chunks
      // run at once on other threads, and counting into one set from all of them lost counts.
      let p =
        sched.SpawnApply(
          childState,
          entry,
          DApplicable chunkApp,
          parent,
          vm.activeAccess,
          ?stats = InterpreterStats.ForChunk vm.stats,
          runId = runId,
          caps = capsFor me probe.PerElement bytesPerElement (DvalList.length chunk)
        )
      state.spread.counts.Add SpreadCount.Chunks
      // Collected through this, so its slot is back before the op goes on: a map right after
      // this one finds every slot free.
      let released =
        p.completion.Task.ContinueWith(
          (fun (_ : Task<ExecutionResult>) -> slots.Release() |> ignore<int>),
          TaskContinuationOptions.ExecuteSynchronously
        )
      struct (offset, copies, trace, p, released))
    |> Array.ofList

  // Its answer is kept for an `Exec.await` until taken; nobody else will take it.
  let forget (p : Sched.Process) =
    p.completion.Task.ContinueWith(
      (fun (_ : Task<ExecutionResult>) ->
        sched.TakeResult p.id |> ignore<Option<ExecutionResult>>),
      TaskContinuationOptions.ExecuteSynchronously
    )
    |> ignore<Task>

  let cancelFrom (first : int) =
    for j in first .. procs.Length - 1 do
      let struct (_, _, _, p, _) = procs[j]
      sched.Cancel p.id |> ignore<bool>
      forget p

  // A chunk that is taken counts as if this process had run it: what it spent against this
  // process's caps, what it counted, and what it recorded, less what applying its builtin cost
  // (`entry`), which a serial run does not spend. So the totals are a serial run's however the
  // work was split. Over a cap, this process stops now, as serially it would have on its way
  // through the same elements. A chunk not taken counts for nothing: its stretch runs again here.
  let taken (i : int) (p : Sched.Process) (trace : Tracing.ChunkTrace voption) =
    vm.stats.AddChunk p.vm.stats
    vm.stats.TakeOffChunkEntry(entryInstrs, builtinName)
    match trace with
    | ValueSome t -> t.take (fun () -> Interpreter.takeFrameId vm) vm.currentFrame.id
    | ValueNone -> ()
    match me with
    | Some m ->
      m.instructionsTaken <- m.instructionsTaken + ownWork p
      m.allocated <- m.allocated + p.allocated
      match Sched.overCap m with
      | null -> ()
      | reason ->
        cancelFrom (i + 1)
        RuntimeError.UncaughtException(reason, []) |> raiseRTE
    | None -> ()

  // The chunks' results in order, up to the first that found something or failed, and where
  // collecting stopped. The chunks run on other workers, or on this one once this process parks,
  // so this waits for them as a task and never blocks the thread.
  let rec collect (i : int) (acc : List<DvalList>) : Task<List<DvalList> * Stopped> =
    if i >= procs.Length then
      Task.FromResult((List.rev acc, AllFinished))
    else
      task {
        let struct (_, copies, trace, p, released) = procs[i]
        match me with
        | Some m -> m.parkHint <- ValueSome(Sched.OnProcess p.id)
        | None -> ()
        do! released
        let! result = p.completion.Task
        sched.TakeResult p.id |> ignore<Option<ExecutionResult>>
        let answer =
          match result with
          | Ok answer when Interpreter.learnFromChunk atSpawn checks copies ->
            answerOf answer
          | _ -> NotAnAnswer
        match answer with
        | Finished xs ->
          taken i p trace
          return! collect (i + 1) (xs :: acc)
        | Found dv ->
          taken i p trace
          return (List.rev acc, FoundAt(i, dv))
        | NotAnAnswer -> return (List.rev acc, FailedAt i)
      }

  task {
    let! results, stop = collect 0 []
    let results = List.fold DvalList.append DvalList.empty results

    // Asked to stop while it waited (`Cancel`, `Kill`, a parent finishing): the chunks failed
    // because they were stopped with it, and running the rest here would be doing exactly the
    // work the stop was for. So the stop is what this answers.
    match me, stop with
    | Some m, FailedAt failedAt when m.stop.IsSome ->
      cancelFrom (failedAt + 1)
      RuntimeError.UncaughtException(m.stop.Value.reason, []) |> raiseRTE
    | _ -> ()

    match stop with
    | AllFinished ->
      return { results = results; leftoverIndex = count; found = ValueNone }
    | FoundAt(at, answer) ->
      cancelFrom (at + 1)
      return { results = results; leftoverIndex = count; found = ValueSome answer }
    | FailedAt failedAt ->
      state.spread.counts.Add SpreadCount.Fallbacks
      let struct (offset, _, _, failed, _) = procs[failedAt]
      // Stopped by a cap rather than refused: the body was dearer than it looked, not impure,
      // so it may be tried again.
      match me with
      | Some m when failed.stop |> Option.forall (fun s -> s.asked) ->
        m.spreadFellBack.Add(keyOf app) |> ignore<bool>
      | _ -> ()
      cancelFrom (failedAt + 1)
      return { results = results; leftoverIndex = offset; found = ValueNone }
  }

/// Spread `items` over chunk processes, if there are idle workers to take them. Each chunk
/// applies the builtin `builtinName` to `chunkArgs offset chunk` and then to `app`; `answerOf`
/// reads its answer. `offset` is where the chunk begins in `items`. `ValueNone` when nothing was
/// started, and the op carries on serially as it was.
let private spreadOver
  (state : ExecutionState)
  (vm : VMState)
  (builtinName : string)
  (chunkArgs : int -> DvalList -> List<Dval>)
  (answerOf : Dval -> ChunkAnswer)
  (app : Applicable)
  (probe : Probe)
  (items : DvalList)
  : Task<Outcome voption> =
  let settings = state.spread
  let count = DvalList.length items
  let sched = Sched.Scheduler.CurrentOrShared
  let workers = sched.WorkersFor(state.exec)
  let me = Sched.Scheduler.CurrentProcess
  let perElement = probe.PerElement
  // No more chunks than the work is worth, the idle workers take, or the group has room for.
  let wanted =
    let byWork =
      if settings.minChunk <= 0L then
        count
      else
        int (min (int64 count) (int64 count * perElement / settings.minChunk))
    let free =
      if settings.idleOnly then workers.Idle else List.length workers.Members
    min byWork (min count (free * settings.chunksPerWorker))
  let rec take (got : int) =
    if got < wanted && workers.ChunkSlots.Wait 0 then take (got + 1) else got
  let chunkCount = take 0
  // Contiguous, as even as integer division allows: the first `count % chunkCount` take one more.
  let chunks =
    if chunkCount < 2 then
      []
    else
      let baseSize = count / chunkCount
      let extra = count % chunkCount
      List.init chunkCount (fun i -> baseSize + (if i < extra then 1 else 0))
      |> List.mapFold
        (fun offset size ->
          let chunk = DvalList.take size (DvalList.drop offset items)
          struct (offset, chunk, chunkArgs offset chunk), offset + size)
        0
      |> fst
  let release () =
    if chunkCount > 0 then workers.ChunkSlots.Release chunkCount |> ignore<int>
  if List.isEmpty chunks then
    release ()
    state.spread.counts.Add SpreadCount.NoChunks
    Task.FromResult ValueNone
  // A list holding reads in flight runs where it is: a chunk would carry the parent's reads
  // into a child. About these values, not the callable, so it is not remembered.
  elif List.exists (fun (struct (_, _, args)) -> holdsRead args) chunks then
    release ()
    probe.Stop()
    Task.FromResult ValueNone
  else
    match predict state app items with
    | Purity.Impure ->
      // Known to reach something a chunk would refuse: no chunk is started, and the op runs
      // the whole of it here. Not asked again for this callable.
      release ()
      probe.Stop()
      match me with
      | Some m -> m.spreadFellBack.Add(keyOf app) |> ignore<bool>
      | None -> ()
      Task.FromResult ValueNone
    | Purity.Pure
    | Purity.Unknown ->
      runChunks state vm builtinName answerOf app probe items chunks
      |> Task.map ValueSome


/// What a list op makes of one element's answer (`Walk`).
[<Struct>]
type Step<'acc> =
  /// Go on to the next element, with this so far.
  | Next of acc : 'acc
  /// This is the op's answer, and no element after this one runs.
  | Done of answer : Dval

/// A list op over a callable, the one loop every op that spreads runs: the op says how to run one
/// element, what its answer means, and what a chunk's arguments are, and this owns the rest.
/// Element by element, as a serial run does, until the probe says the rest is worth spreading;
/// then the rest is spread, and whatever the spread did not finish carries on here.
///
/// `builtinName` is the builtin a chunk runs over its stretch: the op itself, or one that applies
/// the callable the same way (`listMap` for `sortBy`'s keys, `listMap2shortest` for a dict's).
///
/// One per call of the op, and it is the continuation each element's answer comes back to, so a
/// call allocates it and the probe and nothing else per list.
[<AbstractClass>]
type Walk<'acc>
  (state : ExecutionState, vm : VMState, app : Applicable, builtinName : string) =
  inherit FSharpFunc<Dval, Dval>()

  // Where the walk is: what the op has so far, the element whose answer is awaited and its index,
  // and the elements after it.
  let mutable acc = Unchecked.defaultof<'acc>
  let mutable current = DUnit
  let mutable index = 0L
  let mutable rest = DvalList.empty
  let mutable probe : Probe = null
  // This, as the function it is: F# has no static coercion to a function type.
  let mutable next : Dval -> Dval = Unchecked.defaultof<_>

  /// Runs the callable on `elem`, the element at `index`, and hands `next` its answer.
  abstract Apply : index : int64 * elem : Dval * next : (Dval -> Dval) -> Dval

  /// Takes an answer to `Landed`. By default as it is, so a read stays in flight, as a map's
  /// results do; an op that looks inside its answers waits for them (`Value`).
  abstract Settle : answer : Dval -> Dval
  default this.Settle answer = this.Landed answer

  /// What `answer`, the callable's for `elem`, makes of what the op has so far.
  abstract Step : acc : 'acc * elem : Dval * answer : Dval -> Step<'acc>

  /// A chunk's arguments, before the callable: `index` is where `chunk` begins in the list.
  abstract ChunkArgs : index : int64 * chunk : DvalList -> List<Dval>

  /// What a chunk's answer means. By default a list of results, one per element or fewer.
  abstract AnswerOf : answer : Dval -> ChunkAnswer
  default _.AnswerOf answer = listAnswer answer

  /// What the op has so far, once a spread over `spread` came back with `outcome`.
  abstract Absorb : acc : 'acc * spread : DvalList * outcome : Outcome -> 'acc

  /// The op's answer, from what a chunk found. Only an op whose `AnswerOf` finds asks.
  abstract Found : found : Dval -> Dval
  default _.Found found = found

  /// The op's answer once every element has run.
  abstract Finish : acc : 'acc -> Dval

  override this.Invoke(answer : Dval) : Dval = this.Settle answer

  /// `Landed` with `answer` once it is a value (`Interpreter.withValue`).
  member this.Value(answer : Dval) : Dval =
    match answer with
    | DPromise _ -> Interpreter.withValue vm answer this.Landed
    | _ -> this.Landed answer

  member this.Landed(answer : Dval) : Dval =
    match this.Step(acc, current, answer) with
    | Done result -> result
    | Next stepped ->
      acc <- stepped
      if DvalList.isEmpty rest then
        this.Finish acc
      elif not (isNull probe) && probe.Ask(DvalList.length rest) then
        this.SpreadRest()
      else
        this.CarryOn(index + 1L, rest)

  member private this.SpreadRest() : Dval =
    let start = index + 1L
    let spread = rest
    let after (outcome : Outcome voption) =
      match outcome with
      // Nothing started: no idle worker, or reads in flight. Asked again further on.
      | ValueNone -> this.CarryOn(start, spread)
      | ValueSome outcome ->
        probe.Stop()
        match outcome.found with
        | ValueSome found -> this.Found found
        | ValueNone ->
          acc <- this.Absorb(acc, spread, outcome)
          if outcome.leftoverIndex >= DvalList.length spread then
            this.Finish acc
          else
            this.CarryOn(
              start + int64 outcome.leftoverIndex,
              DvalList.drop outcome.leftoverIndex spread
            )
    let outcome =
      spreadOver
        state
        vm
        builtinName
        (fun offset chunk -> this.ChunkArgs(start + int64 offset, chunk))
        this.AnswerOf
        app
        probe
        spread
    if outcome.IsCompletedSuccessfully then
      after outcome.Result
    else
      task {
        let! outcome = outcome
        return! Pending.toTask (after outcome)
      }
      |> Pending.ofTask

  /// Runs the element at `at`, the first of `items`, which is not empty.
  member private this.CarryOn(at : int64, items : DvalList) : Dval =
    let elem = DvalList.head items
    current <- elem
    index <- at
    rest <- DvalList.tail items
    this.Apply(at, elem, next)

  /// Runs the op over `items`, starting from `empty`.
  member this.Run(empty : 'acc, items : DvalList) : Dval =
    if DvalList.isEmpty items then
      this.Finish empty
    else
      acc <- empty
      next <- box this :?> (Dval -> Dval)
      probe <-
        if eligible state && DvalList.length items > 1 then
          Probe(vm, state.spread, app)
        else
          null
      this.CarryOn(0L, items)

/// Runs `op` over `items`, starting from `empty` (`Walk`).
let walk (op : Walk<'acc>) (empty : 'acc) (items : DvalList) : Dval =
  op.Run(empty, items)


// -- Off the program's order: a function of a read still in flight --
//
// `Stdlib.__whenReady(x, f)` over a read in flight must not wait for it there, or a response
// middleware would make every GET wait where it is sent, and `List.map(urls, get)` would run one
// request at a time. So its call is a read (`Interpreter.callReadsOnly`), handed back in flight,
// and this is its body: wait for the read, then apply `f` in a process of its own, as a chunk is
// (`spreadChild`). It runs whenever the read lands, by which time the program has moved on, so an
// effect there would land out of order; a chunk refuses it before it does anything
// (`EffectWhereOnlyComputing`), and the refusal is what the read fails with, where the program
// waits for it. A read that has landed already, and a plain value, are applied at once, on this
// thread, but under the same `spreadChild` state a chunk runs with: no process is spawned for a
// value already here, and whether `f` may take an effect still never depends on the method or
// on how fast the read was.

/// What the read lands as, waiting again while it lands as another read. A failed read raises
/// its own failure. Nothing on the VM is looked at: by now it belongs to whatever ran on.
let rec private landedValue (dv : Dval) : Task<Dval> =
  match dv with
  | DPromise p ->
    task {
      let! landed = p.Task
      return! landedValue landed
    }
  | v -> Task.FromResult v

/// `app` applied to what `read` lands as (itself, when it is not a read), computed as a chunk is:
/// the body of `Stdlib.__whenReady`. Applied at once when `read` has landed already, else in a
/// process of its own once it lands. Everything it needs from the caller (the scheduler, the
/// parent process, the access) is taken now, before the first wait.
let whenLanded
  (state : ExecutionState)
  (vm : VMState)
  (app : Applicable)
  (read : Dval)
  : Dval =
  let access = vm.activeAccess
  let childState = { state with spreadChild = true }
  let answer (result : ExecutionResult) =
    match result with
    | Ok v -> v
    | Error(rte, _) -> raiseRTE rte
  let landed = landedValue read
  if landed.IsCompletedSuccessfully then
    task {
      let! result = Execution.executeApplicable1 childState access app landed.Result // landed
      return answer result
    }
    |> Pending.ofTask
  else
    let sched = Sched.Scheduler.CurrentOrShared
    let parent = Sched.Scheduler.CurrentProcess |> Option.map (fun p -> p.id)
    task {
      let! value = landed
      let p = sched.SpawnApply(childState, app, value, parent, access)
      let! result = p.completion.Task
      sched.TakeResult p.id |> ignore<Option<ExecutionResult>>
      return answer result
    }
    |> Pending.ofTask
