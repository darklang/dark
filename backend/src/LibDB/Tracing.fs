/// Tracing for real execution
module LibDB.Tracing

open Fumble

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module Exe = LibExecution.Execution
module Blob = LibExecution.Blob
module RTToDT = LibExecution.RuntimeTypesToDarkTypes
module BinarySer = LibSerialization.Binary.Serialization

/// Tracing can go overboard, so use a per-handler feature flag to control it. If
/// sampling is disabled for a scope, no traces will be recorded to be saved to the
/// DBs, but tlids will still be recorded as they are needed by APIs.
module TraceSamplingRule =
  type T =
    | SampleNone
    | SampleAll
    /// Sample one every `n`
    | SampleOneIn of n : int
    | SampleAllWithTelemetry

  let parseRule (ruleString : string) : Result<T, string> =
    match ruleString with
    | "sample-none" -> Ok SampleNone
    | "sample-all" -> Ok SampleAll
    | "sample-all-with-telemetry" -> Ok SampleAllWithTelemetry
    | _ ->
      try
        let prefix = "sample-one-in-"
        if String.startsWith prefix ruleString then
          let number = ruleString |> String.dropLeft (String.length prefix) |> int
          Ok(SampleOneIn number)
        else
          Error "Invalid sample"
      with _ ->
        Error "Exception thrown"

  /// Get the trace sampling rule for a handler. Always returns SampleAll now that
  /// LaunchDarkly has been removed.
  let ruleForHandler (_tlid : tlid) : T = SampleAll



/// Simplified version of the TraceSamplingRule. Resolves the one-in-x option into
/// DoTrace or DontTrace
module TracingConfig =
  type T =
    | DoTrace
    | DontTrace
    | TraceWithTelemetry

  let fromRule (rule : TraceSamplingRule.T) (traceID : AT.TraceID.T) : T =
    match rule with
    | TraceSamplingRule.SampleAll -> DoTrace
    | TraceSamplingRule.SampleNone -> DontTrace
    | TraceSamplingRule.SampleAllWithTelemetry -> TraceWithTelemetry
    | TraceSamplingRule.SampleOneIn freq ->
      // Use the traceID as an existing source of entropy.
      let random =
        (AT.TraceID.toUUID traceID).ToByteArray() |> System.BitConverter.ToInt64
      if random % (int64 freq) = 0L then DoTrace else DontTrace

  let forHandler (tlid : tlid) (traceID : AT.TraceID.T) : T =
    let samplingRule = TraceSamplingRule.ruleForHandler tlid
    fromRule samplingRule traceID

  let shouldTrace (config : T) =
    match config with
    | DoTrace
    | TraceWithTelemetry -> true
    | DontTrace -> false



module TraceResults =
  type T = { tlids : HashSet.HashSet<tlid> }

  let empty () : T = { tlids = HashSet.empty () }


/// Whether to record traces at all. Orthogonal to TraceSamplingRule:
/// sampling decides *whether to trace this run*; detail just toggles
/// the whole storage path. Override at startup with the
/// `DARK_CONFIG_TRACE_DETAIL` env var (`off` to disable).
module TraceDetail =
  type T =
    | Off
    /// Only effectful builtin calls, with their ordinal: the log a run can be resumed or forked
    /// from (`docs/processes.md`, "Executions"). Thin: nothing pure, no frames.
    | Effects
    /// Every call, frame and lambda: the tree `traces view` renders.
    | On

  // Default OFF: traces have no retention/GC (see TraceStorage.store) and a single row can reach ~1 GB (a
  // `serve` request records its whole response — e.g. a sync op/blob batch — as trace args), so a long-running
  // `serve`/daemon fills the disk unbounded. Traces are dev telemetry (stripped from the exported seed), so
  // the shipped binary must not accumulate them; dev/CI opt in via DARK_CONFIG_TRACE_DETAIL=on. Default back
  // to on only once retention (size cap + GC) exists.
  let private readEnv () : T =
    match System.Environment.GetEnvironmentVariable "DARK_CONFIG_TRACE_DETAIL" with
    | "on" -> On
    | "effects" -> Effects
    | _ -> Off

  let mutable current : T = readEnv ()

  /// Test seam: tests can pin the level without rebuilding config.
  let setForTesting (level : T) : unit = current <- level



/// Collections of functions and values used during a single execution
type T =
  {
    /// Store the tracing input (varname + dval) for a handler execution
    /// (kind, path, modifier) triple — was `PT.Handler.HandlerDesc`
    /// before Handler was deleted. Trace recorders synthesize a triple
    /// for each request (e.g. ("HTTP", "/foo", "GET")) so traces.list
    /// has something to show in the handler column.
    storeTraceInput : (string * string * string) -> string -> RT.Dval -> unit

    /// Store the trace results calculated over the execution, if enabled.
    /// Takes the live ExecutionState so ephemeral blob refs (which die
    /// when the request scope pops) can be promoted to persistent ones
    /// before serialization. Without that, traces would record blob refs
    /// pointing at gone bytes and `traces view` / `gen-test` couldn't
    /// reconstruct request/response bodies.
    storeTraceResults : RT.ExecutionState -> Ply.Ply<unit>

    /// The functions to run tracing during execution
    executionTracing : RT.Tracing.Tracing

    /// Results of the execution
    results : TraceResults.T
    enabled : bool
  }


/// Resolve package fn hashes to human-readable names. Cached in-process so
/// only the first reference to each hash hits the DB; subsequent calls
/// return the resolved name directly. Falls back to the raw hash if the
/// fn isn't found (e.g. it was deleted).
module FnNameCache =
  open LibDB.Sqlite

  let mutable private cache : Map<string, string> = Map.empty

  let resolve (hash : string) : string =
    match Map.tryFind hash cache with
    | Some name -> name
    | None ->
      let result =
        try
          Sql.query
            "SELECT owner, modules, name FROM locations
             WHERE item_hash = @hash AND item_type = 'fn'
             LIMIT 1"
          |> Sql.parameters [ "hash", Sql.string hash ]
          |> Sql.executeRowOptionAsync (fun read ->
            let owner = read.string "owner"
            let modules = read.string "modules"
            let name = read.string "name"
            let modules = if modules = "" then "" else $"{modules}."
            $"{owner}.{modules}{name}")
          |> Async.AwaitTask
          |> Async.RunSynchronously
          |> Ok
        with ex ->
          print $"[tracing] FnNameCache failed to resolve {hash}: {ex.Message}"
          Telemetry.event
            "trace.fnNameCacheResolveFailed"
            [ "hash", hash; "message", ex.Message ]
          Error()

      // Cache the miss too, as the raw hash. Caching only hits means a hash with no row in `locations`
      // re-queries SQLite on every reference. A miss is as stable an answer as a hit, and the contract is
      // already that a trace records the name as of execution time.
      //
      // A *failure* is not a miss, though, and must not be cached: the store being briefly unreadable
      // (locked mid-reload, say) would otherwise degrade every later reference in the process to a raw
      // hash, permanently, for a transient reason. Fall back to the hash for this one lookup and retry
      // next time.
      match result with
      | Error() -> hash
      | Ok found ->
        let resolved =
          match found with
          | Some name -> name
          | None -> hash
        cache <- Map.add hash resolved cache
        resolved


/// Display name written into the fn_hash column. Resolved at write time
/// (via FnNameCache for package fns) so the reader can render traces with
/// a flat SELECT — no JOIN against locations needed. The trade-off: the
/// trace records the name as it was at execution time, so subsequent
/// renames/deletions don't change historical traces.
let private fnNameToSimpleString (name : RT.FQFnName.FQFnName) : string =
  match name with
  | RT.FQFnName.Builtin b ->
    if b.version = 0 then b.name else $"{b.name}_v{b.version}"
  | RT.FQFnName.Package(RT.Hash h) -> FnNameCache.resolve h


/// Completed call event ready to emit to trace_fn_calls.
type CompletedEvent =
  {
    callId : string
    parentCallId : string option
    kind : string // "function" | "lambda" | "builtin"
    fnHash : string option // function/builtin only
    lambdaExprId : id option // lambda only
    args : List<RT.Dval>
    result : RT.Dval
    durationMs : int64 // 0 for builtins (no frame-entry hook); real ms for fn/lambda
    /// The process that made the call. `Guid.Empty` for a run nobody scheduled.
    processId : System.Guid
    /// Position in the whole trace, across processes: the order the calls completed in. Reading
    /// them all in `seq` order is the interleaving.
    seq : int64
    /// For an effectful builtin call, its ordinal among the process's effectful calls, taken when
    /// the call was made; -1 for everything else. One process's rows in `ord` order are its log,
    /// and what a replay keys on.
    ord : int64
  }


/// Partial event held on the writer's stack between storeFrameEntry and
/// the matching storeFnResult / storeLambdaResult. The kind isn't stored —
/// the finalizer (storeFnResult vs storeLambdaResult) already knows it.
type PartialEvent =
  {
    callId : string
    parentCallId : string option
    fnHash : string option
    lambdaExprId : id option
    args : List<RT.Dval>
    /// Stopwatch ticks at frame-entry. Subtract at exit and convert to ms.
    startedAtTicks : int64
  }


/// Cap on how many call events a single trace retains.
///
/// A trace is a debugging aid a human reads; past a few thousand calls it stops being one. Meanwhile every
/// retained event pins its arguments and result alive, gets walked by `prepareTraceForStorage`, and gets
/// binary-serialized twice (args and result) at store time. Uncapped, a list-heavy script spends most of
/// its run and most of its allocation writing the trace rather than executing the program.
///
/// Override with `DARK_CONFIG_TRACE_MAX_EVENTS`; 0 means unlimited, for when you genuinely need the whole
/// thing and are willing to pay for it.
module TraceLimits =
  let private fromEnv () : int =
    match
      System.Environment.GetEnvironmentVariable "DARK_CONFIG_TRACE_MAX_EVENTS"
    with
    | null
    | "" -> 10_000
    | s ->
      match System.Int32.TryParse s with
      | true, n when n >= 0 -> n
      | _ -> 10_000

  let mutable maxEvents : int = fromEnv ()

  /// TEST-ONLY: run with a small cap, so a test can cross it without generating (and rendering) ten
  /// thousand events. Call `resetMaxEventsForTesting` when done. NOT parallel-safe: it mutates
  /// process-global state, so callers must be `testSequenced`.
  let useMaxEventsForTesting (n : int) : unit = maxEvents <- n

  /// TEST-ONLY: restore the configured cap after `useMaxEventsForTesting`.
  let resetMaxEventsForTesting () : unit = maxEvents <- fromEnv ()


/// Mutable per-trace tracer state. Captures every event in completion order and keeps one open
/// call stack per process, so children find their parent in their own process's stack.
///
/// One trace, many processes: a script's expressions and anything they spawn write here from
/// whichever scheduler thread steps them, so every touch is under `sync`. Uncontended in the
/// one-process case, which is nearly every run.
type TracerState =
  {
    events : System.Collections.Generic.List<CompletedEvent>
    stacks :
      System.Collections.Generic.Dictionary<System.Guid, System.Collections.Generic.Stack<PartialEvent>>
    /// Events past `TraceLimits.maxEvents`, counted so the trace can say it was truncated rather than
    /// quietly looking complete.
    mutable dropped : int
    /// The next `seq`, handed out as events complete.
    mutable nextSeq : int64
    /// The next effectful-call ordinal per process, handed out as calls are made.
    ordinals : System.Collections.Generic.Dictionary<System.Guid, int64 ref>
    /// What a replay answers from: the recorded result of each effectful call, by process and
    /// ordinal. Empty for a fresh run.
    replay :
      System.Collections.Generic.Dictionary<struct (System.Guid * int64), RT.Dval>
    /// Processes whose replay has ended: the log had no answer for an ordinal they asked for,
    /// so they are live from there and nothing later in the log may be handed to them (a fork
    /// cut by position can leave a later ordinal without its earlier ones).
    replayEnded : System.Collections.Generic.HashSet<System.Guid>
    sync : obj
  }


let private newState () : TracerState =
  { events = System.Collections.Generic.List<CompletedEvent>()
    stacks = System.Collections.Generic.Dictionary()
    dropped = 0
    nextSeq = 0L
    ordinals = System.Collections.Generic.Dictionary()
    replay = System.Collections.Generic.Dictionary()
    replayEnded = System.Collections.Generic.HashSet()
    sync = obj () }


/// The next ordinal for `pid`'s effectful calls. Under `sync`.
let private nextOrdinal (state : TracerState) (pid : System.Guid) : int64 =
  match state.ordinals.TryGetValue pid with
  | true, r ->
    let n = r.Value
    r.Value <- n + 1L
    n
  | false, _ ->
    state.ordinals[pid] <- ref 1L
    0L


/// The open call stack of one process. Under `sync`.
let private stackFor
  (state : TracerState)
  (pid : System.Guid)
  : System.Collections.Generic.Stack<PartialEvent> =
  match state.stacks.TryGetValue pid with
  | true, stack -> stack
  | false, _ ->
    let stack = System.Collections.Generic.Stack<PartialEvent>()
    state.stacks[pid] <- stack
    stack


/// Frames still open, over every process: the ancestors of whatever completes next, which is what
/// `addEvent` reserves room for.
let private openFrames (state : TracerState) : int =
  let mutable n = 0
  for stack in state.stacks.Values do
    n <- n + stack.Count
  n


/// Retain an event unless we're at the cap, **reserving a slot for every frame still on the stack**.
///
/// That reservation is the whole trick, and without it the cap is worse than useless. Events are appended
/// on *completion*, so they arrive in post-order: leaves first, the entry frame last. A naive "keep the
/// first N" therefore keeps only the deepest calls and drops every one of their ancestors, including the
/// single root. `formatFnCalls` renders by walking down from roots, so the result is a trace where every
/// retained event is an orphan nothing walks to, and the viewer shows the truncation marker and nothing
/// else.
///
/// Reserving `stack.Count` fixes it, because the frames on the stack are exactly the ancestors of whatever
/// is completing now. Each pop both frees a reservation and consumes it, so `events.Count + stack.Count`
/// never exceeds the cap and every ancestor of a retained event is itself retained. What gets dropped is
/// deep siblings, which is what you want: the tree stays walkable and loses breadth, not its spine.
///
/// The *stack* is deliberately not capped: it's bounded by call depth rather than call count, and
/// pushes/pops have to stay balanced or parent linkage breaks for the events we do keep.
///
/// Under `sync`; `seq` is assigned here, so the order of `seq` is the order of completion across
/// every process writing the trace.
let private addEvent (state : TracerState) (ev : CompletedEvent) : unit =
  if
    TraceLimits.maxEvents = 0
    || state.events.Count + openFrames state < TraceLimits.maxEvents
  then
    let seq = state.nextSeq
    state.nextSeq <- seq + 1L
    state.events.Add { ev with seq = seq }
  else
    state.dropped <- state.dropped + 1


let private currentParentCallId
  (stack : System.Collections.Generic.Stack<PartialEvent>)
  : string option =
  if stack.Count = 0 then None else Some(stack.Peek().callId)


let private newCallId () : string = string (System.Guid.NewGuid())


/// Convert a Stopwatch-tick delta to milliseconds, clamping at zero so a
/// monotonic-clock blip can't surface as a negative duration.
let private ticksToMs (deltaTicks : int64) : int64 =
  let ms = deltaTicks * 1000L / System.Diagnostics.Stopwatch.Frequency
  if ms < 0L then 0L else ms


/// Fired when a Function or Lambda frame is pushed. We assign this call
/// its own call_id immediately so children entered before this call exits
/// can record us as their parent_call_id.
let private makeStoreFrameEntry
  (state : TracerState)
  (pid : System.Guid)
  : RT.Tracing.StoreFrameEntry =
  fun _ ep args ->
    let fnHash, lambdaExprId =
      match ep with
      | RT.Function name -> Some(fnNameToSimpleString name), None
      | RT.Lambda(_, exprId) -> None, Some exprId
      | RT.Source ->
        Exception.raiseInternal
          "Source ExecutionPoint cannot be pushed as a frame"
          []
    let startedAt = System.Diagnostics.Stopwatch.GetTimestamp()
    lock state.sync (fun () ->
      let stack = stackFor state pid
      let partial =
        { callId = newCallId ()
          parentCallId = currentParentCallId stack
          fnHash = fnHash
          lambdaExprId = lambdaExprId
          args = args
          startedAtTicks = startedAt }
      stack.Push(partial))


/// Fired for both fn frame returns and synchronous builtin calls. We
/// dispatch on the FQFnName: builtins emit a synchronous event with the
/// current top of stack as parent; package fn returns pop the matching
/// frame entry and finalize with the result.
let private makeStoreFnResult
  (state : TracerState)
  (pid : System.Guid)
  : RT.Tracing.StoreFnResult =
  fun (_, name) ord args result ->
    match name with
    | RT.FQFnName.Builtin _ ->
      lock state.sync (fun () ->
        addEvent
          state
          { callId = newCallId ()
            parentCallId = currentParentCallId (stackFor state pid)
            kind = "builtin"
            fnHash = Some(fnNameToSimpleString name)
            lambdaExprId = None
            args = NEList.toList args
            result = result
            // No frame-entry counterpart for builtins, so no real duration.
            durationMs = 0L
            processId = pid
            seq = 0L
            ord = ord })
    | RT.FQFnName.Package _ ->
      let endedAt = System.Diagnostics.Stopwatch.GetTimestamp()
      lock state.sync (fun () ->
        let stack = stackFor state pid
        if stack.Count > 0 then
          let partial = stack.Pop()
          addEvent
            state
            { callId = partial.callId
              parentCallId = partial.parentCallId
              kind = "function"
              fnHash = partial.fnHash
              lambdaExprId = None
              args = partial.args
              result = result
              durationMs = ticksToMs (endedAt - partial.startedAtTicks)
              processId = pid
              seq = 0L
              ord = -1L })


/// Fired when a Lambda frame returns. Pop the matching entry and finalize.
let private makeStoreLambdaResult
  (state : TracerState)
  (pid : System.Guid)
  : RT.Tracing.StoreLambdaResult =
  fun _ result ->
    let endedAt = System.Diagnostics.Stopwatch.GetTimestamp()
    lock state.sync (fun () ->
      let stack = stackFor state pid
      if stack.Count > 0 then
        let partial = stack.Pop()
        addEvent
          state
          { callId = partial.callId
            parentCallId = partial.parentCallId
            kind = "lambda"
            fnHash = None
            lambdaExprId = partial.lambdaExprId
            args = partial.args
            result = result
            durationMs = ticksToMs (endedAt - partial.startedAtTicks)
            processId = pid
            seq = 0L
            ord = -1L })


/// The interpreter hooks for one process writing this trace. `forProcess` hands a spawned process
/// its own; the hooks share the event list and get their own call stack and ordinals. Under the
/// `Effects` level only effectful builtin calls are recorded and the interpreter keeps its fast
/// paths (`skipTracing`); under `On`, everything.
let rec private executionTracingFor
  (state : TracerState)
  (level : TraceDetail.T)
  (pid : System.Guid)
  : RT.Tracing.Tracing =
  { Exe.noTracing with
      storeFrameEntry = makeStoreFrameEntry state pid
      storeFnResult = makeStoreFnResult state pid
      storeLambdaResult = makeStoreLambdaResult state pid
      skipTracing = (level <> TraceDetail.On)
      traceEffects = true
      nextEffect = (fun () -> lock state.sync (fun () -> nextOrdinal state pid))
      replayEffect =
        (fun ord ->
          lock state.sync (fun () ->
            if state.replayEnded.Contains pid then
              ValueNone
            else
              match state.replay.TryGetValue(struct (pid, ord)) with
              | true, dv -> ValueSome dv
              | false, _ ->
                state.replayEnded.Add pid |> ignore<bool>
                ValueNone))
      forProcess = executionTracingFor state level }


/// Store trace data to SQLite.
///
/// TODO: retention / GC. Every CLI eval / run / `serve` request writes
/// a full trace into `traces` + `trace_fn_calls`, and nothing prunes
/// them. Plan when the time comes:
///   - sampling
///   - per-row size cap on `dval_json` writes (one massive payload
///     could fill the disk on its own; truncate + tag the row)
///   - background sweeper that drops trace rows older than N days,
///     or trims to the most recent K traces per handler
/// `Builtins.Matter/Libs/Traces.fs` already has a `clear-before`
/// command path; the missing piece is the policy + a default cadence.
module TraceStorage =
  open LibDB.Sqlite

  /// Serialize a list of args as a single Dval (DList Unknown args) so
  /// the binary writer can roundtrip the whole sequence in one blob.
  /// `Unknown` value type is fine — args don't carry coherent type
  /// info at the trace boundary, and the reader just unwraps the list.
  let private serializeArgs (args : List<RT.Dval>) : byte[] =
    let asList = RT.DList(LibExecution.ValueType.unknownTODO, args)
    BinarySer.RT.Dval.serialize "trace_fn_calls.args" asList

  let private serializeDval (id : string) (dv : RT.Dval) : byte[] =
    BinarySer.RT.Dval.serialize id dv

  let store
    (rootTLID : tlid)
    (traceID : AT.TraceID.T)
    (handlerDesc : string)
    (inputVarName : string)
    (inputDval : RT.Dval)
    (events : List<CompletedEvent>)
    (accountID : Option<System.Guid>)
    : unit =
    if TraceDetail.current = TraceDetail.Off then
      ()
    else

      let traceIdStr = string traceID
      let timestamp = NodaTime.Instant.now().ToString()
      let traceIdParam = [ "traceId", Sql.string traceIdStr ]

      let inputBytes = serializeDval "traces.input_value" inputDval

      let accountIDSql =
        match accountID with
        | Some a -> Sql.uuid a
        | None -> Sql.dbnull

      // DELETE-before-INSERT on trace_fn_calls matches INSERT OR REPLACE
      // on traces, so re-running store for a trace_id replaces rather than
      // accumulates. Input is stored inline on the trace row. account_id
      // is nullable — anonymous / outer-CLI runs leave it NULL.
      let baseStatements =
        [ "INSERT OR REPLACE INTO traces
          (id, root_tlid, handler_desc, timestamp,
           input_name, input_value, account_id)
         VALUES
          (@id, @rootTlid, @handlerDesc, @timestamp,
           @inputName, @inputValue, @accountId)",
          [ [ "id", Sql.string traceIdStr
              "rootTlid", Sql.int64 (int64 rootTLID)
              "handlerDesc", Sql.string handlerDesc
              "timestamp", Sql.string timestamp
              "inputName", Sql.string inputVarName
              "inputValue", Sql.bytes inputBytes
              "accountId", accountIDSql ] ]

          "DELETE FROM trace_fn_calls WHERE trace_id = @traceId", [ traceIdParam ] ]

      // Skip the events INSERT when empty: fumble rejects zero-param-row
      // prepared statements, hit when a trace errors before any call fires.
      // The DELETE above still runs.
      let eventStmt =
        match events with
        | [] -> []
        | _ ->
          [ "INSERT INTO trace_fn_calls
            (trace_id, call_id, parent_call_id, kind, fn_hash,
             lambda_expr_id, args, result, duration_ms, process_id, seq, ord)
           VALUES
            (@traceId, @callId, @parentCallId, @kind, @fnHash,
             @lambdaExprId, @args, @result, @durationMs, @processId, @seq, @ord)",
            events
            |> List.map (fun ev ->
              let argsBytes = serializeArgs ev.args
              let resultBytes = serializeDval "trace_fn_calls.result" ev.result
              [ "traceId", Sql.string traceIdStr
                "callId", Sql.string ev.callId
                "parentCallId", Sql.stringOrNone ev.parentCallId
                "kind", Sql.string ev.kind
                "fnHash", Sql.stringOrNone ev.fnHash
                "lambdaExprId",
                (ev.lambdaExprId |> Option.map string |> Sql.stringOrNone)
                "args", Sql.bytes argsBytes
                "result", Sql.bytes resultBytes
                "durationMs", Sql.int64 ev.durationMs
                "processId",
                (if ev.processId = System.Guid.Empty then
                   Sql.string ""
                 else
                   Sql.string (string ev.processId))
                "seq", Sql.int64 ev.seq
                "ord", Sql.int64 ev.ord ]) ]

      let _ = Sql.executeTransactionSync (baseStatements @ eventStmt)
      ()


/// Rewrite a Dval for the trace-storage boundary:
///   - DStream → DStreamStub (the live pull fn closes over this VM's
///     exeState; draining would consume the user's stream).
///   - DBlob(Ephemeral _) → DBlob(Persistent _), promoting bytes
///     into package_blobs so the trace survives the producing VM.
/// Recursion and container rebuilding are handled by `Dval.rewriteWith`,
/// so nested DStream values (inside lists, records, closures, ...) are
/// stubbed just like top-level ones.
let prepareDvalForStorage
  (exeState : RT.ExecutionState)
  (dv : RT.Dval)
  : Ply.Ply<RT.Dval> =
  let promoteBlob = Blob.promoteEphemeralLeaf exeState.blobs.persist
  dv
  |> RT.Dval.rewriteWith (fun dv ->
    uply {
      match dv with
      | RT.DStream(impl, _, _) -> return Some(RTToDT.Dval.streamStubDT impl)
      | _ -> return! promoteBlob dv
    })


/// Walk every captured Dval through [prepareDvalForStorage]. Mutates
/// `state.events` in place; returns the prepared input dval.
let private prepareTraceForStorage
  (exeState : RT.ExecutionState)
  (inputDval : RT.Dval)
  (events : CompletedEvent[])
  : Ply.Ply<RT.Dval> =
  uply {
    let prep = prepareDvalForStorage exeState
    let! preparedInput = prep inputDval
    for i in 0 .. events.Length - 1 do
      let ev = events[i]
      let! preparedArgs = ev.args |> Ply.List.mapSequentially prep
      let! preparedResult = prep ev.result
      events[i] <- { ev with args = preparedArgs; result = preparedResult }

    return preparedInput
  }


/// Shared helper: store a trace to SQLite with error handling. Runs
/// every captured Dval through [prepareTraceForStorage] first —
/// stubs DStream values and promotes ephemeral blob bytes so the
/// trace survives the producing VM.
let private storeTrace
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  (handlerDesc : string)
  (inputVarName : string)
  (inputDval : RT.Dval)
  (state : TracerState)
  (exeState : RT.ExecutionState)
  : Ply.Ply<unit> =
  uply {
    // Trace detail OFF must be a true no-op. `prepareTraceForStorage` (below) promotes captured ephemeral
    // blobs into package_blobs before `TraceStorage.store`'s own off-check, so gating only the store still
    // grows package_blobs on every traced request. Bail here so neither the promote nor the store runs. This
    // is the single choke point for both the sqlite and CLI tracers (the serve uses the CLI one).
    if TraceDetail.current = TraceDetail.Off then
      return ()
    else

      let traceIdStr = string traceID
      use _span = Telemetry.span "trace.store" [ "traceId", traceIdStr ]
      // A copy taken under the lock: a run suspended by Ctrl-C stores while its processes may
      // still be recording, and the copy is what gets prepared and written.
      let struct (events, dropped, nextSeq) =
        lock state.sync (fun () ->
          struct (state.events.ToArray(), state.dropped, state.nextSeq))
      if dropped > 0 then
        Telemetry.event
          "trace.truncated"
          [ "kept", string events.Length; "dropped", string dropped ]
      try
        let! preparedInput = prepareTraceForStorage exeState inputDval events
        TraceStorage.store
          rootTLID
          traceID
          handlerDesc
          inputVarName
          preparedInput
          // A truncated trace carries a final marker row rather than just ending. Without it the trace
          // reads as complete, and "the call I'm looking for isn't here" is indistinguishable from "it
          // never happened" -- which is the one thing a debugging aid must never be ambiguous about.
          (if dropped > 0 then
             (List.ofArray events)
             @ [ { callId = newCallId ()
                   parentCallId = None
                   kind = "truncated"
                   fnHash =
                     Some
                       $"trace truncated: {dropped} further calls not recorded (cap {TraceLimits.maxEvents}, raise with DARK_CONFIG_TRACE_MAX_EVENTS)"
                   lambdaExprId = None
                   args = []
                   result = RT.DUnit
                   durationMs = 0L
                   processId = System.Guid.Empty
                   seq = nextSeq
                   ord = -1L } ]
           else
             List.ofArray events)
          exeState.accountID
      with ex ->
        let inner =
          match ex.InnerException with
          | null -> ""
          | e -> $" ({e.Message})"
        System.Console.Error.WriteLine
          $"[tracing] Failed to store trace: {ex.Message}{inner}"
        Telemetry.event
          "trace.storeFailed"
          [ "traceId", traceIdStr
            "exception", ex.GetType().FullName
            "message", ex.Message ]
  }


let createSqliteTracer (rootTLID : tlid) (traceID : AT.TraceID.T) : T =
  let results = TraceResults.empty ()
  let state = newState ()
  let mutable storedInputVarName = ""
  let mutable storedInputDval : RT.Dval = RT.DUnit
  let mutable handlerDesc = ""

  { enabled = true
    results = results
    executionTracing =
      executionTracingFor state TraceDetail.current System.Guid.Empty
    storeTraceInput =
      fun desc varname input ->
        let (kind, path, modifier) = desc
        handlerDesc <- $"{kind} {path} {modifier}"
        storedInputVarName <- varname
        storedInputDval <- input
    storeTraceResults =
      fun exeState ->
        storeTrace
          rootTLID
          traceID
          handlerDesc
          storedInputVarName
          storedInputDval
          state
          exeState }


let createCliTracer
  (traceID : AT.TraceID.T)
  (description : string)
  (inputVarName : string)
  (inputDval : RT.Dval)
  : T =
  let results = TraceResults.empty ()
  let state = newState ()

  // With detail off, collect nothing. `storeTrace` refuses to write in that case, so installing the
  // hooks anyway means building an event per frame and holding every argument and result alive for the
  // whole run, to discard all of it at the end.
  //
  // `Exe.noTracing` sets `skipTracing = true`, which also lets the interpreter skip its own per-frame
  // bookkeeping (`pendingCallArgs`) rather than just calling no-op hooks.
  if TraceDetail.current = TraceDetail.Off then
    { enabled = false
      results = results
      executionTracing = Exe.noTracing
      storeTraceInput = fun _ _ _ -> ()
      storeTraceResults = fun _ -> uply { return () } }
  else
    { enabled = true
      results = results
      executionTracing =
        executionTracingFor state TraceDetail.current System.Guid.Empty
      storeTraceInput = fun _ _ _ -> ()
      storeTraceResults =
        fun exeState ->
          storeTrace 0UL traceID description inputVarName inputDval state exeState }


/// A CLI tracer that replays a stored log: every effectful call whose `(process, ordinal)` the
/// log has is answered from it rather than performed, and the run records as it goes, so the
/// stored trace ends up as the replayed prefix plus whatever ran live after it.
///
/// The process ids in the log are the recorded run's; a resumed run's processes are new. A
/// script's processes start in a fixed order (the expressions, one after another), and a process's
/// first effectful call comes after its start, so the recorded processes are listed in the order
/// they first appear in the log and each new process, as the interpreter meets it, is matched to
/// the next one (`forProcess`). Anything past the recorded list replays nothing.
let createReplayTracer
  (traceID : AT.TraceID.T)
  (description : string)
  (inputVarName : string)
  (inputDval : RT.Dval)
  (log : List<System.Guid * int64 * RT.Dval>)
  : T =
  let results = TraceResults.empty ()
  let state = newState ()
  let mutable unmatched = log |> List.map (fun (pid, _, _) -> pid) |> List.distinct
  // A run nobody scheduled recorded under `Guid.Empty`, and a resume nobody schedules asks
  // under it too, through the root hooks below, so those rows answer directly as well as
  // through the matching.
  for (pid, ord, dv) in log do
    if pid = System.Guid.Empty then
      state.replay[struct (System.Guid.Empty, ord)] <- dv
  let rec tracingFor (pid : System.Guid) : RT.Tracing.Tracing =
    lock state.sync (fun () ->
      match unmatched with
      | recorded :: rest ->
        unmatched <- rest
        for (rpid, ord, dv) in log do
          if rpid = recorded then state.replay[struct (pid, ord)] <- dv
      | [] -> ())
    { executionTracingFor state TraceDetail.current pid with
        forProcess = tracingFor }
  { enabled = true
    results = results
    // The root's own hooks (the CLI's process, which makes no effectful calls of its own in a
    // script; the expressions are child processes and go through `forProcess`).
    executionTracing =
      { executionTracingFor state TraceDetail.current System.Guid.Empty with
          forProcess = tracingFor }
    storeTraceInput = fun _ _ _ -> ()
    storeTraceResults =
      fun exeState ->
        storeTrace 0UL traceID description inputVarName inputDval state exeState }


let createNonTracer (_traceID : AT.TraceID.T) : T =
  let results = TraceResults.empty ()
  { enabled = false
    results = results
    executionTracing = LibExecution.Execution.noTracing
    storeTraceResults = fun _ -> uply { return () }
    storeTraceInput = fun _ _ _ -> () }


let create (rootTLID : tlid) (traceID : AT.TraceID.T) : T =
  // Trace detail OFF must mean FULLY off — not just "don't write the trace rows". The sqlite tracer captures
  // call events during execution and, at store time, `prepareDvalForStorage` PROMOTES their ephemeral blobs into
  // package_blobs (so a trace survives its VM) BEFORE `TraceStorage.store`'s off-check runs. So gating only the
  // store still leaves that blob-promotion firing on every `serve` request — which, for the sync endpoints
  // (responses = whole op/blob batches), grew package_blobs unboundedly even with trace storage "off". Returning
  // the non-tracer here makes Off a true no-op: no capture, no promote, no store.
  if TraceDetail.current = TraceDetail.Off then
    createNonTracer traceID
  else
    let config = TracingConfig.forHandler rootTLID traceID
    match config with
    | TracingConfig.DoTrace
    | TracingConfig.TraceWithTelemetry -> createSqliteTracer rootTLID traceID
    | TracingConfig.DontTrace -> createNonTracer traceID
