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


/// How much a run records, as one ladder: each level is the one before it plus more.
///
///   off       nothing at all.
///   inputs    the run and what it was given: one row, no call log. Classic's "input value",
///             and enough to list a run, see what it was asked to do, and run it again from
///             the top.
///   effects   ... and every impure call, with its arguments and its result, in order. This
///             is what a resume answers from, and what a fork branches. The default.
///   values    ... and every other call, frame and lambda. What inline live values replay
///             through, and the one that fills a disk: a night of ordinary work at this level
///             left 15.8 GB in `trace_fn_calls`, and classic, which had no other level,
///             reached 10 TB with 99.7% of it traces. A development setting.
///
/// `DARK_CONFIG_TRACE_DETAIL` names one of the four. The default is `effects`, because the
/// impure-only log is thin and makes every run resumable, and `TraceRetention` keeps the
/// tables bounded.
///
/// Orthogonal to TraceSamplingRule, which decides whether to trace this run at all.
module TraceDetail =
  type T =
    | Off
    /// The row and its input. No calls.
    | Inputs
    /// ... and the impure calls (non-empty `callEffects`), each with its ordinal: what a
    /// resume serves from (`docs/processes.md`).
    | Effects
    /// ... and every other call, frame and lambda (`docs/live.md`, "Live values").
    | Values

  let private readEnv () : T =
    match System.Environment.GetEnvironmentVariable "DARK_CONFIG_TRACE_DETAIL" with
    | "off" -> Off
    | "inputs" -> Inputs
    | "values" -> Values
    // `on` was the old spelling of "record everything", and before that of "record at all".
    // It means the default now, which is what somebody who wrote it wanted either way.
    | _ -> Effects

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
      System.Collections.Generic.Dictionary<struct (System.Guid * int64), RT.Tracing.ReplayStep>
    /// The package functions this run went through, by hash, for the `trace_fns` index. A
    /// hash-set add per call, resolved to names once at store time.
    fns : System.Collections.Generic.HashSet<RT.Hash>
    /// Processes whose replay has ended: the log had no answer for an ordinal they asked for,
    /// so they are live from there and nothing later in the log may be handed to them (a fork
    /// cut by position can leave a later ordinal without its earlier ones).
    replayEnded : System.Collections.Generic.HashSet<System.Guid>
    sync : obj
  }


let private newState () : TracerState =
  { events = System.Collections.Generic.List<CompletedEvent>()
    fns = System.Collections.Generic.HashSet<RT.Hash>()
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


/// Secrets out of the log (`docs/processes.md`, "Traces").
///
/// The log holds every effectful call's arguments and results, and tracing is on by default, so
/// a token in a header or an environment variable would sit in `data.db` in the clear. Two
/// things are taken out before a row is written, and nothing else is: a request header whose
/// name is in `secretHeaders`, and the value an environment read answered. Arguments can be
/// redacted freely, since a replay serves results, not arguments. An environment read's secret
/// IS its result, so the result is not stored and the replay performs that read again for real
/// (`PerformOnce`, decided by the builtin's name in the row) rather than serving a
/// value it does not have. File contents and response bodies are stored as they are: a secret
/// file read into a run is in the log, which `dark docs processes` says.
module Redact =
  let secretHeaders : Set<string> =
    Set.ofList
      [ "authorization"; "cookie"; "set-cookie"; "x-api-key"; "proxy-authorization" ]

  /// Builtins whose result is a secret the log must not keep. Their rows are written with no
  /// result at all.
  let secretResults : Set<string> =
    Set.ofList [ "environmentGet"; "environmentGetAll" ]

  /// Builtins a replay performs again instead of serving from the log, going on replaying
  /// after (`ReplayStep.PerformOnce`). Two reasons land a name here:
  ///
  /// - the log has no result to serve, because the result was the secret (`secretResults`);
  /// - the logged result is a HANDLE, which only means something inside the run that made it.
  ///   A spawned process's id names a process that no longer exists, so serving it would make
  ///   the next `Exec.await` fail with "no process has this handle". Spawning again is what
  ///   makes the resume work: the new child replays the recorded child's own rows.
  let performAgain : Set<string> =
    Set.union
      secretResults
      (Set.ofList [ "execSpawn"; "execSpawnDetached"; "execCancel"; "execKill" ])

  let private redactedText = RT.DString "[redacted]"

  /// A header list as the HTTP builtins take it: a list of (name, value) tuples.
  let private headers (dv : RT.Dval) : RT.Dval =
    match dv with
    | RT.DList(vt, items) ->
      let redactOne (item : RT.Dval) : RT.Dval =
        match item with
        | RT.DTuple(RT.DString name, _value, []) when
          Set.contains (String.toLowercase name) secretHeaders
          ->
          RT.DTuple(RT.DString name, redactedText, [])
        | other -> other
      RT.DList(vt, List.map redactOne items)
    | other -> other

  /// The arguments of `builtin` as they should be stored.
  let args (builtin : string) (args : List<RT.Dval>) : List<RT.Dval> =
    if builtin.StartsWith "httpClient" || builtin.StartsWith "http" then
      args |> List.map headers
    else
      args

  /// The result of `builtin` as it should be stored.
  let result (builtin : string) (result : RT.Dval) : RT.Dval =
    if Set.contains builtin secretResults then RT.DUnit else result


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
            args = Redact.args (fnNameToSimpleString name) (NEList.toList args)
            result = Redact.result (fnNameToSimpleString name) result
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
      noteFunction =
        (fun hash -> lock state.sync (fun () -> state.fns.Add hash |> ignore<bool>))
      skipTracing = (level <> TraceDetail.Values)
      // `inputs` keeps the row and nothing under it.
      traceEffects = (level = TraceDetail.Effects || level = TraceDetail.Values)
      nextEffect = (fun () -> lock state.sync (fun () -> nextOrdinal state pid))
      replayEffect =
        (fun ord ->
          lock state.sync (fun () ->
            if state.replayEnded.Contains pid then
              RT.Tracing.ReplayStep.PerformOnwards
            else
              match state.replay.TryGetValue(struct (pid, ord)) with
              | true, answer -> answer
              | false, _ ->
                state.replayEnded.Add pid |> ignore<bool>
                RT.Tracing.ReplayStep.PerformOnwards))
      forProcess = executionTracingFor state level }


/// Keeps the trace tables bounded: after a store, the oldest traces past the caps go, except
/// one a suspended or pinned run needs (its log is what `resume` replays) and the newest run
/// of each entry, which the count cap alone never drops. `trace.keep` is
/// how many traces to keep (200 unset; a served request is one), `trace.maxMb` how many
/// megabytes of args and results (256 unset); 0 disables a cap. Both are store config keys the
/// host reads at startup (`configure`). A pass runs at most every ten seconds, since a `serve`
/// stores a trace per request. The manual `traces prune|delete|clear` go through here too, so
/// an execution row never outlives its trace.
module TraceRetention =
  open LibDB.Sqlite

  let mutable keep : int64 = 200L
  let mutable maxBytes : int64 = 256L * 1024L * 1024L
  let mutable private lastPass = System.DateTime.MinValue

  /// The caps, from the store's `trace.keep` and `trace.maxMb` as the host read them (a
  /// missing or unparseable value keeps the default).
  let configure (keepTraces : Option<string>) (maxMb : Option<string>) : unit =
    let parse (v : string) : Option<int64> =
      match System.Int64.TryParse v with
      | true, n when n >= 0L -> Some n
      | _ -> None
    keepTraces |> Option.bind parse |> Option.iter (fun n -> keep <- n)
    maxMb
    |> Option.bind parse
    |> Option.iter (fun n -> maxBytes <- n * 1024L * 1024L)

  /// Test seam.
  let setForTesting (keepTraces : int64) (bytes : int64) : unit =
    keep <- keepTraces
    maxBytes <- bytes
    lastPass <- System.DateTime.MinValue

  /// Delete these runs: their calls and their rows.
  let deleteTraces (ids : List<string>) : unit =
    match ids with
    | [] -> ()
    | _ ->
      let ps = ids |> List.map (fun id -> [ "id", Sql.string id ])
      Sql.executeTransactionSync
        [ "DELETE FROM trace_fn_calls WHERE trace_id = @id", ps
          "DELETE FROM trace_fns WHERE trace_id = @id", ps
          "DELETE FROM traces WHERE id = @id", ps ]
      |> ignore<List<int>>

  /// Drop the oldest traces past `keepTraces` and `bytes` (`None`: no cap on that axis), never
  /// one a suspended or pinned run needs, never the newest over the byte cap alone, and never the
  /// newest run of an entry over the COUNT cap alone. Returns how many went.
  ///
  /// The last of those is the floor classic had as "the last 10 traces per route": without it,
  /// a `serve` under load writes a trace per request and evicts the `eval` you were working on
  /// within `trace.keep` requests. The entry is the trace's `handler_desc` (`eval`,
  /// `run <file>`, `GET /path`) with any query string cut off, so `/search?q=a` and
  /// `/search?q=b` are one entry rather than two -- otherwise the floor is unbounded, since
  /// every distinct query string would be an entry of its own. The BYTE cap still applies to a
  /// floored trace: one huge recording should not be kept forever because it is the newest of
  /// its kind.
  let prune (keepTraces : Option<int64>) (bytes : Option<int64>) : int =
    // Newest first, with each trace's byte weight, whether it is suspended or pinned, and
    // whether it is the newest of its entry.
    let rows =
      Sql.query
        "SELECT t.id AS id,
                COALESCE((SELECT SUM(LENGTH(c.args) + LENGTH(c.result))
                          FROM trace_fn_calls c WHERE c.trace_id = t.id), 0) AS bytes,
                (t.status = 'suspended' OR t.pinned = 1) AS needed,
                ROW_NUMBER() OVER (
                  PARTITION BY CASE
                                 WHEN INSTR(t.handler_desc, '?') > 0
                                 THEN SUBSTR(t.handler_desc, 1, INSTR(t.handler_desc, '?') - 1)
                                 ELSE t.handler_desc
                               END
                  ORDER BY t.timestamp DESC, t.rowid DESC) AS entry_rank
         FROM traces t ORDER BY t.timestamp DESC, t.rowid DESC"
      |> Sql.executeAsync (fun read ->
        read.string "id",
        read.int64 "bytes",
        read.int "needed" = 1,
        read.int64 "entry_rank" = 1L)
      |> fun t -> t.Result
    // Walk newest to oldest, keeping until a cap is hit; everything older goes. The newest
    // stays even over the byte cap alone, so a run's own trace survives its own store.
    let mutable seenCount = 0L
    let mutable seenBytes = 0L
    let doomed =
      rows
      |> List.filter (fun (_, bytes', needed, newestOfEntry) ->
        seenCount <- seenCount + 1L
        seenBytes <- seenBytes + bytes'
        let overCount =
          match keepTraces with
          | Some n -> seenCount > n && not newestOfEntry
          | None -> false
        let overBytes =
          match bytes with
          | Some n -> seenBytes > n && seenCount > 1L
          | None -> false
        (overCount || overBytes) && not needed)
    deleteTraces (doomed |> List.map (fun (id, _, _, _) -> id))
    List.length doomed

  /// The pass after a store: at most every ten seconds, and the byte scan only once there are
  /// enough traces for the byte cap to matter (a count is one index read).
  let run () : int =
    if keep = 0L && maxBytes = 0L then
      0
    else
      let now = System.DateTime.UtcNow
      if (now - lastPass).TotalSeconds < 10.0 then
        0
      else
        let count =
          Sql.query "SELECT COUNT(*) AS n FROM traces"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
          |> fun t -> t.Result
        let overCount = keep > 0L && count > keep
        if not overCount && count < 50L then
          0
        else
          lastPass <- now
          let cap (n : int64) = if n = 0L then None else Some n
          prune (cap keep) (cap maxBytes)


/// Store trace data to SQLite.
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
    (fns : List<RT.Hash>)
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

      // DELETE-before-INSERT on trace_fn_calls matches the upsert on traces, so re-running
      // store for a trace replaces rather than accumulates. Input is stored inline on the row.
      // account_id is nullable: anonymous / outer-CLI runs leave it NULL.
      //
      // An UPSERT that names only the recorder's own columns, NOT `INSERT OR REPLACE`: the row
      // is usually already there, written by `Traces.create` when the run started, and it
      // carries the run's half -- status, fork lineage, pinned, and the timestamp of the START.
      // A REPLACE would silently default all of that away on every flush, and a flush happens
      // on every suspend.
      let baseStatements =
        [ "INSERT INTO traces
          (id, root_tlid, handler_desc, timestamp,
           input_name, input_value, account_id, status, updated)
         VALUES
          (@id, @rootTlid, @handlerDesc, @timestamp,
           @inputName, @inputValue, @accountId, 'done', @timestamp)
         ON CONFLICT(id) DO UPDATE SET
           root_tlid = excluded.root_tlid,
           handler_desc = excluded.handler_desc,
           input_name = excluded.input_name,
           input_value = excluded.input_value,
           account_id = excluded.account_id,
           updated = excluded.updated",
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

      // Which functions the run went through, names only (`trace_fns`). `INSERT OR IGNORE`
      // because a resume rewrites its trace in place and the pairs are the same.
      let fnStmt =
        match fns with
        | [] -> []
        | _ ->
          [ "INSERT OR IGNORE INTO trace_fns (trace_id, fn_name) VALUES (@traceId, @fnName)",
            fns
            |> List.map (fun hash ->
              [ "traceId", Sql.string traceIdStr
                "fnName",
                Sql.string (fnNameToSimpleString (RT.FQFnName.Package hash)) ]) ]

      let _ = Sql.executeTransactionSync (baseStatements @ eventStmt @ fnStmt)
      TraceRetention.run () |> ignore<int>


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
      let struct (events, dropped, nextSeq, fns) =
        lock state.sync (fun () ->
          struct (state.events.ToArray(),
                  state.dropped,
                  state.nextSeq,
                  List.ofSeq state.fns))
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
          fns
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
  (log : List<System.Guid * int64 * RT.Tracing.ReplayStep>)
  : T =
  let results = TraceResults.empty ()
  let state = newState ()
  // The recorded processes, in the order they first appear in the log. `Guid.Empty` is NOT one
  // of them: those rows are the unscheduled root's, seeded below and answered through the root
  // hooks. Leaving it in the list would hand the FIRST spawned child the root's rows, and the
  // child's own log would never be reached (its first call would answer with the root's first
  // answer, which is how a replayed spawn came back with a fresh uuid).
  let mutable unmatched =
    log
    |> List.map (fun (pid, _, _) -> pid)
    |> List.distinct
    |> List.filter (fun pid -> pid <> System.Guid.Empty)
  // A run nobody scheduled recorded under `Guid.Empty`, and a resume nobody schedules asks
  // under it too, through the root hooks below, so those rows answer directly as well as
  // through the matching.
  for (pid, ord, answer) in log do
    if pid = System.Guid.Empty then
      state.replay[struct (System.Guid.Empty, ord)] <- answer
  let rec tracingFor (pid : System.Guid) : RT.Tracing.Tracing =
    lock state.sync (fun () ->
      match unmatched with
      | recorded :: rest ->
        unmatched <- rest
        for (rpid, ord, answer) in log do
          if rpid = recorded then state.replay[struct (pid, ord)] <- answer
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


/// A tracer for VIEWING a run: every effectful call is answered by its name and arguments from
/// that run's log, none is performed, and every expression's value is collected on the way.
/// Classic called this Preview. Nothing is written: a preview is not itself a run.
///
/// The key is `(name, arguments)` rather than the `(process, ordinal)` a resume uses, and for
/// the reason classic had: a view has to survive the code having moved on. Add a call in the
/// middle and every ordinal after it shifts, so an ordinal-keyed view would go blank from
/// there; a name-and-arguments key still answers every call you did not touch. Last write wins,
/// as classic's `DISTINCT ON ... ORDER BY timestamp DESC` did.
///
/// `collected` is where the values land, keyed by the source expression's id.
let createPreviewTracer
  (rows : List<string * byte[] * RT.Dval>)
  (collected : System.Collections.Generic.List<int64 * RT.Dval>)
  : T =
  let answers = System.Collections.Generic.Dictionary<string, RT.Dval>()
  for (name, argsBytes, result) in rows do
    answers[name + "\u0000" + System.Convert.ToBase64String argsBytes] <- result

  let lookup (name : string) (args : RT.Dval[]) : Option<RT.Dval> =
    // The same shape the recorder wrote: one `DList` blob of the arguments.
    let argsBytes =
      BinarySer.RT.Dval.serialize
        "trace_fn_calls.args"
        (RT.DList(LibExecution.ValueType.unknownTODO, List.ofArray args))
    match
      answers.TryGetValue(name + "\u0000" + System.Convert.ToBase64String argsBytes)
    with
    | true, v -> Some v
    | false, _ -> None

  // Every process of the run previews, not just the first. The CLI spawns each expression as a
  // process of its own, and the scheduler asks the tracer for that process's own hooks
  // (`forProcess`); handing back the default there is handing back a tracer that performs
  // effects for real, which is the one thing a preview must never do.
  let rec previewTracing () : RT.Tracing.Tracing =
    { Exe.noTracing with
        skipTracing = false
        traceEffects = false
        storeExprResult = fun exprId dv -> collected.Add(int64 exprId, dv)
        previewEffect = Some lookup
        forProcess = fun _ -> previewTracing () }

  // `enabled = false` is doing real work: it is what stops the host giving this run a row of
  // its own, and what makes the store a no-op.
  { enabled = false
    results = TraceResults.empty ()
    storeTraceInput = fun _ _ _ -> ()
    storeTraceResults = fun _ -> uply { return () }
    executionTracing = previewTracing () }


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
