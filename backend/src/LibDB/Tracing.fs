/// Tracing for real execution
module LibDB.Tracing

open Fumble

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module Exe = LibExecution.Execution
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable

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
    | On

  let private readEnv () : T =
    match System.Environment.GetEnvironmentVariable "DARK_CONFIG_TRACE_DETAIL" with
    | "off" -> Off
    | _ -> On

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
        with ex ->
          print $"[tracing] FnNameCache failed to resolve {hash}: {ex.Message}"
          Telemetry.event
            "trace.fnNameCacheResolveFailed"
            [ "hash", hash; "message", ex.Message ]
          None

      match result with
      | Some name ->
        cache <- Map.add hash name cache
        name
      | None -> hash


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
  { callId : string
    parentCallId : string option
    kind : string // "function" | "lambda" | "builtin"
    fnHash : string option // function/builtin only
    lambdaExprId : id option // lambda only
    args : List<RT.Dval>
    result : RT.Dval
    durationMs : int64 } // 0 for builtins (no frame-entry hook); real ms for fn/lambda


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


/// Mutable per-trace tracer state. Captures every event in execution order
/// and tracks the open call stack so children can find their parent.
type TracerState =
  { events : System.Collections.Generic.List<CompletedEvent>
    stack : System.Collections.Generic.Stack<PartialEvent> }


let private newState () : TracerState =
  { events = System.Collections.Generic.List<CompletedEvent>()
    stack = System.Collections.Generic.Stack<PartialEvent>() }


let private currentParentCallId (state : TracerState) : string option =
  if state.stack.Count = 0 then None else Some(state.stack.Peek().callId)


let private newCallId () : string = string (System.Guid.NewGuid())


/// Convert a Stopwatch-tick delta to milliseconds, clamping at zero so a
/// monotonic-clock blip can't surface as a negative duration.
let private ticksToMs (deltaTicks : int64) : int64 =
  let ms = deltaTicks * 1000L / System.Diagnostics.Stopwatch.Frequency
  if ms < 0L then 0L else ms


/// Fired when a Function or Lambda frame is pushed. We assign this call
/// its own call_id immediately so children entered before this call exits
/// can record us as their parent_call_id.
let private makeStoreFrameEntry (state : TracerState) : RT.Tracing.StoreFrameEntry =
  fun _ ep args ->
    let fnHash, lambdaExprId =
      match ep with
      | RT.Function name -> Some(fnNameToSimpleString name), None
      | RT.Lambda(_, exprId) -> None, Some exprId
      | RT.Source ->
        Exception.raiseInternal
          "Source ExecutionPoint cannot be pushed as a frame"
          []
    let partial =
      { callId = newCallId ()
        parentCallId = currentParentCallId state
        fnHash = fnHash
        lambdaExprId = lambdaExprId
        args = args
        startedAtTicks = System.Diagnostics.Stopwatch.GetTimestamp() }
    state.stack.Push(partial)


/// Fired for both fn frame returns and synchronous builtin calls. We
/// dispatch on the FQFnName: builtins emit a synchronous event with the
/// current top of stack as parent; package fn returns pop the matching
/// frame entry and finalize with the result.
let private makeStoreFnResult (state : TracerState) : RT.Tracing.StoreFnResult =
  fun (_, name) args result ->
    match name with
    | RT.FQFnName.Builtin _ ->
      state.events.Add(
        { callId = newCallId ()
          parentCallId = currentParentCallId state
          kind = "builtin"
          fnHash = Some(fnNameToSimpleString name)
          lambdaExprId = None
          args = NEList.toList args
          result = result
          // No frame-entry counterpart for builtins, so no real duration.
          durationMs = 0L }
      )
    | RT.FQFnName.Package _ ->
      if state.stack.Count > 0 then
        let partial = state.stack.Pop()
        let endedAt = System.Diagnostics.Stopwatch.GetTimestamp()
        state.events.Add(
          { callId = partial.callId
            parentCallId = partial.parentCallId
            kind = "function"
            fnHash = partial.fnHash
            lambdaExprId = None
            args = partial.args
            result = result
            durationMs = ticksToMs (endedAt - partial.startedAtTicks) }
        )


/// Fired when a Lambda frame returns. Pop the matching entry and finalize.
let private makeStoreLambdaResult
  (state : TracerState)
  : RT.Tracing.StoreLambdaResult =
  fun _ result ->
    if state.stack.Count > 0 then
      let partial = state.stack.Pop()
      let endedAt = System.Diagnostics.Stopwatch.GetTimestamp()
      state.events.Add(
        { callId = partial.callId
          parentCallId = partial.parentCallId
          kind = "lambda"
          fnHash = None
          lambdaExprId = partial.lambdaExprId
          args = partial.args
          result = result
          durationMs = ticksToMs (endedAt - partial.startedAtTicks) }
      )


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

  /// Build a JSON array string from a list of pre-serialized JSON elements.
  /// (Don't double-encode: each element is already valid JSON.)
  let private jsonArrayOf (elements : List<string>) : string =
    "[" + String.concat "," elements + "]"

  let store
    (rootTLID : tlid)
    (traceID : AT.TraceID.T)
    (handlerDesc : string)
    (inputVarName : string)
    (inputDval : RT.Dval)
    (events : List<CompletedEvent>)
    : unit =
    if TraceDetail.current = TraceDetail.Off then
      ()
    else

      let traceIdStr = string traceID
      let timestamp = NodaTime.Instant.now().ToString()
      let traceIdParam = [ "traceId", Sql.string traceIdStr ]

      let inputJson = DvalReprInternalRoundtrippable.toJsonV0 inputDval

      // DELETE-before-INSERT on trace_fn_calls matches INSERT OR REPLACE
      // on traces, so re-running store for a trace_id replaces rather than
      // accumulates. Input is stored inline on the trace row.
      let baseStatements =
        [ "INSERT OR REPLACE INTO traces
          (id, root_tlid, handler_desc, timestamp,
           input_name, input_value_json)
         VALUES
          (@id, @rootTlid, @handlerDesc, @timestamp,
           @inputName, @inputValueJson)",
          [ [ "id", Sql.string traceIdStr
              "rootTlid", Sql.int64 (int64 rootTLID)
              "handlerDesc", Sql.string handlerDesc
              "timestamp", Sql.string timestamp
              "inputName", Sql.string inputVarName
              "inputValueJson", Sql.string inputJson ] ]

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
             lambda_expr_id, args_json, result_json, duration_ms)
           VALUES
            (@traceId, @callId, @parentCallId, @kind, @fnHash,
             @lambdaExprId, @argsJson, @resultJson, @durationMs)",
            events
            |> List.map (fun ev ->
              let argsJson =
                ev.args
                |> List.map DvalReprInternalRoundtrippable.toJsonV0
                |> jsonArrayOf
              let resultJson = DvalReprInternalRoundtrippable.toJsonV0 ev.result
              [ "traceId", Sql.string traceIdStr
                "callId", Sql.string ev.callId
                "parentCallId", Sql.stringOrNone ev.parentCallId
                "kind", Sql.string ev.kind
                "fnHash", Sql.stringOrNone ev.fnHash
                "lambdaExprId",
                (ev.lambdaExprId |> Option.map string |> Sql.stringOrNone)
                "argsJson", Sql.string argsJson
                "resultJson", Sql.string resultJson
                "durationMs", Sql.int64 ev.durationMs ]) ]

      let _ =
        Sql.executeTransactionSync (baseStatements @ eventStmt)
      ()


/// Walk every captured Dval through `Blob.promote` so ephemeral blob refs
/// resolve to persistent ones (writing the bytes into package_blobs in the
/// process). Mutates `state.events` in place; returns the promoted input
/// dval. Without this step, every trace would record blob UUIDs pointing
/// at gone bytes.
let private promoteBlobs
  (exeState : RT.ExecutionState)
  (inputDval : RT.Dval)
  (state : TracerState)
  : Ply.Ply<RT.Dval> =
  uply {
    let persist = exeState.blobs.persist
    let! promotedInput = LibExecution.Blob.promote exeState persist inputDval
    for i in 0 .. state.events.Count - 1 do
      let ev = state.events[i]
      let! promotedArgs =
        ev.args
        |> List.map (fun a -> LibExecution.Blob.promote exeState persist a)
        |> Ply.List.flatten
      let! promotedResult = LibExecution.Blob.promote exeState persist ev.result
      state.events[i] <- { ev with args = promotedArgs; result = promotedResult }

    return promotedInput
  }


/// Shared helper: store a trace to SQLite with error handling. Promotes
/// every Dval through `Blob.promote` first so blob bytes survive the
/// per-request blob scope.
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
    try
      let! promotedInput = promoteBlobs exeState inputDval state
      TraceStorage.store
        rootTLID
        traceID
        handlerDesc
        inputVarName
        promotedInput
        (Seq.toList state.events)
    with ex ->
      print $"[tracing] Failed to store trace: {ex.Message}"
      Telemetry.event
        "trace.storeFailed"
        [ "exception", ex.GetType().FullName; "message", ex.Message ]
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
      { Exe.noTracing with
          storeFrameEntry = makeStoreFrameEntry state
          storeFnResult = makeStoreFnResult state
          storeLambdaResult = makeStoreLambdaResult state
          skipTracing = false }
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
  { enabled = true
    results = results
    executionTracing =
      { Exe.noTracing with
          storeFrameEntry = makeStoreFrameEntry state
          storeFnResult = makeStoreFnResult state
          storeLambdaResult = makeStoreLambdaResult state
          skipTracing = false }
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
  let config = TracingConfig.forHandler rootTLID traceID
  match config with
  | TracingConfig.DoTrace
  | TracingConfig.TraceWithTelemetry -> createSqliteTracer rootTLID traceID
  | TracingConfig.DontTrace -> createNonTracer traceID
