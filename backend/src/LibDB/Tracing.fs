/// The recorder: what the interpreter calls as a run goes, and what is written at the end.
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

/// Whether a run is recorded at all. Two settings, `off` and `on`:
///
///   off   nothing is stored. Nothing to list, resume, fork or read values from.
///   on    the run -- what it was, what it answered, where it stands -- and every impure
///         call in order, with its arguments, its result and how long it took. That log is
///         what a resume answers from, what a fork branches, and what `dark traces show`
///         reads to put a run's values beside your code.
///
/// Off is the default, so nothing is recorded until someone asks for it. Three ways to ask,
/// narrowest first:
///
///   `dark --trace <command>`     this one command (and `--no-trace` for the opposite)
///   `trace.record` in config     this instance, until changed (`dark traces record on`)
///   `DARK_CONFIG_TRACE_DETAIL`   every run under this environment: our dev containers and CI
///
/// A stored setting beats the environment, because the environment here is a container-wide
/// default and the stored one is a decision somebody made in this store.
///
/// What is NOT stored, at any setting: the pure calls. The view re-runs pure code against
/// the recorded impure answers, so a pure value is recomputed rather than kept. Storing them
/// as well cost 300x the bytes (0.59 MB against 0.002 MB for the same ten thousand calls) and
/// bought only a call tree for profiling, which is worth its own feature rather than a third
/// setting here.
module TraceDetail =
  type T =
    | Off
    /// The run and its impure calls (non-empty `callEffects`), each with its ordinal and its
    /// duration: what a resume serves from and what a view answers from
    /// (`docs/processes.md`).
    | On

  /// `Some` for `on` or `off`, `None` for anything else, so a caller can refuse a typo rather
  /// than quietly recording something the person did not ask for.
  let parse (s : string) : Option<T> =
    match s with
    | "off" -> Some Off
    | "on" -> Some On
    | _ -> None

  /// What `traces record` prints back, and what the setting is spelled as everywhere.
  let name (level : T) : string =
    match level with
    | Off -> "off"
    | On -> "on"

  let private readEnv () : T =
    match System.Environment.GetEnvironmentVariable "DARK_CONFIG_TRACE_DETAIL" with
    | null
    | "" -> Off
    | s ->
      // A typo records nothing rather than something, matching the default: recording is
      // opt-in, so an unreadable opt-in has not happened.
      parse s |> Option.defaultValue Off

  let mutable current : T = readEnv ()

  /// Set once `--trace` / `--no-trace` has spoken for this run, so the stored setting read at
  /// startup does not then overwrite it.
  let mutable private pinnedForRun = false

  /// `--trace` / `--no-trace`: this run only, whatever is stored or in the environment.
  let setForRun (level : T) : unit =
    current <- level
    pinnedForRun <- true

  /// The store's `trace.record` as the host read it at startup. Unset or unreadable leaves the
  /// environment's answer standing; `--trace` on the command line beats both.
  let configure (stored : Option<string>) : unit =
    if not pinnedForRun then
      stored |> Option.bind parse |> Option.iter (fun level -> current <- level)

  /// Test seam, and how a session applies a setting it has just stored without waiting for the
  /// next command. Nothing persistent.
  let setForTesting (level : T) : unit = current <- level



/// One run's recorder: the hooks the interpreter calls, and the write at the end.
type T =
  {
    /// The interpreter hooks for this run.
    executionTracing : RT.Tracing.Tracing

    /// Write what was collected. Takes the live `ExecutionState` because an ephemeral blob ref
    /// dies when the request scope pops, so the bytes are promoted to persistent ones before
    /// they are serialized; without that a trace records refs to bytes that are gone and
    /// `traces inspect` cannot reconstruct a request body.
    storeTraceResults : RT.ExecutionState -> Ply.Ply<unit>

    /// Whether this run gets a row of its own. A view does not: looking at a run is not a run.
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


/// One recorded call, ready to write to `trace_fn_calls`.
///
/// Only impure calls are recorded, so every one of these is a builtin, there is no nesting to
/// record, and the row's `parent_call_id`, `lambda_expr_id` and `kind` columns are written flat
/// (`TraceStorage.store`). The log is a SEQUENCE, not a tree, and that is the whole shape.
type CompletedEvent =
  {
    callId : string
    /// The builtin's name, as the reader renders it.
    fnName : string
    args : List<RT.Dval>
    result : RT.Dval
    /// Wall clock for the call. For a read in flight it is measured at the landing, so an HTTP
    /// call carries its round trip.
    durationMs : int64
    /// The process that made the call. `Guid.Empty` for a run nobody scheduled.
    processId : System.Guid
    /// Position in the whole trace, across processes: the order the calls completed in. Reading
    /// them all in `seq` order is the interleaving.
    seq : int64
    /// The call's ordinal among its process's effectful calls, taken when the call was made.
    /// One process's rows in `ord` order are its log, and what a replay keys on.
    ord : int64
    /// The frame this call was made in, which is what points it into the shape.
    frameId : System.Guid
  }


/// One frame a recorded run pushed: what made it, what it runs, and which pass it is.
///
/// Recorded for the shape, not for values: a replay recomputes what every line evaluated to,
/// but it can only reach as far as the log takes it. A run that was suspended mid-loop, or
/// whose replay stops at an effect the log cannot answer, has a shape the replay will never
/// see. This is that shape, written down.
type RecordedFrame =
  { parent : System.Guid
    executionPoint : RT.ExecutionPoint
    /// Which pass this is among its siblings at the same call site.
    pass : int
    /// The order this frame was pushed, across the whole run.
    ord : int
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
    /// Every frame this run pushed, for the shape. Pruned at store time to the ones that are
    /// ancestors of a recorded call, which is what keeps it bounded: a pure helper called in a
    /// tight loop pushes frames nobody will ever ask about.
    frames : System.Collections.Generic.Dictionary<System.Guid, RecordedFrame>
    /// (parent frame, call site) -> how many frames have been seen there, for `pass`.
    frameSites : System.Collections.Generic.Dictionary<struct (System.Guid * int64), int>
    mutable nextFrameOrd : int
    /// Processes whose replay has ended: the log had no answer for an ordinal they asked for,
    /// so they are live from there and nothing later in the log may be handed to them (a fork
    /// cut by position can leave a later ordinal without its earlier ones).
    replayEnded : System.Collections.Generic.HashSet<System.Guid>
    /// Wall clock for the whole run, started when the tracer was made. What `traces inspect` prints
    /// as `took`, and the only honest source for it: the row's `timestamp` and `updated` are
    /// both the store instant for a served request, and on a resumed run they span however long
    /// it sat suspended. A resumed run's clock is its own, replay included.
    elapsed : System.Diagnostics.Stopwatch
    sync : obj
  }


let private newState () : TracerState =
  { events = System.Collections.Generic.List<CompletedEvent>()
    fns = System.Collections.Generic.HashSet<RT.Hash>()
    dropped = 0
    nextSeq = 0L
    ordinals = System.Collections.Generic.Dictionary()
    replay = System.Collections.Generic.Dictionary()
    replayEnded = System.Collections.Generic.HashSet()
    frames = System.Collections.Generic.Dictionary()
    frameSites = System.Collections.Generic.Dictionary()
    nextFrameOrd = 0
    elapsed = System.Diagnostics.Stopwatch.StartNew()
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


/// Keep an event unless we are at the cap. Under `sync`; `seq` is assigned here, so the order of
/// `seq` is the order of completion across every process writing this trace.
let private addEvent (state : TracerState) (ev : CompletedEvent) : unit =
  if TraceLimits.maxEvents = 0 || state.events.Count < TraceLimits.maxEvents then
    let seq = state.nextSeq
    state.nextSeq <- seq + 1L
    state.events.Add { ev with seq = seq }
  else
    state.dropped <- state.dropped + 1


let private newCallId () : string = string (System.Guid.NewGuid())


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


/// Fired when a call completes. Only impure calls reach here -- the interpreter decides, by
/// `ord >= 0` (`Interpreter.recordsCall`) -- so a package frame's return is never recorded and
/// the `ord = -1` case is ignored.
let private makeStoreFnResult
  (state : TracerState)
  (pid : System.Guid)
  : RT.Tracing.StoreFnResult =
  fun (_, name) meta args result ->
    if meta.ord >= 0L then
      let simpleName = fnNameToSimpleString name
      lock state.sync (fun () ->
        addEvent
          state
          { callId = newCallId ()
            fnName = simpleName
            args = Redact.args simpleName (NEList.toList args)
            result = Redact.result simpleName result
            durationMs = meta.durationMs
            processId = pid
            seq = 0L
            ord = meta.ord
            frameId = meta.frameId })


/// The interpreter hooks for one process writing this trace. `forProcess` hands a spawned process
/// its own; they share the event list and get their own ordinals.
///
/// `recordAllCalls` stays FALSE even while recording, which is what keeps the interpreter's fast
/// paths out of a recorded trace: a pure value is recomputed by a replay rather than stored.
///
/// `collectFrames` is TRUE, and costs the fast paths nothing: a lambda application and a package
/// call push a real frame either way, and the only frames a shortcut skips are elided operator
/// wrappers, which no reader wants a frame for. What it buys is the SHAPE, which a replay cannot
/// always recover -- a run suspended mid-loop has passes the replay will never reach. Pruned at
/// store time to the frames a recorded call actually sits under.
let rec private executionTracingFor
  (state : TracerState)
  (pid : System.Guid)
  : RT.Tracing.Tracing =
  { Exe.noTracing with
      storeFnResult = makeStoreFnResult state pid
      noteFunction =
        (fun hash -> lock state.sync (fun () -> state.fns.Add hash |> ignore<bool>))
      collectExprValues = false
      collectFrames = true
      recordAllCalls = false
      traceEffects = true
      storeFrameEntry =
        (fun frameId parentId ep _args ->
          lock state.sync (fun () ->
            // The site key is the lambda's own expression id, or the callee's hash. NOT the
            // execution point formatted as a string: a lambda's execution point carries its
            // parent, so formatting one walks the whole chain, once per frame.
            let siteKey =
              match ep with
              | RT.ExecutionPoint.Source -> 0L
              | RT.ExecutionPoint.Lambda(_, lambdaExprId) -> int64 lambdaExprId
              | RT.ExecutionPoint.Function name -> int64 (hash name)
            let site = struct (parentId, siteKey)
            let mutable seen = 0
            state.frameSites.TryGetValue(site, &seen) |> ignore<bool>
            state.frameSites[site] <- seen + 1
            let ord = state.nextFrameOrd
            state.nextFrameOrd <- ord + 1
            state.frames[frameId] <-
              { parent = parentId; executionPoint = ep; pass = seen; ord = ord }))
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
      forProcess = executionTracingFor state }


/// Keeps the trace tables bounded: after a store, the oldest traces past the caps go, except
/// one a RUNNING, suspended or pinned run needs (its log is what `resume` replays, and a
/// running one is still being written) and the newest run of each entry, which the count cap
/// alone never drops. `trace.keep` is
/// how many traces to keep (200 unset; a served request is one), `trace.maxMb` how many
/// megabytes of args and results (256 unset); 0 disables a cap. Both are store config keys the
/// host reads at startup (`configure`). A pass runs at most every ten seconds, since a `serve`
/// stores a trace per request. The manual `traces prune|delete|clear` go through here too, so
/// a run and its log go together, always.
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
  /// one a running, suspended or pinned run needs, never the newest over the byte cap alone, and
  /// never the newest run of an entry over the COUNT cap alone. Returns how many went.
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
                (t.status IN ('running', 'suspended') OR t.pinned = 1) AS needed,
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
    //
    // A run that cannot be dropped counts toward the COUNT cap -- `trace.keep` is a promise
    // about how many runs are here -- but not toward the BYTE cap. Counting its bytes would
    // let a handful of big pinned runs fill `trace.maxMb` on their own and evict every
    // ordinary run behind them, forever, which is the opposite of what pinning one asks for.
    let mutable seenCount = 0L
    let mutable seenBytes = 0L
    let doomed =
      rows
      |> List.filter (fun (_, bytes', needed, newestOfEntry) ->
        seenCount <- seenCount + 1L
        if not needed then seenBytes <- seenBytes + bytes'
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
    (traceID : AT.TraceID.T)
    (handlerDesc : string)
    (inputVarName : string)
    (inputDval : RT.Dval)
    (events : List<CompletedEvent>)
    (fns : List<RT.Hash>)
    /// Every frame the run pushed. Pruned here to the ones a recorded call sits under.
    (frames : System.Collections.Generic.Dictionary<System.Guid, RecordedFrame>)
    (accountID : Option<System.Guid>)
    /// Wall clock for the whole run, from the tracer's own stopwatch.
    (durationMs : int64)
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
           input_name, input_value, account_id, status, updated, duration_ms)
         VALUES
          (@id, 0, @handlerDesc, @timestamp,
           @inputName, @inputValue, @accountId, 'done', @timestamp, @durationMs)
         ON CONFLICT(id) DO UPDATE SET
           handler_desc = excluded.handler_desc,
           input_name = excluded.input_name,
           input_value = excluded.input_value,
           account_id = excluded.account_id,
           updated = excluded.updated,
           duration_ms = excluded.duration_ms",
          [ [ "id", Sql.string traceIdStr
              "handlerDesc", Sql.string handlerDesc
              "timestamp", Sql.string timestamp
              "inputName", Sql.string inputVarName
              "inputValue", Sql.bytes inputBytes
              "accountId", accountIDSql
              "durationMs", Sql.int64 durationMs ] ]

          "DELETE FROM trace_fn_calls WHERE trace_id = @traceId", [ traceIdParam ] ]

      // Skip the events INSERT when empty: fumble rejects zero-param-row
      // prepared statements, hit when a trace errors before any call fires.
      // The DELETE above still runs.
      let eventStmt =
        match events with
        | [] -> []
        | _ ->
          // `parent_call_id` and `lambda_expr_id` stay NULL and `kind` stays 'builtin': the log
          // is a sequence of impure calls, and the tree they sit in is `trace_frames`, which
          // `frame_id` points into. The two old columns stay because `08-traces.sql` has merged
          // and is frozen.
          [ "INSERT INTO trace_fn_calls
            (trace_id, call_id, parent_call_id, kind, fn_hash,
             lambda_expr_id, args, result, duration_ms, process_id, seq, ord, frame_id)
           VALUES
            (@traceId, @callId, NULL, 'builtin', @fnHash,
             NULL, @args, @result, @durationMs, @processId, @seq, @ord, @frameId)",
            events
            |> List.map (fun ev ->
              let argsBytes = serializeArgs ev.args
              let resultBytes = serializeDval "trace_fn_calls.result" ev.result
              [ "traceId", Sql.string traceIdStr
                "callId", Sql.string ev.callId
                "fnHash", Sql.string ev.fnName
                "args", Sql.bytes argsBytes
                "result", Sql.bytes resultBytes
                "durationMs", Sql.int64 ev.durationMs
                "processId",
                (if ev.processId = System.Guid.Empty then
                   Sql.string ""
                 else
                   Sql.string (string ev.processId))
                "seq", Sql.int64 ev.seq
                "ord", Sql.int64 ev.ord
                "frameId",
                (if ev.frameId = System.Guid.Empty then
                   Sql.dbnull
                 else
                   Sql.string (string ev.frameId)) ]) ]

      // THE SHAPE, pruned to the frames that matter.
      //
      // A run pushes a frame for every package call and every lambda application, which for
      // anything with a loop in it is thousands. Almost none of them will ever be asked about:
      // what a reader opens is the frame a recorded call sits in, and the frames between that
      // and the entry. So the walk goes UP from each recorded call, marking ancestors, and
      // everything unmarked is dropped.
      //
      // That is what makes the shape cost a fraction of the log rather than a multiple of it.
      // `fib 20` pushes twenty-two thousand frames and records none, so it stores none.
      let keptFrames =
        let wanted = System.Collections.Generic.Dictionary<System.Guid, RecordedFrame>()
        let rec walkUp (id : System.Guid) =
          if id <> System.Guid.Empty && not (wanted.ContainsKey id) then
            match frames.TryGetValue id with
            | true, f ->
              wanted[id] <- f
              // A root frame is its own parent, which is how the interpreter starts. Following
              // that would not terminate.
              if f.parent <> id then walkUp f.parent
            | false, _ -> ()
        for ev in events do
          walkUp ev.frameId
        wanted |> Seq.map (fun kv -> (kv.Key, kv.Value)) |> List.ofSeq

      let keptIds =
        keptFrames |> List.map fst |> System.Collections.Generic.HashSet

      let frameStmt =
        match keptFrames with
        | [] -> []
        | _ ->
          [ "INSERT OR REPLACE INTO trace_frames
              (trace_id, frame_id, parent_frame_id, kind, call_site, fn_hash, pass, ord)
             VALUES
              (@traceId, @frameId, @parent, @kind, @callSite, @fnHash, @pass, @ord)",
            keptFrames
            |> List.map (fun (id, f) ->
              let kind, callSite, fnHash =
                match f.executionPoint with
                | RT.ExecutionPoint.Source -> "source", Sql.dbnull, Sql.dbnull
                | RT.ExecutionPoint.Lambda(_, lambdaExprId) ->
                  "lambda", Sql.string (string lambdaExprId), Sql.dbnull
                | RT.ExecutionPoint.Function name ->
                  let h =
                    match name with
                    | RT.FQFnName.Package hash -> string hash
                    | RT.FQFnName.Builtin b -> b.name
                  "function", Sql.dbnull, Sql.string h
              [ "traceId", Sql.string traceIdStr
                "frameId", Sql.string (string id)
                "parent",
                // NULL when the parent is not itself a kept frame: the VM's initial frame is
                // never pushed through the hook, so the outermost recorded frame would
                // otherwise point at a row that does not exist. A dangling parent reads as a
                // tree with a missing node rather than as the root, which is worse than no
                // pointer at all.
                (if f.parent = id
                    || f.parent = System.Guid.Empty
                    || not (keptIds.Contains f.parent) then
                   Sql.dbnull
                 else
                   Sql.string (string f.parent))
                "kind", Sql.string kind
                "callSite", callSite
                "fnHash", fnHash
                "pass", Sql.int64 (int64 f.pass)
                "ord", Sql.int64 (int64 f.ord) ]) ]

      // Which functions the run went through (`trace_fns`), by name AND by the hash the run
      // actually went through. `INSERT OR REPLACE`, not IGNORE: a resume rewrites its trace in
      // place, and its live half may have gone through a newer hash for a name already there.
      let fnStmt =
        match fns with
        | [] -> []
        | _ ->
          [ "INSERT OR REPLACE INTO trace_fns (trace_id, fn_name, fn_hash)
             VALUES (@traceId, @fnName, @fnHash)",
            fns
            |> List.map (fun hash ->
              [ "traceId", Sql.string traceIdStr
                "fnName",
                Sql.string (fnNameToSimpleString (RT.FQFnName.Package hash))
                "fnHash", Sql.string (string hash) ]) ]

      let _ =
        Sql.executeTransactionSync (baseStatements @ eventStmt @ fnStmt @ frameStmt)
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
      let struct (events, dropped, nextSeq, fns, frames) =
        lock state.sync (fun () ->
          struct (state.events.ToArray(),
                  state.dropped,
                  state.nextSeq,
                  List.ofSeq state.fns,
                  System.Collections.Generic.Dictionary(state.frames)))
      if dropped > 0 then
        Telemetry.event
          "trace.truncated"
          [ "kept", string events.Length; "dropped", string dropped ]
      try
        let! preparedInput = prepareTraceForStorage exeState inputDval events
        TraceStorage.store
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
                   fnName =
                     $"trace truncated: {dropped} further calls not recorded (cap {TraceLimits.maxEvents}, raise with DARK_CONFIG_TRACE_MAX_EVENTS)"
                   args = []
                   result = RT.DUnit
                   durationMs = 0L
                   processId = System.Guid.Empty
                   seq = nextSeq
                   ord = -1L
                   frameId = System.Guid.Empty } ]
           else
             List.ofArray events)
          fns
          frames
          exeState.accountID
          state.elapsed.ElapsedMilliseconds
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


let createCliTracer
  (traceID : AT.TraceID.T)
  (description : string)
  (inputVarName : string)
  (inputDval : RT.Dval)
  : T =
  let state = newState ()

  // With detail off, collect nothing. `storeTrace` refuses to write in that case, so installing the
  // hooks anyway means building an event per frame and holding every argument and result alive for the
  // whole run, to discard all of it at the end.
  //
  // `Exe.noTracing` leaves `recordAllCalls` false, which also lets the interpreter skip its own per-frame
  // bookkeeping (`pendingCallArgs`) rather than just calling no-op hooks.
  if TraceDetail.current = TraceDetail.Off then
    { enabled = false
      executionTracing = Exe.noTracing
      storeTraceResults = fun _ -> uply { return () } }
  else
    { enabled = true
      executionTracing = executionTracingFor state System.Guid.Empty
      storeTraceResults =
        fun exeState ->
          storeTrace traceID description inputVarName inputDval state exeState }


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
    { executionTracingFor state pid with forProcess = tracingFor }
  { enabled = true
    // The root's own hooks (the CLI's process, which makes no effectful calls of its own in a
    // script; the expressions are child processes and go through `forProcess`).
    executionTracing =
      { executionTracingFor state System.Guid.Empty with forProcess = tracingFor }
    storeTraceResults =
      fun exeState ->
        storeTrace traceID description inputVarName inputDval state exeState }


/// One frame the replay walked: what made it, what it runs, and which pass it is at that call
/// site. `valuesKept` is false past the cap, where a frame is counted but its values are not
/// held, so the view can still say how many passes there were.
type ViewFrame =
  { parent : System.Guid
    executionPoint : RT.ExecutionPoint
    pass : int
    /// The order this frame was pushed, across the whole view.
    ///
    /// `pass` counts within ONE call site, so it cannot order frames at different sites: three
    /// calls to the same function from three passes of a loop are each `pass = 0` at their own
    /// site. And the frames come back from a dictionary, whose iteration order is not a
    /// promise. This is the one thing that says what happened first.
    ord : int
    args : List<RT.Dval>
    valuesKept : bool }


/// A tracer for VIEWING a run: every effectful call is answered by its name and arguments from
/// that run's log, none is performed, and every expression's value is collected on the way.
/// Classic called this Preview. Nothing is written: a view is not itself a run.
///
/// The key is `(name, arguments)` rather than the `(process, ordinal)` a resume uses, and for
/// the reason classic had: a view has to survive the code having moved on. Add a call in the
/// middle and every ordinal after it shifts, so an ordinal-keyed view would go blank from
/// there; a name-and-arguments key still answers every call you did not touch. Last write wins,
/// as classic's `DISTINCT ON ... ORDER BY timestamp DESC` did.
///
/// `collected` is where the values land, keyed by (frame, source expression id). `frames` is
/// the frame tree the replay walked, which is what turns those values from a flat bag into
/// something a reader can navigate: a loop's passes are the sibling frames that share a parent
/// and a lambda id, in the order they ran.
///
/// `lastByExpr` is the flat view every current consumer still wants: one value per expression,
/// the last one executed. It is kept as the run goes rather than derived from `collected`
/// afterwards, because a dictionary does not iterate in insertion order and "the last pass" is
/// exactly what would be lost.
let createViewTracer
  (rows : List<string * byte[] * RT.Dval>)
  (collected :
    System.Collections.Generic.Dictionary<struct (System.Guid * int64), RT.Dval>)
  (frames : System.Collections.Generic.Dictionary<System.Guid, ViewFrame>)
  (lastByExpr : System.Collections.Generic.Dictionary<int64, RT.Dval>)
  : T =
  // Every recorded result for a key, in the order it was recorded, rather than just the last.
  //
  // The key is (name, arguments), which is classic's and survives the code moving. What it does
  // NOT do on its own is tell two identical calls apart: `Uuid.generate ()` called twice has
  // one key and two answers, and last-write-wins served the second one to both callers. A run
  // that made ten different uuids showed one value, ten times, with no error and no marker.
  //
  // So the answers are a queue per key and a lookup CONSUMES one. The n-th call gets the n-th
  // recorded result, which is what a reader is looking at.
  //
  // When the queue runs out the last recorded value is served again. That is the old behaviour,
  // kept deliberately: replaying against code that now calls something more times than the
  // recording did should degrade to a repeated value rather than stop the whole view.
  let answers =
    System.Collections.Generic.Dictionary<
      string,
      System.Collections.Generic.Queue<RT.Dval>
      >()
  let lastAnswer = System.Collections.Generic.Dictionary<string, RT.Dval>()
  for (name, argsBytes, result) in rows do
    let key = name + "\u0000" + System.Convert.ToBase64String argsBytes
    let mutable q = Unchecked.defaultof<System.Collections.Generic.Queue<RT.Dval>>
    if not (answers.TryGetValue(key, &q)) then
      q <- System.Collections.Generic.Queue<RT.Dval>()
      answers[key] <- q
    q.Enqueue result
    lastAnswer[key] <- result

  // The queues are consumed, so two processes of a viewed run must not race on them.
  let answersGate = obj ()

  let lookup (name : string) (args : RT.Dval[]) : RT.Tracing.ReplayStep voption =
    if Set.contains name Redact.performAgain then
      // Serving these is what `performAgain` exists to prevent: a spawn's recorded result is a
      // handle to a process that no longer exists, so serving it makes the next `await` fail.
      // They are made again instead. A spawned child inherits this tracer, so it is viewed too
      // and nothing it does reaches the world either.
      ValueSome RT.Tracing.ReplayStep.PerformOnce
    else
      // The same shape the recorder wrote: one `DList` blob of the arguments.
      let argsBytes =
        BinarySer.RT.Dval.serialize
          "trace_fn_calls.args"
          (RT.DList(LibExecution.ValueType.unknownTODO, List.ofArray args))
      let key = name + "\u0000" + System.Convert.ToBase64String argsBytes
      lock answersGate (fun () ->
        let mutable q = Unchecked.defaultof<System.Collections.Generic.Queue<RT.Dval>>
        if answers.TryGetValue(key, &q) && q.Count > 0 then
          ValueSome(RT.Tracing.ReplayStep.Serve(q.Dequeue()))
        else
          // Either the code now makes this call more often than the recording did, or it never
          // made it at all. The first degrades to the last recorded value; the second has no
          // answer and the view stops there and says which call stopped it.
          let mutable last = Unchecked.defaultof<RT.Dval>
          if lastAnswer.TryGetValue(key, &last) then
            ValueSome(RT.Tracing.ReplayStep.Serve last)
          else
            ValueNone)

  // How many passes of one loop, or calls at one call site, keep their values.
  //
  // Without a cap the values are back to scaling with the RUN rather than with what can be
  // shown: `fib 20` pushes about twenty-two thousand frames, and keeping every one of them
  // costs what appending every expression execution used to cost. Twenty is the threshold the
  // view summarises at anyway, so past it a frame is counted and its values are not held.
  //
  // Reaching a pass past the cap is a second replay asking for that one pass, which is cheap
  // now: opening a trace of `fib 20` is 95ms, so a targeted re-run for pass 147 is not a wait.
  // That is only affordable because the replay got fast; it would not have been before.
  let passCap = 20

  // A cheap identity for the CALL SITE a frame belongs to, for counting passes.
  //
  // This was `string ep` for one measurement, and that alone cost 17x on a view of `fib 20`:
  // an `ExecutionPoint.Lambda` carries its parent, so formatting one walks and allocates the
  // whole chain, once per frame, twenty-two thousand times. A lambda's own expression id and a
  // function's hash are already unique per site and are plain values.
  let siteKey (ep : RT.ExecutionPoint) : int64 =
    match ep with
    | RT.ExecutionPoint.Source -> 0L
    | RT.ExecutionPoint.Lambda(_, lambdaExprId) -> int64 lambdaExprId
    // A structural hash, so two different functions called from ONE frame could in principle
    // collide and share a pass counter. The effect would be a wrong pass number on a call that
    // is not a loop, where the number is not shown; the frame ids stay distinct either way.
    | RT.ExecutionPoint.Function name -> int64 (hash name)

  // (parent frame, which call site) -> how many frames have been seen there
  let siteCounts =
    System.Collections.Generic.Dictionary<struct (System.Guid * int64), int>()

  let mutable frameOrd = 0

  // A scheduler is one thread, but a worker group is one per core, and every process of a
  // viewed run shares these three maps: `forProcess` hands each child a tracer that closes
  // over the same ones, which is how a spawned child's values reach the same view.
  //
  // So a viewed `parallelMap` writes to them from several threads at once. A plain
  // Dictionary corrupts under that -- a concurrent resize can spin forever, not merely lose a
  // write -- and the pass counter is a read-modify-write that has to be atomic or two passes
  // take the same number. The lock is held for a dictionary write, nowhere near the interpreter
  // loop, and it costs nothing measurable against what a view does per expression.
  let gate = obj ()

  let noteFrame
    (frameId : System.Guid)
    (parentId : System.Guid)
    (ep : RT.ExecutionPoint)
    (args : List<RT.Dval>)
    : unit =
    let site = struct (parentId, siteKey ep)
    lock gate (fun () ->
      let mutable seen = 0
      siteCounts.TryGetValue(site, &seen) |> ignore<bool>
      siteCounts[site] <- seen + 1
      // `pass` is the ordinal among siblings at this call site, which is what a loop's passes
      // are numbered by. Kept on every frame, including those past the cap, so the view can
      // say "200 passes" truthfully while holding twenty of them.
      let ord = frameOrd
      frameOrd <- frameOrd + 1

      frames[frameId] <-
        { parent = parentId
          executionPoint = ep
          pass = seen
          ord = ord
          args = (if seen < passCap then args else [])
          valuesKept = seen < passCap })

  // Every process of the run is viewed, not just the first. The CLI spawns each expression as a
  // process of its own, and the scheduler asks the tracer for that process's own hooks
  // (`forProcess`); handing back the default there is handing back a tracer that performs
  // effects for real, which is the one thing a view must never do.
  let rec viewTracing () : RT.Tracing.Tracing =
    { Exe.noTracing with
        // Values, not calls. The fast paths stay on: a view reads the value a call left in
        // its register, which the shortcut writes just as the long way round does.
        collectExprValues = true
        collectFrames = true
        recordAllCalls = false
        traceEffects = false
        // Keyed by (frame, expression). The expression id alone is not unique within a run: a
        // loop body writes the same id once per pass, so keying on it alone kept the last pass
        // and lost the other hundred and ninety-nine.
        //
        // It is still bounded by what can be shown rather than by what ran, which is the
        // property that made opening a trace stop scaling with compute. The cap is what keeps
        // it: past `passCap` a frame is counted and its values are dropped.
        storeExprResult =
          fun exprId frameId dv ->
            lock gate (fun () ->
              // Last EXECUTED wins, which is what every consumer had when the key was the
              // expression id alone and the values arrived as a list in execution order. A
              // dictionary does not iterate in insertion order, so flattening `collected`
              // afterwards would hand back an arbitrary pass instead of the last one. Kept
              // here, where the order is still known.
              lastByExpr[int64 exprId] <- dv
              let mutable f = Unchecked.defaultof<ViewFrame>
              if not (frames.TryGetValue(frameId, &f)) || f.valuesKept then
                collected[struct (frameId, int64 exprId)] <- dv)
        storeFrameEntry = noteFrame
        viewEffect = Some lookup
        forProcess = fun _ -> viewTracing () }

  // `enabled = false` is doing real work: it is what stops the host giving this run a row of
  // its own, and what makes the store a no-op.
  { enabled = false
    storeTraceResults = fun _ -> uply { return () }
    executionTracing = viewTracing () }
