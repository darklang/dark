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

    /// Write what was collected, under the run's account (from the live `ExecutionState`). An
    /// ephemeral blob ref dies when the request scope pops, so its bytes are kept with the trace
    /// before anything is serialized; without that a trace records refs to bytes that are gone
    /// and `traces inspect` cannot reconstruct a request body.
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
          printErr $"[tracing] FnNameCache failed to resolve {hash}: {ex.Message}"
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
  | RT.FQFnName.TraitMethod { trait_ = RT.Hash t; method_ = m; implFn = _ } ->
    $"{FnNameCache.resolve t}.{m}"


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
/// What the log recorded at one step of a run: which builtin, with what arguments. A resume
/// checks the call it is about to answer against it, so a program that has changed since the
/// run is not handed the old program's answers. `args` is None when the row's arguments could
/// not be read back; the builtin's name is still checked.
type RecordedCall = { builtin : string; args : Option<List<RT.Dval>> }


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
    /// How many times each loop went round, by the lambda's own expression id.
    ///
    /// A COUNT, not a tree. The only question anyone asks of a recorded run's shape is how many
    /// passes a loop had, so that a view which could not reach them all can still say "pass 3
    /// of 5" instead of "pass 3 of 3". Storing a row per frame to answer it wrote 8,523 rows
    /// for a 2000-pass loop.
    loopPasses : System.Collections.Generic.Dictionary<int64, int>

    /// Processes whose replay has ended: the log had no answer for an ordinal they asked for,
    /// so they are live from there and nothing later in the log may be handed to them (a fork
    /// cut by position can leave a later ordinal without its earlier ones).
    replayEnded : System.Collections.Generic.HashSet<System.Guid>
    /// What the log recorded at each step a replay answers, by the same key as `replay`.
    expected :
      System.Collections.Generic.Dictionary<struct (System.Guid * int64), RecordedCall>
    /// The run being resumed, for the message a refusal prints. Empty for a fresh run.
    mutable resuming : string
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
    expected = System.Collections.Generic.Dictionary()
    resuming = ""
    loopPasses = System.Collections.Generic.Dictionary()
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

  /// Reads of a package item by its content hash. The answer is the item at that hash, and a
  /// traced type check reads the same items over and over: on one dev store these five were
  /// 571 MB of 795 MB of logged results, 20 MB of it distinct per trace. So each distinct
  /// answer is serialized once (`TraceStorage`), and a replay gets exactly what the run got,
  /// whether or not the store still holds the item.
  let fromTheStore : Set<string> =
    Set.ofList
      [ "pmGetFn"; "pmGetType"; "pmGetValue"; "pmGetTrait"; "pmGetTraitImpl" ]

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
  fun name meta args result ->
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
            ord = meta.ord })


/// The interpreter hooks for one process writing this trace. `forProcess` hands a spawned process
/// its own; they share the event list and get their own ordinals.
///
/// `collectFrames` is TRUE, and costs the fast paths nothing: a lambda application and a package
/// call push a real frame either way, and the only frames a shortcut skips are elided operator
/// wrappers, which no reader wants a frame for. What it buys is the SHAPE, which a replay cannot
/// always recover -- a run suspended mid-loop has passes the replay will never reach. Pruned at
/// store time to the frames a recorded call actually sits under.
/// Whether the call a resume is about to answer is the call the log recorded at that step.
module ReplayCheck =
  /// The arguments as the recorder would have stored them, so the two sides compare like for
  /// like: redacted the same way, an ephemeral blob by the hash its stored form names (nothing
  /// is persisted), a stream as its stub. A function value compares equal to any other: what
  /// it closes over is not something the log can be held to.
  let private asStored (builtin : string) (args : List<RT.Dval>) : List<RT.Dval> =
    let leaf (dv : RT.Dval) : Ply.Ply<RT.Dval option> =
      uply {
        match dv with
        | RT.DBlob(RT.Ephemeral eph) ->
          let n : int64 = System.Convert.ToInt64 eph.bytes.Length
          return Some(RT.DBlob(RT.Persistent(Blob.sha256Hex eph.bytes, n)))
        | RT.DStream(impl, _, _) -> return Some(RTToDT.Dval.streamStubDT impl)
        | RT.DApplicable _ -> return Some RT.DUnit
        | _ -> return None
      }
    Redact.args builtin args
    |> List.map (fun dv ->
      match Ply.trySync (RT.Dval.rewriteWith leaf dv) with
      | ValueSome dv -> dv
      // Every arm above returns without waiting, so this cannot happen; if it ever does, the
      // argument is kept as it was rather than guessed at, and may then fail to match.
      | ValueNone -> dv)

  let private brief (dv : RT.Dval) : string =
    let s =
      match dv with
      | RT.DString s -> $"\"{s}\""
      | other -> string other
    if s.Length > 80 then s.Substring(0, 77) + "..." else s

  /// None when the call is the one recorded; otherwise what differs, for the refusal.
  let difference
    (recorded : RecordedCall)
    (builtin : string)
    (args : RT.Dval[])
    : Option<string> =
    if recorded.builtin <> builtin then
      Some
        $"this program calls `{builtin}` where the run called `{recorded.builtin}`"
    else
      match recorded.args with
      | None -> None
      | Some recordedArgs ->
        let now = asStored builtin (List.ofArray args)
        let was = asStored builtin recordedArgs
        if List.length now <> List.length was then
          Some(
            $"this program calls `{builtin}` with {List.length now} arguments where the run "
            + $"gave it {List.length was}"
          )
        else
          let differing =
            List.zip was now
            |> List.indexed
            |> List.filter (fun (_, (w, n)) -> not (LibExecution.Dval.equals w n))
          match differing with
          | [] -> None
          | _ ->
            let lines =
              differing
              |> List.map (fun (i, (w, n)) ->
                $"argument {i + 1} was {brief w}, now {brief n}")
              |> String.concat "; "
            Some $"this program calls `{builtin}` differently from the run: {lines}"


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
      traceEffects = true
      // Count the lambda applications, and nothing else.
      //
      // Every pass of every loop, including the ones that made no impure call. That last part
      // is the point: the old shape kept only frames an effectful call sat under, so a pure
      // loop recorded nothing, its count came back zero, and a view silently reported "pass 3
      // of 3" about a loop that went round five times.
      storeFrameEntry =
        (fun _frameId _parentId ep _args ->
          match ep with
          | RT.ExecutionPoint.Lambda(_, lambdaExprId) ->
            let key = int64 lambdaExprId
            lock state.sync (fun () ->
              let mutable seen = 0
              state.loopPasses.TryGetValue(key, &seen) |> ignore<bool>
              state.loopPasses[key] <- seen + 1)
          | _ -> ())
      nextEffect = (fun () -> lock state.sync (fun () -> nextOrdinal state pid))
      replayEffect =
        (fun ord builtin args ->
          lock state.sync (fun () ->
            if state.replayEnded.Contains pid then
              RT.Tracing.ReplayStep.PerformOnwards
            else
              match state.replay.TryGetValue(struct (pid, ord)) with
              | true, answer ->
                // The log's answer belongs to the call the old program made at this step. A
                // program edited since may make a different one here; answering it from the log
                // would report an effect that never happened as done.
                let difference =
                  match state.expected.TryGetValue(struct (pid, ord)) with
                  | true, recorded -> ReplayCheck.difference recorded builtin args
                  | false, _ -> None
                match difference with
                | None -> answer
                | Some what ->
                  RT.Tracing.ReplayStep.Diverged(
                    $"{what}. The log answers what that program did, not this one, so the "
                    + "run is left as it was."
                    + (if state.resuming = "" then
                         ""
                       else
                         $" `dark traces rerun {state.resuming}` runs it again from the start, "
                         + "performing every effect again, the ones already done included.")
                  )
              | false, _ ->
                state.replayEnded.Add pid |> ignore<bool>
                RT.Tracing.ReplayStep.PerformOnwards))
      forProcess = executionTracingFor state }


/// Keeps the trace tables bounded: after a store, the oldest traces past the caps go, except
/// one a RUNNING, suspended or pinned run needs (its log is what `resume` replays, and a
/// running one is still being written) and the newest run of each of the most recent entries,
/// which the count cap alone never drops. `trace.keep` is how many traces to keep (200 unset;
/// a served request is one), `trace.maxMb` how many megabytes they may hold, input, logged
/// calls and captured blobs together (256 unset); 0 disables a cap. Both are store config keys the
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
          // Added with the table. Every other per-trace table is dropped here, and a loop row
          // outlives its trace with nothing that can ever read it again.
          "DELETE FROM trace_loops WHERE trace_id = @id", ps
          "DELETE FROM trace_blobs WHERE trace_id = @id", ps
          "DELETE FROM traces WHERE id = @id", ps ]
      |> ignore<List<int>>

  /// How many entries the floor protects. Without a bound the floor grows with every entry
  /// there has ever been, and an entry is a concrete path: `keep = 20` kept 120 traces after a
  /// server answered 100 distinct paths, every one of them the newest of its own entry.
  let floorEntries = 20

  /// Drop the oldest traces past `keepTraces` and `bytes` (`None`: no cap on that axis), never
  /// one a running, suspended or pinned run needs, never the newest over the byte cap alone, and
  /// never the newest run of an entry over the COUNT cap alone. Returns how many went.
  ///
  /// The last of those is the floor classic had as "the last 10 traces per route": without it,
  /// a `serve` under load writes a trace per request and evicts the `eval` you were working on
  /// within `trace.keep` requests. The entry is the trace's `handler_desc` (`eval`,
  /// `run <file>`, `GET /path`) with any query string cut off, so `/search?q=a` and
  /// `/search?q=b` are one entry rather than two. A path is still an entry of its own, so the
  /// floor covers only the `floorEntries` most recent entries. The BYTE cap still applies to a
  /// floored trace: one huge recording should not be kept forever because it is the newest of
  /// its kind.
  let prune (keepTraces : Option<int64>) (bytes : Option<int64>) : int =
    // Newest first, with each trace's byte weight (its own `bytes` column), whether it is
    // suspended or pinned, and its entry.
    let rows =
      Sql.query
        "SELECT t.id AS id, t.bytes AS bytes,
                (t.status IN ('running', 'suspended') OR t.pinned = 1) AS needed,
                CASE
                  WHEN INSTR(t.handler_desc, '?') > 0
                  THEN SUBSTR(t.handler_desc, 1, INSTR(t.handler_desc, '?') - 1)
                  ELSE t.handler_desc
                END AS entry
         FROM traces t ORDER BY t.timestamp DESC, t.rowid DESC"
      |> Sql.executeAsync (fun read ->
        read.string "id",
        read.int64 "bytes",
        read.int "needed" = 1,
        read.string "entry")
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
    let entries = System.Collections.Generic.HashSet<string>()
    let doomed =
      rows
      |> List.filter (fun (_, bytes', needed, entry) ->
        seenCount <- seenCount + 1L
        if not needed then seenBytes <- seenBytes + bytes'
        let newestOfEntry = entries.Count < floorEntries && entries.Add entry
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

  /// The pass after a store: at most every ten seconds, and only when a cap is exceeded. Both
  /// caps are checked from one read of `traces`, whatever the trace count; an early version
  /// skipped the byte check below 50 traces, so twelve traces held 790 MB under a 256 MB cap.
  let run () : int =
    if keep = 0L && maxBytes = 0L then
      0
    else
      let now = System.DateTime.UtcNow
      if (now - lastPass).TotalSeconds < 10.0 then
        0
      else
        let (count, total) =
          Sql.query "SELECT COUNT(*) AS n, COALESCE(SUM(bytes), 0) AS b FROM traces"
          |> Sql.executeRowAsync (fun read -> read.int64 "n", read.int64 "b")
          |> fun t -> t.Result
        let overCount = keep > 0L && count > keep
        let overBytes = maxBytes > 0L && total > maxBytes
        if not overCount && not overBytes then
          0
        else
          lastPass <- now
          let cap (n : int64) = if n = 0L then None else Some n
          prune (cap keep) (cap maxBytes)


/// Store trace data to SQLite.
module TraceStorage =
  open LibDB.Sqlite

  /// Where `prepareDvalForStorage` keeps a blob captured after its trace was stored, such as a
  /// served response's body: in `trace_blobs` under that trace, counted in what it weighs. Only a
  /// blob the trace did not already hold adds to the weight.
  let keepBlobIn (traceId : System.Guid) : string -> byte[] -> Ply.Ply<unit> =
    fun hash bytes ->
      uply {
        let! added =
          Sql.query
            "INSERT OR IGNORE INTO trace_blobs (trace_id, hash, bytes)
             VALUES (@traceId, @hash, @bytes)"
          |> Sql.parameters
            [ "traceId", Sql.string (string traceId)
              "hash", Sql.string hash
              "bytes", Sql.bytes bytes ]
          |> Sql.executeNonQueryAsync
        if added > 0 then
          do!
            Sql.query "UPDATE traces SET bytes = bytes + @n WHERE id = @traceId"
            |> Sql.parameters
              [ "traceId", Sql.string (string traceId)
                "n", Sql.int64 (int64 bytes.Length) ]
            |> Sql.executeStatementAsync
      }


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
    /// The blobs the trace captured, one per hash. Kept in `trace_blobs`, with the trace.
    (blobs : List<string * byte[]>)
    (events : List<CompletedEvent>)
    (fns : List<RT.Hash>)
    /// How many times each loop went round, by the lambda's expression id.
    (loopPasses : System.Collections.Generic.Dictionary<int64, int>)
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

      // Every result lives in the trace's blobs, once per distinct value, and its row holds
      // the value's hash. A type check reads the same items and gets the same small answers
      // over and over, so most rows of a heavy trace share a handful of values.
      let results = System.Collections.Generic.Dictionary<string, byte[]>()
      // A by-hash read that found its item is serialized once per hash asked: an item IS its
      // hash, so a later `Some` for the same hash is the same item. Anything else is serialized
      // every time, since a `None` can turn into the item later in the run.
      let foundByAsk = System.Collections.Generic.Dictionary<string, string>()
      let keep (ev : CompletedEvent) : string =
        let bytes = serializeDval "trace_fn_calls.result" ev.result
        let hash = Blob.sha256Hex bytes
        results[hash] <- bytes
        hash
      let serializedEvents =
        events
        |> List.map (fun ev ->
          let argsBytes = serializeArgs ev.args
          let hash =
            match ev.result with
            | RT.DEnum(_, _, _, "Some", [ _ ]) when
              Set.contains ev.fnName Redact.fromTheStore
              ->
              let ask = ev.fnName + ":" + System.Convert.ToBase64String argsBytes
              match foundByAsk.TryGetValue ask with
              | true, hash -> hash
              | _ ->
                let hash = keep ev
                foundByAsk[ask] <- hash
                hash
            | _ -> keep ev
          (ev, argsBytes, hash))
      let blobs =
        blobs @ (results |> Seq.map (fun kv -> kv.Key, kv.Value) |> List.ofSeq)

      // What this trace weighs, kept on its row: retention applies `trace.maxMb` from this
      // column, never by summing `trace_fn_calls`, the table it exists to bound.
      let weight =
        int64 inputBytes.Length
        + (serializedEvents
           |> List.sumBy (fun (_, a, h) -> int64 a.Length + int64 h.Length))
        + (blobs |> List.sumBy (fun (_, b) -> int64 b.Length))

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
           input_name, input_value, account_id, status, updated, duration_ms, bytes)
         VALUES
          (@id, 0, @handlerDesc, @timestamp,
           @inputName, @inputValue, @accountId, 'done', @timestamp, @durationMs, @bytes)
         ON CONFLICT(id) DO UPDATE SET
           handler_desc = excluded.handler_desc,
           input_name = excluded.input_name,
           input_value = excluded.input_value,
           account_id = excluded.account_id,
           updated = excluded.updated,
           duration_ms = excluded.duration_ms,
           bytes = excluded.bytes",
          [ [ "id", Sql.string traceIdStr
              "handlerDesc", Sql.string handlerDesc
              "timestamp", Sql.string timestamp
              "inputName", Sql.string inputVarName
              "inputValue", Sql.bytes inputBytes
              "accountId", accountIDSql
              "durationMs", Sql.int64 durationMs
              "bytes", Sql.int64 weight ] ]

          "DELETE FROM trace_fn_calls WHERE trace_id = @traceId", [ traceIdParam ]
          "DELETE FROM trace_loops WHERE trace_id = @traceId", [ traceIdParam ]
          "DELETE FROM trace_blobs WHERE trace_id = @traceId", [ traceIdParam ] ]

      let blobStmt =
        match blobs with
        | [] -> []
        | _ ->
          [ "INSERT OR REPLACE INTO trace_blobs (trace_id, hash, bytes)
             VALUES (@traceId, @hash, @bytes)",
            blobs
            |> List.map (fun (hash, bytes) ->
              [ "traceId", Sql.string traceIdStr
                "hash", Sql.string hash
                "bytes", Sql.bytes bytes ]) ]

      // Skip the events INSERT when empty: fumble rejects zero-param-row
      // prepared statements, hit when a trace errors before any call fires.
      // The DELETE above still runs.
      let eventStmt =
        match events with
        | [] -> []
        | _ ->
          // `parent_call_id` and `lambda_expr_id` stay NULL and `kind` stays 'builtin': the log
          // is a sequence of impure calls, not a tree of frames. The columns stay because
          // `08-traces.sql` has merged and is frozen.
          [ "INSERT INTO trace_fn_calls
            (trace_id, call_id, parent_call_id, kind, fn_hash,
             lambda_expr_id, args, result, duration_ms, process_id, seq, ord)
           VALUES
            (@traceId, @callId, NULL, 'builtin', @fnHash,
             NULL, @args, @result, @durationMs, @processId, @seq, @ord)",
            serializedEvents
            |> List.map (fun (ev, argsBytes, resultHash) ->
              [ "traceId", Sql.string traceIdStr
                "callId", Sql.string ev.callId
                "fnHash", Sql.string ev.fnName
                "args", Sql.bytes argsBytes
                // The hash of the result, which is in `trace_blobs`.
                "result", Sql.string resultHash
                "durationMs", Sql.int64 ev.durationMs
                "processId",
                (if ev.processId = System.Guid.Empty then
                   Sql.string ""
                 else
                   Sql.string (string ev.processId))
                "seq", Sql.int64 ev.seq
                "ord", Sql.int64 ev.ord ]) ]

      // HOW MANY TIMES EACH LOOP WENT ROUND. One row per loop, not per pass.
      //
      // This is the whole of what anyone asks of a recorded run's shape, and it is asked for
      // one reason: a view recomputes values by replaying, so it can only show the passes it
      // REACHES. Without this a run whose log was capped, or which was suspended mid-loop,
      // reports "pass 3 of 3" about a loop that went round five times.
      //
      // One number per loop rather than a row per frame: the only question anyone asks of a
      // recorded run's shape is how many times a loop went round, and a row per frame is 8,523
      // of them for a 2000-iteration loop.
      let loopStmt =
        match List.ofSeq loopPasses with
        | [] -> []
        | passes ->
          [ "INSERT OR REPLACE INTO trace_loops (trace_id, call_site, passes)
             VALUES (@traceId, @callSite, @passes)",
            passes
            |> List.map (fun (KeyValue(callSite, n)) ->
              [ "traceId", Sql.string traceIdStr
                "callSite", Sql.string (string callSite)
                "passes", Sql.int64 (int64 n) ]) ]

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
        Sql.executeTransactionSync (
          baseStatements @ eventStmt @ fnStmt @ loopStmt @ blobStmt
        )
      TraceRetention.run () |> ignore<int>


/// Rewrite a Dval for the trace-storage boundary:
///   - DStream → DStreamStub (the live pull fn closes over this VM's
///     exeState; draining would consume the user's stream).
///   - DBlob(Ephemeral _) → DBlob(Persistent _), handing the bytes to <param keep> so the
///     trace survives the producing VM. The trace keeps them (`trace_blobs`), not the shared
///     `package_blobs`, which nothing collects.
/// Recursion and container rebuilding are handled by `Dval.rewriteWith`,
/// so nested DStream values (inside lists, records, closures, ...) are
/// stubbed just like top-level ones.
let prepareDvalForStorage
  (keep : string -> byte[] -> Ply.Ply<unit>)
  (dv : RT.Dval)
  : Ply.Ply<RT.Dval> =
  let promoteBlob = Blob.promoteEphemeralLeaf keep
  dv
  |> RT.Dval.rewriteWith (fun dv ->
    uply {
      match dv with
      | RT.DStream(impl, _, _) -> return Some(RTToDT.Dval.streamStubDT impl)
      | _ -> return! promoteBlob dv
    })


/// Walk every captured Dval through [prepareDvalForStorage]. Mutates
/// `state.events` in place; returns the prepared input dval and the blobs the trace keeps,
/// one per hash.
let private prepareTraceForStorage
  (inputDval : RT.Dval)
  (events : CompletedEvent[])
  : Ply.Ply<RT.Dval * List<string * byte[]>> =
  uply {
    let blobs = System.Collections.Generic.Dictionary<string, byte[]>()
    let keep (hash : string) (bytes : byte[]) : Ply.Ply<unit> =
      blobs[hash] <- bytes
      Ply.Ply(())
    let prep = prepareDvalForStorage keep
    let! preparedInput = prep inputDval
    for i in 0 .. events.Length - 1 do
      let ev = events[i]
      let! preparedArgs = ev.args |> Ply.List.mapSequentially prep
      let! preparedResult = prep ev.result
      events[i] <- { ev with args = preparedArgs; result = preparedResult }

    return
      (preparedInput, blobs |> Seq.map (fun kv -> kv.Key, kv.Value) |> List.ofSeq)
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
    // Trace detail OFF must be a true no-op: `prepareTraceForStorage` (below) hashes every captured
    // ephemeral blob, and a request body can be megabytes. Bail here so neither the walk nor the store
    // runs. This is the single choke point for both the sqlite and CLI tracers (the serve uses the CLI one).
    if TraceDetail.current = TraceDetail.Off then
      return ()
    else

      let traceIdStr = string traceID
      use _span = Telemetry.span "trace.store" [ "traceId", traceIdStr ]
      // A copy taken under the lock: a run suspended by Ctrl-C stores while its processes may
      // still be recording, and the copy is what gets prepared and written.
      let struct (events, dropped, nextSeq, fns, loopPasses) =
        lock state.sync (fun () ->
          struct (state.events.ToArray(),
                  state.dropped,
                  state.nextSeq,
                  List.ofSeq state.fns,
                  System.Collections.Generic.Dictionary(state.loopPasses)))
      if dropped > 0 then
        Telemetry.event
          "trace.truncated"
          [ "kept", string events.Length; "dropped", string dropped ]
      try
        let! (preparedInput, blobs) = prepareTraceForStorage inputDval events
        TraceStorage.store
          traceID
          handlerDesc
          inputVarName
          preparedInput
          blobs
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
                   ord = -1L } ]
           else
             List.ofArray events)
          fns
          loopPasses
          exeState.accountID
          state.elapsed.ElapsedMilliseconds
      with ex ->
        let inner =
          match ex.InnerException with
          | null -> ""
          | e -> $" ({e.Message})"
        NonBlockingConsole.writeErrLine
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
  // `Exe.noTracing` leaves everything off, which also lets the interpreter skip its own per-frame
  // per-frame bookkeeping rather than just calling no-op hooks.
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
  (log : List<System.Guid * int64 * RT.Tracing.ReplayStep * RecordedCall>)
  : T =
  let state = newState ()
  state.resuming <- (string (AT.TraceID.toUUID traceID)).Substring(0, 8)
  // The recorded processes, in the order they first appear in the log. `Guid.Empty` is NOT one
  // of them: those rows are the unscheduled root's, seeded below and answered through the root
  // hooks. Leaving it in the list would hand the FIRST spawned child the root's rows, and the
  // child's own log would never be reached (its first call would answer with the root's first
  // answer, which is how a replayed spawn came back with a fresh uuid).
  let mutable unmatched =
    log
    |> List.map (fun (pid, _, _, _) -> pid)
    |> List.distinct
    |> List.filter (fun pid -> pid <> System.Guid.Empty)
  // A run nobody scheduled recorded under `Guid.Empty`, and a resume nobody schedules asks
  // under it too, through the root hooks below, so those rows answer directly as well as
  // through the matching.
  for (pid, ord, answer, recorded) in log do
    if pid = System.Guid.Empty then
      state.replay[struct (System.Guid.Empty, ord)] <- answer
      state.expected[struct (System.Guid.Empty, ord)] <- recorded
  let rec tracingFor (pid : System.Guid) : RT.Tracing.Tracing =
    lock state.sync (fun () ->
      match unmatched with
      | recorded :: rest ->
        unmatched <- rest
        for (rpid, ord, answer, call) in log do
          if rpid = recorded then
            state.replay[struct (pid, ord)] <- answer
            state.expected[struct (pid, ord)] <- call
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


/// One frame the replay walked: what made it, what it runs, and what it was given.
///
/// `valuesKept` is false outside the window a view holds, where a frame is counted but its
/// values are not, so the view can still say how many iterations there were.
type ViewFrame =
  {
    parent : System.Guid
    executionPoint : RT.ExecutionPoint
    /// What this entry was given, in order. Paired with the declaration's parameter names when
    /// shown, so an iteration can say `n = 4` rather than leaving a reader to infer it. Dropped
    /// with the values when the frame falls out of the window.
    args : List<RT.Dval>
    /// The order this frame was pushed, across the whole view.
    ///
    /// The frames come back from a dictionary, whose iteration order is not a promise, and a
    /// count within one site cannot order frames at different sites. This is the one thing that
    /// says what happened first, and it is what numbers a loop's iterations.
    ord : int
    valuesKept : bool
  }


/// A tracer for VIEWING a run: every effectful call is answered by its name and arguments from
/// that run's log, none is performed, and every expression's value is collected on the way.
/// Classic called this Preview. Nothing is written: a view is not itself a run.
///
/// The key is `(name, arguments)` rather than the `(process, ordinal)` a resume uses, and for
/// the reason classic had: a view has to survive the code having moved on. Add a call in the
/// middle and every ordinal after it shifts, so an ordinal-keyed view would go blank from
/// there; a name-and-arguments key still answers every call you did not touch.
///
/// Classic then took the last write for a key (`DISTINCT ON ... ORDER BY timestamp DESC`). We do
/// not: a key holds a QUEUE and a lookup consumes one, so the n-th call gets the n-th recorded
/// answer. Last-write-wins served `Uuid.generate ()` the same value twice, and a run that made
/// ten uuids showed one of them ten times. The code below says it at more length.
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
/// A cheap identity for the SITE a frame belongs to: the thing a trace can enter more than once.
///
/// For a lambda that is its own expression id, so the iterations of `List.map (fun x -> ...)`
/// share one. For a function it is a hash of its name, so the entries of a recursion, or of a
/// function a loop called once per iteration, share one too. Those are the same idea and
/// counting them is the same counting, which is why there is one key rather than two.
///
/// Not `string ep`: an `ExecutionPoint.Lambda` carries its parent, so formatting one walks and
/// allocates the whole chain, once per frame.
///
/// The function case is a structural hash, so two different functions called from ONE frame
/// could in principle collide and share a counter. The effect would be a wrong iteration number
/// on a call that is not a loop, where the number is not shown; the frame ids stay distinct
/// either way.
let siteKeyOf (ep : RT.ExecutionPoint) : int64 =
  match ep with
  | RT.ExecutionPoint.Source -> 0L
  | RT.ExecutionPoint.Lambda(_, lambdaExprId) -> int64 lambdaExprId
  | RT.ExecutionPoint.Function name -> int64 (hash name)


let createViewTracer
  (rows : List<string * byte[] * RT.Dval>)
  (collected :
    System.Collections.Generic.Dictionary<struct (System.Guid * int64), RT.Dval>)
  (frames : System.Collections.Generic.Dictionary<System.Guid, ViewFrame>)
  (lastByExpr : System.Collections.Generic.Dictionary<int64, RT.Dval>)
  /// Iterations to keep BESIDES the window, as (which site, which iteration, counting from 0).
  /// This is how one in the middle of a long loop is reached: the view runs again asking for
  /// it. Empty for an ordinary view.
  (focus : List<struct (int64 * int)>)
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
    System.Collections.Generic.Dictionary<string, System.Collections.Generic.Queue<RT.Dval>>()
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
        let mutable q =
          Unchecked.defaultof<System.Collections.Generic.Queue<RT.Dval>>
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

  // Which iterations of one call site keep their values: the first few and the last few.
  //
  // Without a bound the values scale with the RUN rather than with what can be shown: `fib 20`
  // pushes about twenty-two thousand frames. The first N and the last N is ten frames a site
  // however many times it goes round, and it is the pair people actually compare -- how the
  // loop started against how it ended. A flat "first twenty" could not show the end of a long
  // loop at all, which is the half most often worth seeing.
  //
  // Anything in between is reached by asking for it: `focus` below replays again keeping that
  // one iteration, which costs what opening the trace costs. So nothing is unreachable, and
  // what is held at once is bounded by the window rather than by the loop.

  let headKept = 5
  let tailKept = 5

  // which call site -> how many frames have been seen there, across the whole view
  //
  // The site ALONE, with no parent in the key, because that is what every consumer means by a
  // site: `loopsOf` groups a function's passes by `callSite` and nothing else ("this is one
  // grouping key, not two"), `siteKeyOf` is built so recursion's entries share one key, and
  // `focus` below names an iteration by (site, index). Keying this by (parent, site) instead
  // made two things answer the same question differently, and the cost landed on recursion:
  // every recursive call has a DIFFERENT parent frame, so every call was the first at its own
  // key, `inHead` was always true, and nothing was ever evicted. `fib 20`'s twenty-two thousand
  // frames all kept their arguments and values. That is the quadratic: not the frame count, but
  // a window that recursion could never fall out of.
  //
  // What it means when one loop runs more than once: its passes merge into a single numbered
  // sequence in execution order, so a `List.map` inside a function called twice reads as one
  // loop of 2N rather than two of N. That is already what the page shows, since `loopsOf`
  // merges them; this makes the frames that are KEPT agree with the count that is displayed.
  // The window is five and five per site across the run, and anything in between is reached by
  // `focus`, which now keys the same way and so can actually reach it.
  let siteCounts = System.Collections.Generic.Dictionary<int64, int>()

  // The tail window: the most recent frames at each site that are still holding values, oldest
  // first. When an eleventh iteration arrives the sixth-from-last stops being in the last five,
  // so its values go. Head frames never enter this queue and so are never dropped.
  let tailWindow =
    System.Collections.Generic.Dictionary<int64, System.Collections.Generic.Queue<System.Guid>>()

  // Which expressions each frame wrote, so dropping one is a bounded amount of work rather than
  // a scan of everything collected so far.
  let writtenBy =
    System.Collections.Generic.Dictionary<System.Guid, ResizeArray<int64>>()

  let dropValuesOf (frameId : System.Guid) : unit =
    let mutable exprs = Unchecked.defaultof<ResizeArray<int64>>
    if writtenBy.TryGetValue(frameId, &exprs) then
      for exprId in exprs do
        collected.Remove(struct (frameId, exprId)) |> ignore<bool>
      writtenBy.Remove frameId |> ignore<bool>
    // The frame itself stays, with `valuesKept` turned off: the COUNT has to remain honest, so
    // a view can still say a loop went round two thousand times while holding ten of them.
    let mutable f = Unchecked.defaultof<ViewFrame>
    if frames.TryGetValue(frameId, &f) then
      frames[frameId] <- { f with valuesKept = false; args = [] }

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
    let site = siteKeyOf ep
    lock gate (fun () ->
      let mutable seen = 0
      siteCounts.TryGetValue(site, &seen) |> ignore<bool>
      siteCounts[site] <- seen + 1
      // `pass` is the ordinal among siblings at this call site, which is what a loop's passes
      // are numbered by. Kept on every frame, including those past the cap, so the view can
      // say "200 passes" truthfully while holding twenty of them.
      // Every pass gets a frame, so the COUNT is honest: a view has to be able to say a loop
      // went round two thousand times. Past the cap the frame is a marker -- no arguments, no
      // values -- which is what keeps the cost of a big loop in the count rather than in the
      // contents.
      let ord = frameOrd
      frameOrd <- frameOrd + 1

      // In the head, or the one iteration this view was asked to go and get.
      let isFocused =
        focus |> List.exists (fun (struct (site', at)) -> site' = site && at = seen)

      let inHead = seen < headKept

      frames[frameId] <-
        { parent = parentId
          executionPoint = ep
          ord = ord
          args = args
          valuesKept = true }

      // Everything past the head joins the tail window, and the window pushes the oldest out.
      // A focused frame is not queued, so nothing can evict the iteration we came back for.
      if not inHead && not isFocused then
        let mutable q =
          Unchecked.defaultof<System.Collections.Generic.Queue<System.Guid>>
        if not (tailWindow.TryGetValue(site, &q)) then
          q <- System.Collections.Generic.Queue<System.Guid>()
          tailWindow[site] <- q
        q.Enqueue frameId
        if q.Count > tailKept then dropValuesOf (q.Dequeue()))

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
        traceEffects = false
        // Keyed by (frame, expression). An expression id alone is not unique within a run: a
        // loop body writes the same id once per pass, so keying on it alone keeps only the last.
        //
        // Bounded by what can be SHOWN rather than by what ran, which is what stops a view
        // scaling with compute. The head-and-tail window is what keeps it so.
        storeExprResult =
          fun exprId frameId dv ->
            lock gate (fun () ->
              // Last EXECUTED wins, which is what every consumer had when the key was the
              // expression id alone and the values arrived as a list in execution order. A
              // dictionary does not iterate in insertion order, so flattening `collected`
              // afterwards would hand back an arbitrary pass instead of the last one. Kept
              // here, where the order is still known.
              lastByExpr[int64 exprId] <- dv
              // Only for a frame under the cap. Past it the frame is a marker that exists to
              // be counted; the flat view above still holds the last value, which is what a
              // collapsed line shows.
              let mutable f = Unchecked.defaultof<ViewFrame>
              if frames.TryGetValue(frameId, &f) && f.valuesKept then
                collected[struct (frameId, int64 exprId)] <- dv
                let mutable exprs = Unchecked.defaultof<ResizeArray<int64>>
                if not (writtenBy.TryGetValue(frameId, &exprs)) then
                  exprs <- ResizeArray()
                  writtenBy[frameId] <- exprs
                exprs.Add(int64 exprId))
        storeFrameEntry = noteFrame
        viewEffect = Some lookup
        forProcess = fun _ -> viewTracing () }

  // `enabled = false` is doing real work: it is what stops the host giving this run a row of
  // its own, and what makes the store a no-op.
  { enabled = false
    storeTraceResults = fun _ -> uply { return () }
    executionTracing = viewTracing () }
