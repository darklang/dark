/// Builtin functions for querying the trace store.
/// Companion to `LibDB.Tracing` (the recorder side).
module Builtins.Matter.Libs.Traces

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Sqlite
open LibExecution.Effects

module Dval = LibExecution.Dval
module BinarySer = LibSerialization.Binary.Serialization
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution
module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes
module Execution = LibExecution.Execution
module TracesRefs = LibExecution.PackageRefs.Type.Tracing

let dvalTypeName () =
  FQTypeName.fqPackage (
    LibExecution.PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
  )
let traceTypeName () = FQTypeName.fqPackage (TracesRefs.trace ())
let statusTypeName () = FQTypeName.fqPackage (TracesRefs.status ())
let inputVarTypeName () = FQTypeName.fqPackage (TracesRefs.inputVar ())
let fnCallTypeName () = FQTypeName.fqPackage (TracesRefs.fnCall ())
let traceDataTypeName () = FQTypeName.fqPackage (TracesRefs.traceData ())
let traceQueryTypeName () = FQTypeName.fqPackage (TracesRefs.traceQuery ())
let traceAnswerTypeName () = FQTypeName.fqPackage (TracesRefs.traceAnswer ())
let traceSelectionTypeName () = FQTypeName.fqPackage (TracesRefs.traceSelection ())


/// Columns read by traceRowToDT, qualified to avoid ambiguous names in joins.
let private traceColumns : string =
  [ "id"
    "handler_desc"
    "input_name"
    "input_value"
    "status"
    "parent_id"
    "parent_seq"
    "pinned"
    "timestamp"
    "updated"
    "result_value"
    "entry_hash"
    "duration_ms" ]
  |> List.map (fun c -> $"t.{c}")
  |> String.concat ", "
  |> fun cs -> $"{cs}, {LibDB.Traces.ScriptOrEvalInput.selectColumns}"

let private fromTraces : string =
  let join = LibDB.Traces.ScriptOrEvalInput.join "t"
  $"FROM traces t {join}"

let private statusToDT (status : string) : Dval =
  let tn = statusTypeName ()
  let case =
    match status with
    | "running" -> "Running"
    | "failed" -> "Failed"
    | "suspended" -> "Suspended"
    | "done" -> "Done"
    | other ->
      // `LibDB.Traces.Status.parse` raises on an unknown status; reading it as a finished run
      // here would be the two halves of one column disagreeing.
      Exception.raiseInternal "unknown run status" [ "status", other ]
  DEnum(tn, tn, [], case, [])

let private traceRowToDT (read : RowReader) : Dval =
  let tn = traceTypeName ()
  let inputValue =
    BinarySer.RT.Dval.deserialize "traces.input_value" (read.bytes "input_value")
  let scriptOrEvalInput = LibDB.Traces.ScriptOrEvalInput.fromColumns read
  let parentKT = KTTuple(ValueType.Known KTString, ValueType.Known KTInt64, [])
  let parent =
    match read.stringOrNone "parent_id", read.int64OrNone "parent_seq" with
    | Some p, Some seq ->
      Dval.optionSome parentKT (DTuple(DString p, DInt64 seq, []))
    | _ -> Dval.optionNone parentKT
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "id", DString(read.string "id")
        "entry", DString(read.string "handler_desc")
        "input", RT2DT.Dval.toDT inputValue
        "servedRequest",
        DBool(LibDB.Traces.isServedRequest (read.string "input_name"))
        "scriptOrEvalInput",
        (let kt = KTCustomType(LibDB.Traces.ScriptOrEvalInput.typeName (), [])
         Dval.option
           kt
           (Option.map LibDB.Traces.ScriptOrEvalInput.toDT scriptOrEvalInput))
        // The value itself, not a rendering of it: Dark formats it, the way it formats a
        // recorded call's result.
        "result",
        (let kt = KTCustomType(dvalTypeName (), [])
         match read.bytesOrNone "result_value" with
         | Some bytes ->
           Dval.optionSome
             kt
             (bytes
              |> BinarySer.RT.Dval.deserialize "traces.result_value"
              |> RT2DT.Dval.toDT)
         | None -> Dval.optionNone kt)
        "status", statusToDT (read.string "status")
        "parent", parent
        "pinned", DBool(read.int "pinned" = 1)
        "created", DString(read.string "timestamp")
        "updated", DString(read.string "updated")
        // Wall clock for the whole run. Dark decides how to say it; a `0` means a run that is
        // still going, or one recorded before the column existed.
        "durationMs", DInt64(read.int64 "duration_ms")
        // Which handler served a request. The only thing that names the ROOT of a served
        // trace, which is what `traces show <trace>` wants to open.
        "entryHash",
        (match read.stringOrNone "entry_hash" with
         | Some h -> Dval.optionSome KTString (DString h)
         | None -> Dval.optionNone KTString) ]
  )

/// Read a binary-serialized dval back into a darklang-typed Dval (the
/// custom type produced by RT2DT.Dval.toDT) so the trace-view fn-call
/// records carry the right shape. The binary format is the same one
/// LibDB/Tracing.fs writes via `BinarySer.RT.Dval.serialize`.
let private parseDvalBytes (bytes : byte[]) : Dval =
  bytes
  |> BinarySer.RT.Dval.deserialize "trace_fn_calls.result"
  |> RT2DT.Dval.toDT

/// Args are stored as a single `DList(Unknown, …)` blob (see
/// `LibDB/Tracing.fs::serializeArgs`). Unwrap the list and convert
/// each element through the darklang-typed Dval pipeline.
let private parseArgsBytes (bytes : byte[]) : List<Dval> =
  match BinarySer.RT.Dval.deserialize "trace_fn_calls.args" bytes with
  | DList(_, items) -> items |> List.map RT2DT.Dval.toDT
  | other ->
    Exception.raiseInternal "trace_fn_calls.args was not a DList" [ "actual", other ]


/// Load call events for a trace, ordered by rowid (= execution order).
/// Args/result are stored as binary RT.Dval blobs; deserialize per row.
/// The display name was resolved at write time and lives in fn_hash, so
/// reads are a flat SELECT — lambdas have NULL fn_hash and render as
/// "(lambda)".
let private loadFnCalls (traceId : string) : Ply<Dval> =
  let typeName = fnCallTypeName ()
  let dvalKT = KTCustomType(dvalTypeName (), [])
  uply {
    let! events =
      Sql.query
        "SELECT c.call_id, c.fn_hash,
                c.args, r.bytes AS result, c.duration_ms, c.process_id, c.seq, c.ord
         FROM trace_fn_calls c
         JOIN trace_blobs r ON r.trace_id = c.trace_id AND r.hash = c.result
         WHERE c.trace_id = @traceId
         ORDER BY c.seq, c.rowid"
      |> Sql.parameters [ "traceId", Sql.string traceId ]
      |> Sql.executeAsync (fun read ->
        {| callId = read.string "call_id"
           fnHash = read.stringOrNone "fn_hash"
           argsBytes = read.bytes "args"
           resultBytes = read.bytes "result"
           durationMs = read.int64 "duration_ms"
           processId = read.string "process_id"
           seq = read.int64 "seq"
           ord = read.int64 "ord" |})

    // A row whose args / result will not deserialize is logged and dropped, not stood in for:
    // the renderer wants each `FnCall`'s `args` / `result` as the canonical Dval custom type,
    // so a placeholder `DString` fails the type check at apply time instead. Dropping keeps
    // the rest of the trace readable; a raise here would lose all of it.
    let enriched =
      events
      |> List.choose (fun ev ->
        try
          // `kind` is not read: the writer puts 'builtin' on every row, because the log is a
          // sequence of impure builtin calls. Nor are `parent_call_id` and `lambda_expr_id`,
          // which are NULL on every row of every trace. The columns stay in the schema, which
          // has merged; the reader stops pretending they carry anything.
          let displayName =
            match ev.fnHash with
            | Some name -> name
            | None -> "(unknown)"
          let args = parseArgsBytes ev.argsBytes
          let result = parseDvalBytes ev.resultBytes
          let fields =
            RecordFields.ofList
              [ "callId", DString ev.callId
                "fnName", DString displayName
                "args", Dval.list dvalKT args
                "result", result
                "durationMs", Dval.dint64 (int64 ev.durationMs)
                "processId",
                (match System.Guid.TryParse ev.processId with
                 | true, g when g <> System.Guid.Empty ->
                   Dval.optionSome KTUuid (DUuid g)
                 | _ -> Dval.optionNone KTUuid)
                "seq", DInt64 ev.seq
                "ord", DInt64 ev.ord ]
          Some(DRecord(typeName, typeName, [], fields))
        with ex ->
          printErr $"[tracing] dropping corrupt fn_call row: {ex.Message}"
          Telemetry.event
            "trace.row.parseFailed"
            [ "callId", ev.callId; "message", ex.Message ]
          None)

    return enriched |> Dval.list (KTCustomType(typeName, []))
  }

// ───────── reads: one query, one answer ─────────

/// `Tracing.TraceQuery`, read off its Dval. One case per question Dark asks of the store.
type private Query =
  | Newest of limit : int64
  | Calling of fnName : string * limit : int64
  | WentThrough of fnName : string * fnHash : string * limit : int64
  | TimeIn of fnName : string * fnHash : string * limit : int64
  | ById of traceID : string
  | Containing of pattern : string * limit : int64
  | Detail of traceID : string
  | IdsStartingWith of prefix : string
  | SavedInput of traceID : string
  | FnsOf of traceID : string
  | LoopPasses of traceID : string
  | FnCounts of traceLimit : int64
  | Hotspots of traceLimit : int64
  | StatsByHandler of traceLimit : int64

let private queryFromDT (q : Dval) : Query =
  match q with
  | DEnum(_, _, _, "Newest", [ DInt64 limit ]) -> Newest limit
  | DEnum(_, _, _, "Calling", [ DString fnName; DInt64 limit ]) ->
    Calling(fnName, limit)
  | DEnum(_, _, _, "WentThrough", [ DString fnName; DString fnHash; DInt64 limit ]) ->
    WentThrough(fnName, fnHash, limit)
  | DEnum(_, _, _, "TimeIn", [ DString fnName; DString fnHash; DInt64 limit ]) ->
    TimeIn(fnName, fnHash, limit)
  | DEnum(_, _, _, "ById", [ DString traceID ]) -> ById traceID
  | DEnum(_, _, _, "Containing", [ DString pattern; DInt64 limit ]) ->
    Containing(pattern, limit)
  | DEnum(_, _, _, "Detail", [ DString traceID ]) -> Detail traceID
  | DEnum(_, _, _, "IdsStartingWith", [ DString prefix ]) -> IdsStartingWith prefix
  | DEnum(_, _, _, "SavedInput", [ DString traceID ]) -> SavedInput traceID
  | DEnum(_, _, _, "FnsOf", [ DString traceID ]) -> FnsOf traceID
  | DEnum(_, _, _, "LoopPasses", [ DString traceID ]) -> LoopPasses traceID
  | DEnum(_, _, _, "FnCounts", [ DInt64 traceLimit ]) -> FnCounts traceLimit
  | DEnum(_, _, _, "Hotspots", [ DInt64 traceLimit ]) -> Hotspots traceLimit
  | DEnum(_, _, _, "StatsByHandler", [ DInt64 traceLimit ]) ->
    StatsByHandler traceLimit
  | _ -> Exception.raiseInternal "not a Tracing.TraceQuery" [ "query", q ]


/// The rows a run query asks for, as SQL with its parameters. Every value is bound, none
/// spliced, so nothing a person typed can change the statement.
let private runsSql (query : Query) =
  let newestFirst = "ORDER BY t.timestamp DESC, t.rowid DESC LIMIT @limit"
  match query with
  | Newest limit ->
    $"SELECT {traceColumns} {fromTraces} {newestFirst}", [ "limit", Sql.int64 limit ]

  | Calling(fnName, limit) ->
    // Both builtins and package fns store their display name in fn_hash (resolved at write
    // time), so one LIKE matches either. `%` and `_` are escaped so a literal one in the name
    // matches itself rather than any string.
    let escaped = fnName.Replace(@"\", @"\\").Replace("%", @"\%").Replace("_", @"\_")
    $"SELECT DISTINCT {traceColumns} {fromTraces}
      JOIN trace_fn_calls c ON t.id = c.trace_id
      WHERE c.fn_hash LIKE @pattern ESCAPE '\\'
      {newestFirst}",
    [ "pattern", Sql.string $"%%{escaped}%%"; "limit", Sql.int64 limit ]

  | WentThrough(fnName, fnHash, limit) ->
    // By name OR by hash: see `Tracing.Store.wentThrough` for why it takes both.
    $"SELECT {traceColumns} {fromTraces}
      JOIN trace_fns f ON f.trace_id = t.id
      WHERE f.fn_name = @fn
         OR (@hash <> '' AND f.fn_hash = @hash)
      {newestFirst}",
    [ "fn", Sql.string fnName; "hash", Sql.string fnHash; "limit", Sql.int64 limit ]

  | ById traceID ->
    $"SELECT {traceColumns} {fromTraces} WHERE t.id = @id {newestFirst}",
    [ "id", Sql.string traceID; "limit", Sql.int64 1L ]

  | _ -> Exception.raiseInternal "not a run query" []


// TODO (perf, foot-gun): we walk every trace + its fn_calls in
// F#-land, deserialize each Dval to a string repr, then substring-
// match. The previous shape was SQL `LIKE` over JSON columns —
// dropped when trace storage went binary. Fine on small dev DBs,
// O(N×M) on a populated store. Possible mitigations: cache a
// searchable text repr alongside each row, or use FTS5 over a
// computed/text-shadow column.
/// The runs whose recorded input, result, or any logged call's args or result contains the
/// substring (case-sensitive), matched against the developer-repr form of each Dval. Searches
/// the newest 2000 runs and stops at `limit` matches.
let private containing
  (exeState : ExecutionState)
  (pattern : string)
  (limit : int64)
  : Ply<List<Dval>> =
  uply {
    // Newest-first, deserializing each run's input and then, only if that misses, its
    // calls; stop at `limit` matches. Bounded at both ends on purpose: the scan window
    // keeps a store with retention turned off from loading every input blob it has, and
    // `limit` keeps the deserializing short. A match older than the window is not found,
    // which is what the description says.
    let containsPattern (dv : Dval) : Ply<bool> =
      uply {
        let! repr = Execution.dvalToRepr exeState dv
        return repr.Contains(pattern)
      }

    let! traces =
      Sql.query
        $"SELECT {traceColumns} {fromTraces}
          ORDER BY t.timestamp DESC, t.rowid DESC LIMIT 2000"
      |> Sql.executeAsync (fun read ->
        {| id = read.string "id"
           row = traceRowToDT read
           inputBytes = read.bytes "input_value"
           resultBytes = read.bytesOrNone "result_value" |})

    let! (_, hits) =
      traces
      |> Ply.List.foldSequentially
        (fun (count : int64, hits : List<Dval>) t ->
          if count >= limit then
            uply { return (count, hits) }
          else
            uply {
              let inputDval =
                BinarySer.RT.Dval.deserialize "traces.input_value" t.inputBytes
              let! inputHit = containsPattern inputDval
              // What the run answered counts as the run's own text too: a pure computation
              // logs no calls, so its result is the only place its value appears.
              let! resultHit =
                match t.resultBytes with
                | Some bytes ->
                  containsPattern (
                    BinarySer.RT.Dval.deserialize "traces.result_value" bytes
                  )
                | None -> uply { return false }
              let inputMatches = inputHit || resultHit

              let! matchesViaCalls =
                if inputMatches then
                  uply { return true }
                else
                  uply {
                    let! callRows =
                      Sql.query
                        "SELECT c.args, r.bytes AS result
                         FROM trace_fn_calls c
                         JOIN trace_blobs r
                           ON r.trace_id = c.trace_id AND r.hash = c.result
                         WHERE c.trace_id = @traceId"
                      |> Sql.parameters [ "traceId", Sql.string t.id ]
                      |> Sql.executeAsync (fun read ->
                        {| argsBytes = read.bytes "args"
                           resultBytes = read.bytes "result" |})

                    return!
                      callRows
                      |> Ply.List.foldSequentially
                        (fun found row ->
                          if found then
                            uply { return true }
                          else
                            uply {
                              let argsDval =
                                BinarySer.RT.Dval.deserialize
                                  "trace_fn_calls.args"
                                  row.argsBytes
                              let resultDval =
                                BinarySer.RT.Dval.deserialize
                                  "trace_fn_calls.result"
                                  row.resultBytes
                              let! argsMatch = containsPattern argsDval
                              if argsMatch then
                                return true
                              else
                                return! containsPattern resultDval
                            })
                        false
                  }

              if matchesViaCalls then
                return (count + 1L, t.row :: hits)
              else
                return (count, hits)
            })
        (0L, [])

    return List.rev hits
  }


/// One run with its inputs and its calls, as a `Tracing.TraceData`.
let private detail (traceID : string) : Ply<Option<Dval>> =
  uply {
    // One SELECT covers metadata + input — both live on the trace row.
    let! row =
      Sql.query
        $"SELECT {traceColumns} {fromTraces}
          WHERE t.id = @traceId"
      |> Sql.parameters [ "traceId", Sql.string traceID ]
      |> Sql.executeRowOptionAsync (fun read ->
        {| id = read.string "id"
           trace = traceRowToDT read
           inputName = read.string "input_name"
           inputValueBytes = read.bytes "input_value" |})

    match row with
    | Some r ->
      let! fnCalls = loadFnCalls r.id
      let inputVarType = inputVarTypeName ()
      let inputFields =
        RecordFields.ofList
          [ "name", DString r.inputName; "value", parseDvalBytes r.inputValueBytes ]
      let inputs =
        [ DRecord(inputVarType, inputVarType, [], inputFields) ]
        |> Dval.list (KTCustomType(inputVarType, []))
      let typeName = traceDataTypeName ()
      let fields =
        RecordFields.ofList
          [ "trace", r.trace; "inputs", inputs; "functionCalls", fnCalls ]
      return Some(DRecord(typeName, typeName, [], fields))
    | None -> return None
  }


/// A recorded CLI invocation, without its result. Served requests, and invocations that are
/// missing, incomplete or corrupt, have none.
let private savedInput (traceID : string) : Ply<Option<Dval>> =
  uply {
    match System.Guid.TryParse traceID with
    | false, _ -> return None
    | true, id ->
      let! input = LibDB.Traces.getCliInput id
      return Option.map LibDB.Traces.ScriptOrEvalInput.toDT input
  }


/// Per-entry aggregate over the last N traces: (entry, traceCount, totalMs, maxMs). The ms are
/// each TRACE's own wall clock, which is the number a person reading "how long does this take"
/// is asking for.
let private statsByHandler (traceLimit : int64) : Ply<List<Dval>> =
  uply {
    // The TRACE's own duration, not the sum of its impure calls.
    //
    // It used to join `trace_fn_calls` and sum those, and its own description said
    // per-trace latency "would need a separate column on `traces`". That column exists
    // now. Summing the calls meant a pure `eval` that took 69 ms reported 0, which is a
    // number nobody can act on and everybody notices.
    //
    // No join, so no double counting: the aggregate is over the traces themselves.
    let! rows =
      Sql.query
        "SELECT handler_desc AS handler,
                COUNT(*) AS trace_count,
                COALESCE(SUM(duration_ms), 0) AS total_ms,
                COALESCE(MAX(duration_ms), 0) AS max_ms
         FROM (
           SELECT handler_desc, duration_ms FROM traces ORDER BY rowid DESC LIMIT @traceLimit
         )
         GROUP BY handler_desc
         ORDER BY trace_count DESC, handler"
      |> Sql.parameters [ "traceLimit", Sql.int64 traceLimit ]
      |> Sql.executeAsync (fun read ->
        DTuple(
          DString(read.string "handler"),
          Dval.dint64 (read.int64 "trace_count"),
          [ Dval.dint64 (read.int64 "total_ms"); Dval.dint64 (read.int64 "max_ms") ]
        ))
    return rows
  }


/// Fn-call timing across the last N traces: (fnName, callCount, totalMs, maxMs), most total time
/// first. Lambdas are excluded (no fn_hash to bucket by); builtins included, with their measured
/// duration.
let private hotspots (traceLimit : int64) : Ply<List<Dval>> =
  uply {
    // Subquery: the last N trace IDs by recency.
    // Outer GROUP BY rolls duration up per fn_hash.
    // WHERE fn_hash IS NOT NULL drops lambda rows.
    let! rows =
      Sql.query
        "SELECT fn_hash AS name,
                COUNT(*) AS call_count,
                SUM(duration_ms) AS total_ms,
                MAX(duration_ms) AS max_ms
         FROM trace_fn_calls
         WHERE trace_id IN (
           SELECT id FROM traces ORDER BY rowid DESC LIMIT @traceLimit
         )
         AND fn_hash IS NOT NULL
         GROUP BY fn_hash
         ORDER BY total_ms DESC, call_count DESC
         LIMIT 50"
      |> Sql.parameters [ "traceLimit", Sql.int64 traceLimit ]
      |> Sql.executeAsync (fun read ->
        DTuple(
          DString(read.string "name"),
          Dval.dint64 (read.int64 "call_count"),
          [ Dval.dint64 (read.int64 "total_ms"); Dval.dint64 (read.int64 "max_ms") ]
        ))
    return rows
  }


/// What each of the runs `WentThrough` finds spent in that function: (trace id, calls, total
/// microseconds, slowest call's microseconds), newest first. The same runs in the same order,
/// so a caller can lay the two side by side.
let private timeIn
  (fnName : string)
  (fnHash : string)
  (limit : int64)
  : Ply<List<Dval>> =
  uply {
    let! rows =
      Sql.query
        "SELECT t.id AS id, f.calls AS calls, f.total_us AS total_us,
                f.slowest_us AS slowest_us
         FROM traces t
         JOIN trace_fns f ON f.trace_id = t.id
         WHERE f.fn_name = @fn
            OR (@hash <> '' AND f.fn_hash = @hash)
         ORDER BY t.timestamp DESC, t.rowid DESC LIMIT @limit"
      |> Sql.parameters
        [ "fn", Sql.string fnName
          "hash", Sql.string fnHash
          "limit", Sql.int64 limit ]
      |> Sql.executeAsync (fun read ->
        DTuple(
          DString(read.string "id"),
          Dval.dint64 (read.int64 "calls"),
          [ Dval.dint64 (read.int64 "total_us")
            Dval.dint64 (read.int64 "slowest_us") ]
        ))
    return rows
  }


/// The package functions one run went through: each one's name, and the hash the run actually
/// went through. The names are what lets `traces show <run>` offer the code the run ran; the
/// hashes are how a resume tells you which of them have been edited since.
let private fnsOf (traceID : string) : Ply<List<Dval>> =
  uply {
    let! rows =
      Sql.query
        "SELECT fn_name, fn_hash FROM trace_fns WHERE trace_id = @t ORDER BY fn_name"
      |> Sql.parameters [ "t", Sql.string traceID ]
      |> Sql.executeAsync (fun read ->
        DTuple(DString(read.string "fn_name"), DString(read.string "fn_hash"), []))
    return rows
  }


/// Which package functions recent runs went through, and how many runs went through each, most
/// first. `Hotspots` cannot answer it: it reads the effect log, which holds only the impure
/// calls, so it can only ever name builtins.
let private fnCounts (traceLimit : int64) : Ply<List<Dval>> =
  uply {
    let! rows =
      Sql.query
        "SELECT f.fn_name AS name, COUNT(DISTINCT f.trace_id) AS runs
         FROM trace_fns f
         WHERE f.trace_id IN
           (SELECT id FROM traces ORDER BY rowid DESC LIMIT @traceLimit)
         GROUP BY f.fn_name
         ORDER BY runs DESC, name ASC"
      |> Sql.parameters [ "traceLimit", Sql.int64 traceLimit ]
      |> Sql.executeAsync (fun read ->
        DTuple(DString(read.string "name"), DInt64(read.int64 "runs"), []))
    return rows
  }


/// How many times each loop in a recorded run actually went round, as it was RECORDED: the
/// lambda's expression id, and the number of passes.
///
/// Read rather than replayed, which is the point. A view recomputes values by re-running the
/// code with its effects answered from the log, so it can only show what it reaches: a run
/// suspended mid-loop, or one whose replay stops at an effect the log cannot answer, has passes
/// that happened and that no re-running will show. Without this a view says "pass 3 of 3" about
/// a loop that went round seven times, which is a confident lie in a debugging tool.
///
/// One row per loop, counted as the run went. Every pass is counted, including the ones that
/// made no impure call. Empty for a run recorded before counts were kept.
let private loopPasses (traceID : string) : Ply<List<Dval>> =
  uply {
    let! rows =
      Sql.query "SELECT call_site, passes FROM trace_loops WHERE trace_id = @t"
      |> Sql.parameters [ "t", Sql.string traceID ]
      |> Sql.executeAsync (fun read ->
        DTuple(DString(read.string "call_site"), DInt64(read.int64 "passes"), []))
    return rows
  }


/// A `Tracing.TraceAnswer`: the field the query asked for, every other one empty.
type private Answer =
  { runs : List<Dval>
    detail : Option<Dval>
    ids : List<string>
    savedInput : Option<Dval>
    fns : List<Dval>
    counts : List<Dval>
    totals : List<Dval> }

let private noAnswer : Answer =
  { runs = []
    detail = None
    ids = []
    savedInput = None
    fns = []
    counts = []
    totals = [] }

let private answerToDT (a : Answer) : Dval =
  let tn = traceAnswerTypeName ()
  let totalsKT = KTTuple(VT.string, VT.int64, [ VT.int64; VT.int64 ])
  DRecord(
    tn,
    tn,
    [],
    RecordFields.ofList
      [ "runs", Dval.list (KTCustomType(traceTypeName (), [])) a.runs
        "detail", Dval.option (KTCustomType(traceDataTypeName (), [])) a.detail
        "ids", a.ids |> List.map DString |> Dval.list KTString
        "savedInput",
        Dval.option
          (KTCustomType(LibDB.Traces.ScriptOrEvalInput.typeName (), []))
          a.savedInput
        "fns", Dval.list (KTTuple(VT.string, VT.string, [])) a.fns
        "counts", Dval.list (KTTuple(VT.string, VT.int64, [])) a.counts
        "totals", Dval.list totalsKT a.totals ]
  )

let private answer (exeState : ExecutionState) (query : Query) : Ply<Answer> =
  uply {
    match query with
    | Newest _
    | Calling _
    | WentThrough _
    | ById _ ->
      let sql, ps = runsSql query
      let! rows = Sql.query sql |> Sql.parameters ps |> Sql.executeAsync traceRowToDT
      return { noAnswer with runs = rows }
    | Containing(pattern, limit) ->
      let! rows = containing exeState pattern limit
      return { noAnswer with runs = rows }
    | Detail traceID ->
      let! d = detail traceID
      return { noAnswer with detail = d }
    | IdsStartingWith prefix ->
      // Six, not five: enough for the caller to tell "one more" from "several more".
      let! ids = LibDB.Traces.idsStartingWith prefix 6
      return { noAnswer with ids = ids }
    | SavedInput traceID ->
      let! input = savedInput traceID
      return { noAnswer with savedInput = input }
    | FnsOf traceID ->
      let! rows = fnsOf traceID
      return { noAnswer with fns = rows }
    | TimeIn(fnName, fnHash, limit) ->
      let! rows = timeIn fnName fnHash limit
      return { noAnswer with totals = rows }
    | LoopPasses traceID ->
      let! rows = loopPasses traceID
      return { noAnswer with counts = rows }
    | FnCounts traceLimit ->
      let! rows = fnCounts traceLimit
      return { noAnswer with counts = rows }
    | Hotspots traceLimit ->
      let! rows = hotspots traceLimit
      return { noAnswer with totals = rows }
    | StatsByHandler traceLimit ->
      let! rows = statsByHandler traceLimit
      return { noAnswer with totals = rows }
  }


/// The ids a `Tracing.TraceSelection` names, for the cases that delete by id.
let private selectedIds
  (selection : Dval)
  : System.Threading.Tasks.Task<List<string>> =
  let ids (sql : string) ps =
    Sql.query sql
    |> Sql.parameters ps
    |> Sql.executeAsync (fun read -> read.string "id")
  match selection with
  | DEnum(_, _, _, "One", [ DString traceID ]) ->
    ids "SELECT id FROM traces WHERE id = @id" [ "id", Sql.string traceID ]
  | DEnum(_, _, _, "All", []) -> ids "SELECT id FROM traces" []
  | DEnum(_, _, _, "Before", [ DString cutoff ]) ->
    // The timestamp column is ISO 8601, which sorts as text the way it sorts as time.
    ids
      "SELECT id FROM traces WHERE timestamp < @cutoff"
      [ "cutoff", Sql.string cutoff ]
  | _ ->
    Exception.raiseInternal "not a Tracing.TraceSelection" [ "selection", selection ]


let fns () : List<BuiltInFn> =
  [ { name = fn "tracesRecording" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TBool
      description =
        "Whether this run is being recorded. Worth asking before reporting an empty result, "
        + "since with recording off every query comes back empty whatever ran."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          (LibDB.Tracing.TraceDetail.current <> LibDB.Tracing.TraceDetail.Off)
          |> DBool
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "tracesApplyRecording" 0
      typeParams = []
      parameters = [ Param.make "on" TBool "" ]
      returnType = TUnit
      description =
        "Make THIS process record, or stop recording, without waiting for the next command. "
        + "The lasting setting is `trace.record` in the store's config, which the caller writes "
        + "first; this is only how a session already running picks it up. Touches nothing "
        + "persistent and no other process. Like `trace.record`, only the CLI itself may."
      fn =
        (function
        | state, _, _, [| DBool on |] ->
          if not state.canManagePolicies then
            RuntimeError.UncaughtException(LibDB.Config.refusal "trace.record", [])
            |> raiseUntargetedRTE
          LibDB.Tracing.TraceDetail.setForTesting (
            if on then
              LibDB.Tracing.TraceDetail.On
            else
              LibDB.Tracing.TraceDetail.Off
          )
          Ply DUnit
        | _ -> incorrectArgs ())
      previewable = Impure
      // Turning recording off hides what this process does, so it is a trace write, not nothing.
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "tracesQuery" 0
      typeParams = []
      parameters =
        [ Param.make
            "query"
            (TCustomType(NR.ok (traceQueryTypeName ()), []))
            "what to read: runs, one run's detail, an id prefix, a run's saved input, the functions or loops one run went through, or totals over recent runs" ]
      returnType = TCustomType(NR.ok (traceAnswerTypeName ()), [])
      description =
        "Read the trace store: a `Tracing.TraceAnswer` with the field <param query> asks for "
        + "filled and the rest empty. Every trace read Dark makes comes through here, so the "
        + "tables' layout stays an F# matter. Runs come newest first."
      fn =
        (function
        | exeState, _, _, [| q |] ->
          uply {
            let! a = answer exeState (queryFromDT q)
            return answerToDT a
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesDelete" 0
      typeParams = []
      parameters =
        [ Param.make
            "which"
            (TCustomType(NR.ok (traceSelectionTypeName ()), []))
            "one run by full id, every run, the runs before a cutoff, or all but the newest few" ]
      returnType = TInt64
      description =
        "Delete the runs a `Tracing.TraceSelection` names, with their calls and the executions "
        + "they were the logs of. Returns how many runs went. `AllButNewest` is the same pass "
        + "retention runs after every store, so it keeps any run a suspended execution still "
        + "needs."
      fn =
        (function
        | _, _, _, [| DEnum(_, _, _, "AllButNewest", [ DInt64 keepN ]) |] ->
          let deleted = LibDB.Tracing.TraceRetention.prune (Some keepN) None
          LibDB.Tracing.TraceRetention.sweepOrphans ()
          Dval.dint64 (int64 deleted) |> Ply
        | _, _, _, [| (DEnum _ as which) |] ->
          uply {
            let! ids = selectedIds which
            match which, ids with
            // One id nobody has: nothing to do, and no sweep either.
            | DEnum(_, _, _, "One", _), [] -> ()
            | _ ->
              LibDB.Tracing.TraceRetention.deleteTraces ids
              LibDB.Tracing.TraceRetention.sweepOrphans ()
            return Dval.dint64 (int64 (List.length ids))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "tracesSetPinned" 0
      typeParams = []
      parameters =
        [ Param.make "traceID" TString "the full id"; Param.make "pinned" TBool "" ]
      returnType = TBool
      description =
        "Pin a run so retention never drops it, or unpin it. Returns whether a run had that id."
      fn =
        (function
        | _, _, _, [| DString traceID; DBool pinned |] ->
          uply {
            let! found = LibDB.Traces.setPinned traceID pinned
            return DBool found
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    // ───────── a trace is a run: resuming it, forking it ─────────

    { name = fn "tracesFork" 0
      typeParams = []
      parameters =
        [ Param.make "traceID" TString ""
          Param.make "at" (TypeReference.option TInt64) "" ]
      returnType = TypeReference.result TString TString
      description =
        "A new run branched from this one: the same input, its log up to `at` (a position in "
        + "the log; the whole log when None), suspended so `resume` takes it from there."
      fn =
        (function
        | _, _, _, [| DString traceID; at |] ->
          uply {
            let at =
              match at with
              | DEnum(_, _, _, "Some", [ DInt64 n ]) -> Some n
              | _ -> None
            match System.Guid.TryParse traceID with
            | false, _ ->
              return Dval.resultError KTString KTString (DString "not an id")
            | true, id ->
              match! LibDB.Traces.fork id at with
              | Ok child ->
                return Dval.resultOk KTString KTString (DString(string child))
              | Error msg -> return Dval.resultError KTString KTString (DString msg)
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "tracesArmResume" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "" ]
      returnType = TBool
      description =
        "Arm a resume of this run: the next `eval` or `run` this instance starts replays its "
        + "log instead of performing the effects, then goes live. False for an id nobody has."
      fn =
        (function
        | _, _, _, [| DString traceID |] ->
          uply {
            match System.Guid.TryParse traceID with
            | false, _ -> return DBool false
            | true, id ->
              let! armed = LibDB.Traces.Replay.arm id
              return DBool armed
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
