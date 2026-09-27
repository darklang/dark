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


/// The columns of a run, as the Dark `Tracing.Trace` wants them.
/// The columns `traceRowToDT` reads, qualified by the table or the alias they come from.
///
/// Qualified rather than bare because `traces` and `trace_fn_calls` both have a `duration_ms` now
/// -- one is the whole run, the other one call -- and a bare column list in a join between them is
/// ambiguous. SQLite says so at run time, in a query no F# compiler looks at, which is a whole
/// test run to find out.
let private traceColumnsOf (prefix : string) : string =
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
    "duration_ms" ]
  |> List.map (fun c -> $"{prefix}.{c}")
  |> String.concat ", "

let private traceColumns = traceColumnsOf "traces"

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

/// A run's input as one line of text: the expression or the script's source as it was written.
/// A served request is a record, not a string; it is named rather than dumped, since the whole
/// thing is one `traces show` away and a table cell is 40 characters.
/// A recorded dval as one line, for a table cell or a summary line. Shared by the input and
/// the result, because the two want the same treatment.
let private oneLineDval (label : string) (bytes : byte[]) : string =
  match BinarySer.RT.Dval.deserialize label bytes with
  | DString s -> s
  | DRecord(_, _, _, fields) ->
    let field name =
      match Map.tryFind name fields with
      | Some(DString s) -> Some s
      | _ -> None
    match field "method", field "url" with
    | Some m, Some u -> $"{m} {u}"
    | _ -> "(a request)"
  | _ -> "(not source)"

let private traceRowToDT (read : RowReader) : Dval =
  let tn = traceTypeName ()
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
    Map
      [ "id", DString(read.string "id")
        "entry", DString(read.string "handler_desc")
        "input", DString(oneLineDval "traces.input_value" (read.bytes "input_value"))
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
        "durationMs", DInt64(read.int64 "duration_ms") ]
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
        "SELECT call_id, parent_call_id, kind, fn_hash, lambda_expr_id,
                args, result, duration_ms, process_id, seq, ord
         FROM trace_fn_calls
         WHERE trace_id = @traceId
         ORDER BY seq, rowid"
      |> Sql.parameters [ "traceId", Sql.string traceId ]
      |> Sql.executeAsync (fun read ->
        {| callId = read.string "call_id"
           parentCallId = read.stringOrNone "parent_call_id"
           kind = read.string "kind"
           fnHash = read.stringOrNone "fn_hash"
           lambdaExprId = read.stringOrNone "lambda_expr_id"
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
          let displayName =
            match ev.kind, ev.fnHash with
            | "lambda", _ -> "(lambda)"
            | _, Some name -> name
            | _, None -> "(unknown)"
          let args = parseArgsBytes ev.argsBytes
          let result = parseDvalBytes ev.resultBytes
          let fields =
            Map
              [ "callId", DString ev.callId
                "fnName", DString displayName
                "args", Dval.list dvalKT args
                "result", result
                "durationMs", Dval.int (bigint ev.durationMs)
                "processId",
                (match System.Guid.TryParse ev.processId with
                 | true, g when g <> System.Guid.Empty ->
                   Dval.optionSome KTUuid (DUuid g)
                 | _ -> Dval.optionNone KTUuid)
                "seq", DInt64 ev.seq
                "ord", DInt64 ev.ord ]
          Some(DRecord(typeName, typeName, [], fields))
        with ex ->
          print $"[tracing] dropping corrupt fn_call row: {ex.Message}"
          Telemetry.event
            "trace.row.parseFailed"
            [ "callId", ev.callId; "message", ex.Message ]
          None)

    return enriched |> Dval.list (KTCustomType(typeName, []))
  }


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
      sqlSpec = NotQueryable
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
        + "persistent and no other process."
      fn =
        (function
        | _, _, _, [| DBool on |] ->
          LibDB.Tracing.TraceDetail.setForTesting (
            if on then
              LibDB.Tracing.TraceDetail.On
            else
              LibDB.Tracing.TraceDetail.Off
          )
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "tracesList" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt "Max number of traces to return" ]
      returnType = TList(TCustomType(NR.ok (traceTypeName ()), []))
      description = "The most recent runs, newest first."
      fn =
        (function
        | _, vm, _, [| DInt limitArg |] ->
          let limit = intToInt64 vm limitArg
          uply {
            let typeName = traceTypeName ()
            let! rows =
              Sql.query
                $"SELECT {traceColumns}
                  FROM traces
                  ORDER BY timestamp DESC, rowid DESC
                  LIMIT @limit"
              |> Sql.parameters [ "limit", Sql.int64 limit ]
              |> Sql.executeAsync traceRowToDT

            return rows |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesView" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to view" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (traceDataTypeName ()), []))
      description =
        "View trace details by trace ID. Returns a TraceData record with structured inputs and function calls."
      fn =
        (function
        | _, _, _, [| DString traceID |] ->
          uply {
            // One SELECT covers metadata + input — both live on the trace row.
            let! row =
              Sql.query $"SELECT {traceColumns} FROM traces WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                {| id = read.string "id"
                   trace = traceRowToDT read
                   inputName = read.string "input_name"
                   inputValueBytes = read.bytes "input_value" |})

            let typeName = traceDataTypeName ()
            match row with
            | Some r ->
              let! fnCalls = loadFnCalls r.id
              let inputVarType = inputVarTypeName ()
              let inputFields =
                Map
                  [ "name", DString r.inputName
                    "value", parseDvalBytes r.inputValueBytes ]
              let inputs =
                [ DRecord(inputVarType, inputVarType, [], inputFields) ]
                |> Dval.list (KTCustomType(inputVarType, []))
              let fields =
                Map [ "trace", r.trace; "inputs", inputs; "functionCalls", fnCalls ]
              return
                DRecord(typeName, typeName, [], fields)
                |> Dval.optionSome (KTCustomType(typeName, []))
            | None -> return Dval.optionNone (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesListByFn" 0
      typeParams = []
      parameters =
        [ Param.make "fnName" TString "Function name to search for"
          Param.make "limit" TInt "Max number of traces to return" ]
      returnType = TList(TCustomType(NR.ok (traceTypeName ()), []))
      description = "The most recent runs that called a specific function"
      fn =
        (function
        | _, vm, _, [| DString fnName; DInt limitArg |] ->
          let limit = intToInt64 vm limitArg
          uply {
            // Both builtins and package fns store their display name in
            // fn_hash (resolved at write time), so one LIKE matches either.
            let typeName = traceTypeName ()
            // Escape SQL LIKE wildcards so a literal `%` or `_` in the
            // user-supplied fnName matches a literal char, not any string.
            let escaped =
              fnName
              |> fun s -> s.Replace(@"\", @"\\")
              |> fun s -> s.Replace("%", @"\%")
              |> fun s -> s.Replace("_", @"\_")
            let pattern = $"%%{escaped}%%"
            let cols = traceColumnsOf "t"
            let! rows =
              Sql.query
                $"SELECT DISTINCT {cols}
                  FROM traces t
                  JOIN trace_fn_calls c ON t.id = c.trace_id
                  WHERE c.fn_hash LIKE @pattern ESCAPE '\\'
                  ORDER BY t.timestamp DESC, t.rowid DESC
                  LIMIT @limit"
              |> Sql.parameters
                [ "pattern", Sql.string pattern; "limit", Sql.int64 limit ]
              |> Sql.executeAsync traceRowToDT

            return rows |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesStatsByHandler" 0
      typeParams = []
      parameters =
        [ Param.make "traceLimit" TInt "Aggregate over the last N traces (e.g. 100)" ]
      returnType = TList(TTuple(TString, TInt, [ TInt; TInt ]))
      description =
        "Per-handler aggregate over the last N traces: (handler, traceCount, totalMs, maxMs). Total ms sums every fn-call duration in each trace; per-trace latency would need a separate column on `traces`."
      fn =
        (function
        | _, vm, _, [| DInt traceLimitArg |] ->
          let traceLimit = intToInt64 vm traceLimitArg
          uply {
            // Subquery: the last N trace IDs (and their handler_desc).
            // LEFT JOIN so traces with zero fn_calls still get counted.
            // SUM/MAX of NULL → 0 via COALESCE — sqlite quirk.
            let! rows =
              Sql.query
                "SELECT t.handler_desc AS handler,
                        COUNT(DISTINCT t.id) AS trace_count,
                        COALESCE(SUM(c.duration_ms), 0) AS total_ms,
                        COALESCE(MAX(c.duration_ms), 0) AS max_ms
                 FROM traces t
                 LEFT JOIN trace_fn_calls c ON c.trace_id = t.id
                 WHERE t.id IN (
                   SELECT id FROM traces ORDER BY rowid DESC LIMIT @traceLimit
                 )
                 GROUP BY t.handler_desc
                 ORDER BY trace_count DESC, handler"
              |> Sql.parameters [ "traceLimit", Sql.int64 traceLimit ]
              |> Sql.executeAsync (fun read ->
                {| handler = read.string "handler"
                   traceCount = read.int64 "trace_count"
                   totalMs = read.int64 "total_ms"
                   maxMs = read.int64 "max_ms" |})

            return
              rows
              |> List.map (fun r ->
                DTuple(
                  DString r.handler,
                  Dval.int (bigint r.traceCount),
                  [ Dval.int (bigint r.totalMs); Dval.int (bigint r.maxMs) ]
                ))
              |> Dval.list (KTTuple(VT.string, VT.int, [ VT.int; VT.int ]))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesHotspots" 0
      typeParams = []
      parameters =
        [ Param.make "traceLimit" TInt "Aggregate over the last N traces (e.g. 100)" ]
      returnType = TList(TTuple(TString, TInt, [ TInt; TInt ]))
      description =
        "Aggregate fn-call timing across the last N traces. Returns (fnName, callCount, totalMs, maxMs) tuples sorted by totalMs desc. Lambdas are excluded (no fn_hash to bucket by); builtins included but always have 0ms duration."
      fn =
        (function
        | _, vm, _, [| DInt traceLimitArg |] ->
          let traceLimit = intToInt64 vm traceLimitArg
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
                {| name = read.string "name"
                   callCount = read.int64 "call_count"
                   totalMs = read.int64 "total_ms"
                   maxMs = read.int64 "max_ms" |})

            return
              rows
              |> List.map (fun r ->
                DTuple(
                  DString r.name,
                  Dval.int (bigint r.callCount),
                  [ Dval.int (bigint r.totalMs); Dval.int (bigint r.maxMs) ]
                ))
              |> Dval.list (KTTuple(VT.string, VT.int, [ VT.int; VT.int ]))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    // TODO (perf, foot-gun): we walk every trace + its fn_calls in
    // F#-land, deserialize each Dval to a string repr, then substring-
    // match. The previous shape was SQL `LIKE` over JSON columns —
    // dropped when trace storage went binary. Fine on small dev DBs,
    // O(N×M) on a populated store. Possible mitigations: cache a
    // searchable text repr alongside each row, or use FTS5 over a
    // computed/text-shadow column.
    { name = fn "tracesFind" 0
      typeParams = []
      parameters =
        [ Param.make "pattern" TString "Substring to find in inputs/args/results"
          Param.make "limit" TInt "Max number of traces to return" ]
      returnType = TList(TCustomType(NR.ok (traceTypeName ()), []))
      description =
        "List runs whose recorded input or any logged call's args/result contains the substring "
        + "(case-sensitive), matched against the developer-repr form of each Dval. Searches the "
        + "newest 2000 runs and stops at `limit` matches."
      fn =
        (function
        | exeState, vm, _, [| DString pattern; DInt limitArg |] ->
          let limit = intToInt64 vm limitArg
          uply {
            let typeName = traceTypeName ()

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
                $"SELECT {traceColumns} FROM traces
                  ORDER BY timestamp DESC, rowid DESC LIMIT 2000"
              |> Sql.executeAsync (fun read ->
                {| id = read.string "id"
                   row = traceRowToDT read
                   inputBytes = read.bytes "input_value"
                   resultBytes = read.bytesOrNone "result_value" |})

            let mutable hits : List<Dval> = []
            let mutable cursor = 0

            while cursor < List.length traces && int64 (List.length hits) < limit do
              let t = traces[cursor]
              cursor <- cursor + 1

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
                        "SELECT args, result FROM trace_fn_calls
                         WHERE trace_id = @traceId"
                      |> Sql.parameters [ "traceId", Sql.string t.id ]
                      |> Sql.executeAsync (fun read ->
                        {| argsBytes = read.bytes "args"
                           resultBytes = read.bytes "result" |})

                    let mutable found = false
                    let mutable i = 0
                    while not found && i < List.length callRows do
                      let row = callRows[i]
                      i <- i + 1
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
                        found <- true
                      else
                        let! resultMatch = containsPattern resultDval
                        if resultMatch then found <- true
                    return found
                  }

              if matchesViaCalls then hits <- t.row :: hits

            return hits |> List.rev |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesGetInput" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to get input from" ]
      returnType = TypeReference.option TString
      description =
        "Get the stored input code for a trace (eval / run only — HTTP traces, whose input is a Request record, return None)."
      fn =
        (function
        | _, _, _, [| DString traceID |] ->
          uply {
            let! row =
              Sql.query "SELECT input_value FROM traces WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read -> read.bytes "input_value")

            match row with
            | None -> return Dval.optionNone KTString
            | Some valueBytes ->
              try
                let dval =
                  BinarySer.RT.Dval.deserialize "traces.input_value" valueBytes
                match dval with
                | DString code -> return Dval.optionSome KTString (DString code)
                | _ -> return Dval.optionNone KTString
              with ex ->
                print $"[traces] Failed to parse input for replay: {ex.Message}"
                return Dval.optionNone KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesClearBefore" 0
      typeParams = []
      parameters =
        [ Param.make
            "cutoffISO"
            TString
            "ISO 8601 timestamp (e.g. 2026-05-02T01:00:00Z); traces with timestamp < cutoff are deleted." ]
      returnType = TInt
      description =
        "Delete runs older than the given cutoff, with their calls. Returns count deleted. Caller is responsible for computing the cutoff (e.g. `DateTime.now() |> subtractSeconds 3600` for 'last hour')."
      fn =
        (function
        | _, _, _, [| DString cutoffISO |] ->
          uply {
            // The timestamp column is ISO 8601, which sorts as text the way it sorts as time.
            let! ids =
              Sql.query "SELECT id FROM traces WHERE timestamp < @cutoff"
              |> Sql.parameters [ "cutoff", Sql.string cutoffISO ]
              |> Sql.executeAsync (fun read -> read.string "id")
            LibDB.Tracing.TraceRetention.deleteTraces ids
            return Dval.int (bigint (List.length ids))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "tracesClear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "Ignored" ]
      returnType = TInt
      description = "Delete every run and its calls. Returns how many runs went."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            let! count =
              Sql.query "SELECT COUNT(*) as c FROM traces"
              |> Sql.executeRowAsync (fun read -> read.int64 "c")
            Sql.executeTransactionSync
              [ ("DELETE FROM trace_fn_calls", [ [] ])
                ("DELETE FROM trace_fns", [ [] ])
                ("DELETE FROM traces", [ [] ]) ]
            |> ignore<List<int>>
            return Dval.int (bigint count)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "tracesDelete" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "Full trace ID to delete" ]
      returnType = TInt
      description =
        "Delete one trace, its calls and the execution it was the log of. Returns 1 if a row was deleted, 0 otherwise. Caller is responsible for resolving prefixes via tracesResolveID first."
      fn =
        (function
        | _, _, _, [| DString traceID |] ->
          uply {
            let! existed =
              Sql.query "SELECT 1 AS x FROM traces WHERE id = @traceId LIMIT 1"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun _ -> ())
            match existed with
            | None -> return Dval.int 0I
            | Some _ ->
              LibDB.Tracing.TraceRetention.deleteTraces [ traceID ]
              return Dval.int 1I
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "tracesPruneKeep" 0
      typeParams = []
      parameters = [ Param.make "keepN" TInt "Number of most-recent traces to keep" ]
      returnType = TInt
      description =
        "Delete all but the N most recent traces, keeping any a suspended execution still needs; the same pass retention runs after every store. Returns the count deleted."
      fn =
        (function
        | _, vm, _, [| DInt keepNArg |] ->
          let keepN = intToInt64 vm keepNArg
          Dval.int (bigint (LibDB.Tracing.TraceRetention.prune (Some keepN) None))
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceWrite ]
      deprecated = NotDeprecated }


    // ───────── a trace is a run: getting one, resuming it, forking it, pinning it ─────────

    { name = fn "tracesGet" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "" ]
      returnType = TypeReference.option (TCustomType(NR.ok (traceTypeName ()), []))
      description = "One run, or None for an id nobody has."
      fn =
        (function
        | _, _, _, [| DString traceID |] ->
          uply {
            let typeName = traceTypeName ()
            let! row =
              Sql.query $"SELECT {traceColumns} FROM traces WHERE id = @id"
              |> Sql.parameters [ "id", Sql.string traceID ]
              |> Sql.executeRowOptionAsync traceRowToDT
            return Dval.option (KTCustomType(typeName, [])) row
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "tracesRunsCalling" 0
      typeParams = []
      parameters =
        [ Param.make "fnName" TString "the function's dotted name"
          Param.make "limit" TInt64 "" ]
      returnType =
        TList(TCustomType(NR.ok (FQTypeName.fqPackage (TracesRefs.trace ())), []))
      description =
        "The runs that went through this function, newest first. Read from the names-only "
        + "index every recorded run writes, so it answers at the shipped recording level."
      fn =
        (function
        | _, _, _, [| DString fnName; DInt64 limit |] ->
          uply {
            let typeName = FQTypeName.fqPackage (TracesRefs.trace ())
            let cols = traceColumnsOf "t"
            let! rows =
              Sql.query
                $"SELECT {cols}
                  FROM traces t
                  JOIN trace_fns f ON f.trace_id = t.id
                  WHERE f.fn_name = @fn
                  ORDER BY t.timestamp DESC, t.rowid DESC
                  LIMIT @limit"
              |> Sql.parameters [ "fn", Sql.string fnName; "limit", Sql.int64 limit ]
              |> Sql.executeAsync traceRowToDT
            return rows |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.TraceRead ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
