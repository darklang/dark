/// Builtin functions for querying the trace store.
/// Companion to `LibDB.Tracing` (the recorder side).
module Builtins.Matter.Libs.Traces

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Sqlite

module Dval = LibExecution.Dval
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
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
let traceSummaryTypeName () = FQTypeName.fqPackage (TracesRefs.traceSummary ())
let inputVarTypeName () = FQTypeName.fqPackage (TracesRefs.inputVar ())
let fnCallTypeName () = FQTypeName.fqPackage (TracesRefs.fnCall ())
let traceDataTypeName () = FQTypeName.fqPackage (TracesRefs.traceData ())


let private parseDvalJson (json : string) : Dval =
  json |> DvalReprInternalRoundtrippable.parseJsonV0 |> RT2DT.Dval.toDT

let private parseArgsJson (argsJson : string) : List<Dval> =
  use doc = JsonDocument.Parse(argsJson)
  doc.RootElement.EnumerateArray()
  |> Seq.toList
  |> List.map (fun el -> el.GetRawText() |> parseDvalJson)


/// Load call events for a trace, ordered by rowid (= execution order).
/// Args/result are stored as JSON inline; parse them per row. The display
/// name was resolved at write time and lives in fn_hash, so reads are a
/// flat SELECT — lambdas have NULL fn_hash and render as "(lambda)".
let private loadFnCalls (traceId : string) : Ply<Dval> =
  let typeName = fnCallTypeName ()
  let dvalKT = KTCustomType(dvalTypeName (), [])
  uply {
    let! events =
      Sql.query
        "SELECT call_id, parent_call_id, kind, fn_hash, lambda_expr_id,
                args_json, result_json, duration_ms
         FROM trace_fn_calls
         WHERE trace_id = @traceId
         ORDER BY rowid"
      |> Sql.parameters [ "traceId", Sql.string traceId ]
      |> Sql.executeAsync (fun read ->
        {| callId = read.string "call_id"
           parentCallId = read.stringOrNone "parent_call_id"
           kind = read.string "kind"
           fnHash = read.stringOrNone "fn_hash"
           lambdaExprId = read.stringOrNone "lambda_expr_id"
           argsJson = read.string "args_json"
           resultJson = read.string "result_json"
           durationMs = read.int64 "duration_ms" |})

    // Skip rows whose args_json / result_json fail to parse rather
    // than substitute a placeholder Dval — the downstream renderer
    // expects each FnCall record's `args` / `result` to be the
    // canonical Dval custom type, and a stand-in DString fails the
    // type check at apply time. One bad row used to abort the whole
    // tracesView / call-tree render via an unhandled raise from
    // parseArgsJson / parseDvalJson; now we log + telemetry + drop
    // the row, so the rest of the trace still renders.
    let enriched =
      events
      |> List.choose (fun ev ->
        try
          let displayName =
            match ev.kind, ev.fnHash with
            | "lambda", _ -> "(lambda)"
            | _, Some name -> name
            | _, None -> "(unknown)"
          let args = parseArgsJson ev.argsJson
          let result = parseDvalJson ev.resultJson
          let parentCallIdDval =
            match ev.parentCallId with
            | Some p -> Dval.optionSome KTString (DString p)
            | None -> Dval.optionNone KTString
          let lambdaExprIdDval =
            match ev.lambdaExprId with
            | Some i -> Dval.optionSome KTString (DString i)
            | None -> Dval.optionNone KTString
          let fields =
            Map
              [ "callId", DString ev.callId
                "parentCallId", parentCallIdDval
                "kind", DString ev.kind
                "fnName", DString displayName
                "lambdaExprId", lambdaExprIdDval
                "args", Dval.list dvalKT args
                "result", result
                "durationMs", DInt64 ev.durationMs ]
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
  [ { name = fn "tracesList" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt64 "Max number of traces to return" ]
      returnType = TList(TVariable "a")
      description = "List recent traces"
      fn =
        (function
        | _, _, _, [ DInt64 limit ] ->
          uply {
            let typeName = traceSummaryTypeName ()
            let! rows =
              Sql.query
                "SELECT id, handler_desc, timestamp
                 FROM traces
                 ORDER BY rowid DESC
                 LIMIT @limit"
              |> Sql.parameters [ "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                {| id = read.string "id"
                   handler = read.string "handler_desc"
                   timestamp = read.string "timestamp" |})

            return
              rows
              |> List.map (fun r ->
                let fields =
                  Map
                    [ "traceId", DString r.id
                      "handler", DString r.handler
                      "timestamp", DString r.timestamp ]
                DRecord(typeName, typeName, [], fields))
              |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesView" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to view" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "View trace details by trace ID. Returns a TraceData record with structured inputs and function calls."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            // One SELECT covers metadata + input — both live on the trace row.
            let! row =
              Sql.query
                "SELECT id, handler_desc, timestamp, input_name, input_value_json
                 FROM traces
                 WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                {| id = read.string "id"
                   handlerDesc = read.string "handler_desc"
                   timestamp = read.string "timestamp"
                   inputName = read.string "input_name"
                   inputValueJson = read.string "input_value_json" |})

            let typeName = traceDataTypeName ()
            match row with
            | Some r ->
              let! fnCalls = loadFnCalls r.id
              let inputVarType = inputVarTypeName ()
              let inputFields =
                Map
                  [ "name", DString r.inputName
                    "value", parseDvalJson r.inputValueJson ]
              let inputs =
                [ DRecord(inputVarType, inputVarType, [], inputFields) ]
                |> Dval.list (KTCustomType(inputVarType, []))
              let fields =
                Map
                  [ "traceId", DString r.id
                    "handler", DString r.handlerDesc
                    "timestamp", DString r.timestamp
                    "inputs", inputs
                    "functionCalls", fnCalls ]
              return
                DRecord(typeName, typeName, [], fields)
                |> Dval.optionSome (KTCustomType(typeName, []))
            | None -> return Dval.optionNone (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesListByFn" 0
      typeParams = []
      parameters =
        [ Param.make "fnName" TString "Function name to search for"
          Param.make "limit" TInt64 "Max number of traces to return" ]
      returnType = TList(TVariable "a")
      description = "List traces that called a specific function"
      fn =
        (function
        | _, _, _, [ DString fnName; DInt64 limit ] ->
          uply {
            // Both builtins and package fns store their display name in
            // fn_hash (resolved at write time), so one LIKE matches either.
            let typeName = traceSummaryTypeName ()
            // Escape SQL LIKE wildcards so a literal `%` or `_` in the
            // user-supplied fnName matches a literal char, not any string.
            let escaped =
              fnName
              |> fun s -> s.Replace(@"\", @"\\")
              |> fun s -> s.Replace("%", @"\%")
              |> fun s -> s.Replace("_", @"\_")
            let pattern = $"%%{escaped}%%"
            let! rows =
              Sql.query
                "SELECT DISTINCT t.id, t.handler_desc, t.timestamp
                 FROM traces t
                 JOIN trace_fn_calls c ON t.id = c.trace_id
                 WHERE c.fn_hash LIKE @pattern ESCAPE '\\'
                 ORDER BY t.rowid DESC
                 LIMIT @limit"
              |> Sql.parameters
                [ "pattern", Sql.string pattern; "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                {| id = read.string "id"
                   handler = read.string "handler_desc"
                   timestamp = read.string "timestamp" |})

            return
              rows
              |> List.map (fun r ->
                let fields =
                  Map
                    [ "traceId", DString r.id
                      "handler", DString r.handler
                      "timestamp", DString r.timestamp ]
                DRecord(typeName, typeName, [], fields))
              |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesStatsByHandler" 0
      typeParams = []
      parameters =
        [ Param.make
            "traceLimit"
            TInt64
            "Aggregate over the last N traces (e.g. 100)" ]
      returnType = TList(TTuple(TString, TInt64, [ TInt64; TInt64 ]))
      description =
        "Per-handler aggregate over the last N traces: (handler, traceCount, totalMs, maxMs). Total ms sums every fn-call duration in each trace; per-trace latency would need a separate column on `traces`."
      fn =
        (function
        | _, _, _, [ DInt64 traceLimit ] ->
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
                  DInt64 r.traceCount,
                  [ DInt64 r.totalMs; DInt64 r.maxMs ]
                ))
              |> Dval.list (KTTuple(VT.string, VT.int64, [ VT.int64; VT.int64 ]))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesHotspots" 0
      typeParams = []
      parameters =
        [ Param.make
            "traceLimit"
            TInt64
            "Aggregate over the last N traces (e.g. 100)" ]
      returnType = TList(TTuple(TString, TInt64, [ TInt64; TInt64 ]))
      description =
        "Aggregate fn-call timing across the last N traces. Returns (fnName, callCount, totalMs, maxMs) tuples sorted by totalMs desc. Lambdas are excluded (no fn_hash to bucket by); builtins included but always have 0ms duration."
      fn =
        (function
        | _, _, _, [ DInt64 traceLimit ] ->
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
                  DInt64 r.callCount,
                  [ DInt64 r.totalMs; DInt64 r.maxMs ]
                ))
              |> Dval.list (KTTuple(VT.string, VT.int64, [ VT.int64; VT.int64 ]))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // TODO (perf, foot-gun): the LEFT JOIN below produces traces ×
    // trace_fn_calls cardinality before DISTINCT collapses it, and
    // the LIKE matches over full args_json / result_json (no index
    // support). Fine on small dev DBs, slow when the trace store has
    // grown. Possible mitigations: add a generated/indexed
    // "summary" column, or use FTS5. Not a correctness bug.
    { name = fn "tracesFind" 0
      typeParams = []
      parameters =
        [ Param.make "pattern" TString "Substring to find in inputs/args/results"
          Param.make "limit" TInt64 "Max number of traces to return" ]
      returnType = TList(TVariable "a")
      description =
        "List traces whose recorded input or any fn-call args/result contains the substring (case-sensitive). Searches the JSON form, so values like `\"errCode\":503` work."
      fn =
        (function
        | _, _, _, [ DString pattern; DInt64 limit ] ->
          uply {
            let typeName = traceSummaryTypeName ()
            // Escape SQL LIKE wildcards (%, _) and the escape char (\)
            // so a user pattern like `errCode_503` matches the literal
            // string and not "errCode" + any-char + "503". Without this,
            // `find %` and `find _` would match every trace.
            let escaped =
              pattern
              |> fun s -> s.Replace(@"\", @"\\")
              |> fun s -> s.Replace("%", @"\%")
              |> fun s -> s.Replace("_", @"\_")
            let likePattern = $"%%{escaped}%%"
            let! rows =
              Sql.query
                "SELECT DISTINCT t.id, t.handler_desc, t.timestamp, t.rowid AS rid
                 FROM traces t
                 LEFT JOIN trace_fn_calls c ON t.id = c.trace_id
                 WHERE t.input_value_json LIKE @pattern ESCAPE '\\'
                    OR c.args_json LIKE @pattern ESCAPE '\\'
                    OR c.result_json LIKE @pattern ESCAPE '\\'
                 ORDER BY t.rowid DESC
                 LIMIT @limit"
              |> Sql.parameters
                [ "pattern", Sql.string likePattern; "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                {| id = read.string "id"
                   handler = read.string "handler_desc"
                   timestamp = read.string "timestamp" |})

            return
              rows
              |> List.map (fun r ->
                let fields =
                  Map
                    [ "traceId", DString r.id
                      "handler", DString r.handler
                      "timestamp", DString r.timestamp ]
                DRecord(typeName, typeName, [], fields))
              |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesResolveID" 0
      typeParams = []
      parameters =
        [ Param.make "input" TString "Full trace ID or unambiguous prefix" ]
      returnType = TypeReference.result TString TString
      description =
        "Resolve a trace ID prefix to the full ID. `Ok fullID` when exactly one trace matches; `Error <message>` when zero (not-found) or multiple (ambiguous, lists candidates)."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString
        (function
        | _, _, _, [ DString input ] ->
          uply {
            // Up to 6 so we can distinguish "1 match" / "many matches"
            // without fetching everything. The user-facing error caps the
            // candidate list at 5 anyway.
            //
            // Escape `%` and `_` so a user-supplied prefix matches literal
            // chars (consistent with tracesListByFn / tracesFind).
            let escaped =
              input
              |> fun s -> s.Replace(@"\", @"\\")
              |> fun s -> s.Replace("%", @"\%")
              |> fun s -> s.Replace("_", @"\_")
            let pattern = escaped + "%"
            let! matches =
              Sql.query
                "SELECT id FROM traces
                 WHERE id LIKE @pattern ESCAPE '\\'
                 ORDER BY rowid DESC
                 LIMIT 6"
              |> Sql.parameters [ "pattern", Sql.string pattern ]
              |> Sql.executeAsync (fun read -> read.string "id")

            match matches with
            | [] -> return resultError (DString $"Trace not found: {input}")
            | [ fullID ] -> return resultOk (DString fullID)
            | candidates ->
              let shown = List.truncate 5 candidates
              let suffix =
                if List.length candidates > 5 then
                  $"\n  - … (and {List.length candidates - 5} more)"
                else
                  ""
              let body =
                shown |> List.map (fun id -> $"  - {id}") |> String.concat "\n"
              return
                resultError (
                  DString $"Ambiguous trace ID '{input}'. Matches:\n{body}{suffix}"
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesGetInput" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to get input from" ]
      returnType = TypeReference.option TString
      description =
        "Get the stored input code for a trace (eval / run only — HTTP traces, whose input is a Request record, return None)."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! row =
              Sql.query "SELECT input_value_json FROM traces WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                read.string "input_value_json")

            match row with
            | None -> return Dval.optionNone KTString
            | Some valueJson ->
              try
                let dval = DvalReprInternalRoundtrippable.parseJsonV0 valueJson
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
      deprecated = NotDeprecated }


    { name = fn "tracesClearBefore" 0
      typeParams = []
      parameters =
        [ Param.make
            "cutoffISO"
            TString
            "ISO 8601 timestamp (e.g. 2026-05-02T01:00:00Z); traces with timestamp < cutoff are deleted." ]
      returnType = TInt64
      description =
        "Delete traces older than the given cutoff (and their fn_calls). Returns count deleted. Caller is responsible for computing the cutoff (e.g. `DateTime.now() |> subtractSeconds 3600` for 'last hour')."
      fn =
        (function
        | _, _, _, [ DString cutoffISO ] ->
          uply {
            // Timestamp column is ISO 8601 ("2026-05-02T02:03:53Z") which
            // sorts lexicographically — string compare works as date compare.
            let countToDelete =
              Sql.query "SELECT COUNT(*) AS c FROM traces WHERE timestamp < @cutoff"
              |> Sql.parameters [ "cutoff", Sql.string cutoffISO ]
              |> Sql.executeRowAsync (fun read -> read.int64 "c")
            let! count = countToDelete

            // Both DELETEs run in one transaction so an interrupt can't
            // leave fn_calls orphan rows pointing at a deleted trace.
            // There's no FK cascade by design (schema kept additive for
            // migration ease), so cleanup is purely procedural.
            if count > 0L then
              let p = [ [ "cutoff", Sql.string cutoffISO ] ]
              let _ =
                Sql.executeTransactionSync
                  [ ("DELETE FROM trace_fn_calls
                      WHERE trace_id IN (
                        SELECT id FROM traces WHERE timestamp < @cutoff
                      )",
                     p)
                    ("DELETE FROM traces WHERE timestamp < @cutoff", p) ]
              ()

            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesClear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "Ignored" ]
      returnType = TInt64
      description = "Delete all traces, returns count deleted"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! count =
              Sql.query "SELECT COUNT(*) as c FROM traces"
              |> Sql.executeRowAsync (fun read -> read.int64 "c")
            // Both DELETEs in one transaction (same shape as
            // tracesClearBefore — no FK cascade in the schema).
            let _ =
              Sql.executeTransactionSync
                [ ("DELETE FROM trace_fn_calls", [ [] ])
                  ("DELETE FROM traces", [ [] ]) ]
            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesDelete" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "Full trace ID to delete" ]
      returnType = TInt64
      description =
        "Delete one trace (and its fn_calls). Returns 1 if a row was deleted, 0 otherwise. Caller is responsible for resolving prefixes via tracesResolveID first."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! existed =
              Sql.query "SELECT 1 AS x FROM traces WHERE id = @traceId LIMIT 1"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun _ -> ())
            match existed with
            | None -> return DInt64 0L
            | Some _ ->
              do!
                Sql.query "DELETE FROM trace_fn_calls WHERE trace_id = @traceId"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeStatementAsync
              do!
                Sql.query "DELETE FROM traces WHERE id = @traceId"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeStatementAsync
              return DInt64 1L
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "tracesPruneKeep" 0
      typeParams = []
      parameters =
        [ Param.make "keepN" TInt64 "Number of most-recent traces to keep" ]
      returnType = TInt64
      description =
        "Delete all but the N most-recent traces (and their fn_calls). Returns the count deleted. Useful for bounded retention."
      fn =
        (function
        | _, _, _, [ DInt64 keepN ] ->
          uply {
            // Subquery picks the rowids to keep; outer DELETE removes the rest.
            // Wipe child rows first to avoid dangling fn_calls — there's no
            // FK cascade in the schema (kept additive for migration ease).
            let countToDelete =
              Sql.query
                "SELECT COUNT(*) AS c FROM traces
                 WHERE rowid NOT IN (
                   SELECT rowid FROM traces ORDER BY rowid DESC LIMIT @keepN
                 )"
              |> Sql.parameters [ "keepN", Sql.int64 keepN ]
              |> Sql.executeRowAsync (fun read -> read.int64 "c")
            let! count = countToDelete

            // Both DELETEs run in one transaction. The "keep N most-
            // recent rowids" subquery is repeated in each statement; the
            // transaction's snapshot keeps the three evaluations
            // (count + two DELETEs) consistent — without it, a
            // concurrent insert between the count and the first DELETE
            // (or between the two DELETEs) would let them see different
            // "kept" sets and orphan child rows.
            if count > 0L then
              let p = [ [ "keepN", Sql.int64 keepN ] ]
              let _ =
                Sql.executeTransactionSync
                  [ ("DELETE FROM trace_fn_calls WHERE trace_id IN (
                       SELECT id FROM traces
                       WHERE rowid NOT IN (
                         SELECT rowid FROM traces ORDER BY rowid DESC LIMIT @keepN
                       )
                     )",
                     p)
                    ("DELETE FROM traces
                      WHERE rowid NOT IN (
                        SELECT rowid FROM traces ORDER BY rowid DESC LIMIT @keepN
                      )",
                     p) ]
              ()

            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
