/// Builtin functions for querying the trace store. Companion to
/// `LibDB.Tracing` (the recorder side).
///
/// Originally lived in `Builtins.CliHost/Libs/Traces.fs` with a `cli`
/// prefix, when traces were CLI-only (eval/run). Now that HTTP traces
/// flow through the same path, the placement and naming were stale —
/// moved here and renamed to drop the prefix.
module Builtins.Matter.Libs.Traces

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibSqlite.Db

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

    let enriched =
      events
      |> List.map (fun ev ->
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
        DRecord(typeName, typeName, [], fields))

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
      deprecated = NotDeprecated
      accessibility = Any }


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
      deprecated = NotDeprecated
      accessibility = Any }


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
      deprecated = NotDeprecated
      accessibility = Any }


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
      deprecated = NotDeprecated
      accessibility = Any }


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
      deprecated = NotDeprecated
      accessibility = Any }


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
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesGetExprValues" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "Trace to read expr values from" ]
      returnType = TList(TTuple(TString, TVariable "a", []))
      description =
        "Per-AST-node values recorded via the `traceDval` hook. Returns (exprId-as-string, Dval) tuples ordered by exprId. Captures `let` RHS, `if` branch result, matched `match` arm, and pipe-stage results — see PT2RT's `TraceDval` emissions."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! rows =
              Sql.query
                "SELECT expr_id, dval_json FROM trace_expr_values
                 WHERE trace_id = @traceId
                 ORDER BY expr_id"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeAsync (fun read ->
                {| exprId = read.string "expr_id"
                   dvalJson = read.string "dval_json" |})

            return
              rows
              |> List.map (fun r ->
                let dval = parseDvalJson r.dvalJson
                DTuple(DString r.exprId, dval, []))
              |> Dval.list (KTTuple(VT.string, VT.unknown, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


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
            let pattern = input + "%"
            let! matches =
              Sql.query
                "SELECT id FROM traces
                 WHERE id LIKE @pattern
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
                shown
                |> List.map (fun id -> $"  - {id}")
                |> String.concat "\n"
              return
                resultError (
                  DString $"Ambiguous trace ID '{input}'. Matches:\n{body}{suffix}"
                )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesGetInput" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to get input from" ]
      returnType = TypeReference.option TString
      description = "Get the stored input code for a trace"
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
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesGetExpectedOutput" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID" ]
      returnType = TypeReference.option TString
      description =
        "Render the trace's last top-level fn-call result as the same string format `cliEvaluateExpression` returns. Returns None when the trace has no top-level fn calls (e.g. eval of a literal)."
      fn =
        (function
        | exeState, _, _, [ DString traceID ] ->
          uply {
            // Last top-level call's result_json is the eval expression's
            // final value — eval wraps each top-level expression as its
            // own top call; the displayed answer is the last one.
            let! row =
              Sql.query
                "SELECT result_json FROM trace_fn_calls
                 WHERE trace_id = @traceId AND parent_call_id IS NULL
                 ORDER BY rowid DESC LIMIT 1"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read -> read.string "result_json")

            match row with
            | None -> return Dval.optionNone KTString
            | Some resultJson ->
              try
                let dval = DvalReprInternalRoundtrippable.parseJsonV0 resultJson
                let! rendered = LibExecution.Execution.dvalToRepr exeState dval
                return Dval.optionSome KTString (DString rendered)
              with ex ->
                print
                  $"[traces] Failed to parse recorded output for diff: {ex.Message}"
                return Dval.optionNone KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesExportJson" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to export" ]
      returnType = TypeReference.option TString
      description =
        "Serialize a trace (metadata + input + all fn calls) to a portable JSON string. Returns None if the trace doesn't exist."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
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

            match row with
            | None -> return Dval.optionNone KTString
            | Some r ->
              let! events =
                Sql.query
                  "SELECT call_id, parent_call_id, kind, fn_hash,
                          lambda_expr_id, args_json, result_json, duration_ms
                   FROM trace_fn_calls
                   WHERE trace_id = @traceId
                   ORDER BY rowid"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeAsync (fun read ->
                  {| callId = read.string "call_id"
                     parentCallId = read.stringOrNone "parent_call_id"
                     kind = read.string "kind"
                     fnHash = read.stringOrNone "fn_hash"
                     lambdaExprId = read.stringOrNone "lambda_expr_id"
                     argsJson = read.string "args_json"
                     resultJson = read.string "result_json"
                     durationMs = read.int64 "duration_ms" |})

              let! exprValues =
                Sql.query
                  "SELECT expr_id, dval_json FROM trace_expr_values
                   WHERE trace_id = @traceId
                   ORDER BY expr_id"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeAsync (fun read ->
                  {| exprId = read.string "expr_id"
                     dvalJson = read.string "dval_json" |})

              // Build the export object as Utf8JsonWriter so the embedded
              // dval JSON strings (`input_value_json`, `args_json`,
              // `result_json`) round-trip as objects, not stringified
              // objects. That way `import` can re-parse cleanly.
              let stream = new System.IO.MemoryStream()
              let opts =
                new System.Text.Json.JsonWriterOptions(
                  Indented = true,
                  SkipValidation = false
                )
              let w = new System.Text.Json.Utf8JsonWriter(stream, opts)
              let writeRaw (json : string) =
                if System.String.IsNullOrWhiteSpace(json) then
                  w.WriteNullValue()
                else
                  w.WriteRawValue(json, skipInputValidation = true)

              w.WriteStartObject()
              w.WriteString("id", r.id)
              w.WriteString("handler_desc", r.handlerDesc)
              w.WriteString("timestamp", r.timestamp)
              w.WriteString("input_name", r.inputName)
              w.WritePropertyName("input_value")
              writeRaw r.inputValueJson
              w.WriteStartArray("fn_calls")
              for ev in events do
                w.WriteStartObject()
                w.WriteString("call_id", ev.callId)
                match ev.parentCallId with
                | Some p -> w.WriteString("parent_call_id", p)
                | None -> w.WriteNull("parent_call_id")
                w.WriteString("kind", ev.kind)
                match ev.fnHash with
                | Some h -> w.WriteString("fn_hash", h)
                | None -> w.WriteNull("fn_hash")
                match ev.lambdaExprId with
                | Some l -> w.WriteString("lambda_expr_id", l)
                | None -> w.WriteNull("lambda_expr_id")
                w.WritePropertyName("args")
                writeRaw ev.argsJson
                w.WritePropertyName("result")
                writeRaw ev.resultJson
                w.WriteNumber("duration_ms", ev.durationMs)
                w.WriteEndObject()
              w.WriteEndArray()
              // Per-AST-node values (let bindings, if results, match
              // arms, pipe stages — see TraceDval). Required for
              // `view --with-trace` / `traces inspect` to work after
              // a re-import. Older exports omit this field; import
              // tolerates absence.
              w.WriteStartArray("expr_values")
              for ev in exprValues do
                w.WriteStartObject()
                w.WriteString("expr_id", ev.exprId)
                w.WritePropertyName("dval")
                writeRaw ev.dvalJson
                w.WriteEndObject()
              w.WriteEndArray()
              w.WriteEndObject()
              w.Flush()
              let json = UTF8.ofBytesUnsafe (stream.ToArray())
              return Dval.optionSome KTString (DString json)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesReplayHttp" 0
      typeParams = []
      parameters =
        [ Param.make "traceID" TString "HTTP trace to replay"
          Param.make
            "withDiff"
            TBool
            "When true, also load the recorded response and diff against the new one." ]
      returnType = TypeReference.result TString TString
      description =
        "Replay a recorded HTTP request against the current version of the handler that produced it. With `withDiff = true`, returns a recorded-vs-new diff (status + body); otherwise returns just the new response. The handler is resolved by name from the trace's top-level fn_hash, then re-resolved on the current branch — so this answers 'did my refactor change behavior on this past request?' (diff mode) or 'what does this trace produce now?' (run mode)."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString

        let asRecord (d : Dval) : Option<Map<string, Dval>> =
          match d with
          | DRecord(_, _, _, fields) -> Some fields
          | _ -> None
        let asInt64 (d : Dval) : Option<int64> =
          match d with
          | DInt64 n -> Some n
          | _ -> None

        // Body in the trace is always Persistent (post-promotion).
        // Body in the live response is Ephemeral (just executed). Handle
        // both.
        let blobBytes (exeState : ExecutionState) (d : Dval) : Ply<byte[]> =
          uply {
            match d with
            | DBlob(Persistent(hash, _)) ->
              let! bs = LibDB.RuntimeTypes.Blob.get hash
              return Option.defaultValue [||] bs
            | DBlob(Ephemeral id) ->
              let mutable bs : byte[] = null
              if exeState.blobStore.TryGetValue(id, &bs) then return bs
              else return [||]
            | _ -> return [||]
          }

        // Render bytes as a string for diff display. Truncate long
        // bodies so the diff stays scannable; full bytes still match
        // exactly via the byte-equality check.
        let renderBody (bytes : byte[]) : string =
          let s = UTF8.ofBytesOpt bytes |> Option.defaultValue $"<{bytes.Length} bytes>"
          if s.Length > 200 then s.Substring(0, 200) + "…" else s

        (function
        | exeState, _, _, [ DString traceID; DBool withDiff ] ->
          uply {
            let! traceRow =
              Sql.query
                "SELECT handler_desc, input_value_json
                 FROM traces WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                {| handlerDesc = read.string "handler_desc"
                   inputValueJson = read.string "input_value_json" |})

            match traceRow with
            | None -> return resultError (DString $"Trace not found: {traceID}")
            | Some r when not (r.handlerDesc.Contains(" /")) ->
              return
                resultError (
                  DString
                    $"Trace {traceID} isn't an HTTP trace (handler_desc: {r.handlerDesc}). Use `traces replay <id> --diff` for eval traces."
                )
            | Some r ->
              let! topRow =
                Sql.query
                  "SELECT result_json, fn_hash FROM trace_fn_calls
                   WHERE trace_id = @traceId AND parent_call_id IS NULL
                   ORDER BY rowid DESC LIMIT 1"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeRowOptionAsync (fun read ->
                  {| resultJson = read.string "result_json"
                     fnName = read.stringOrNone "fn_hash" |})

              match topRow with
              | None ->
                return
                  resultError (
                    DString
                      $"Trace {traceID} has no top-level call; can't identify the handler to replay against."
                  )
              | Some top when top.fnName.IsNone ->
                return
                  resultError (
                    DString
                      $"Trace {traceID}'s top-level call is a lambda (no qualified name). Replay needs a named fn to re-resolve on the current branch."
                  )
              | Some top ->
                // Parse the recorded fn name back into a PackageLocation.
                // fn_hash stored the dotted name post-fnNameToSimpleString,
                // not a hex hash.
                let handlerName = top.fnName.Value
                let parts = handlerName.Split('.')
                if parts.Length < 2 then
                  return
                    resultError (
                      DString
                        $"Recorded handler name '{handlerName}' isn't a package fn (likely a builtin); can't replay."
                    )
                else
                  let owner = parts[0]
                  let name = parts[parts.Length - 1]
                  let modules =
                    parts[1 .. parts.Length - 2] |> Array.toList
                  let location : PT.PackageLocation =
                    { owner = owner; modules = modules; name = name }

                  let! branchChain =
                    LibDB.Branches.getBranchChain
                      exeState.branchId
                  let! hashOpt =
                    LibDB.ProgramTypes.Fn.find branchChain location

                  match hashOpt with
                  | None ->
                    return
                      resultError (
                        DString
                          $"Handler `{handlerName}` not found on the current branch (deleted, renamed, or unlisted?)."
                      )
                  | Some hash ->
                    let (PT.Hash hashStr) = hash
                    let fqName = FQFnName.fqPackage hashStr
                    let requestDval =
                      DvalReprInternalRoundtrippable.parseJsonV0
                        r.inputValueJson
                    let! result =
                      Execution.executeFunction
                        exeState
                        fqName
                        []
                        (NEList.singleton requestDval)
                    match result with
                    | Error(rte, _) ->
                      let! errStr = Execution.runtimeErrorToString exeState rte
                      let errMsg =
                        match errStr with
                        | Ok(DString s) -> s
                        | _ -> string rte
                      return
                        resultError (
                          DString $"Replay failed during execution: {errMsg}"
                        )
                    | Ok newRespDval ->
                      match asRecord newRespDval with
                      | None ->
                        return
                          resultError (
                            DString
                              "Replayed result isn't a Stdlib.Http.Response record."
                          )
                      | Some newFields ->
                        let newStatus =
                          Map.tryFind "statusCode" newFields
                          |> Option.bind asInt64
                          |> Option.defaultValue -1L
                        let! newBody =
                          Map.tryFind "body" newFields
                          |> Option.map (blobBytes exeState)
                          |> Option.defaultWith (fun () ->
                            uply { return [||] })

                        if withDiff then
                          let recordedRespDval =
                            DvalReprInternalRoundtrippable.parseJsonV0
                              top.resultJson
                          match asRecord recordedRespDval with
                          | None ->
                            return
                              resultError (
                                DString
                                  "Recorded result isn't a Stdlib.Http.Response record."
                              )
                          | Some oldFields ->
                            let oldStatus =
                              Map.tryFind "statusCode" oldFields
                              |> Option.bind asInt64
                              |> Option.defaultValue -1L
                            let! oldBody =
                              Map.tryFind "body" oldFields
                              |> Option.map (blobBytes exeState)
                              |> Option.defaultWith (fun () ->
                                uply { return [||] })

                            let statusMatch = oldStatus = newStatus
                            let bodyMatch = oldBody = newBody
                            let verdict =
                              if statusMatch && bodyMatch then "✓ matches"
                              elif not statusMatch && not bodyMatch then
                                "✗ status and body differ"
                              elif not statusMatch then "✗ status differs"
                              else "✗ body differs"

                            let diff =
                              $"Handler:  {handlerName}\n"
                              + "\n"
                              + $"Recorded: HTTP {oldStatus}\n"
                              + $"  body: {renderBody oldBody}\n"
                              + "\n"
                              + $"Replayed: HTTP {newStatus}\n"
                              + $"  body: {renderBody newBody}\n"
                              + "\n"
                              + verdict
                            return resultOk (DString diff)
                        else
                          let summary =
                            $"Handler:  {handlerName}\n"
                            + "\n"
                            + $"Replayed: HTTP {newStatus}\n"
                            + $"  body: {renderBody newBody}"
                          return resultOk (DString summary)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesGetSummary" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "Trace to inspect" ]
      returnType = TypeReference.option (TTuple(TString, TString, []))
      description =
        "Top-line metadata for a trace: (handler_desc, timestamp). Used by `traces inspect` to print a context banner before the annotated source. None if the trace doesn't exist."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! row =
              Sql.query
                "SELECT handler_desc, timestamp FROM traces WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                (read.string "handler_desc", read.string "timestamp"))

            match row with
            | Some(handler, ts) ->
              return
                Dval.optionSome
                  (KTTuple(VT.string, VT.string, []))
                  (DTuple(DString handler, DString ts, []))
            | None -> return Dval.optionNone (KTTuple(VT.string, VT.string, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesGetHandlerName" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "Trace to inspect" ]
      returnType = TypeReference.option TString
      description =
        "Return the qualified name of the handler that produced this trace (the top-level fn_hash, post-fnNameToSimpleString resolution). Returns None for non-HTTP traces, traces with no top-level call, or top-level lambdas."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! row =
              Sql.query
                "SELECT fn_hash FROM trace_fn_calls
                 WHERE trace_id = @traceId AND parent_call_id IS NULL
                 ORDER BY rowid DESC LIMIT 1"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                read.stringOrNone "fn_hash")

            match row with
            | Some(Some name) -> return Dval.optionSome KTString (DString name)
            | _ -> return Dval.optionNone KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesGenTest" 0
      typeParams = []
      parameters =
        [ Param.make "traceID" TString "Trace to generate the fixture from" ]
      returnType = TypeReference.result TString TString
      description =
        "Generate a `testfiles/http-server/*.test` fixture skeleton from a recorded HTTP trace. Request + response wire-format blocks are filled in from the trace; the [http-handler] block has a TODO placeholder for the user to paste the handler body."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString

        let asRecord (d : Dval) : Option<Map<string, Dval>> =
          match d with
          | DRecord(_, _, _, fields) -> Some fields
          | _ -> None
        let asString (d : Dval) : Option<string> =
          match d with
          | DString s -> Some s
          | _ -> None
        let asInt64 (d : Dval) : Option<int64> =
          match d with
          | DInt64 n -> Some n
          | _ -> None

        // Stdlib.Http.{Request,Response}.headers is List<String * String>:
        // a DList of DTuple(DString key, DString value, []).
        let asHeaders (d : Dval) : List<string * string> =
          match d with
          | DList(_, items) ->
            items
            |> List.choose (fun item ->
              match item with
              | DTuple(DString k, DString v, []) -> Some(k, v)
              | _ -> None)
          | _ -> []

        // body : Blob — post-promotion, always Persistent(hash, length).
        let blobBytes (d : Dval) : Ply<byte[]> =
          uply {
            match d with
            | DBlob(Persistent(hash, _)) ->
              let! bs = LibDB.RuntimeTypes.Blob.get hash
              return Option.defaultValue [||] bs
            | _ -> return [||]
          }

        // Common HTTP reason phrases. Unknown codes get a generic
        // placeholder — tests don't assert against the reason anyway,
        // but a real one keeps the fixture readable.
        let reasonPhrase (status : int64) : string =
          match status with
          | 200L -> "OK"
          | 201L -> "Created"
          | 204L -> "No Content"
          | 301L -> "Moved Permanently"
          | 302L -> "Found"
          | 304L -> "Not Modified"
          | 400L -> "Bad Request"
          | 401L -> "Unauthorized"
          | 403L -> "Forbidden"
          | 404L -> "Not Found"
          | 413L -> "Request Entity Too Large"
          | 500L -> "Internal Server Error"
          | _ -> "Status"

        // url is "http://host/path?q" — drop scheme+authority, return
        // just "/path?q" which is the request-target form.
        let urlToRequestTarget (url : string) : string =
          let withoutScheme =
            if url.StartsWith("http://") then url.Substring(7)
            elif url.StartsWith("https://") then url.Substring(8)
            else url
          match withoutScheme.IndexOf('/') with
          | -1 -> "/"
          | i -> withoutScheme.Substring(i)

        // Method is the first token of handler_desc ("GET /foo"); fall
        // back to GET so a malformed desc still produces a runnable
        // fixture skeleton.
        let methodFromHandlerDesc (desc : string) : string =
          match desc.Split(' ') with
          | [||] -> "GET"
          | parts -> parts[0]

        let formatHeaders (headers : List<string * string>) : string =
          headers
          |> List.map (fun (k, v) -> $"{k}: {v}")
          |> String.concat "\n"

        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! row =
              Sql.query
                "SELECT handler_desc, timestamp, input_value_json
                 FROM traces WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                {| handlerDesc = read.string "handler_desc"
                   timestamp = read.string "timestamp"
                   inputValueJson = read.string "input_value_json" |})

            match row with
            | None -> return resultError (DString $"Trace not found: {traceID}")
            | Some r when not (r.handlerDesc.Contains(" /")) ->
              return
                resultError (
                  DString
                    $"Trace {traceID} isn't an HTTP trace (handler_desc: {r.handlerDesc}). gen-test only handles HTTP for now."
                )
            | Some r ->
              let! topRow =
                Sql.query
                  "SELECT result_json, fn_hash FROM trace_fn_calls
                   WHERE trace_id = @traceId AND parent_call_id IS NULL
                   ORDER BY rowid DESC LIMIT 1"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeRowOptionAsync (fun read ->
                  {| resultJson = read.string "result_json"
                     fnName = read.stringOrNone "fn_hash" |})

              match topRow with
              | None ->
                return
                  resultError (
                    DString
                      $"Trace {traceID} has no top-level call result; can't reconstruct response."
                  )
              | Some top ->
                let resultJson = top.resultJson
                let inputDval =
                  DvalReprInternalRoundtrippable.parseJsonV0 r.inputValueJson
                let resultDval =
                  DvalReprInternalRoundtrippable.parseJsonV0 resultJson

                match asRecord inputDval, asRecord resultDval with
                | Some reqFields, Some respFields ->
                  let url =
                    Map.tryFind "url" reqFields
                    |> Option.bind asString
                    |> Option.defaultValue "/"
                  let reqHeaders =
                    Map.tryFind "headers" reqFields
                    |> Option.map asHeaders
                    |> Option.defaultValue []
                  let! reqBodyBytes =
                    Map.tryFind "body" reqFields
                    |> Option.map blobBytes
                    |> Option.defaultWith (fun () -> uply { return [||] })

                  let statusCode =
                    Map.tryFind "statusCode" respFields
                    |> Option.bind asInt64
                    |> Option.defaultValue 200L
                  let respHeaders =
                    Map.tryFind "headers" respFields
                    |> Option.map asHeaders
                    |> Option.defaultValue []
                  let! respBodyBytes =
                    Map.tryFind "body" respFields
                    |> Option.map blobBytes
                    |> Option.defaultWith (fun () -> uply { return [||] })

                  let method = methodFromHandlerDesc r.handlerDesc
                  let path = urlToRequestTarget url
                  let reqBody = UTF8.ofBytesOpt reqBodyBytes |> Option.defaultValue ""
                  let respBody =
                    UTF8.ofBytesOpt respBodyBytes |> Option.defaultValue ""

                  // Drop the recorded Host header (test substitutes HOST
                  // placeholder) and any Server/Date from request side
                  // (rare, but if present they'd confuse the harness).
                  let cleanReqHeaders =
                    reqHeaders
                    |> List.filter (fun (k, _) ->
                      let kl = k.ToLowerInvariant()
                      kl <> "host"
                      && kl <> "content-length"
                      && kl <> "x-http-method")
                  let reqHeaderBlock =
                    if List.isEmpty cleanReqHeaders then ""
                    else formatHeaders cleanReqHeaders + "\n"

                  // Response side: handler-set headers come first, then
                  // the test harness expects auto-injected Server/HSTS/
                  // Content-Length. Date is the normalized placeholder.
                  let cleanRespHeaders =
                    respHeaders
                    |> List.filter (fun (k, _) ->
                      let kl = k.ToLowerInvariant()
                      kl <> "content-length"
                      && kl <> "date"
                      && kl <> "server"
                      && kl <> "strict-transport-security")
                  let respHeaderBlock =
                    if List.isEmpty cleanRespHeaders then ""
                    else formatHeaders cleanRespHeaders + "\n"

                  // Sentinel — Dark side substitutes either the
                  // auto-resolved handler body (via PackageManager
                  // lookup + PrettyPrinter) or a TODO block if the fn
                  // can't be found on the current branch.
                  let fixture =
                    $"[http-handler {method} {path}]\n"
                    + "%%HANDLER_BODY%%\n"
                    + "\n"
                    + "[request]\n"
                    + $"{method} {path} HTTP/1.1\n"
                    + "Host: HOST\n"
                    + $"Date: {r.timestamp}\n"
                    + reqHeaderBlock
                    + $"Content-Length: {reqBodyBytes.Length}\n"
                    + "\n"
                    + reqBody
                    + "\n"
                    + "\n"
                    + "[response]\n"
                    + $"HTTP/1.1 {statusCode} {reasonPhrase statusCode}\n"
                    + "Date: xxx, xx xxx xxxx xx:xx:xx xxx\n"
                    + respHeaderBlock
                    + "Server: darklang\n"
                    + "Strict-Transport-Security: max-age=31536000; includeSubDomains; preload\n"
                    + $"Content-Length: {respBodyBytes.Length}\n"
                    + "\n"
                    + respBody

                  return resultOk (DString fixture)
                | _ ->
                  return
                    resultError (
                      DString
                        "Trace's input or top-level result isn't a record (Stdlib.Http.{Request,Response}); gen-test can't extract fields."
                    )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesImport" 0
      typeParams = []
      parameters =
        [ Param.make "json" TString "JSON exported via tracesExportJson" ]
      returnType = TypeReference.result TString TString
      description =
        "Import a trace from a JSON dump (the `traces export` format). Re-creates the trace row + all fn_call rows under the importing process's scope. Returns the trace ID on success."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString
        (function
        | _, _, _, [ DString json ] ->
          uply {
            try
              use doc = JsonDocument.Parse(json)
              let root = doc.RootElement
              let getStr (key : string) = root.GetProperty(key).GetString()
              let nullableStr (el : JsonElement) (key : string) =
                match el.TryGetProperty(key) with
                | true, prop when prop.ValueKind = JsonValueKind.Null -> None
                | true, prop -> Some(prop.GetString())
                | false, _ -> None

              let id = getStr "id"
              let handlerDesc = getStr "handler_desc"
              let timestamp = getStr "timestamp"
              let inputName = getStr "input_name"
              let inputValueJson = root.GetProperty("input_value").GetRawText()
              let dbScopeStr = string System.Guid.Empty

              // Mirrors TraceStorage.store: INSERT OR REPLACE the trace row,
              // DELETE-then-INSERT the fn_calls. Re-importing the same id
              // overwrites cleanly. root_tlid is synthetic on import; the
              // browsing layer doesn't read it.
              let baseStatements =
                [ "INSERT OR REPLACE INTO traces
                    (id, account_id, root_tlid, handler_desc, timestamp,
                     input_name, input_value_json)
                   VALUES
                    (@id, @accountID, @rootTlid, @handlerDesc, @timestamp,
                     @inputName, @inputValueJson)",
                  [ [ "id", Sql.string id
                      "accountID", Sql.string dbScopeStr
                      "rootTlid", Sql.int64 0L
                      "handlerDesc", Sql.string handlerDesc
                      "timestamp", Sql.string timestamp
                      "inputName", Sql.string inputName
                      "inputValueJson", Sql.string inputValueJson ] ]

                  "DELETE FROM trace_fn_calls WHERE trace_id = @traceId",
                  [ [ "traceId", Sql.string id ] ]

                  "DELETE FROM trace_expr_values WHERE trace_id = @traceId",
                  [ [ "traceId", Sql.string id ] ] ]

              let fnCalls = root.GetProperty("fn_calls")
              let eventStmt =
                if fnCalls.GetArrayLength() = 0 then
                  []
                else
                  // Older exports (pre-duration_ms) lack the field; default to 0.
                  let durationMs (ev : JsonElement) =
                    match ev.TryGetProperty("duration_ms") with
                    | true, prop -> prop.GetInt64()
                    | false, _ -> 0L

                  let rows =
                    fnCalls.EnumerateArray()
                    |> Seq.toList
                    |> List.map (fun ev ->
                      [ "traceId", Sql.string id
                        "callId", Sql.string (ev.GetProperty("call_id").GetString())
                        "parentCallId",
                        Sql.stringOrNone (nullableStr ev "parent_call_id")
                        "kind", Sql.string (ev.GetProperty("kind").GetString())
                        "fnHash", Sql.stringOrNone (nullableStr ev "fn_hash")
                        "lambdaExprId",
                        Sql.stringOrNone (nullableStr ev "lambda_expr_id")
                        "argsJson", Sql.string (ev.GetProperty("args").GetRawText())
                        "resultJson",
                        Sql.string (ev.GetProperty("result").GetRawText())
                        "durationMs", Sql.int64 (durationMs ev) ])
                  [ "INSERT INTO trace_fn_calls
                      (trace_id, call_id, parent_call_id, kind, fn_hash,
                       lambda_expr_id, args_json, result_json, duration_ms)
                     VALUES
                      (@traceId, @callId, @parentCallId, @kind, @fnHash,
                       @lambdaExprId, @argsJson, @resultJson, @durationMs)",
                    rows ]

              // expr_values is optional: pre-`709e2868e` exports won't
              // have it. Skip silently if absent.
              let exprValuesStmt =
                match root.TryGetProperty("expr_values") with
                | true, exprArr when exprArr.GetArrayLength() > 0 ->
                  let rows =
                    exprArr.EnumerateArray()
                    |> Seq.toList
                    |> List.map (fun ev ->
                      [ "traceId", Sql.string id
                        "exprId", Sql.string (ev.GetProperty("expr_id").GetString())
                        "dvalJson",
                        Sql.string (ev.GetProperty("dval").GetRawText()) ])
                  [ "INSERT OR REPLACE INTO trace_expr_values
                      (trace_id, expr_id, dval_json)
                     VALUES
                      (@traceId, @exprId, @dvalJson)",
                    rows ]
                | _ -> []

              let _ =
                Sql.executeTransactionSync
                  (baseStatements @ eventStmt @ exprValuesStmt)
              return resultOk (DString id)
            with ex ->
              return resultError (DString $"Failed to import trace: {ex.Message}")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


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
              Sql.query
                "SELECT COUNT(*) AS c FROM traces WHERE timestamp < @cutoff"
              |> Sql.parameters [ "cutoff", Sql.string cutoffISO ]
              |> Sql.executeRowAsync (fun read -> read.int64 "c")
            let! count = countToDelete

            if count > 0L then
              do!
                Sql.query
                  "DELETE FROM trace_fn_calls
                   WHERE trace_id IN (
                     SELECT id FROM traces WHERE timestamp < @cutoff
                   )"
                |> Sql.parameters [ "cutoff", Sql.string cutoffISO ]
                |> Sql.executeStatementAsync
              do!
                Sql.query
                  "DELETE FROM trace_expr_values
                   WHERE trace_id IN (
                     SELECT id FROM traces WHERE timestamp < @cutoff
                   )"
                |> Sql.parameters [ "cutoff", Sql.string cutoffISO ]
                |> Sql.executeStatementAsync
              do!
                Sql.query "DELETE FROM traces WHERE timestamp < @cutoff"
                |> Sql.parameters [ "cutoff", Sql.string cutoffISO ]
                |> Sql.executeStatementAsync

            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


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
            do! Sql.query "DELETE FROM trace_fn_calls" |> Sql.executeStatementAsync
            do! Sql.query "DELETE FROM trace_expr_values" |> Sql.executeStatementAsync
            do! Sql.query "DELETE FROM traces" |> Sql.executeStatementAsync
            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesDelete" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "Full trace ID to delete" ]
      returnType = TInt64
      description =
        "Delete one trace (and its fn_calls + expr_values). Returns 1 if a row was deleted, 0 otherwise. Caller is responsible for resolving prefixes via tracesResolveID first."
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
                Sql.query
                  "DELETE FROM trace_fn_calls WHERE trace_id = @traceId"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeStatementAsync
              do!
                Sql.query
                  "DELETE FROM trace_expr_values WHERE trace_id = @traceId"
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
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "tracesPruneKeep" 0
      typeParams = []
      parameters =
        [ Param.make "keepN" TInt64 "Number of most-recent traces to keep" ]
      returnType = TInt64
      description =
        "Delete all but the N most-recent traces (and their fn_calls + expr_values). Returns the count deleted. Useful for bounded retention."
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

            if count > 0L then
              do!
                Sql.query
                  "DELETE FROM trace_fn_calls WHERE trace_id IN (
                     SELECT id FROM traces
                     WHERE rowid NOT IN (
                       SELECT rowid FROM traces ORDER BY rowid DESC LIMIT @keepN
                     )
                   )"
                |> Sql.parameters [ "keepN", Sql.int64 keepN ]
                |> Sql.executeStatementAsync
              do!
                Sql.query
                  "DELETE FROM trace_expr_values WHERE trace_id IN (
                     SELECT id FROM traces
                     WHERE rowid NOT IN (
                       SELECT rowid FROM traces ORDER BY rowid DESC LIMIT @keepN
                     )
                   )"
                |> Sql.parameters [ "keepN", Sql.int64 keepN ]
                |> Sql.executeStatementAsync
              do!
                Sql.query
                  "DELETE FROM traces
                   WHERE rowid NOT IN (
                     SELECT rowid FROM traces ORDER BY rowid DESC LIMIT @keepN
                   )"
                |> Sql.parameters [ "keepN", Sql.int64 keepN ]
                |> Sql.executeStatementAsync

            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
