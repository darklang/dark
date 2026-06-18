/// Builtin functions for querying the trace store.
/// Companion to `LibDB.Tracing` (the recorder side).
module Builtins.Matter.Libs.Traces

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Sqlite

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
let traceSummaryTypeName () = FQTypeName.fqPackage (TracesRefs.traceSummary ())
let inputVarTypeName () = FQTypeName.fqPackage (TracesRefs.inputVar ())
let fnCallTypeName () = FQTypeName.fqPackage (TracesRefs.fnCall ())
let traceDataTypeName () = FQTypeName.fqPackage (TracesRefs.traceData ())


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
                args, result, duration_ms
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
           argsBytes = read.bytes "args"
           resultBytes = read.bytes "result"
           durationMs = read.int64 "duration_ms" |})

    // Skip rows whose args / result fail to deserialize rather than
    // substitute a placeholder Dval — the downstream renderer expects
    // each FnCall record's `args` / `result` to be the canonical Dval
    // custom type, and a stand-in DString fails the type check at
    // apply time. One bad row used to abort the whole tracesView /
    // call-tree render via an unhandled raise; now we log + drop the
    // row so the rest of the trace still renders.
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
                "durationMs", Dval.int (bigint ev.durationMs) ]
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
      parameters = [ Param.make "limit" TInt "Max number of traces to return" ]
      returnType = TList(TVariable "a")
      description = "List recent traces"
      fn =
        (function
        | _, _, _, [ DInt limitArg ] ->
          let limit = int64 (DarkInt.toBigInt limitArg)
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
      capabilities = LibExecution.Capabilities.noCaps
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
                "SELECT id, handler_desc, timestamp, input_name, input_value
                 FROM traces
                 WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                {| id = read.string "id"
                   handlerDesc = read.string "handler_desc"
                   timestamp = read.string "timestamp"
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "tracesListByFn" 0
      typeParams = []
      parameters =
        [ Param.make "fnName" TString "Function name to search for"
          Param.make "limit" TInt "Max number of traces to return" ]
      returnType = TList(TVariable "a")
      description = "List traces that called a specific function"
      fn =
        (function
        | _, _, _, [ DString fnName; DInt limitArg ] ->
          let limit = int64 (DarkInt.toBigInt limitArg)
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
      capabilities = LibExecution.Capabilities.noCaps
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
        | _, _, _, [ DInt traceLimitArg ] ->
          let traceLimit = int64 (DarkInt.toBigInt traceLimitArg)
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
      capabilities = LibExecution.Capabilities.noCaps
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
        | _, _, _, [ DInt traceLimitArg ] ->
          let traceLimit = int64 (DarkInt.toBigInt traceLimitArg)
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
      capabilities = LibExecution.Capabilities.noCaps
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
      returnType = TList(TVariable "a")
      description =
        "List traces whose recorded input or any fn-call args/result contains the substring (case-sensitive). Match is on the developer-repr form of each Dval."
      fn =
        (function
        | exeState, _, _, [ DString pattern; DInt limitArg ] ->
          let limit = int64 (DarkInt.toBigInt limitArg)
          uply {
            let typeName = traceSummaryTypeName ()

            // Walk traces newest-first; for each, deserialize the
            // input + every fn_call's args/result and check the repr
            // for the pattern. Stop once we've collected `limit`
            // matches. Bounded by `limit` rather than walking the
            // whole table — common case is the user wants the most
            // recent N matches.
            let containsPattern (dv : Dval) : Ply<bool> =
              uply {
                let! repr = Execution.dvalToRepr exeState dv
                return repr.Contains(pattern)
              }

            let! traces =
              Sql.query
                "SELECT id, handler_desc, timestamp, input_value
                 FROM traces
                 ORDER BY rowid DESC"
              |> Sql.executeAsync (fun read ->
                {| id = read.string "id"
                   handler = read.string "handler_desc"
                   timestamp = read.string "timestamp"
                   inputBytes = read.bytes "input_value" |})

            let mutable hits
              : List<{| id : string; handler : string; timestamp : string |}> =
              []
            let mutable cursor = 0

            while cursor < List.length traces && int64 (List.length hits) < limit do
              let t = traces[cursor]
              cursor <- cursor + 1

              let inputDval =
                BinarySer.RT.Dval.deserialize "traces.input_value" t.inputBytes
              let! inputMatches = containsPattern inputDval

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

              if matchesViaCalls then
                hits <-
                  hits
                  @ [ {| id = t.id; handler = t.handler; timestamp = t.timestamp |} ]

            return
              hits
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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

            return Dval.int (bigint count)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "tracesClear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "Ignored" ]
      returnType = TInt
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
            return Dval.int (bigint count)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "tracesDelete" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "Full trace ID to delete" ]
      returnType = TInt
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
            | None -> return Dval.int 0I
            | Some _ ->
              do!
                Sql.query "DELETE FROM trace_fn_calls WHERE trace_id = @traceId"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeStatementAsync
              do!
                Sql.query "DELETE FROM traces WHERE id = @traceId"
                |> Sql.parameters [ "traceId", Sql.string traceID ]
                |> Sql.executeStatementAsync
              return Dval.int 1I
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "tracesPruneKeep" 0
      typeParams = []
      parameters = [ Param.make "keepN" TInt "Number of most-recent traces to keep" ]
      returnType = TInt
      description =
        "Delete all but the N most-recent traces (and their fn_calls). Returns the count deleted. Useful for bounded retention."
      fn =
        (function
        | _, _, _, [ DInt keepNArg ] ->
          let keepN = int64 (DarkInt.toBigInt keepNArg)
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

            return Dval.int (bigint count)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
