/// Tracing for real execution
module LibCloud.Tracing

open FSharp.Control.Tasks
open System.Threading.Tasks
open Fumble

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module Exe = LibExecution.Execution
module DvalReprInternalHash = LibExecution.DvalReprInternalHash
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable

/// Tracing can go overboard, so use a per-handler feature flag to control it. If
/// sampling is disabled for a canvas, no traces will be recorded to be saved to the
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
  let ruleForHandler (_canvasID : CanvasID) (_tlid : tlid) : T = SampleAll



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

  let forHandler (canvasID : CanvasID) (tlid : tlid) (traceID : AT.TraceID.T) : T =
    let samplingRule = TraceSamplingRule.ruleForHandler canvasID tlid
    fromRule samplingRule traceID

  let shouldTrace (config : T) =
    match config with
    | DoTrace
    | TraceWithTelemetry -> true
    | DontTrace -> false



module TraceResults =
  type T = { tlids : HashSet.HashSet<tlid> }

  let empty () : T = { tlids = HashSet.empty () }



/// Collections of functions and values used during a single execution
type T =
  {
    /// Store the tracing input (varname + dval) for a handler execution
    storeTraceInput : PT.Handler.HandlerDesc -> string -> RT.Dval -> unit

    /// Store the trace results calculated over the execution, if enabled
    storeTraceResults : unit -> unit

    /// The functions to run tracing during execution
    executionTracing : RT.Tracing.Tracing

    /// Results of the execution
    results : TraceResults.T
    enabled : bool
  }


/// Resolve package function hashes to human-readable names
module FnNameCache =
  open LibDB.Db

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
          None

      match result with
      | Some name ->
        cache <- Map.add hash name cache
        name
      | None -> hash


let fnNameToSimpleString (name : RT.FQFnName.FQFnName) : string =
  match name with
  | RT.FQFnName.Builtin b ->
    if b.version = 0 then b.name else $"{b.name}_v{b.version}"
  | RT.FQFnName.Package(RT.Hash h) -> FnNameCache.resolve h


/// Stored function call record for trace data.
/// We keep the raw FQFnName and resolve to a human-readable string at
/// storage time, avoiding synchronous DB lookups on the hot path.
type StoredFnCall =
  { fnName : RT.FQFnName.FQFnName
    argsHash : string
    argsJson : List<string>
    resultJson : string }

/// Store trace data to SQLite
module TraceStorage =
  open LibDB.Db
  open System.Text.Json

  let private serializeArgsList (argsJson : List<string>) : string =
    use stream = new System.IO.MemoryStream()
    use writer = new Utf8JsonWriter(stream)
    writer.WriteStartArray()
    for argJson in argsJson do
      writer.WriteRawValue(argJson)
    writer.WriteEndArray()
    writer.Flush()
    System.Text.Encoding.UTF8.GetString(stream.ToArray())

  let store
    (canvasID : CanvasID)
    (rootTLID : tlid)
    (traceID : AT.TraceID.T)
    (handlerDesc : string)
    (inputVarName : string)
    (inputJson : string)
    (fnCalls : List<StoredFnCall>)
    : unit =
    let traceIdStr = string traceID
    let timestamp = NodaTime.Instant.now().ToString()

    let statements =
      [ // Main trace row
        "INSERT OR REPLACE INTO traces_v0
          (id, trace_id, canvas_id, root_tlid, callgraph_tlids, handler_desc, timestamp)
         VALUES
          (@id, @id, @canvasId, @rootTlid, '', @handlerDesc, @timestamp)",
        [ [ "id", Sql.string traceIdStr
            "canvasId", Sql.string (string canvasID)
            "rootTlid", Sql.int64 (int64 rootTLID)
            "handlerDesc", Sql.string handlerDesc
            "timestamp", Sql.string timestamp ] ]

        // Input
        "INSERT INTO trace_inputs_v0 (trace_id, name, value_json)
         VALUES (@traceId, @name, @valueJson)",
        [ [ "traceId", Sql.string traceIdStr
            "name", Sql.string inputVarName
            "valueJson", Sql.string inputJson ] ]

        // Function results (one param set per call)
        "INSERT INTO trace_fn_results_v0 (trace_id, fn_name, args_hash, hash_version, result_json)
         VALUES (@traceId, @fnName, @argsHash, @hashVersion, @resultJson)",
        fnCalls
        |> List.map (fun fc ->
          [ "traceId", Sql.string traceIdStr
            "fnName", Sql.string (fnNameToSimpleString fc.fnName)
            "argsHash", Sql.string fc.argsHash
            "hashVersion", Sql.int DvalReprInternalHash.currentHashVersion
            "resultJson", Sql.string fc.resultJson ])

        // Function arguments (one param set per call)
        "INSERT INTO trace_fn_arguments_v0 (trace_id, fn_name, args_hash, args_json)
         VALUES (@traceId, @fnName, @argsHash, @argsJson)",
        fnCalls
        |> List.map (fun fc ->
          [ "traceId", Sql.string traceIdStr
            "fnName", Sql.string (fnNameToSimpleString fc.fnName)
            "argsHash", Sql.string fc.argsHash
            "argsJson", Sql.string (serializeArgsList fc.argsJson) ]) ]

    let _ = Sql.executeTransactionSync statements
    ()


/// Shared storeFnResult callback that only records top-level calls (from user code).
/// Internal stdlib-calls-stdlib chains are skipped by checking the execution point.
let private makeStoreFnResult
  (fnCalls : System.Collections.Generic.List<StoredFnCall>)
  : RT.Tracing.StoreFnResult =
  fun ((ep, _), name) args result ->
    match ep with
    | RT.Source ->
      let hash =
        args |> DvalReprInternalHash.hash DvalReprInternalHash.currentHashVersion
      let argsJson =
        args |> NEList.toList |> List.map DvalReprInternalRoundtrippable.toJsonV0
      let resultJson = DvalReprInternalRoundtrippable.toJsonV0 result
      fnCalls.Add(
        { fnName = name
          argsHash = hash
          argsJson = argsJson
          resultJson = resultJson }
      )
    | _ -> ()


/// Shared helper: store a trace to SQLite with error handling
let private storeTrace
  (canvasID : CanvasID)
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  (handlerDesc : string)
  (inputVarName : string)
  (inputJson : string)
  (fnCalls : System.Collections.Generic.List<StoredFnCall>)
  : unit =
  try
    TraceStorage.store
      canvasID
      rootTLID
      traceID
      handlerDesc
      inputVarName
      inputJson
      (Seq.toList fnCalls)
  with ex ->
    print $"[tracing] Failed to store trace: {ex.Message}"


let createSqliteTracer
  (canvasID : CanvasID)
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  : T =
  let results = TraceResults.empty ()
  let fnCalls = System.Collections.Generic.List<StoredFnCall>()
  let mutable storedInputVarName = ""
  let mutable storedInputJson = ""
  let mutable handlerDesc = ""

  { enabled = true
    results = results
    executionTracing =
      { Exe.noTracing with
          storeFnResult = makeStoreFnResult fnCalls
          skipTracing = false }
    storeTraceInput =
      fun desc varname input ->
        let (kind, path, modifier) = desc
        handlerDesc <- $"{kind} {path} {modifier}"
        storedInputVarName <- varname
        storedInputJson <- DvalReprInternalRoundtrippable.toJsonV0 input
    storeTraceResults =
      fun () ->
        storeTrace
          canvasID
          rootTLID
          traceID
          handlerDesc
          storedInputVarName
          storedInputJson
          fnCalls }


let createCliTracer
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  (description : string)
  (inputVarName : string)
  (inputDval : RT.Dval)
  : T =
  let results = TraceResults.empty ()
  let fnCalls = System.Collections.Generic.List<StoredFnCall>()

  // Skip per-fn-call tracing: makeStoreFnResult uses reflection-based JSON
  // (DvalReprInternalRoundtrippable) which is stripped by the .NET trimmer in
  // release/AOT builds. Top-level trace input uses DvalReprDeveloper (string
  // repr, no reflection) as a trim-safe fallback.
  // TODO: use binary serialization or Darklang-native JSON for full trace support.
  { enabled = true
    results = results
    executionTracing =
      { Exe.noTracing with
#if DEBUG
          storeFnResult = makeStoreFnResult fnCalls
#endif
          skipTracing = false }
    storeTraceInput = fun _ _ _ -> ()
    storeTraceResults =
      fun () ->
#if DEBUG
        let inputJson = DvalReprInternalRoundtrippable.toJsonV0 inputDval
#else
        ignore<RT.Dval> inputDval
        let inputJson = "(release: trace serialization unavailable)"
#endif
        storeTrace canvasID 0UL traceID description inputVarName inputJson fnCalls }


let createNonTracer (_canvasID : CanvasID) (_traceID : AT.TraceID.T) : T =
  let results = TraceResults.empty ()
  { enabled = false
    results = results
    executionTracing = LibExecution.Execution.noTracing
    storeTraceResults = fun () -> ()
    storeTraceInput = fun _ _ _ -> () }


let create (canvasID : CanvasID) (rootTLID : tlid) (traceID : AT.TraceID.T) : T =
  let config = TracingConfig.forHandler canvasID rootTLID traceID
  match config with
  | TracingConfig.DoTrace
  | TracingConfig.TraceWithTelemetry -> createSqliteTracer canvasID rootTLID traceID
  | TracingConfig.DontTrace -> createNonTracer canvasID traceID
