module LibBackend.Traces

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

// -------------------------
// Input variables (including samples)
// -------------------------
let incomplete = RT.DIncomplete RT.SourceNone


let sampleLegacyHttpRequestInputVars : AT.InputVars =
  let sampleRequest : RT.Dval =
    [ ("body", incomplete)
      ("jsonBody", incomplete)
      ("formBody", incomplete)
      ("queryParams", incomplete)
      ("headers", incomplete)
      ("fullBody", incomplete)
      ("url", incomplete) ]
    |> Map
    |> RT.Dval.DObj

  [ ("request", sampleRequest) ]

let sampleBytesHttpRequestInputVars : AT.InputVars =
  let sampleRequest : RT.Dval =
    [ ("body", incomplete)
      ("queryParams", incomplete)
      ("headers", incomplete)
      ("url", incomplete) ]
    |> Map
    |> RT.Dval.DObj

  [ ("request", sampleRequest) ]

let sampleEventInputVars : AT.InputVars = [ ("event", RT.DIncomplete RT.SourceNone) ]

let sampleModuleInputVars (h : PT.Handler.T) : AT.InputVars =
  match h.spec with
  | PT.Handler.HTTP _ -> sampleLegacyHttpRequestInputVars
  | PT.Handler.HTTPBytes _ -> sampleBytesHttpRequestInputVars
  | PT.Handler.Cron _ -> []
  | PT.Handler.REPL _ -> []
  | PT.Handler.UnknownHandler _ ->
    // HttpBytesTODO: sampleBytesHttpRequestInputVars is currently a subset of
    // sampleLegacyHttpRequestInputVars. We could @ it here, but we'd then have
    // 'duplicate' definitions of body, url, etc. What should we do?
    sampleLegacyHttpRequestInputVars @ sampleEventInputVars
  | PT.Handler.Worker _
  | PT.Handler.OldWorker _ -> sampleEventInputVars

let sampleRouteInputVars (h : PT.Handler.T) : AT.InputVars =
  match h.spec with
  | PT.Handler.HTTP (route, _, _)
  | PT.Handler.HTTPBytes (route, _, _) ->
    route
    |> Routing.routeVariables
    |> List.map (fun k -> (k, RT.DIncomplete RT.SourceNone))

  | PT.Handler.Worker _
  | PT.Handler.OldWorker _
  | PT.Handler.Cron _
  | PT.Handler.REPL _
  | PT.Handler.UnknownHandler _ -> []

let sampleInputVars (h : PT.Handler.T) : AT.InputVars =
  sampleModuleInputVars h @ sampleRouteInputVars h

let sampleFunctionInputVars (f : PT.UserFunction.T) : AT.InputVars =
  f.parameters |> List.map (fun p -> (p.name, incomplete))


let savedInputVars
  (h : PT.Handler.T)
  (requestPath : string)
  (event : RT.Dval)
  : AT.InputVars =
  match h.spec with
  | PT.Handler.HTTP (route, _, _)
  | PT.Handler.HTTPBytes (route, _, _) ->
    let boundRouteVariables =
      if route <> "" then
        // Check the trace actually matches the route, if not the client
        // has made a mistake in matching the traceid to this handler, but
        // that might happen due to a race condition. If it does, carry
        // on, if it doesn't -- just don't do any bindings and inject the
        // sample variables. Communicating to the frontend that this
        // trace doesn't match the handler should be done in the future
        // somehow. TODO
        if Routing.requestPathMatchesRoute route requestPath then
          Routing.routeInputVars route requestPath
          |> Exception.unwrapOptionInternal
               "invalid routeInputVars"
               [ "route", route; "requestPath", requestPath ]
        else
          sampleRouteInputVars h
      else
        []

    [ ("request", event) ] @ boundRouteVariables
  | PT.Handler.OldWorker _
  | PT.Handler.Worker _ -> [ ("event", event) ]
  | PT.Handler.Cron _ -> []
  | PT.Handler.UnknownHandler _ -> []
  | PT.Handler.REPL _ -> []


// -------------------------
// Fetch and filter traces
// -------------------------


let handlerTrace
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (h : PT.Handler.T)
  : Task<AT.Trace> =
  task {
    let! event = TraceInputs.loadEventForTrace canvasID traceID

    let input, timestamp =
      match event with
      | Some (requestPath, timestamp, event) ->
        (savedInputVars h requestPath event, timestamp)
      | None -> (sampleInputVars h, NodaTime.Instant.UnixEpoch)

    let! functionResults = TraceFunctionResults.load canvasID traceID h.tlid

    return
      (traceID,
       { input = input; timestamp = timestamp; function_results = functionResults })
  }


let userfnTrace
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (fn : PT.UserFunction.T)
  : Task<AT.Trace> =
  task {
    let! event = TraceFunctionArguments.loadForAnalysis canvasID traceID fn.tlid

    let ivs, timestamp =
      match event with
      | Some (inputVars, timestamp) -> (inputVars, timestamp)
      | None -> (sampleFunctionInputVars fn, NodaTime.Instant.UnixEpoch)

    let! functionResults = TraceFunctionResults.load canvasID traceID fn.tlid

    return
      (traceID,
       { input = ivs; timestamp = timestamp; function_results = functionResults })
  }


let traceIDofTLID (tlid : tlid) : AT.TraceID =
  Uuid.uuidV5 (string tlid) (Uuid.nilNamespace)


let traceIDsForHandler (c : Canvas.T) (h : PT.Handler.T) : Task<List<AT.TraceID>> =
  task {
    match PTParser.Handler.Spec.toEventDesc h.spec with
    | Some desc ->
      let! events = TraceInputs.loadEventIDs c.meta.id desc

      return
        events
        |> List.filterMap (fun (traceID, path) ->

          match h.spec with
          | PT.Handler.Spec.HTTP _
          | PT.Handler.Spec.HTTPBytes _ ->
            // Ensure we only return trace_ids that would bind to this
            // handler if the trace was executed for real now. (There
            // may be other handlers which also match the route)
            c.handlers
            // Filter and order the handlers that would match the trace's path
            |> Map.values
            |> Routing.filterMatchingHandlers path
            |> List.head
            |> Option.bind (fun matching ->
              if matching.tlid = h.tlid then Some traceID else None)

          | PT.Handler.Spec.Worker _
          | PT.Handler.Spec.OldWorker _
          | PT.Handler.Spec.Cron _
          | PT.Handler.Spec.REPL _
          | PT.Handler.Spec.UnknownHandler _ ->
            // Don't use HTTP filtering stack for non-HTTP traces
            Some traceID)
        // If there's no matching traces, add the default trace
        |> (fun traces ->
          match traces with
          | [] -> [ traceIDofTLID h.tlid ]
          | x -> x)
    | None ->
      // If the event description isn't complete, add the default trace
      return [ traceIDofTLID h.tlid ]
  }


let traceIDsForUserFn
  (canvasID : CanvasID)
  (fnTLID : tlid)
  : Task<List<AT.TraceID>> =
  TraceFunctionArguments.loadTraceIDs canvasID fnTLID
