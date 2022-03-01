/// Setup and utilities for using Telemetry.
/// This is where we do tracing/OpenTelemetry/Honeycomb
module LibService.Telemetry

// Note names are confusing in .NET. Here is a good rundown.
// https://github.com/open-telemetry/opentelemetry-dotnet/issues/947
//
// This is a functional-ish API over the built-in .NET tracing/OpenTelemetry
// facilities. .NET uses *implicit* spans (which it calls activities) - creating a
// span sets that span as the thread- and async- local span until it is cleaned up.
// That means to use create a child span you need to do:
//
//   use span = Telemetry.child "name" ["some attrs", 0]
//
// That span will be cleaned up when it goes out of scope. It's also important that
// if you're using Tasks that the `use` goes inside a `task` CE or else it won't be
// appropriately taken care of.
//
// Root spans can be added with:
//
//   Telemetry.createRoot "Root span name"
//
// The appropriate span is then tracked, and so you can add tags and events to the
// current span implicitly by using:
//
//   Telemetry.addTag "some tag" "some value"
//   Telemetry.addtags ["tag1", "value" :> // obj; "tag2", 2 :> obj]
//   Telemetry.addEvent "some event name" []
//
// The type of the value is `obj`, so anything is allowed.

open Prelude
open Prelude.Tablecloth
open Tablecloth

open OpenTelemetry
open OpenTelemetry.Trace
open OpenTelemetry.Resources
open Honeycomb.OpenTelemetry
open Npgsql

open Microsoft.AspNetCore.Http.Extensions

module Internal =
  // initialized via `init`, below
  let mutable _source : System.Diagnostics.ActivitySource = null

module Span =
  // .NET calls them Activity, OpenTelemetry and everyone else calls them Spans
  type T = System.Diagnostics.Activity

  // Spans (Activities) need to stop or they'll have the wrong end-time. You can
  // either use `use` when allocating them, which will mean they are stopped as soon
  // as they go out of scope, or you can explicitly call stop.
  let root (name : string) : T =
    assert_
      "Telemetry must be initialized before creating root"
      (Internal._source <> null)
    // Deliberately created with no parent to make this a root
    // From https://github.com/open-telemetry/opentelemetry-dotnet/issues/984
    System.Diagnostics.Activity.Current <- null
    let span =
      Internal._source.CreateActivity(name, System.Diagnostics.ActivityKind.Internal)
    span.Start()


  // Get the Span/Activity for this execution. It is thread and also async-local.
  // See https://twitter.com/ChetHusk/status/1466589986786971649 For the sake of
  // explicitness, and probably a little bit for performance, only use this when
  // necessary, and prefer to pass created spans around otherwise.

  // It is technically possible for Span/Activities to be null if things are not
  // configured right. The solution there is to fix the configuration, not to allow
  // null checks.
  let current () : T = System.Diagnostics.Activity.Current

  // Spans (Activities) need to stop or they'll have the wrong end-time. You can
  // either use `use` when allocating them, which will mean they are stopped as soon
  // as they go out of scope, or you can explicitly call stop.
  let child (name : string) (parent : T) (tags : Metadata) : T =
    assert_
      "Telemetry must be initialized before creating root"
      (Internal._source <> null)
    let tags = tags |> List.map Tuple2.toKeyValuePair
    let parentId = if parent = null then null else parent.Id
    let kind = System.Diagnostics.ActivityKind.Internal
    // the Sampler is called within this, and only the tags available here will be
    // passed to the sampler
    let result = Internal._source.CreateActivity(name, kind, parentId, tags)
    result.Start()

  let addTag (name : string) (value : obj) (span : T) : unit =
    span.SetTag(name, value) |> ignore<T>

  let addTags (tags : Metadata) (span : T) : unit =
    List.iter (fun (name, value : obj) -> span.SetTag(name, value) |> ignore<T>) tags

  let addEvent (name : string) (tags : Metadata) (span : T) : unit =
    let e = span.AddEvent(System.Diagnostics.ActivityEvent name)
    List.iter (fun (name, value : obj) -> e.SetTag(name, value) |> ignore<T>) tags


// This creates a new root. The correct way to use this is to call `use span =
// Telemetry.child` so that it falls out of scope properly and the parent takes over
// again
let child (name : string) (tags : Metadata) : Span.T =
  Span.child name (Span.current ()) tags

let createRoot (name : string) : Span.T = Span.root name

let addTag (name : string) (value : obj) : unit =
  Span.addTag name value (Span.current ())

let addTags (tags : Metadata) : unit = Span.addTags tags (Span.current ())

let addEvent (name : string) (tags : Metadata) : unit =
  let span = Span.current ()
  let tagCollection = System.Diagnostics.ActivityTagsCollection()
  List.iter (fun (k, v) -> tagCollection[k] <- v) tags
  let event =
    System.Diagnostics.ActivityEvent(
      name,
      NodaTime.Instant.now().ToDateTimeOffset(),
      tagCollection
    )
  span.AddEvent(event) |> ignore<Span.T>

// Add a notification. You can find these in Honeycomb by searching for name =
// 'notification'. This is used for anything out of the ordinary which should be
// inspected.
let notify (name : string) (tags : Metadata) : unit =
  let span = Span.current ()
  let tagCollection = System.Diagnostics.ActivityTagsCollection()
  List.iter (fun (k, v) -> tagCollection[k] <- v) tags
  tagCollection["notification_type"] <- name
  let event =
    System.Diagnostics.ActivityEvent(
      "notification",
      NodaTime.Instant.now().ToDateTimeOffset(),
      tagCollection
    )
  span.AddEvent(event) |> ignore<Span.T>



let addException (e : exn) : unit =
  // https://github.com/open-telemetry/opentelemetry-dotnet/blob/1191a2a3da7be8deae7aa94083b5981cb7610080/src/OpenTelemetry.Api/Trace/ActivityExtensions.cs#L79
  // The .NET RecordException function doesn't take tags, despite it being a MUST in the [spec](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/api.md#record-exception), so we implement our own.
  let exceptionTags =
    [ "exception", true :> obj
      "exception.type", e.GetType().FullName :> obj
      "exception.stacktrace", string e.StackTrace
      "exception.message", e.Message ]
    @ Exception.toMetadata e
  addEvent e.Message exceptionTags




// Call, passing with serviceName for this service, such as "ApiServer"
let init (serviceName : string) : unit =
  print "Configuring Telemetry"
  // Not enabled by default - https://jimmybogard.com/building-end-to-end-diagnostics-and-tracing-a-primer-trace-context/
  System.Diagnostics.Activity.DefaultIdFormat <-
    System.Diagnostics.ActivityIdFormat.W3C

  Internal._source <- new System.Diagnostics.ActivitySource($"Dark")
  // We need all this or .NET will create null Activities
  // https://github.com/dotnet/runtime/issues/45070
  let activityListener = new System.Diagnostics.ActivityListener()
  activityListener.ShouldListenTo <- fun s -> true
  activityListener.SampleUsingParentId <-
    // If we use AllData instead of AllDataAndActivities, the http span won't be recorded
    fun _ -> System.Diagnostics.ActivitySamplingResult.AllDataAndRecorded
  activityListener.Sample <-
    fun _ -> System.Diagnostics.ActivitySamplingResult.AllDataAndRecorded
  System.Diagnostics.ActivitySource.AddActivityListener(activityListener)

  // Allow exceptions to be associated with the right span (the one in which they're created)
  Prelude.exceptionCallback <-
    (fun (e : exn) ->
      // These won't have stacktraces. We could add them but they're expensive, and
      // if they make it to the top they'll get a stacktrace. So let's not do
      // stacktraces until we learn we need them
      addException e)
  // Ensure there is always a root span
  Span.root $"Starting {serviceName}" |> ignore<Span.T>
  print " Configured Telemetry"


let honeycombOptions : HoneycombOptions =
  let options = HoneycombOptions()
  options.ApiKey <- Config.honeycombApiKey
  options.Dataset <- Config.honeycombDataset
  options.Endpoint <- Config.honeycombEndpoint
  options

let configureAspNetCore
  (options : Instrumentation.AspNetCore.AspNetCoreInstrumentationOptions)
  =

  // https://github.com/open-telemetry/opentelemetry-dotnet/blob/main/src/OpenTelemetry.Instrumentation.AspNetCore/README.md
  let enrich =
    (fun (activity : Span.T) (eventName : string) (rawObject : obj) ->
      match (eventName, rawObject) with
      | "OnStartActivity", (:? Microsoft.AspNetCore.Http.HttpRequest as httpRequest) ->
        // CLEANUP
        // The .NET instrumentation uses http.{path,url}, etc, but we used
        // request.whatever in the OCaml version. To make sure that I can compare the
        // old and new requests, I'm also adding request.whatever for now, but they
        // can be removed once it's been switched over. Events are infinitely wide so
        // this shouldn't cause any issues.  This is separate from rollbar data,
        // which is done by the library and does not use the same prefixes
        let ipAddress =
          try
            httpRequest.Headers.["x-forward-for"].[0]
            |> String.split ";"
            |> List.head
            |> Option.unwrap (
              string httpRequest.HttpContext.Connection.RemoteIpAddress
            )
          with
          | _ -> ""
        activity
        |> Span.addTags [ "meta.type", "http_request"
                          "meta.server_version", Config.buildHash
                          "http.remote_addr", ipAddress
                          "request.method", httpRequest.Method
                          "request.path", httpRequest.Path
                          "request.remote_addr", ipAddress
                          "request.host", httpRequest.Host
                          "request.url", httpRequest.GetDisplayUrl()
                          "request.header.user_agent",
                          httpRequest.Headers["User-Agent"] ]
      | "OnStopActivity", (:? Microsoft.AspNetCore.Http.HttpResponse as httpResponse) ->
        activity
        |> Span.addTags [ "response.contentLength", httpResponse.ContentLength
                          "http.contentLength", httpResponse.ContentLength
                          "http.contentType", httpResponse.ContentType ]
      | _ -> ())
  options.Enrich <- enrich
  options.RecordException <- true

type DarkSampler() =
  // A sampler is used to reduce the number of events, to not overwhelm the results.
  // In our case, we want to control costs too - we only have 1.5B honeycomb events
  // per month, and it's easy to use them very quickly in a loop
  inherit OpenTelemetry.Trace.Sampler()
  let keep = SamplingResult(SamplingDecision.RecordAndSample)
  let drop = SamplingResult(SamplingDecision.Drop)
  let getInt (name : string) (map : Map<string, obj>) : Option<int> =
    try
      match Map.get name map with
      | Some result ->
        if typeof<int> = result.GetType() then Some(result :?> int) else None
      | None -> None
    with
    | _ -> None
  let getFloat (name : string) (map : Map<string, obj>) : Option<float> =
    try
      match Map.get name map with
      | Some result ->
        if typeof<float> = result.GetType() then Some(result :?> float) else None
      | None -> None
    with
    | _ -> None

  let getString (name : string) (map : Map<string, obj>) : Option<string> =
    try
      match Map.get name map with
      | Some result ->
        if typeof<string> = result.GetType() then Some(result :?> string) else None
      | None -> None
    with
    | _ -> None

  override this.ShouldSample(p : SamplingParameters inref) : SamplingResult =
    // This turned out to be useless for the initial need (trimming short DB queries)
    keep

let sampler = DarkSampler()

type TraceDBQueries =
  | TraceDBQueries
  | DontTraceDBQueries

let addTelemetry
  (serviceName : string)
  (traceDBQueries : TraceDBQueries)
  (builder : TracerProviderBuilder)
  : TracerProviderBuilder =
  builder
  |> fun b ->
       List.fold
         b
         (fun b exporter ->
           match exporter with
           | Config.Honeycomb -> b.AddHoneycomb(honeycombOptions)
           | Config.Console -> b.AddConsoleExporter())
         Config.telemetryExporters
  |> fun b ->
       b.SetResourceBuilder(ResourceBuilder.CreateDefault().AddService(serviceName))
  |> fun b -> b.AddAspNetCoreInstrumentation(configureAspNetCore)
  |> fun b ->
       b.AddHttpClientInstrumentation (fun options ->
         options.SetHttpFlavor <- true // Record HTTP version
         options.RecordException <- true
         ())
  |> fun b ->
       match traceDBQueries with
       | TraceDBQueries -> b.AddNpgsql()
       | DontTraceDBQueries -> b
  |> fun b -> b.AddSource("Dark")
  |> fun b -> b.SetSampler(sampler)


// An execution ID was an int64 ID in the OCaml version, but since we're using
// OpenTelemetry from the start, we use the Trace ID instead. This should be used to
// create a TraceID for anywhere there's a thread and a trace available. The
// execution ID should be constant no matter when this is called in a thread, but for
// safety, call it at the top and pass it down.
let executionID () = ExecutionID(string System.Diagnostics.Activity.Current.TraceId)

module Console =
  // For webservers, tracing is added by ASP.NET middlewares. For non-webservers, we
  // also need to add tracing. This does that.
  let loadTelemetry (serviceName : string) (traceDBQueries : TraceDBQueries) : unit =
    Sdk.CreateTracerProviderBuilder()
    |> addTelemetry serviceName traceDBQueries
    |> fun tp -> tp.Build()
    |> ignore<TracerProvider>
    // Create a default root span, to ensure that one exists. This span will not be
    // cleaned up, and therefor it will not be printed in real-time (and you won't be
    // able to find it in honeycomb). Instead, start a new root for each "action"
    // (such as a http request, or a loop of the cronchecker)
    Span.root serviceName |> ignore<Span.T>

module AspNet =
  open Microsoft.Extensions.DependencyInjection

  let addTelemetryToServices
    (serviceName : string)
    (traceDBQueries : TraceDBQueries)
    (services : IServiceCollection)
    : IServiceCollection =
    services.AddOpenTelemetryTracing (fun builder ->
      addTelemetry serviceName traceDBQueries builder
      |> ignore<TracerProviderBuilder>)
