/// Setup and utilities for using Telemetry.
/// This is where we do tracing/OpenTelemetry/Honeycomb
module LibService.Telemetry

// Note that names are confusing in .NET. Here is a good rundown.
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

open OpenTelemetry
open OpenTelemetry.Trace
open OpenTelemetry.Resources
open Honeycomb.OpenTelemetry

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
      []
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
      []
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

let rootID () : string =
  let root = System.Diagnostics.Activity.Current.RootId
  if isNull root then "null" else string root

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

/// <summary>Add a notification.</summary>
/// <remarks>
/// You can find these in Honeycomb by searching for name = 'notification'.
/// This is used for anything out of the ordinary which should be inspected.
/// </remarks>
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

let rec exceptionTags (count : int) (e : exn) : Metadata =
  let prefix = if count = 0 then "" else $"inner[{count}]."
  let defaultTags =
    [ $"exception.{prefix}type", e.GetType().FullName :> obj
      $"exception.{prefix}stacktrace", string e.StackTrace
      $"exception.{prefix}message", e.Message ]
  let metadata = Exception.toMetadata e
  let innerTags =
    if (isNull e.InnerException) then
      []
    else
      exceptionTags (count + 1) e.InnerException
  metadata @ defaultTags @ innerTags


let addException (metadata : Metadata) (e : exn) : unit =
  // https://github.com/open-telemetry/opentelemetry-dotnet/blob/1191a2a3da7be8deae7aa94083b5981cb7610080/src/OpenTelemetry.Api/Trace/ActivityExtensions.cs#L79
  // The .NET RecordException function doesn't take tags, despite it being a MUST in the
  // [spec](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/api.md#record-exception),
  // so we implement our own.
  let errorTags = [ "exception", true :> obj; "error", true ]
  let exceptionTags = exceptionTags 0 e
  let tags = metadata @ errorTags @ exceptionTags
  addEvent e.Message tags



let serverTags : List<string * obj> =
  // Ensure that these are all stringified on startup, not when they're added to the
  // span
  let (tags : List<string * string>) =
    [ "meta.environment", Config.envDisplayName
      "meta.server.hash", Config.buildHash
      "meta.server.hostname", Config.hostName
      "meta.server.machinename", string System.Environment.MachineName
      "meta.server.os", string System.Environment.OSVersion
      "meta.dbhost", Config.pgHost
      "meta.process.path", string System.Environment.ProcessPath
      "meta.process.pwd", string System.Environment.CurrentDirectory
      "meta.process.command_line", string System.Environment.CommandLine
      "meta.dotnet.framework.version",
      string System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription ]
  List.map (fun (k, v) -> (k, v :> obj)) tags


let addServerTags (span : Span.T) : unit = Span.addTags serverTags span

// Call, passing with serviceName for this service, such as "BwdServer"
let init (serviceName : string) : unit =
  printTime "Configuring Telemetry"
  // Not enabled by default - https://jimmybogard.com/building-end-to-end-diagnostics-and-tracing-a-primer-trace-context/
  System.Diagnostics.Activity.DefaultIdFormat <-
    System.Diagnostics.ActivityIdFormat.W3C

  Internal._source <- new System.Diagnostics.ActivitySource($"Dark")

  // We need all this or .NET will create null Activities
  // https://github.com/dotnet/runtime/issues/45070
  let activityListener =
    let al = new System.Diagnostics.ActivityListener()

    al.ShouldListenTo <- fun s -> true

    al.SampleUsingParentId <-
      // If we use AllData instead of AllDataAndActivities, the http span won't be recorded
      fun _ -> System.Diagnostics.ActivitySamplingResult.AllDataAndRecorded

    al.Sample <-
      fun _ -> System.Diagnostics.ActivitySamplingResult.AllDataAndRecorded

    // Do it now so that the parent has
    al.ActivityStarted <-
      // Use ParentId instead of Parent as Parent is null in more cases
      (fun span -> if span.ParentId = null then addServerTags span)

    al

  System.Diagnostics.ActivitySource.AddActivityListener(activityListener)

  // Allow exceptions to be associated with the right span (the one in which they're created)
  Exception.exceptionCallback <-
    (fun (e : exn) ->
      // These won't have stacktraces. We could add them but they're expensive, and
      // if they make it to the top they'll get a stacktrace. So let's not do
      // stacktraces until we learn we need them
      addException [] e)

  // Ensure there is always a root span
  Span.root $"Starting {serviceName}" |> ignore<Span.T>

  printTime " Configured Telemetry"


let honeycombOptions (serviceName : string) : HoneycombOptions =
  let options = HoneycombOptions()
  options.ServiceName <- serviceName
  options.ApiKey <- Config.honeycombApiKey
  options.Dataset <- Config.honeycombDataset
  options.Endpoint <- Config.honeycombEndpoint
  options


open System.Collections.Generic
open System.Threading.Tasks

let configureAspNetCore
  (options : Instrumentation.AspNetCore.AspNetCoreTraceInstrumentationOptions)
  =

  // https://github.com/open-telemetry/opentelemetry-dotnet/blob/main/src/OpenTelemetry.Instrumentation.AspNetCore/README.md
  let enrichHttpRequest
    (activity)
    (httpRequest : Microsoft.AspNetCore.Http.HttpRequest)
    =
    let ipAddress =
      try
        httpRequest.Headers["x-forwarded-for"][0]
        |> String.split ","
        |> List.head
        |> Option.unwrap (string httpRequest.HttpContext.Connection.RemoteIpAddress)
      with _ ->
        ""
    let proto =
      try
        httpRequest.Headers["x-forwarded-proto"][0]
      with _ ->
        ""

    activity
    |> Span.addTags
      [ "meta.type", "http_request"
        "http.request.cookies.count", httpRequest.Cookies.Count
        "http.request.content_type", httpRequest.ContentType
        "http.request.content_length", httpRequest.ContentLength
        "http.remote_addr", ipAddress
        "http.proto", proto ]

  let enrichHttpResponse
    activity
    (httpResponse : Microsoft.AspNetCore.Http.HttpResponse)
    =
    activity
    |> Span.addTags
      [ "http.response.content_length", httpResponse.ContentLength
        "http.response.content_type", httpResponse.ContentType ]


  options.EnrichWithHttpRequest <-
    System.Action<System.Diagnostics.Activity, Microsoft.AspNetCore.Http.HttpRequest>
      enrichHttpRequest
  options.EnrichWithHttpResponse <-
    System.Action<System.Diagnostics.Activity, Microsoft.AspNetCore.Http.HttpResponse>
      enrichHttpResponse

  options.RecordException <- true

#nowarn "1182"
#nowarn "9"

/// A sampler is used to reduce the number of events, to not overwhelm the results.
/// In our case, we want to control costs too - we only have 1.5B honeycomb events
/// per month, and it's easy to use them very quickly in a loop
type Sampler(serviceName : string) =
  inherit OpenTelemetry.Trace.Sampler()

  let keep = SamplingResult(SamplingDecision.RecordAndSample)
  let drop = SamplingResult(SamplingDecision.Drop)

  override this.ShouldSample(ps : SamplingParameters inref) : SamplingResult =
    // Sampling means that we lose lot of precision and might miss something. By
    // adding a feature flag, we can dynamically turn up precision when we need it
    // (eg if we can't find something or there's an outage). Ideally, we'd keep error
    // traces all the time, but that's not something that's possible with
    // OpenTelemetry right now.

    // Note we tweak sampling by service, so we can have 100% of one service and 10%
    // of another
    let percentage = LaunchDarkly.telemetrySamplePercentage serviceName
    if percentage >= 100.0 then
      keep
    else
      let scaled = int ((percentage / 100.0) * float System.Int32.MaxValue)
      // Deterministic sampler, will produce the same result for every span in a trace
      // Originally based on https://github.com/open-telemetry/opentelemetry-dotnet/blob/b2fb873fcd9ceca2552b152a60bf192e2ea12b99/src/OpenTelemetry/Trace/TraceIdRatioBasedSampler.cs#LL76
      let traceIDAsInt = ps.TraceId.GetHashCode() |> System.Math.Abs
      if traceIDAsInt < scaled then keep else drop


type TraceDBQueries =
  | TraceDBQueries
  | DontTraceDBQueries

let mutable tracerProvider : TracerProvider = null

/// Flush all Telemetry. Used on shutdown
let flush () : unit =
  if tracerProvider <> null then tracerProvider.ForceFlush() |> ignore<bool>



let addTelemetry
  (serviceName : string)
  (traceDBQueries : TraceDBQueries)
  (builder : TracerProviderBuilder)
  : TracerProviderBuilder =
  builder
  |> fun b -> b.SetSampler(Sampler(serviceName))
  |> fun b ->
      List.fold
        (fun (b : TracerProviderBuilder) exporter ->
          match exporter with
          | Config.Honeycomb -> b.AddHoneycomb(honeycombOptions serviceName)
          | Config.Console -> b.AddConsoleExporter())
        b
        Config.telemetryExporters
  |> fun b ->
      b.SetResourceBuilder(ResourceBuilder.CreateDefault().AddService(serviceName))
  |> fun b ->
      b.AddHttpClientInstrumentation(fun options ->
        options.RecordException <- true
        ())
  // TODO HttpClient instrumentation isn't working, so let's try to add it
  // before AspNetCoreInstrumentation
  |> fun b -> b.AddAspNetCoreInstrumentation(configureAspNetCore)
  // |> fun b ->
  //     match traceDBQueries with
  //     | TraceDBQueries -> b.AddNpgsql()
  //     | DontTraceDBQueries -> b
  |> fun b -> b.AddSource("Dark")


module Console =
  /// For webservers, tracing is added by ASP.NET middlewares.
  /// For non-webservers, we also need to add tracing. This does that.
  let loadTelemetry (serviceName : string) (traceDBQueries : TraceDBQueries) : unit =
    let tp =
      Sdk.CreateTracerProviderBuilder()
      |> addTelemetry serviceName traceDBQueries
      |> fun tp -> tp.Build()
    tracerProvider <- tp

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
    services
      .AddOpenTelemetry()
      .WithTracing(fun builder ->
        // TODO: save tracerProvider to flush when it's finished
        addTelemetry serviceName traceDBQueries builder
        |> ignore<TracerProviderBuilder>
        ())
      .Services
