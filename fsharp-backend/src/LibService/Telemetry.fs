module LibService.Telemetry

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
  // CLEANUP: can a DiagnosticSource be used here instead?
  let mutable _source : System.Diagnostics.ActivitySource = null

module Span =
  // .NET calls them Activity, OpenTelemetry and everyone else calls them Spans
  type T = System.Diagnostics.Activity

  // Spans created here should use `use`, or explicitly call `.stop` Forgetting to do
  // this will cause the end time to be incorrectly delayed

  // Create a root if there is not one already (typically prefer calling [current] instead).
  let root (name : string) : T =
    let result = Internal._source.StartActivity(name)
    assert_ "Telemetry must be initialized before creating root" (result <> null)
    result

  // It is technically possible for Span/Activities to be null if things are not
  // configured right. The solution there is to fix the configuration, not to allow
  // null checks.
  let current () : T = System.Diagnostics.Activity.Current

  let child (name : string) (parent : T) : T =
    let result = Internal._source.StartActivity(name).SetParentId(parent.Id)
    result.DisplayName <- name
    assert_ "Telemetry must be initialized before creating child" (result <> null)
    result

  let addTag (name : string) (value : obj) (span : T) : unit =
    span.AddTag(name, value) |> ignore<T>

  let addTags (tags : List<string * obj>) (span : T) : unit =
    List.iter (fun (name, value : obj) -> span.AddTag(name, value) |> ignore<T>) tags

  let addEvent (name : string) (tags : List<string * obj>) (span : T) : unit =
    let e = span.AddEvent(System.Diagnostics.ActivityEvent name)
    List.iter (fun (name, value : obj) -> e.AddTag(name, value) |> ignore<T>) tags


let span (name : string) (tags : List<string * obj>) : Span.T =
  let span = Span.child name (Span.current ())
  List.iter
    (fun (name, value : obj) -> span.AddTag(name, value) |> ignore<Span.T>)
    tags
  span

let addTag (name : string) (value : obj) : unit =
  Span.addTag name value (Span.current ())

let addTags (tags : List<string * obj>) : unit = Span.addTags tags (Span.current ())

let addEvent (name : string) (tags : List<string * obj>) : unit =
  let span = Span.current ()
  let e = span.AddEvent(System.Diagnostics.ActivityEvent name)
  Span.addTags tags e

let addError (name : string) (tags : List<string * obj>) : unit =
  addEvent name (("level", "error") :: tags)




// Call, passing with serviceName for this service, such as "ApiServer"
let init (serviceName : string) =
  let version = Config.buildHash
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

  // Make sure exceptions make it into telemetry as soon as they're called
  Prelude.exceptionCallback <-
    (fun typ msg tags ->
      addError msg (("exception", true) :: ("exceptionType", typ) :: tags))


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
        // The .NET instrumentation uses http.{path,url}, etc, but we used
        // request.whatever in the OCaml version. To make sure that I can compare
        // the old and new requests, I'm also adding request.whatever for now, but
        // they can be removed once it's been switched over. Events are infinitely
        // wide so this shouldn't cause any issues.
        // ; ("execution_id", `String (Types.string_of_id execution_id)) // FSTODO
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

let addTelemetry
  (name : string)
  (builder : TracerProviderBuilder)
  : TracerProviderBuilder =
  builder
  |> fun b ->
       match Config.telemetryExporter with
       | Config.Honeycomb -> b.AddHoneycomb(honeycombOptions).AddConsoleExporter()
       | Config.NoExporter -> b
       | Config.Console -> b.AddConsoleExporter()
  |> fun b -> b.SetResourceBuilder(ResourceBuilder.CreateDefault().AddService(name))
  |> fun b -> b.AddAspNetCoreInstrumentation(configureAspNetCore)
  |> fun b -> b.AddHttpClientInstrumentation()
  |> fun b -> b.AddNpgsql()
  |> fun b -> b.AddSource("Dark")


// An execution ID was an int64 ID in the OCaml version, but since we're using
// OpenTelemetry from the start, we use the Trace ID instead. This should be used to
// create a TraceID for anywhere there's a thread and a trace available. The
// execution ID should be constant no matter when this is called in a thread, but for
// safety, call it at the top and pass it down.
let executionID () = ExecutionID(string System.Diagnostics.Activity.Current.TraceId)

module Console =
  // For webservers, tracing is added by ASP.NET middlewares. For non-webservers, we
  // also need to add tracing. This does that.
  let loadTelemetry (serviceName : string) : unit =
    Sdk.CreateTracerProviderBuilder().SetSampler(new AlwaysOnSampler())
    |> addTelemetry serviceName
    |> fun tp -> tp.Build()
    |> ignore<TracerProvider>

module AspNet =
  open Microsoft.Extensions.DependencyInjection

  let addTelemetryToServices
    (serviceName : string)
    (services : IServiceCollection)
    : IServiceCollection =
    services.AddOpenTelemetryTracing (fun builder ->
      addTelemetry serviceName builder |> ignore<TracerProviderBuilder>)
