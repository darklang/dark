module LibService.Telemetry

open Prelude
open Prelude.Tablecloth
open Tablecloth

open OpenTelemetry
open OpenTelemetry.Trace
open Honeycomb.OpenTelemetry

open Microsoft.AspNetCore.Http.Extensions

module Internal =
  // CLEANUP: can a DiagnosticSource be used here instead?
  let mutable _source : System.Diagnostics.ActivitySource = null

module Span =
  type T = System.Diagnostics.Activity

  // Spans created here should use `use` instead of `let` so that they are
  // promptly ended. Forgetting to use `use` will cause the end time to be
  // incorrectly delayed
  let root (name : string) : T =
    let result = Internal._source.StartActivity(name)
    assert_ "Telemetry must be initialized before creating root" (result <> null)
    result

  // Spans created here should use `use` instead of `let` so that they are
  // promptly ended. Forgetting to use `use` will cause the end time to be
  // incorrectly delayed
  let child (name : string) (parent : T) : T =
    let result = Internal._source.StartActivity(name).SetParentId parent.Id
    assert_ "Telemetry must be initialized before creating child" (result <> null)
    result

  let addEvent (name : string) (span : T) : unit =
    let (_ : T) = span.AddEvent(System.Diagnostics.ActivityEvent name)
    ()

  let addTag (name : string) (value : string) (span : T) : T =
    span.AddTag(name, value)

  let addTagID (name : string) (value : id) (span : T) : T = span.AddTag(name, value)

  let addTagFloat (name : string) (value : float) (span : T) : T =
    span.AddTag(name, value)

  let addTagInt (name : string) (value : int) (span : T) : T =
    span.AddTag(name, value)

  let addTagUUID (name : string) (value : System.Guid) (span : T) : T =
    span.AddTag(name, value)

  let addTagBool (name : string) (value : bool) (span : T) : T =
    span.AddTag(name, value)

  let addTag' (name : string) (value : string) (span : T) : unit =
    let (_ : T) = span.AddTag(name, value)
    ()

  let addTagID' (name : string) (value : id) (span : T) : unit =
    let (_ : T) = span.AddTag(name, value)
    ()

  let addTagFloat' (name : string) (value : float) (span : T) : unit =
    let (_ : T) = span.AddTag(name, value)
    ()

  let addTagInt' (name : string) (value : int) (span : T) : unit =
    let (_ : T) = span.AddTag(name, value)
    ()

  let addTagUUID' (name : string) (value : System.Guid) (span : T) : unit =
    let (_ : T) = span.AddTag(name, value)
    ()

  let addTagBool' (name : string) (value : bool) (span : T) : unit =
    let (_ : T) = span.AddTag(name, value)
    ()


// Call, passing with serviceName for this service, such as "ApiServer"
let init (serviceName : string) =
  let version = Config.buildHash
  // Not enabled by default - https://jimmybogard.com/building-end-to-end-diagnostics-and-tracing-a-primer-trace-context/
  System.Diagnostics.Activity.DefaultIdFormat <-
    System.Diagnostics.ActivityIdFormat.W3C

  Internal._source <-
    new System.Diagnostics.ActivitySource(
      $"Dark.FSharpBackend.{serviceName}",
      version
    )

let honeycombOptions : HoneycombOptions =
  let options = HoneycombOptions()
  options.ApiKey <- Config.honeycombApiKey
  options.Dataset <- Config.honeycombDataset
  options.Endpoint <- Config.honeycombEndpoint
  options

let enrichHttpRequests
  (options : Instrumentation.AspNetCore.AspNetCoreInstrumentationOptions)
  =
  // https://github.com/open-telemetry/opentelemetry-dotnet/blob/main/src/OpenTelemetry.Instrumentation.AspNetCore/README.md
  let handler =
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
          .SetTag("meta.type", "http_request")
          .SetTag("meta.server_version", Config.buildHash)
          .SetTag("http.remote_addr", httpRequest.Path)
          .SetTag("request.method", httpRequest.Method)
          .SetTag("request.path", httpRequest.Path)
          .SetTag("request.remote_addr", httpRequest.Path)
          .SetTag("request.host", httpRequest.Host)
          .SetTag("request.url", UriHelper.GetEncodedUrl(httpRequest))
          .SetTag(
            ("request.header.user_agent",
             Exception.catch (fun () -> httpRequest.Headers.["User-Agent"].[0])
             |> Option.unwrap "")
          )
        |> ignore<Span.T>
      | "OnStopActivity", (:? Microsoft.AspNetCore.Http.HttpResponse as httpResponse) ->
        activity.SetTag("responseLength", httpResponse.ContentLength)
        |> ignore<Span.T>
      | _ -> ())
  options.Enrich <- handler

let addTelemetry (builder : TracerProviderBuilder) : TracerProviderBuilder =
  builder.Configure(
    (fun _ builder ->
      builder
      |> fun b ->
           match Config.telemetryExporter with
           | Config.Honeycomb ->
             b.AddHoneycomb(honeycombOptions).AddConsoleExporter()
           | Config.NoExporter -> b
           | Config.Console -> b.AddConsoleExporter()
      |> fun b -> b.AddAspNetCoreInstrumentation(enrichHttpRequests)
      |> fun b -> b.AddHttpClientInstrumentation()
      // FSTODO AddSqlClientInstrumentation
      |> ignore<TracerProviderBuilder>)
  )

// An execution ID was an ID in the OCaml version, but sine we're using OpenTelemetry
// from the start, we use the Activity ID instead (note that ASP.NET has
// HttpContext.TraceIdentifier. It's unclear to me why that's different than the
// Activity.ID.
// gets an executionID. The execution ID it returns will change as new
// Activitys/Events are created, so the correct way to use this is to call it at the
// root of execution and pass the result down.
let executionID () = ExecutionID System.Diagnostics.Activity.Current.Id

module Console =
  // For webservers, tracing is added by ASP.NET middlewares. For non-webservers, we
  // also need to add tracing. This does that.
  let loadTelemetry () : unit =
    Sdk
      .CreateTracerProviderBuilder()
      .SetSampler(new AlwaysOnSampler())
      .AddSource("Dark.*")
    |> addTelemetry
    |> fun tp -> tp.Build()
    |> ignore<TracerProvider>

module AspNet =
  open Microsoft.Extensions.DependencyInjection

  let addTelemetryToServices
    (serviceName : string)
    (services : IServiceCollection)
    : IServiceCollection =
    services.AddOpenTelemetryTracing (fun builder ->
      addTelemetry builder |> ignore<TracerProviderBuilder>)
