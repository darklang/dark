module LibService.Telemetry

open Prelude
open Prelude.Tablecloth
open Tablecloth


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

  Internal._source <-
    new System.Diagnostics.ActivitySource(
      $"Dark.FSharpBackend.{serviceName}",
      version
    )

module Console =
  open OpenTelemetry
  open OpenTelemetry.Trace

  // For webservers, tracing is added by middlewares. For non-webservers, we also
  // need to add tracing. This does that.
  let loadTelemetry () : unit =
    let (_ : TracerProvider) =
      Sdk
        .CreateTracerProviderBuilder()
        .SetSampler(new AlwaysOnSampler())
        .AddSource("Dark.*")
        .Build()

    ()

module AspNet =
  open Microsoft.Extensions.DependencyInjection
  open Honeycomb.OpenTelemetry
  open OpenTelemetry.Trace
  open OpenTelemetry.Resources
  open OpenTelemetry.Exporter

  let honeycombOptions : HoneycombOptions =
    let options = HoneycombOptions()
    options.ApiKey <- Config.honeycombApiKey
    options.Dataset <- Config.honeycombDataset
    options.Endpoint <- Config.honeycombEndpoint
    options

  let addTelemetryToServices
    (serviceName : string)
    (services : IServiceCollection)
    : IServiceCollection =
    services.AddOpenTelemetryTracing
      (fun (builder : TracerProviderBuilder) ->
        builder
        |> fun b -> b.AddAspNetCoreInstrumentation()
        |> fun b -> b.AddHttpClientInstrumentation()
        |> fun b ->
             match Config.telemetryExporter with
             | Config.Honeycomb -> b.UseHoneycomb(honeycombOptions)
             | Config.NoExporter -> b
             | Config.Console -> b.AddConsoleExporter()
        |> fun b -> b.Build() |> ignore)
