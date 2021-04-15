module LibBackend.Telemetry

open System.Diagnostics

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
  let root (name : string) : T = Internal._source.StartActivity(name)

  // Spans created here should use `use` instead of `let` so that they are
  // promptly ended. Forgetting to use `use` will cause the end time to be
  // incorrectly delayed
  let child (name : string) (parent : T) : T =
    Internal._source.StartActivity(name).SetParentId parent.Id

  let addEvent (name : string) (span : T) : unit =
    let (_ : Activity) = span.AddEvent(ActivityEvent name)
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
  let version = LibBackend.Config.buildHash

  Internal._source <-
    new ActivitySource($"Dark.FSharpBackend.{serviceName}", version)
