module ClientTypes.Init

open Prelude

let init (serviceName : string) =
  print $"Initing ClientTypes in {serviceName}"

  Json.Vanilla.registerConverter (Converters.STJ.WorkerStateConverter())

  // Pusher
  do Json.Vanilla.allow<Pusher.Payload.NewTrace> "ClientTypes.Pusher"
  do Json.Vanilla.allow<Pusher.Payload.NewStaticDeploy> "ClientTypes.Pusher"
  do Json.Vanilla.allow<Pusher.Payload.New404> "ClientTypes.Pusher"

  // API
  // TODO

  // Wasm/Analysis
  // TODO

  print $"Inited ClientTypes in {serviceName}"
