module ClientTypes.Init

open Prelude


let init (serviceName : string) =
  print $"Initing ClientTypes in {serviceName}"

  Json.Vanilla.registerConverter (Converters.STJ.WorkerStateConverter())

  // Pusher
  do Json.Vanilla.allow<Pusher.Payload.NewTrace> "ClientTypes.Pusher"
  do Json.Vanilla.allow<Pusher.Payload.NewStaticDeploy> "ClientTypes.Pusher"
  do Json.Vanilla.allow<Pusher.Payload.New404> "ClientTypes.Pusher"
  do Json.Vanilla.allow<Pusher.Payload.AddOpV1> "ClientTypes.Pusher"
  //do Json.Vanilla.allow<Pusher.Payload.AddOpV1PayloadTooBig> "ClientTypes.Pusher" // this is so-far unused
  do Json.Vanilla.allow<Pusher.Payload.UpdateWorkerStates> "ClientTypes.Pusher"

  // API
  // TODO should the allowances be coordiated _here_ or in ApiServer?

  // Wasm/Analysis
  // TODO should the allowances be coordiated _here_ or in Analysis.fsproj/Wasm.fsproj?

  // Regarding the two above TODOs:
  // maybe we'd be well off with `let initPusherPayloads = ...`-style fns, and call
  // _those_ from the appropriate parties 'above'? (rather than one big init fn)

  print $"Inited ClientTypes in {serviceName}"
